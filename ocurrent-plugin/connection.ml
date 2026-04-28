open Capnp_rpc.Std

module Cluster_api = Cluster_api_eio

module Metrics = struct
  open Prometheus

  let namespace = "ocluster"
  let subsystem = "ocurrent"

  let queue =
    let help = "Items in cluster queue by state" in
    Gauge.v_label ~label_name:"state" ~help ~namespace ~subsystem "queue_state"

  let queue_connect = queue "connect"
  let queue_rate_limit = queue "rate-limit"
  let queue_get_ticket = queue "get-ticket"
  let queue_get_worker = queue "get-worker"
end

(* The scheduler connection state machine. We hold at most one live
   capability; when it's lost we transparently reconnect on the next call. *)
type sched_state =
  | Disconnected
  | Connecting of Cluster_api.Submission.t Eio.Promise.t
  | Connected  of Cluster_api.Submission.t

type t = {
  sw : Eio.Switch.t;
  clock : float Eio.Time.clock_ty Eio.Resource.t;
  sr : [`Submission_f4e8a768b32a7c42] Sturdy_ref.t;
  mutable sched : sched_state;
  mu : Eio.Mutex.t;
  (* Limit how many items we queue up at the scheduler (including
     assigned to workers) for each (OCluster pool, urgency). *)
  rate_limits : ((string * bool), Eio.Semaphore.t) Hashtbl.t;
  max_pipeline : int;
}

let with_state metric fn =
  Prometheus.Gauge.inc_one metric;
  Fun.protect fn ~finally:(fun () -> Prometheus.Gauge.dec_one metric)

(* Return a working scheduler capability. If we don't have one, connect
   (with retry); only one fiber does the work and others wait. *)
let sched ~job t =
  let try_use cap =
    if Capability.problem cap = None then Some cap else None
  in
  let connect () =
    let p, r = Eio.Promise.create () in
    t.sched <- Connecting p;
    Eio.Fiber.fork ~sw:t.sw (fun () ->
      let rec aux () =
        match
          let cap = Sturdy_ref.connect_exn t.sr in
          Capability.await_settled_exn cap;
          cap
        with
        | cap ->
          t.sched <- Connected cap;
          Eio.Promise.resolve r cap
        | exception ex ->
          Log.warn (fun f -> f "Error connecting to build cluster (will retry): %a" Fmt.exn ex);
          Eio.Time.sleep t.clock 10.0;
          aux ()
      in
      aux ());
    p
  in
  Eio.Mutex.use_rw ~protect:false t.mu (fun () ->
    match t.sched with
    | Connected cap ->
      (match try_use cap with
       | Some cap -> `Ready cap
       | None ->
         Current.Job.log job "Connecting to build cluster…";
         t.sched <- Disconnected;
         `Wait (connect ()))
    | Connecting p -> `Wait p
    | Disconnected ->
      Current.Job.log job "Connecting to build cluster…";
      `Wait (connect ()))
  |> function
  | `Ready cap -> cap
  | `Wait p -> Eio.Promise.await p

let rate_limit t pool urgent =
  let key = (pool, urgent) in
  match Hashtbl.find_opt t.rate_limits key with
  | Some limiter -> limiter
  | None ->
    let limiter = Eio.Semaphore.make t.max_pipeline in
    Hashtbl.add t.rate_limits key limiter;
    limiter

let urgent_if_high = function
  | `High -> true
  | `Low -> false

(* Called by [Current.Pool.of_fn] once the confirmation threshold and
   fiber-level cancellation allow the job to be submitted. Cancellation
   is wired through [sw]: the [Switch.on_release] hooks below release
   the rate-limit slot and cancel the scheduler ticket when the job's
   switch is torn down, so we don't need to use [~register_cancel]. *)
let submit ~job ~pool ~action ~cache_hint ?src ?secrets ~urgent t ~priority ~sw ~register_cancel:_ =
  let urgent = urgent priority in
  let rec aux () =
    let sched = with_state Metrics.queue_connect (fun () -> sched ~job t) in
    let limiter = rate_limit t pool urgent in
    Prometheus.Gauge.inc_one Metrics.queue_rate_limit;
    Eio.Semaphore.acquire limiter;
    Prometheus.Gauge.dec_one Metrics.queue_rate_limit;
    let release_limiter = ref (fun () -> Eio.Semaphore.release limiter) in
    Eio.Switch.on_release sw (fun () -> !release_limiter ());
    let ticket =
      Cluster_api.Submission.submit ~urgent ?src ?secrets sched
        ~pool ~action ~cache_hint
    in
    let build_job = Cluster_api.Ticket.job ticket in
    (* From here on, releasing the switch should cancel the ticket. *)
    let cancel_ticket = ref (fun () ->
      match Cluster_api.Ticket.cancel ticket with
      | Ok () -> ()
      | Error (`Capnp e) -> Current.Job.log job "Cancel ticket failed: %a" Capnp_rpc.Error.pp e)
    in
    Eio.Switch.on_release sw (fun () -> !cancel_ticket ());
    match
      with_state Metrics.queue_get_ticket
        (fun () -> Capability.await_settled ticket)
    with
    | Error _ ->
      (* Disconnected before the ticket settled. Drop and retry. *)
      cancel_ticket := ignore;
      Capability.dec_ref ticket;
      Capability.dec_ref build_job;
      release_limiter := ignore;
      Eio.Semaphore.release limiter;
      aux ()
    | Ok () ->
      Current.Job.log job "Waiting for worker…";
      match
        with_state Metrics.queue_get_worker
          (fun () -> Capability.await_settled build_job)
      with
      | Error err ->
        cancel_ticket := ignore;
        Capability.dec_ref ticket;
        if Capability.problem sched = None then (
          Capability.dec_ref build_job;
          Fmt.failwith "%a" Capnp_rpc.Exception.pp err
        ) else (
          Capability.dec_ref build_job;
          release_limiter := ignore;
          Eio.Semaphore.release limiter;
          aux ()
        )
      | Ok () ->
        Capability.dec_ref ticket;
        cancel_ticket := ignore;
        build_job
  in
  aux ()

let tail ~job build_job =
  let rec aux start =
    match Cluster_api.Job.log build_job start with
    | Error (`Capnp e) -> Fmt.error_msg "%a" Capnp_rpc.Error.pp e
    | Ok ("", _) -> Ok ()
    | Ok (data, next) ->
      Current.Job.write job data;
      aux next
  in aux 0L

let run_job ~job build_job =
  let on_cancel _ =
    match Cluster_api.Job.cancel build_job with
    | Ok () -> ()
    | Error (`Capnp e) -> Current.Job.log job "Cancel failed: %a" Capnp_rpc.Error.pp e
  in
  Current.Job.with_handler job ~on_cancel @@ fun () ->
  match tail ~job build_job with
  | Error _ as e -> e
  | Ok () ->
    match Cluster_api.Job.result build_job with
    | Error (`Capnp e) -> Error (`Msg (Fmt.to_to_string Capnp_rpc.Error.pp e))
    | Ok _ as x -> x

let create ?(max_pipeline=200) ~sw ~clock sr =
  let rate_limits = Hashtbl.create 10 in
  { sw; clock; sr; sched = Disconnected; mu = Eio.Mutex.create (); rate_limits; max_pipeline }

let pool ~job ~pool ~action ~cache_hint ?src ?secrets ?(urgent=urgent_if_high) t =
  Current.Pool.of_fn ~label:"OCluster" @@ submit ~job ~pool ~action ~cache_hint ~urgent ?src ?secrets t
