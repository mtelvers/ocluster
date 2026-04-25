open Capnp_rpc.Std

type t = Raw.Service.Job.t Capability.t

(* [cancel] is invoked when the client calls [cancel] or the capability is
   released. Typically the caller wires this up to whatever signals the
   job-running fiber to exit (resolving an Eio.Promise, calling
   Eio.Cancel.cancel on a context, etc.).
   [outcome] is awaited when the client requests the result; it should
   resolve with the build outcome.
   [stream_log_data] returns the next chunk of log content (or "" for EOF). *)
let local ~cancel ~outcome ~stream_log_data =
  let module X = Raw.Service.Job in
  X.local @@ object
    inherit X.service

    method log_impl params release_param_caps =
      let open X.Log in
      release_param_caps ();
      let start = Params.start_get params in
      let log, next = stream_log_data ~start in
      let response, results = Service.Response.create Results.init_pointer in
      Results.log_set results log;
      Results.next_set results next;
      Service.return response

    method result_impl _params release_param_caps =
      let open X.Result in
      release_param_caps ();
      match Eio.Promise.await outcome with
      | Error (`Msg m) -> Service.fail "%s" m
      | Ok output ->
        let response, results = Service.Response.create Results.init_pointer in
        Results.output_set results output;
        Service.return response

    method! release =
      cancel ()

    method cancel_impl _params release_param_caps =
      release_param_caps ();
      cancel ();
      Service.return_empty ()
  end

module X = Raw.Client.Job

let log t start =
  let open X.Log in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.start_set params start;
  match Capability.call_for_value t method_id request with
  | Error _ as e -> e
  | Ok x -> Ok (Results.log_get x, Results.next_get x)

let result t =
  let open X.Result in
  let request = Capability.Request.create_no_args () in
  match Capability.call_for_value t method_id request with
  | Error _ as e -> e
  | Ok response -> Ok (Results.output_get response)

let cancel t =
  let open X.Cancel in
  let request = Capability.Request.create_no_args () in
  Capability.call_for_unit t method_id request
