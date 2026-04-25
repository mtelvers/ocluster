open Capnp_rpc.Std

type job_desc = Raw.Reader.JobDescr.t

let local ~pop ~set_active ~release =
  let module X = Raw.Service.Queue in
  X.local @@ object
    inherit X.service

    method pop_impl params release_param_caps =
      let open X.Pop in
      let job = Params.job_get params in
      release_param_caps ();
      match job with
      | None -> Service.fail "Missing job!"
      | Some job ->
        Capability.with_ref job @@ fun job ->
        match pop ~job with
        | Error (`Capnp e) -> Service.error e
        | Error (`Msg m) -> Service.fail "%s" m
        | Ok descr ->
          let response, results = Service.Response.create Results.init_pointer in
          let _ : Raw.Builder.JobDescr.t = Results.descr_set_reader results descr in
          Service.return response

    method set_active_impl params release_param_caps =
      let open X.SetActive in
      let active = Params.active_get params in
      release_param_caps ();
      set_active active;
      Service.return_empty ()

    method! release = release ()
  end

module X = Raw.Client.Queue

type t = X.t Capability.t

(* Note: this operation can be cancelled by Eio fiber cancellation, so
   make sure [call_for_value_exn] is the only blocking operation. *)
let pop t job =
  let open X.Pop in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.job_set params (Some job);
  let results = Capability.call_for_value_exn t method_id request in
  Results.descr_get results

let set_active t active =
  let open X.SetActive in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.active_set params active;
  Capability.call_for_unit_exn t method_id request
