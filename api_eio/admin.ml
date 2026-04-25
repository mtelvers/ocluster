open Capnp_rpc.Std

let local ~pools ~pool ~add_client ~remove_client ~list_clients =
  let module X = Raw.Service.Admin in
  X.local @@ object
    inherit X.service

    method pools_impl _params release_param_caps =
      let open X.Pools in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      Results.names_set_list results (pools ()) |> ignore;
      Service.return response

    method pool_impl params release_param_caps =
      let open X.Pool in
      let name = Params.name_get params in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      let cap = pool name in
      Results.pool_set results (Some cap);
      Capability.dec_ref cap;
      Service.return response

    method add_client_impl params release_param_caps =
      let open X.AddClient in
      let id = Params.id_get params in
      release_param_caps ();
      match add_client id with
      | Error (`Capnp e) -> Service.error e
      | Error (`Msg m) -> Service.fail "%s" m
      | Ok cap ->
        let response, results = Service.Response.create Results.init_pointer in
        Results.cap_set results (Some cap);
        Capability.dec_ref cap;
        Service.return response

    method remove_client_impl params release_param_caps =
      let open X.RemoveClient in
      let id = Params.id_get params in
      release_param_caps ();
      match remove_client id with
      | Error (`Capnp e) -> Service.error e
      | Error (`Msg m) -> Service.fail "%s" m
      | Ok () -> Service.return_empty ()

    method list_clients_impl _params release_param_caps =
      let open X.ListClients in
      release_param_caps ();
      let clients = list_clients () in
      let response, results = Service.Response.create Results.init_pointer in
      let _ : _ Capnp.Array.t = Results.clients_set_list results clients in
      Service.return response
  end

module X = Raw.Client.Admin

type t = X.t Capability.t

let pools t =
  let open X.Pools in
  let request = Capability.Request.create_no_args () in
  let results = Capability.call_for_value_exn t method_id request in
  Results.names_get_list results

let pool t name =
  let open X.Pool in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.name_set params name;
  Capability.call_for_caps t method_id request Results.pool_get_pipelined

let add_client t id =
  let open X.AddClient in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.id_set params id;
  Capability.call_for_caps t method_id request Results.cap_get_pipelined

let remove_client t id =
  let open X.RemoveClient in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.id_set params id;
  Capability.call_for_unit_exn t method_id request

let list_clients t =
  let open X.ListClients in
  let request = Capability.Request.create_no_args () in
  let results = Capability.call_for_value_exn t method_id request in
  Results.clients_get_list results
