open Capnp_rpc.Std

type additional_metric = {
  content_type : string;
  data : string;
}

let local ?(additional_metric = (fun _ -> Ok None)) ~metrics ~self_update () =
  let module X = Raw.Service.Worker in
  X.local @@ object
    inherit X.service

    method metrics_impl params release_param_caps =
      let open X.Metrics in
      let source = Params.source_get params in
      release_param_caps ();
      let collect source =
        match metrics source with
        | Ok (content_type, data) ->
          let response, results = Service.Response.create Results.init_pointer in
          Results.content_type_set results content_type;
          Results.data_set results data;
          Service.return response
        | Error (`Msg msg) -> Service.fail "%s" msg
      in
      match source with
      | Agent -> collect `Agent
      | Host -> collect `Host
      | Undefined _ -> Service.fail "Unknown metrics source"

    method additional_metric_impl params release_param_caps =
      let open X.AdditionalMetric in
      let module B = Raw.Builder.Metric in
      let source = Params.source_get params in
      release_param_caps ();
      match additional_metric source with
      | Ok (Some (content_type, data)) ->
        let response, results = Service.Response.create Results.init_pointer in
        let b = B.init_root () in
        B.content_type_set b content_type;
        B.data_set b data;
        let am = Raw.Builder.AdditionalMetric.init_root () in
        let _ = Raw.Builder.AdditionalMetric.metric_set_builder am b in
        let _ = Results.metric_set_builder results am in
        Service.return response
      | Ok None ->
        let response, results = Service.Response.create Results.init_pointer in
        let am = Raw.Builder.AdditionalMetric.init_root () in
        let _ = Raw.Builder.AdditionalMetric.not_reported_set am in
        let _ = Results.metric_set_builder results am in
        Service.return response
      | Error (`Msg msg) -> Service.fail "%s" msg

    method self_update_impl _params release_param_caps =
      release_param_caps ();
      match self_update () with
      | Error (`Msg m) -> Service.fail "%s" m
      | Ok () -> Service.return_empty ()
  end

module X = Raw.Client.Worker

type t = X.t Capability.t

let metrics t ~source =
  let open X.Metrics in
  let request, params = Capability.Request.create Params.init_pointer in
  let source =
    match source with
    | `Agent -> Raw.Builder.Worker.MetricsSource.Agent
    | `Host -> Raw.Builder.Worker.MetricsSource.Host
  in
  Params.source_set params source;
  match Capability.call_for_value t method_id request with
  | Error _ as e -> e
  | Ok results -> Ok (Results.content_type_get results, Results.data_get results)

let additional_metrics ~extra t =
  let open X.AdditionalMetric in
  let module R = Raw.Reader.AdditionalMetric in
  let request, params = Capability.Request.create Params.init_pointer in
  Params.source_set params extra;
  match Capability.call_for_value t method_id request with
  | Error _ as e -> e
  | Ok result ->
    let metric = Results.metric_get result in
    match R.get metric with
    | R.Metric m ->
      let content_type = Raw.Reader.Metric.content_type_get m in
      let data = Raw.Reader.Metric.data_get m in
      Ok (Some { content_type; data })
    | R.NotReported -> Ok None
    | R.Undefined i -> Error (`Capnp (Capnp_rpc.Error.exn "Undefined %i" i))

let self_update t =
  let open X.SelfUpdate in
  let request = Capability.Request.create_no_args () in
  match Capability.call_for_unit t method_id request with
  | Ok () -> failwith "update reported success, but should have failed with a disconnection error!"
  | Error (`Capnp (`Exception {ty = `Disconnected; _})) -> Ok ()
  | Error _ as e -> e
