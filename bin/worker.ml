open Lwt.Infix

let setup_log ?style_renderer ?formatter default_level =
  (* Smother Cap'n Proto and TLS logging sources *)
  let levels = ["capnp-rpc"; "tls.config"; "tls.tracing"; "endpoint"]
               |> List.map (fun src -> src, Logs.Warning) in
  Prometheus_unix.Logging.init ?formatter ?default_level ~levels ();
  Fmt_tty.setup_std_outputs ?style_renderer ();
  ()

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let check_exit_status = function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED x -> Fmt.failwith "Sub-process failed with exit code %d" x
  | Unix.WSIGNALED x -> Fmt.failwith "Sub-process failed with signal %d" x
  | Unix.WSTOPPED x -> Fmt.failwith "Sub-process stopped with signal %d" x

module Self_update = struct
  let service = "builder_agent"
  let repo = "ocurrent/ocluster-worker"
  let tag = "live"
end

let update_docker () =
  let image_name = Printf.sprintf "%s:%s" Self_update.repo Self_update.tag in
  Lwt_process.exec ("", [| "docker"; "pull"; image_name |]) >|= check_exit_status >>= fun () ->
  Lwt_process.pread_line ("", [| "docker"; "image"; "inspect"; "-f";
                                 "{{ range index .RepoDigests }}{{ . }} {{ end }}"; "--"; image_name |]) >|= fun new_repo_ids ->
  let new_repo_ids = Astring.String.cuts ~sep:" " new_repo_ids in
  let affix = Self_update.repo ^ "@" in
  match List.find_opt (Astring.String.is_prefix ~affix) new_repo_ids with
  | None ->
    Fmt.failwith "No new image starts with %S!" affix
  | Some id ->
    Logs.info (fun f -> f "Latest service version is %s" id);
    fun () ->
      Lwt_process.exec ("", [| "docker"; "service"; "update"; "--image"; id; Self_update.service |])
      >|= check_exit_status

(* Respond to update requests by doing nothing, on the assumption that the
   admin has updated the local package version. *)
let update_normal () =
  Lwt.return (fun () -> Lwt.return_unit)

let main ?style_renderer level ?formatter registration_path capacity name allow_push healthcheck_period prune_threshold docker_max_df_size obuilder_prune_threshold obuilder_prune_limit state_dir obuilder additional_metrics day10_cache day10_prune_threshold day10_cache_max_size day10_rebase_period day10_rebase_days =
  setup_log ?style_renderer ?formatter level;
  let update =
    if Sys.file_exists "/.dockerenv" then update_docker
    else update_normal
  in
  let _check_for_duplicates =
    List.fold_left (fun acc (v, _) -> if List.mem v acc then failwith ("Duplicate additional metrics name: " ^ v) else v :: acc) [] additional_metrics
  in
  Lwt_main.run begin
    let vat = Capnp_rpc_unix.client_only_vat () in
    let sr = Capnp_rpc_unix.Cap_file.load vat registration_path |> or_die in
    Cluster_worker.run ~capacity ~name ~allow_push ~healthcheck_period ?prune_threshold ?docker_max_df_size ~obuilder_prune_threshold ~obuilder_prune_limit ?obuilder ~additional_metrics ~state_dir ?day10_cache ?day10_prune_threshold ?day10_cache_max_size ?day10_rebase_period ~day10_rebase_days ~update sr
  end

(* Command-line parsing *)
let main ~install (style_renderer, args1) (level, args2) ((registration_path, capacity, name, allow_push, healthcheck_period, prune_threshold, docker_max_df_size, obuilder_prune_threshold, obuilder_prune_limit, state_dir, obuilder, additional_metrics, day10_cache, day10_prune_threshold, day10_cache_max_size, day10_rebase_period, day10_rebase_days), args3) =
  if install then
    Ok (Winsvc_wrapper.install name "OCluster Worker" "Run a build worker" (args1 @ args2 @ args3))
  else
    Ok (Winsvc_wrapper.run name state_dir (fun ?formatter () ->
      main ?style_renderer level ?formatter registration_path capacity name allow_push healthcheck_period prune_threshold docker_max_df_size obuilder_prune_threshold obuilder_prune_limit state_dir obuilder additional_metrics day10_cache day10_prune_threshold day10_cache_max_size day10_rebase_period day10_rebase_days))


open Cmdliner

let worker_name =
  Arg.required @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"Unique builder name."
    ~docv:"ID"
    ["name"]

let connect_addr =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Path of register.cap from ocluster-scheduler."
    ~docv:"ADDR"
    ["c"; "connect"]

let capacity =
  Arg.value @@
  Arg.opt Arg.int 10 @@
  Arg.info
    ~doc:"The number of builds that can run in parallel."
    ~docv:"N"
    ["capacity"]

let healthcheck_period =
  Arg.value @@
  Arg.opt Arg.float 600.0 @@
  Arg.info
    ~doc:"Number of second between obuilder health checks. Zero disables health checks."
    ~docv:"SECONDS"
    ["obuilder-healthcheck"]

let prune_threshold =
  Arg.value @@
  Arg.opt Arg.(some float) None @@
  Arg.info
    ~doc:"Run 'docker system prune' when /var/lib/docker's free space falls below this (0-100). \
          If you don't have a partition for /var/lib/docker, then you can use $(b,--docker-max-df-size)."
    ~docv:"PERCENTAGE"
    ["prune-threshold"]

let docker_max_df_size =
  Arg.value @@
  Arg.opt Arg.(some float) None @@
  Arg.info
    ~doc:"Run `docker system df` to get the amount of memory being taken up by the images and if this is \
          greater than this we run `docker system prune`."
    ~docv:"GIGABYTES"
    ["docker-max-df-size"]

let obuilder_prune_threshold =
  Arg.value @@
  Arg.opt Arg.float 30.0 @@
  Arg.info
    ~doc:"If using OBuilder, this threshold is used to prune the stored builds if the free space falls below this (0-100)."
    ~docv:"PERCENTAGE"
    ~docs:"OBUILDER"
    ["obuilder-prune-threshold"]

let obuilder_prune_limit =
  Arg.value @@
  Arg.opt Arg.int 100 @@
  Arg.info
    ~doc:"Number of items to prune from obuilder when the threshold is reached."
    ~docv:"ITEMS"
    ~docs:"OBUILDER"
    ["obuilder-prune-limit"]

let allow_push =
  Arg.value @@
  Arg.opt Arg.(list string) [] @@
  Arg.info
    ~doc:"Docker repositories to which users can push."
    ~docv:"REPO"
    ["allow-push"]

let state_dir =
  Arg.required @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"Directory for caches, etc (e.g. /var/lib/ocluster-worker)."
    ~docv:"PATH"
    ["state-dir"]

let additional_metric_conv =
  let of_string s = match Astring.String.cut ~sep:":" s with
    | Some (name, uri) -> Ok (name, Uri.of_string uri)
    | None -> Error (`Msg ("Malformed additional metric " ^ s))
  in
  let pp ppf (name, uri) = Fmt.pf ppf "%s:%a" name Uri.pp uri in
  Arg.conv (of_string, pp)

let additional_metrics =
  Arg.value @@
  Arg.opt Arg.(list additional_metric_conv) [] @@
  Arg.info
    ~doc:"Additional prometheus endpoints to scrape in the form <name>:<uri> \
    presented as a comma separated list."
    ~docv:"METRICS"
    ["additional-metrics"]

module Obuilder_config = struct
  (** Parse cli arguments for [Obuilder.Store_spec.t option].

      OBuilder support is optional: without [--obuilder-store] the worker still
      handles Docker jobs and rejects OBuilder ones. Only that case is handled
      here; a store that is given is validated by
      [Obuilder.Store_spec.of_t]. *)
  let spec =
    let of_t store rsync_mode =
      match store, rsync_mode with
      | None, None -> Ok None                       (* No OBuilder support. *)
      | None, Some _ ->
        Error "--rsync-mode requires --obuilder-store=rsync:/path"
      | Some _, _ ->
        (* Which stores need an rsync-mode is OBuilder's business, not ours.
           of_t signals a bad combination by raising, so present that as a
           command-line error rather than an uncaught exception. *)
        (match Obuilder.Store_spec.of_t store rsync_mode with
         | store -> Ok (Some store)
         | exception Failure m -> Error m)
    in
    Term.term_result' @@
    Term.(const of_t
          $ Arg.value @@ Obuilder.Store_spec.store ~docs:"OBUILDER" ["obuilder-store"]
          $ Arg.value @@ Obuilder.Store_spec.rsync_mode_opt)

  (** Parse cli arguments for t and initialise a [store], if requested. *)
  let cmdliner =
    Term.(const (Option.map Obuilder.Store_spec.to_store) $ spec)

  let v =
    let make native_conf docker_conf qemu_conf hcs_conf =
      Option.map (function
          | `Native, store -> Cluster_worker.Obuilder_config.v (`Native native_conf) store
          | `Qemu, store -> Cluster_worker.Obuilder_config.v (`Qemu qemu_conf) store
          | `Docker, store -> Cluster_worker.Obuilder_config.v (`Docker docker_conf) store
          | `Hcs, store -> Cluster_worker.Obuilder_config.v (`Hcs hcs_conf) store)
    in
    Term.(const make $ Obuilder.Native_sandbox.cmdliner $ Obuilder.Docker_sandbox.cmdliner $ Obuilder.Qemu_sandbox.cmdliner $ Obuilder.Hcs_sandbox.cmdliner $ cmdliner)
end

let day10_cache =
  Arg.value @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"Enable handling of $(b,day10) custom jobs and use $(docv) as day10's \
          build-cache directory (passed to day10 as $(b,--cache-dir)). When \
          omitted, day10 jobs are rejected. This should be a large, persistent \
          directory, separate from $(b,--state-dir)/$(b,--obuilder-store)."
    ~docv:"DIR"
    ~docs:"DAY10"
    ["day10-cache"]

let day10_prune_threshold =
  Arg.value @@
  Arg.opt Arg.(some float) None @@
  Arg.info
    ~doc:"If using day10, prune the day10 cache (see $(b,--day10-cache)) whenever its \
          partition's free space falls below this (0-100). Pruning gates only new day10 \
          builds, not the whole worker."
    ~docv:"PERCENTAGE"
    ~docs:"DAY10"
    ["day10-prune-threshold"]

let day10_cache_max_size =
  Arg.value @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"Target size the day10 prune keeps the cache within (passed to $(b,day10 prune) as \
          $(b,--max-size), e.g. $(b,40G) or $(b,500M)). day10 evicts least-recently-used \
          entries to fit. When omitted, the prune keeps the newest 90% by size instead."
    ~docv:"SIZE"
    ~docs:"DAY10"
    ["day10-cache-max-size"]

let day10_rebase_period =
  Arg.value @@
  Arg.opt Arg.(some float) None @@
  Arg.info
    ~doc:"If using day10, periodically run $(b,day10 prune --base) to replace the cached base images \
          that have grown old, along with the layers that installed system packages on them; the next \
          build makes a fresh base from the current package index. The value is the schedule interval \
          in hours. It runs only when no build is in flight, and is otherwise tried again an hour \
          later; the worker is never paused for it, and a job arriving during the run waits the few \
          seconds it takes."
    ~docv:"HOURS"
    ~docs:"DAY10"
    ["day10-rebase-period"]

let day10_rebase_days =
  Arg.value @@
  Arg.opt Arg.int 7 @@
  Arg.info
    ~doc:"Passed to $(b,day10 prune --base) as $(b,--days): a base image built within this many days \
          is left alone, so a scheduled run replaces only what has aged."
    ~docv:"DAYS"
    ~docs:"DAY10"
    ["day10-rebase-days"]

let worker_opts_t =
  let worker_opts registration_path capacity name allow_push healthcheck_period prune_threshold docker_max_df_size obuilder_prune_threshold obuilder_prune_limit state_dir obuilder additional_metrics day10_cache day10_prune_threshold day10_cache_max_size day10_rebase_period day10_rebase_days =
    (registration_path, capacity, name, allow_push, healthcheck_period, prune_threshold, docker_max_df_size, obuilder_prune_threshold, obuilder_prune_limit, state_dir, obuilder, additional_metrics, day10_cache, day10_prune_threshold, day10_cache_max_size, day10_rebase_period, day10_rebase_days) in
  Term.(with_used_args
    (const worker_opts $ connect_addr $ capacity $ worker_name $ allow_push $ healthcheck_period
     $ prune_threshold $ docker_max_df_size $ obuilder_prune_threshold $ obuilder_prune_limit $ state_dir $ Obuilder_config.v $ additional_metrics $ day10_cache $ day10_prune_threshold $ day10_cache_max_size $ day10_rebase_period $ day10_rebase_days))

let cmd ~install =
  let doc = "Run a build worker" in
  let man = [
    `P "On $(b,Windows), specify '$(b,install)' as the first \
        command-line parameter to install the worker as a Windows \
        service with the specified parameters, and '$(b,remove) \
        $(i,name)' to remove the worker $(i,name) from the services.";
    `S "DAY10";
    `P "Support for $(b,day10) custom jobs and its build cache. Set \
        $(b,--day10-cache) to enable day10 jobs; the remaining options bound the \
        cache's disk use." ] in
  let info = Cmd.info "ocluster-worker" ~doc ~man ~version:Version.t in
  let docs = Manpage.s_common_options in
  Cmd.v info
    Term.(term_result'
      (const (main ~install)
       $ with_used_args (Fmt_cli.style_renderer ~docs ())
       $ with_used_args (Logs_cli.level ~docs ())
       $ worker_opts_t))

let () =
  let remove name args =
    if args <> [] then begin
      prerr_endline "'remove' should be used only once, in first position.";
      exit 1
    end else
      Winsvc_wrapper.remove name
  in
  match Array.to_list Sys.argv with
  | hd :: "install" :: argv ->
    exit (Cmd.eval ~argv:(Array.of_list (hd :: argv)) (cmd ~install:true))
  | _ :: "remove" :: name :: args -> remove name args
  | _ :: name :: args when Astring.String.is_prefix ~affix:"remove=" name -> remove name args
  | _ ->
    exit (Cmd.eval (cmd ~install:false))
