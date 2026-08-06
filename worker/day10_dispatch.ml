(** Dispatch handler for [Custom] jobs whose [kind] is ["day10"].

    Decodes a {!Cluster_api.Raw.Reader.Day10} payload, makes sure the requested
    opam-repository commit is present in the worker's Git mirror (fetching it
    without checking out a worktree), and shells out to
    [day10 <verb> --cache-dir CACHE --opam-repository <mirror>:<commit> …],
    streaming stdout/stderr to the job log. day10 reads the repository straight
    from the Git object database, so a single shared mirror serves any number of
    concurrent jobs at different commits.

    [cache_dir] is the day10 build cache passed by the worker (from
    [--day10-cache]); it is what enables day10 support on a worker. *)

open Lwt.Infix

let default_opam_repository = "https://github.com/ocaml/opam-repository.git"

let src_log = Logs.Src.create "cluster_worker.day10" ~doc:"day10 dispatch"

module Log = (val Logs.src_log src_log : Logs.LOG)

let read_payload (custom : Cluster_api.Custom.recv) : Cluster_api.Raw.Reader.Day10.t =
  Cluster_api.Raw.Reader.of_pointer (Cluster_api.Custom.payload custom)

(** Build the argv for [day10 <verb>]. Empty Text fields are omitted so day10
    falls back to its own defaults / host detection. [opam_repo] is the value
    for [--opam-repository] (a [<mirror>:<commit>] spec). *)
let day10_argv ~cache_dir ~opam_repo ~src d =
  let module R = Cluster_api.Raw.Reader.Day10 in
  let verb = R.verb_get d in
  let opt name v = match v with "" -> [] | v -> [ "--" ^ name; v ] in
  let flag name b = if b then [ "--" ^ name ] else [] in
  (* [list] does not accept --cache-dir; every other verb requires it. *)
  let cache_flag = match verb with "list" -> [] | _ -> [ "--cache-dir"; cache_dir ] in
  (* --log makes day10 emit the full build log (and terminal marker) so the
     client can show why a build failed. Accepted by build and health-check. *)
  let log_flag = match verb with "build" | "health-check" -> [ "--log" ] | _ -> [] in
  (* --with-test: build + health-check; --with-doc: build only. *)
  let test_flag = match verb with "build" | "health-check" -> flag "with-test" (R.with_test_get d) | _ -> [] in
  let doc_flag = match verb with "build" -> flag "with-doc" (R.with_doc_get d) | _ -> [] in
  (* Positional arguments per verb:
     - build: SRC (the checked-out project) followed by trailing dune args
     - list:  none
     - health-check / revdeps: the package name *)
  let positional = match verb with
    | "build" -> src :: R.dune_args_get_list d
    | "list" -> []
    | _ -> (match R.package_get d with "" -> [] | p -> [ p ])
  in
  [ "day10"; verb ]
  @ cache_flag
  @ log_flag
  @ [ "--opam-repository"; opam_repo ]
  @ opt "ocaml-version" (R.ocaml_version_get d)
  @ opt "arch" (R.arch_get d)
  @ opt "os" (R.os_get d)
  @ opt "os-family" (R.os_family_get d)
  @ opt "os-distribution" (R.os_distribution_get d)
  @ opt "os-version" (R.os_version_get d)
  @ test_flag
  @ doc_flag
  @ positional

let log_summary log d ~mirror ~cache_dir =
  let module R = Cluster_api.Raw.Reader.Day10 in
  let pp_or_default = function "" -> "<default>" | v -> v in
  Log_data.write log
    (Fmt.str
       "day10 dispatch:\n\
       \  verb             : %s\n\
       \  opam-repository  : %s\n\
       \  opam-repo commit : %s\n\
       \  mirror           : %s\n\
       \  cache-dir        : %s\n\
       \  ocaml-version    : %s\n\
       \  package          : %s\n\
       \  os-distribution  : %s\n\
       \  os-version       : %s\n\
       \  with-test        : %b\n"
       (R.verb_get d)
       (pp_or_default (R.opam_repository_get d))
       (R.opam_repository_commit_get d)
       mirror cache_dir
       (pp_or_default (R.ocaml_version_get d))
       (pp_or_default (R.package_get d))
       (pp_or_default (R.os_distribution_get d))
       (pp_or_default (R.os_version_get d))
       (R.with_test_get d))

let run ~cache_dir ~state_dir ~switch ~log ~src custom =
  let d = read_payload custom in
  let module R = Cluster_api.Raw.Reader.Day10 in
  let verb = R.verb_get d in
  let commit = R.opam_repository_commit_get d in
  let url = match R.opam_repository_get d with "" -> default_opam_repository | u -> u in
  match verb with
  | "solve" ->
    Lwt.return (Error (`Msg (Fmt.str "day10 verb %S is not yet implemented in the ocluster dispatch" verb)))
  | _ when commit = "" ->
    Lwt.return (Error (`Msg "day10 job requires opamRepositoryCommit"))
  | _ when R.opam_repository_base_get d <> "" ->
    Lwt.return (Error (`Msg "day10 PR-merge (opamRepositoryBase) is not yet implemented"))
  | "build" when src = "" ->
    Lwt.return (Error (`Msg "day10 build requires a source directory (set the job's repository/commits)"))
  | _ ->
    let ctx = Context.v ~state_dir in
    Context.ensure_opam_repository ctx ~switch ~log ~url ~commit >>= function
    | Error _ as e -> Lwt.return e
    | Ok mirror ->
      log_summary log d ~mirror ~cache_dir;
      let opam_repo = Fmt.str "%s:%s" mirror commit in
      let cmd = day10_argv ~cache_dir ~opam_repo ~src d in
      (* [day10 build] runs dune in [src] and writes _build there. The checkout
         is created by the worker (root-owned, per-job, ephemeral) but day10
         builds as its own container user, so make it writable first. Safe:
         [src] is a unique per-job temp dir, not shared between jobs. *)
      let prepare =
        if verb = "build" then
          Process.check_call ~label:"chmod-src" ~switch ~log [ "chmod"; "-R"; "a+rwX"; src ]
        else Lwt_result.return ()
      in
      prepare >>= (function
      | Error e -> Lwt.return (Error e)
      | Ok () ->
        Log_data.write log
          (Fmt.str "+ %s\n" (String.concat " " (List.map Filename.quote cmd)));
        Log.info (fun f -> f "Dispatching day10 %s (opam-repo %s)" verb opam_repo);
        Process.check_call ~label:"day10" ~switch ~log cmd >|= function
        | Ok () -> Ok (Fmt.str "day10 %s succeeded" verb)
        | Error `Cancelled as e -> e
        | Error (`Msg _) as e -> e)
