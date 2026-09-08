open Lwt.Infix
open Capnp_rpc_lwt

let or_die = function
  | Ok x -> x
  | Error `Msg m -> failwith m

let read_first_line path =
  let ch = open_in_bin path in
  Fun.protect (fun () -> input_line ch)
    ~finally:(fun () -> close_in ch)

let rec tail job start =
  Cluster_api.Job.log job start >>= function
  | Error (`Capnp e) -> Fmt.failwith "Error tailing logs: %a" Capnp_rpc.Error.pp e
  | Ok ("", _) -> Lwt.return_unit
  | Ok (data, next) ->
    output_string stdout data;
    flush stdout;
    tail job next

let run cap_path fn =
  try
    Lwt_main.run begin
      let vat = Capnp_rpc_unix.client_only_vat () in
      let sr = Capnp_rpc_unix.Cap_file.load vat cap_path |> or_die in
      Capnp_rpc_unix.with_cap_exn sr fn
    end
  with Failure msg ->
    Printf.eprintf "%s\n%!" msg;
    exit 1

type submit_options_common = {
  submission_path : string;
  pool : string;
  repository : string option;
  commits : string list;
  cache_hint : string;
  urgent : bool;
  secrets : (string * string) list;
}

type day10_fields = {
  verb : string;
  opam_repository : string;
  opam_repository_commit : string;
  opam_repository_base : string;
  ocaml_version : string;
  package : string;
  with_test : bool;
  with_doc : bool;
  dune_args : string list;
  arch : string;
  os : string;
  os_family : string;
  os_distribution : string;
  os_version : string;
  lower_bound : bool;
  update_invariant : bool;
}

(* Build a Custom job payload for kind="day10". *)
let day10_payload fields builder =
  let module B = Cluster_api.Raw.Builder.Day10 in
  let d = B.init_pointer builder in
  B.verb_set d fields.verb;
  B.opam_repository_set d fields.opam_repository;
  B.opam_repository_commit_set d fields.opam_repository_commit;
  B.opam_repository_base_set d fields.opam_repository_base;
  B.ocaml_version_set d fields.ocaml_version;
  B.package_set d fields.package;
  B.with_test_set d fields.with_test;
  B.with_doc_set d fields.with_doc;
  let _ = B.dune_args_set_list d fields.dune_args in
  B.arch_set d fields.arch;
  B.os_set d fields.os;
  B.os_family_set d fields.os_family;
  B.os_distribution_set d fields.os_distribution;
  B.os_version_set d fields.os_version;
  B.lower_bound_set d fields.lower_bound;
  B.update_invariant_set d fields.update_invariant

let get_action = function
  | `Docker (dockerfile, push_to, options) ->
    begin match dockerfile with
      | `Context_path path -> Lwt.return (`Path path)
      | `Local_path path ->
        Lwt_io.(with_file ~mode:input) path (Lwt_io.read ?count:None) >|= fun data ->
        `Contents data
    end >|= fun dockerfile ->
    Cluster_api.Submission.docker_build ?push_to ~options dockerfile
  | `Obuilder path ->
    Lwt_io.(with_file ~mode:input) path (Lwt_io.read ?count:None) >|= fun spec ->
    Cluster_api.Submission.obuilder_build spec
  | `Day10 fields ->
    Lwt.return @@
    Cluster_api.Submission.custom_build
      (Cluster_api.Custom.v ~kind:"day10" (day10_payload fields))

let read_whole_file path =
  let ic = open_in_bin path in
  Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
  let len = in_channel_length ic in
  really_input_string ic len

let submit () { submission_path; pool; repository; commits; cache_hint; urgent; secrets } spec =
  let src =
    match repository, commits with
    | None, [] -> None
    | None, _ -> failwith "BUG: commits but no repository!"
    | Some repo, [] -> Fmt.failwith "No commits requested from repository %S!" repo
    | Some repo, commits -> Some (repo, commits)
  in
  run submission_path @@ fun submission_service ->
  get_action spec >>= fun action ->
  let secrets = List.map (fun (id, path) -> id, read_whole_file path) secrets in
  Capability.with_ref (Cluster_api.Submission.submit submission_service ~urgent ~pool ~action ~cache_hint ~secrets ?src) @@ fun ticket ->
  Capability.with_ref (Cluster_api.Ticket.job ticket) @@ fun job ->
  let result = Cluster_api.Job.result job in
  Fmt.pr "Tailing log:@.";
  tail job 0L >>= fun () ->
  result >|= function
  | Ok "" -> ()
  | Ok x -> Fmt.pr "Result: %S@." x
  | Error (`Capnp e) ->
    Fmt.pr "%a.@." Capnp_rpc.Error.pp e;
    exit 1

(* Command-line parsing *)

open Cmdliner

let connect_addr =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Path of submission.cap file from ocluster-scheduler."
    ~docv:"ADDR"
    ["c"; "connect"]

let local_obuilder =
  Arg.required @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Path of the local OBuilder spec to submit."
    ~docv:"PATH"
    ["local-file"]

let local_dockerfile =
  Arg.value @@
  Arg.opt Arg.(some file) None @@
  Arg.info
    ~doc:"Path of the local Dockerfile to submit."
    ~docv:"PATH"
    ["local-dockerfile"]

let context_dockerfile =
  Arg.value @@
  Arg.opt Arg.(some string) None @@
  Arg.info
    ~doc:"Path of the Dockerfile within the commit."
    ~docv:"PATH"
    ["context-dockerfile"]

let dockerfile =
  let make local_dockerfile context_dockerfile =
    match local_dockerfile, context_dockerfile with
    | None, None -> Ok (`Context_path "Dockerfile")
    | Some local, None -> Ok (`Local_path local)
    | None, Some context -> Ok (`Context_path context)
    | Some _, Some _ -> Error ("Can't use --local-dockerfile and --context-dockerfile together!")
  in
  Term.(term_result' (const make $ local_dockerfile $ context_dockerfile))

let repo =
  Arg.value @@
  Arg.pos 0 Arg.(some string) None @@
  Arg.info
    ~doc:"URL of the source Git repository."
    ~docv:"URL"
    []

let commits =
  Arg.value @@
  Arg.(pos_right 0 string) [] @@
  Arg.info
    ~doc:"Git commit to use as context (full commit hash)."
    ~docv:"HASH"
    []

let pool =
  Arg.required @@
  Arg.(opt (some string)) None @@
  Arg.info
    ~doc:"Pool to use."
    ~docv:"ID"
    ["pool"]

let cache_hint =
  Arg.value @@
  Arg.(opt string) "" @@
  Arg.info
    ~doc:"Hint used to group similar builds to improve caching."
    ~docv:"STRING"
    ["cache-hint"]

let urgent =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Add job to the urgent queue."
    ["urgent"]

let push_to =
  let target_conv = Arg.conv Cluster_api.Docker.Image_id.(of_string, pp) in
  Arg.value @@
  Arg.(opt (some target_conv)) None @@
  Arg.info
    ~doc:"Where to docker-push the result."
    ~docv:"REPO:TAG"
    ["push-to"]

let push_user =
  Arg.value @@
  Arg.(opt (some string)) None @@
  Arg.info
    ~doc:"Docker registry user account to use when pushing."
    ~docv:"USER"
    ["push-user"]

let push_password_file =
  Arg.value @@
  Arg.(opt (some file)) None @@
  Arg.info
    ~doc:"File containing Docker registry password."
    ~docv:"PATH"
    ["push-password"]

let build_args =
  Arg.value @@
  Arg.(opt_all string) [] @@
  Arg.info
    ~doc:"Docker build argument."
    ~docv:"ARG"
    ["build-arg"]

let secrets =
  (Arg.value @@
   Arg.(opt_all (pair ~sep:':' string file)) [] @@
   Arg.info
     ~doc:"Provide a secret under the form id:file."
     ~docv:"SECRET"
     ["secret"])

let squash =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Whether to squash the layers."
    ["squash"]

let buildkit =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Whether to use BuildKit to build."
    ["buildkit"]

let include_git =
  Arg.value @@
  Arg.flag @@
  Arg.info
    ~doc:"Include the .git clone in the build context."
    ["include-git"]

let push_to =
  let make target user password =
    match target, user, password with
    | None, None, None -> None
    | None, _, _ ->
      Fmt.failwith "Must use --push-to with --push-user/--push-password"
    | Some target, Some user, Some password_file ->
      let password = read_first_line password_file in
      Some { Cluster_api.Docker.Spec.target; auth = Some (user, password) }
    | Some target, None, None ->
      Some { Cluster_api.Docker.Spec.target; auth = None }
    | _, None, Some _
    | _, Some _, None -> Fmt.failwith "Must use --push-user with --push-password"
  in
  Term.(const make $ push_to $ push_user $ push_password_file)

let build_options =
  let make build_args squash buildkit include_git =
    { Cluster_api.Docker.Spec.build_args; squash; buildkit; include_git }
  in
  Term.(const make $ build_args $ squash $ buildkit $ include_git)

let submit_options_common =
  let make submission_path pool repository commits cache_hint urgent secrets =
    { submission_path; pool; repository; commits; cache_hint; urgent; secrets }
  in
  Term.(const make $ connect_addr $ pool $ repo $ commits $ cache_hint $ urgent $ secrets)

let submit_docker_options =
  let make dockerfile push_to build_options =
    `Docker (dockerfile, push_to, build_options)
  in
  Term.(const make $ dockerfile $ push_to $ build_options)

let submit_docker =
  let doc = "Submit a Docker build to the scheduler." in
  let info = Cmd.info "submit-docker" ~doc in
  Cmd.v info
    Term.(const submit $ Logging.cmdliner $ submit_options_common $ submit_docker_options)

let submit_obuilder_options =
  let make spec =
    `Obuilder spec
  in
  Term.(const make $ local_obuilder)

let submit_obuilder =
  let doc = "Submit an OBuilder build to the scheduler." in
  let info = Cmd.info "submit-obuilder" ~doc in
  Cmd.v info
    Term.(const submit $ Logging.cmdliner $ submit_options_common $ submit_obuilder_options)

(* ---- day10 submission ---- *)

let day10_verb =
  Arg.value @@ Arg.opt Arg.string "health-check" @@
  Arg.info ~doc:"day10 sub-command: health-check | revdeps | list." ~docv:"VERB" ["verb"]

let day10_opam_repository =
  Arg.value @@ Arg.opt Arg.string "" @@
  Arg.info
    ~doc:"opam-repository Git URL, used to locate the worker's mirror. \
          Empty = default ocaml/opam-repository."
    ~docv:"URL" ["opam-repository"]

let day10_opam_repository_commit =
  Arg.value @@ Arg.opt Arg.string "" @@
  Arg.info
    ~doc:"opam-repository commit the job builds against (read from the mirror)."
    ~docv:"SHA" ["opam-repository-commit"]

let day10_opam_repository_base =
  Arg.value @@ Arg.opt Arg.string "" @@
  Arg.info
    ~doc:"Base opam-repository commit for a PR overlay (opam-repo-ci). When set, \
          day10 reads both this and --opam-repository-commit (the PR head, which \
          takes precedence). Leave empty for a single repo, or for a PR that \
          deletes a package (pass only the head, whose tree already omits it)."
    ~docv:"SHA" ["opam-repository-base"]

let day10_ocaml_version =
  Arg.value @@ Arg.opt Arg.string "" @@
  Arg.info
    ~doc:"OCaml version to pass to day10 (e.g. ocaml.5.3.0). Empty = day10 default."
    ~docv:"VERSION" ["ocaml-version"]

let day10_package =
  Arg.value @@ Arg.opt Arg.string "" @@
  Arg.info ~doc:"Target package for health-check / revdeps." ~docv:"PKG" ["package"]

let day10_with_test =
  Arg.value @@ Arg.flag @@
  Arg.info ~doc:"Pass --with-test to day10." ["with-test"]

let day10_with_doc =
  Arg.value @@ Arg.flag @@
  Arg.info ~doc:"Pass --with-doc to day10 (build verb)." ["with-doc"]

let day10_dune_args =
  Arg.value @@ Arg.opt_all Arg.string [] @@
  Arg.info
    ~doc:"Extra dune argument passed after the project directory for `day10 build` \
          (repeatable), e.g. --dune-arg=@install --dune-arg=@check --dune-arg=@runtest."
    ~docv:"ARG" ["dune-arg"]

let day10_arch =
  Arg.value @@ Arg.opt Arg.string "" @@
  Arg.info ~doc:"Target arch (e.g. x86_64, arm64). Empty = host." ~docv:"ARCH" ["arch"]

let day10_os =
  Arg.value @@ Arg.opt Arg.string "" @@
  Arg.info ~doc:"Target os (e.g. linux). Empty = host." ~docv:"OS" ["os"]

let day10_os_family =
  Arg.value @@ Arg.opt Arg.string "" @@
  Arg.info ~doc:"Target os-family (e.g. debian). Empty = host." ~docv:"FAMILY" ["os-family"]

let day10_os_distribution =
  Arg.value @@ Arg.opt Arg.string "" @@
  Arg.info ~doc:"Target os-distribution (e.g. ubuntu). Empty = host." ~docv:"DISTRO" ["os-distribution"]

let day10_os_version =
  Arg.value @@ Arg.opt Arg.string "" @@
  Arg.info ~doc:"Target os-version (e.g. 24.04). Empty = host." ~docv:"VERSION" ["os-version"]

let day10_lower_bound =
  Arg.value @@ Arg.flag @@
  Arg.info ~doc:"Pass --prefer-oldest to day10 (lower-bounds test)." ["lower-bound"]

let day10_update_invariant =
  Arg.value @@ Arg.flag @@
  Arg.info ~doc:"Pass --update-invariant to day10 (for a compiler-package target)." ["update-invariant"]

let submit_day10_options =
  let make verb opam_repository opam_repository_commit opam_repository_base ocaml_version package with_test with_doc
      dune_args arch os os_family os_distribution os_version lower_bound update_invariant =
    `Day10 {
      verb;
      opam_repository;
      opam_repository_commit;
      opam_repository_base;
      ocaml_version;
      package;
      with_test;
      with_doc;
      dune_args;
      arch;
      os;
      os_family;
      os_distribution;
      os_version;
      lower_bound;
      update_invariant;
    }
  in
  Term.(const make
        $ day10_verb $ day10_opam_repository $ day10_opam_repository_commit
        $ day10_opam_repository_base
        $ day10_ocaml_version $ day10_package $ day10_with_test $ day10_with_doc
        $ day10_dune_args $ day10_arch $ day10_os $ day10_os_family
        $ day10_os_distribution $ day10_os_version $ day10_lower_bound
        $ day10_update_invariant)

let submit_day10 =
  let doc = "Submit a day10 job to the scheduler." in
  let man = [
    `S Manpage.s_description;
    `P "Submit a Custom job (kind=\"day10\") that runs \
        `day10 <verb> --opam-repository <mirror>:<commit> …` on the worker. \
        The worker resolves the opam-repository commit against its local Git \
        mirror; no repository/commit positionals are needed.";
    `P "Example:";
    `Pre "  ocluster-client submit-day10 -c submission.cap --pool=linux-x86_64 \
          --verb=health-check --opam-repository-commit=abc123def456 \
          --ocaml-version=ocaml.5.3.0 --os-distribution=debian --os-version=13 \
          --package=fmt.0.9.0";
  ] in
  let info = Cmd.info "submit-day10" ~doc ~man in
  Cmd.v info
    Term.(const submit $ Logging.cmdliner $ submit_options_common $ submit_day10_options)

let cmds = [submit_docker; submit_obuilder; submit_day10]

let () =
  let doc = "a command-line client for the ocluster-scheduler" in
  let info = Cmd.info "ocluster-client" ~doc ~version:Version.t in
  exit (Cmd.eval ~argv:Options.argv @@ Cmd.group info cmds)
