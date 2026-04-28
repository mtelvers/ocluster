open Capnp_rpc.Std

module Git = Current_git
module Connection = Connection
module Cluster_api = Cluster_api_eio

type urgency = [ `Auto | `Always | `Never ]

type target = Cluster_api.Docker.Image_id.t
let target_to_yojson x = `String (Cluster_api.Docker.Image_id.to_string x)

type docker_build = {
  dockerfile : [ `Contents of string | `Path of string ];
  options : Cluster_api.Docker.Spec.options;
  push_target : target option;
} [@@deriving to_yojson]

module type S = sig
  type t

  val v :
    ?timeout:Duration.t ->
    ?push_auth:(string * string) ->
    ?secrets:(string * string) list ->
    ?urgent:urgency ->
    unit ->
    t

  val with_timeout : Duration.t option -> t -> t
  val with_push_auth : (string * string) option -> t -> t
  val with_secrets : (string * string) list -> t -> t
  val with_urgent : urgency -> t -> t

  val build :
    ?level:Current.Level.t ->
    ?label:string ->
    ?cache_hint:string ->
    t ->
    pool:string ->
    src:Current_git.Commit_id.t list Current.t ->
    options:Cluster_api.Docker.Spec.options ->
    [ `Contents of string Current.t | `Path of string ] ->
    unit Current.t

  val build_and_push :
    ?level:Current.Level.t ->
    ?label:string ->
    ?cache_hint:string ->
    t ->
    push_target:Cluster_api.Docker.Image_id.t ->
    pool:string ->
    src:Current_git.Commit_id.t list Current.t ->
    options:Cluster_api.Docker.Spec.options ->
    [ `Contents of string Current.t | `Path of string ] ->
    string Current.t

  val build_obuilder :
    ?level:Current.Level.t ->
    ?label:string ->
    ?cache_hint:string ->
    t ->
    pool:string ->
    src:Current_git.Commit_id.t list Current.t ->
    Cluster_api.Obuilder_job.Spec.t Current.t ->
    unit Current.t

  module Raw : sig
    val build :
      ?level:Current.Level.t ->
      ?cache_hint:string ->
      t ->
      pool:string ->
      src:Current_git.Commit_id.t list ->
      options:Cluster_api.Docker.Spec.options ->
      [ `Contents of string | `Path of string ] ->
      unit Current.Primitive.t

    val build_and_push :
      ?level:Current.Level.t ->
      ?cache_hint:string ->
      t ->
      push_target:Cluster_api.Docker.Image_id.t ->
      pool:string ->
      src:Current_git.Commit_id.t list ->
      options:Cluster_api.Docker.Spec.options ->
      [ `Contents of string | `Path of string ] ->
      string Current.Primitive.t

    val build_obuilder :
      ?level:Current.Level.t ->
      ?cache_hint:string ->
      t ->
      pool:string ->
      src:Current_git.Commit_id.t list ->
      Cluster_api.Obuilder_job.Spec.t ->
      unit Current.Primitive.t
  end
end

[@@@ocaml.warning "-67"]

module Make () (E : sig
  val caps : Current_cache.caps
  val connection : Connection.t
end) : S = struct
  (* Per-call defaults. May be copied and modified per job via [with_*]. *)
  type t = {
    timeout : Duration.t option;
    push_auth : (string * string) option; (* Username/password for pushes *)
    secrets : (string * string) list;
    cache_hint : string option;
    level : Current.Level.t option;
    urgent : urgency;
  }

  module Op = struct
    let id = "ocluster-build"

    type nonrec t = t

    module Key = struct
      type commit_id = Git.Commit_id.t
      let commit_id_to_yojson x = `String (Git.Commit_id.hash x)

      type t = {
        action : [`Docker of docker_build | `Obuilder of Cluster_api.Obuilder_job.Spec.t];
        src : commit_id list;
        pool : string;
      } [@@deriving to_yojson]

      let digest t = Yojson.Safe.to_string (to_yojson t)
    end

    module Value = Current.String

    (* Convert a list of commits in the same repository to a [(repo, hashes)] pair.
       Raises an exception if there are commits from different repositories. *)
    let single_repo = function
      | [] -> None
      | x :: _ as commits ->
        let repo = Git.Commit_id.repo x in
        let hash y =
          let repo' = Git.Commit_id.repo y in
          if repo = repo' then Git.Commit_id.hash y
          else
            Fmt.failwith "Commits from different repositories not supported (%S vs %S)" repo repo'
        in
        Some (repo, List.map hash commits)

    let default_hint (action : Cluster_api.Submission.send_action) =
      match action with
      | Custom_build _ -> ""
      | Obuilder_build { spec = `Contents spec } ->
        Astring.String.take ~sat:((<>) '\n') spec (* Use the first line *)
      | Docker_build { dockerfile; _ } ->
        match dockerfile with
        | `Path path -> path          (* Group by Dockerfile path *)
        | `Contents contents ->
          (* No repository; use first FROM line. *)
          let from_line x = Astring.String.is_prefix ~affix:"from" (String.lowercase_ascii x) in
          match List.find_opt from_line (String.split_on_char '\n' contents) with
          | Some line -> line
          | None -> ""

    let build t job { Key.action; src; pool } =
      let action =
        match action with
        | `Docker { dockerfile; options; push_target } ->
          let push_to =
            push_target |> Option.map (fun target ->
                Current.Job.log job "Will push staging image to %a" Cluster_api.Docker.Image_id.pp target;
                { Cluster_api.Docker.Spec.target; auth = t.push_auth }
              )
          in
          begin match dockerfile with
            | `Contents content -> Current.Job.write job (Fmt.str "@.Dockerfile:@.@.\o033[34m%s\o033[0m@.@." content)
            | `Path _ -> ()
          end;
          Cluster_api.Submission.docker_build ?push_to ~options dockerfile
        | `Obuilder { spec = `Contents spec } ->
          Current.Job.write job (Fmt.str "@.OBuilder spec:@.@.\o033[34m%s\o033[0m@.@." spec);
          Cluster_api.Submission.obuilder_build spec
      in
      let src = single_repo src in
      let cache_hint =
        match t.cache_hint with
        | Some hint -> hint               (* User-supplied cache hint *)
        | None ->
          match src, default_hint action with
          | Some (repo, _), "" -> repo        (* Group by source repository *)
          | Some (repo, _), hint -> Printf.sprintf "%s-%s" repo hint
          | None, hint -> hint
      in
      Current.Job.log job "Using cache hint %S" cache_hint;
      let urgent priority =
        match t.urgent with
        | `Always -> true
        | `Never -> false
        | `Auto -> priority = `High
      in
      let build_pool = Connection.pool ~job ~pool ~action ~cache_hint ~urgent ?src ~secrets:t.secrets E.connection in
      let level =
        match t.level with
        | Some level -> level
        | None ->
          match action with
          | Docker_build { push_to = Some _; _ } -> Current.Level.Above_average
          | _ -> Current.Level.Average
      in
      let build_job = Current.Job.start_with ~pool:build_pool job ?timeout:t.timeout ~level in
      Capability.with_ref build_job @@ fun build_job ->
      Connection.run_job ~job build_job

    let pp f {Key.action; src; pool } =
      match action with
      | `Docker { dockerfile = `Path path; _ } ->
        Fmt.pf f "@[<v2>Build %s using %s in@,%a@]"
          path pool
          (Fmt.Dump.list Git.Commit_id.pp) src
      | `Obuilder _ | `Docker { dockerfile = `Contents _; _ } ->
        Fmt.pf f "@[<v2>Build using %s in@,%a@]"
          pool
          (Fmt.Dump.list Git.Commit_id.pp) src

    let auto_cancel = true
  end

  module Build = Current_cache.Make(Op)

  let build_cache = Build.create ~caps:E.caps

  open Current.Syntax

  let v ?timeout ?push_auth ?(secrets=[]) ?(urgent=`Auto) () =
    { timeout; push_auth; secrets; cache_hint = None; level = None; urgent }

  let with_push_auth push_auth t = { t with push_auth }
  let with_secrets secrets t = { t with secrets }
  let with_timeout timeout t = { t with timeout }
  let with_urgent urgent t = { t with urgent }

  let component_label label dockerfile pool =
    let pp_label = Fmt.(option (cut ++ string)) in
    match dockerfile with
    | `Path path -> Current.component "build %s@,%s%a" path pool pp_label label
    | `Contents _ -> Current.component "build@,%s%a" pool pp_label label

  module Raw = struct
    let with_hint ~cache_hint t =
      match cache_hint with
      | Some _ -> { t with cache_hint }
      | None -> t

    let with_level ~level t =
      match level with
      | Some _ -> { t with level }
      | None -> t

    let build_and_push ?level ?cache_hint t ~push_target ~pool ~src ~options dockerfile =
      let t = with_hint ~cache_hint t in
      let t = with_level ~level t in
      Build.get build_cache t { Op.Key.action = `Docker { dockerfile; options; push_target = Some push_target }; src; pool }
      |> Current.Primitive.map_result @@ function
      | Ok "" -> Error (`Msg "No output image (push auth not configured)")
      | Ok x -> Ok x
      | Error _ as e -> e

    let build ?level ?cache_hint t ~pool ~src ~options dockerfile =
      let t = with_hint ~cache_hint t in
      let t = with_level ~level t in
      Build.get build_cache t { Op.Key.action = `Docker {dockerfile; options; push_target = None}; src; pool }
      |> Current.Primitive.map_result (Result.map (function
          | "" -> ()
          | x -> Fmt.failwith "BUG: got a RepoID (%S) but we didn't ask to push!" x
        ))

    let build_obuilder ?level ?cache_hint t ~pool ~src spec =
      let t = with_hint ~cache_hint t in
      let t = with_level ~level t in
      Build.get build_cache t { Op.Key.action = `Obuilder spec; src; pool }
      |> Current.Primitive.map_result (Result.map (fun (_ : string) -> ()))
  end

  let unwrap = function
    | `Path _ as x -> Current.return x
    | `Contents x -> Current.map (fun x -> `Contents x) x

  let build_and_push ?level ?label ?cache_hint t ~push_target ~pool ~src ~options dockerfile =
    component_label label dockerfile pool |>
    let> dockerfile = unwrap dockerfile
    and> src = src in
    Raw.build_and_push ?level ?cache_hint t ~push_target ~pool ~src ~options dockerfile

  let build ?level ?label ?cache_hint t ~pool ~src ~options dockerfile =
    component_label label dockerfile pool |>
    let> dockerfile = unwrap dockerfile
    and> src = src in
    Raw.build ?level ?cache_hint t ~pool ~src ~options dockerfile

  let build_obuilder ?level ?label ?cache_hint t ~pool ~src spec =
    Current.component "obuild@,%s%a" pool Fmt.(option (cut ++ string)) label |>
    let> spec = spec
    and> src = src in
    Raw.build_obuilder ?level ?cache_hint t ~pool ~src spec
end
