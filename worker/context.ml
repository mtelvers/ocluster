open Lwt.Infix

module Hash = Digestif.SHA1

let ( / ) = Filename.concat
let ( >>!= ) = Lwt_result.bind

type t = {
  state_dir : string;
}

let repos_dir t = t.state_dir / "git"

let dir_exists d =
  match Unix.lstat d with
  | Unix.{ st_kind = S_DIR; _ } -> true
  | _ -> false
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> false

let ensure_dir path =
  if not (dir_exists path) then Unix.mkdir path 0o700

let get_tmp_dir t =
  t.state_dir / "tmp"

let git_merge_env =
  let orig = Unix.environment () |> Array.to_list in
  "GIT_AUTHOR_NAME=ocluster" ::
  "GIT_COMMITTER_NAME=ocluster" ::
  "EMAIL=ocluster@ocurrent.org" :: orig
  |> Array.of_list

module Repo = struct
  type nonrec t = {
    context : t;
    url : Uri.t;
    lock : Lwt_mutex.t;
  }

  let id t =
    let base = Filename.basename (Uri.path t.url) in
    let digest = Hash.digest_string (Uri.to_string t.url) in
    Fmt.str "%s-%s" base (Hash.to_hex digest)

  let local_copy t =
    repos_dir t.context / id t

  let v context url =
    let url = Uri.of_string url in
    match Uri.scheme url with
    | Some "git" | Some "http" | Some "https" -> { context; url; lock = Lwt_mutex.create () }
    | Some x -> Fmt.failwith "Unsupported scheme %S in URL %a" x Uri.pp url
    | None -> Fmt.failwith "Missing scheme in URL %a" Uri.pp url

  let has_commits t cs =
    let local_repo = local_copy t in
    if dir_exists local_repo then (
      let rec aux = function
        | [] -> Lwt_result.return true
        | c :: cs ->
          Lwt_process.exec ~cwd:local_repo ("", [| "git"; "cat-file"; "-e"; Hash.to_hex c |]) >>= function
          | Unix.WEXITED 0 -> aux cs
          | Unix.WEXITED _ -> Lwt_result.return false
          | _ -> Fmt.failwith "git cat-file crashed!"
      in
      aux cs
    ) else Lwt_result.return false      (* Don't let ocaml-git try to init a new repository! *)

  let fetch ~switch ~log t =
    let local_repo = local_copy t in
    begin
      if dir_exists local_repo then Lwt_result.return ()
      else (
        Process.check_call ~label:"git-init" ~switch ~log ["git"; "init"; local_repo] >>!= fun () ->
        let config k v = Process.check_call ~label:"git-config" ~switch ~log ["git"; "-C"; local_repo; "config"; "--add"; k; v] in
        config "remote.origin.url" (Uri.to_string t.url) >>!= fun () ->
        config "remote.origin.fetch" "+refs/heads/*:refs/remotes/origin/*" >>!= fun () ->
        config "remote.origin.fetch"
          (match Uri.host t.url with
           | Some "github.com" -> "+refs/pull/*:refs/remotes/pull/*"
           | Some "gitlab.com"
           (* Default to GitLab merge requests refspec for self-hosted instances *)
           | _ -> "+refs/merge-requests/*/head:refs/remotes/origin/merge-requests/*")
      )
    end >>!= fun () ->
    Process.check_call ~label:"git-submodule-update" ~switch ~log ["git"; "-C"; local_repo; "submodule"; "update"] >>!= fun () ->
    (* This reset might avoid `fatal: cannot chdir to '../../../ocurrent': No such file or directory` errors *)
    Process.check_call ~label:"git-reset" ~switch ~log ["git"; "-C"; local_repo; "reset"; "--hard"] >>!= fun () ->
    Process.check_call ~label:"git-submodule-sync" ~switch ~log ["git"; "-C"; local_repo; "submodule"; "sync"] >>!= fun () ->
    Process.check_call ~label:"git-submodule-deinit" ~switch ~log ["git"; "-C"; local_repo; "submodule"; "deinit"; "--all"; "-f"] >>!= fun () ->
    Process.check_call ~label:"git-fetch" ~switch ~log
      ["git"; "-C"; local_repo; "fetch"; "-q"; "--update-head-ok"; "--recurse-submodules=no"; "origin"]
end

(* BEGIN Code taken from ocurrent/lib/process.ml *)
let win32_unlink fn =
  Lwt.catch
    (fun () -> Lwt_unix.unlink fn)
    (function
     | Unix.Unix_error (Unix.EACCES, _, _) as ex ->
        (* Try removing the read-only attribute before retrying unlink. We catch
          any exception here and ignore it in favour of the original [exn]. *)
        Lwt.catch
          (fun () ->
            Lwt_unix.lstat fn >>= fun {st_perm; _} ->
            Lwt_unix.chmod fn 0o666 >>= fun () ->
            Lwt.catch
              (fun () -> Lwt_unix.unlink fn)
              (function _ ->
                 (* If everything succeeded but the final removal still failed,
                   restore original permissions *)
                 Lwt_unix.chmod fn st_perm >>= fun () ->
                 Lwt.reraise ex)
          )
          (fun _ -> Lwt.reraise ex)
     | ex -> Lwt.reraise ex)

let unlink =
  if Sys.win32 then
    win32_unlink
  else
    Lwt_unix.unlink

let rm_f_tree root =
  let rec rmtree path =
    Lwt_unix.lstat path >>= fun info ->
    match info.Unix.st_kind with
    | Unix.S_REG | Unix.S_LNK | Unix.S_BLK | Unix.S_CHR | Unix.S_SOCK
    | Unix.S_FIFO ->
      unlink path
    | Unix.S_DIR ->
      Lwt_unix.chmod path 0o700 >>= fun () ->
      Lwt_unix.files_of_directory path
      |> Lwt_stream.iter_s (function
          | "." | ".." -> Lwt.return_unit
          | leaf -> rmtree (Filename.concat path leaf)
        )
      >>= fun () ->
      Lwt_unix.rmdir path
  in
  rmtree root
(* END Code taken from ocurrent/lib/process.ml *)

let repos = Hashtbl.create 1000

let repo t url =
  match Hashtbl.find_opt repos url with
  | Some x -> x
  | None ->
    let repo = Repo.v t url in
    Hashtbl.add repos url repo;
    repo

let rec lwt_result_list_iter_s f = function
  | [] -> Lwt_result.return ()
  | x :: xs ->
    f x >>!= fun () ->
    lwt_result_list_iter_s f xs

let include_git descr =
  match Cluster_api.Submission.get_action descr with
  | Docker_build db -> db.options.include_git
  | Obuilder_build _ -> false
  | Custom_build _ -> false

let build_context t ~log ~tmpdir descr =
  match Cluster_api.Raw.Reader.JobDescr.commits_get_list descr |> List.map Hash.of_hex with
  | [] ->
    Lwt_result.return ()
  | (c :: cs) as commits ->
    let repository = repo t (Cluster_api.Raw.Reader.JobDescr.repository_get descr) in
    Lwt_mutex.with_lock repository.lock (fun () ->
        let clone = Repo.local_copy repository in
        begin
          if Sys.file_exists (clone / ".git" / "index.lock")
          then (Log_data.info log "Removing corrupted repository %S" clone; rm_f_tree clone)
          else Lwt.return_unit
        end >>= fun () ->
        Lwt_switch.with_switch @@ fun switch -> (* Don't let the user cancel these operations. *)
        begin
          Repo.has_commits repository commits >>!= function
          | true -> Log_data.info log "All commits already cached"; Lwt_result.return ()
          | false -> Repo.fetch ~switch ~log repository
        end >>!= fun () ->
        Process.check_call ~label:"git-submodule-update" ~switch ~log ["git"; "-C"; clone; "submodule"; "update"] >>!= fun () ->
        Process.check_call ~label:"git-reset" ~switch ~log ["git"; "-C"; clone; "reset"; "--hard"; Hash.to_hex c] >>!= fun () ->
        Process.check_call ~label:"git-submodule-sync" ~switch ~log ["git"; "-C"; clone; "submodule"; "sync"] >>!= fun () ->
        Process.check_call ~label:"git-submodule-deinit" ~switch ~log ["git"; "-C"; clone; "submodule"; "deinit"; "--all"; "-f"] >>!= fun () ->
        Process.check_call ~label:"git-clean" ~switch ~log ["git"; "-C"; clone; "clean"; "-fdx"] >>!= fun () ->
        let merge c = Process.check_call ~label:"git-merge" ~switch ~log ~env:git_merge_env ["git"; "-C"; clone; "merge"; Hash.to_hex c] in
        cs |> lwt_result_list_iter_s merge >>!= fun () ->
        Process.check_call ~label:"git-submodule-update" ~switch ~log ["git"; "-C"; clone; "submodule"; "update"; "--init"; "--recursive"] >>!= fun () ->
        Sys.readdir clone |> Array.iter (function
            | ".git" -> ()
            | name -> Unix.rename (clone / name) (tmpdir / name)
          );
        if include_git descr then (
          let cmd, is_success =
            if Sys.win32 then
              ["robocopy"; clone / ".git"; tmpdir / ".git"; "/COPY:DATSO"; "/E"; "/R:0"; "/DCOPY:T"; "/NFL"; "/NDL"; "/NJH"; "/NP"],
              fun s -> s = 1
            else
              ["cp"; "-a"; clone / ".git"; tmpdir / ".git"],
              fun s -> s = 0
          in
          Process.check_call ~label:"cp .git" ~switch ~log ~is_success cmd >>!= fun () ->
          Process.check_call ~label:"git-config" ~switch ~log ["git"; "-C"; tmpdir; "config"; "--unset"; "remote.origin.fetch"; "/pull/"] >>!= fun () ->
          Lwt_result.return ()
        ) else (
          Lwt_result.return ()
        )
      )

let with_build_context t ~log descr fn =
  let tmp = get_tmp_dir t in
  Lwt_io.with_temp_dir ~parent:tmp ~prefix:"build-context-" @@ fun tmpdir ->
  build_context t ~log ~tmpdir descr >>!= fun () ->
  fn tmpdir

let ensure_opam_repository t ~switch ~log ~url ~commits =
  (* Make every commit in [commits] of [url] available in the shared Git mirror
     without checking out a worktree, and return the mirror path. Unlike
     {!build_context} we do not reset/clean/merge; day10 reads each commit
     straight from the object database via [git archive], so many jobs at
     different commits can share one mirror. Passing several commits lets a
     single fetch cover both sides of a PR (base + head), which day10 then
     overlays via repeated [--opam-repository]. *)
  let repository = repo t url in
  Lwt_mutex.with_lock repository.Repo.lock (fun () ->
      let local = Repo.local_copy repository in
      Repo.has_commits repository (List.map Hash.of_hex commits) >>!= function
      | true -> Lwt_result.return local
      | false ->
        Log_data.info log "Fetching opam-repository %s into mirror for commits %s"
          url (String.concat ", " commits);
        Repo.fetch ~switch ~log repository >>!= fun () ->
        Lwt_result.return local)

let capture_stdout cmd =
  let proc = Lwt_process.open_process_in ("", Array.of_list cmd) in
  Lwt_io.read proc#stdout >>= fun out ->
  proc#close >|= fun status -> (status, out)

let first_line s =
  match String.index_opt s '\n' with
  | Some i -> String.sub s 0 i
  | None -> String.trim s

let merge_tree t ~switch ~log ~url ~base ~head =
  (* opam-repo-ci PR: 3-way merge [head] (the PR) onto [base] (master) in the
     object database — no worktree, no 30k-file checkout — and return the OID of
     the merged tree, which day10 reads via [git archive]. A conflict cannot
     occur in practice: opam-repo-ci rejects un-mergeable PRs during analysis, so
     a build job's base+head always merge cleanly. *)
  ensure_opam_repository t ~switch ~log ~url ~commits:[ head; base ] >>!= fun mirror ->
  Log_data.info log "day10: merging PR head %s onto master %s (merge-tree)" head base;
  capture_stdout [ "git"; "-C"; mirror; "merge-tree"; "--write-tree"; base; head ]
  >>= fun (status, out) ->
  match status with
  | Unix.WEXITED 0 -> Lwt_result.return (mirror, first_line out)
  | Unix.WEXITED 1 ->
    Lwt_result.fail (`Msg (Fmt.str "day10: PR head %s does not merge cleanly onto master %s" head base))
  | _ -> Lwt_result.fail (`Msg "day10: git merge-tree failed")

let v ~state_dir =
  ensure_dir state_dir;
  let t = { state_dir } in
  ensure_dir (get_tmp_dir t);
  t
