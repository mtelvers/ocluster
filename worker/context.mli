(** Fetch and cache the Git repositories used to create the build contexts.
    It works like this:

    1. We clone the repository in mirror mode. This means that we also see
       PRs. We could just pull the hashes we want, but future updates can
       be more efficient if we track what we have in branches.

    2. We use a non-bare repository because we want submodules cached too
       (using worktrees, the submodule goes into the worktree subdirectory and
       is lost when the worktree is deleted).

    3. We reset to the first commit and then merge the others. This makes it
       easy to e.g. test a PR merged with master. Then we fetch any submodules
       (we don't attempt to merge submodule changes).

    4. Finally, we move all the checked-out files to our desired temporary
       directory (on the same FS) and release the repository lock. *)

type t

val v : state_dir:string -> t
(** @param state_dir Used for temporary checkouts and Git cache. *)

val with_build_context :
  t ->
  log:Log_data.t ->
  Cluster_api.Raw.Reader.JobDescr.t ->
  (string -> ('a, [`Cancelled | `Msg of string]) Lwt_result.t) ->
  ('a, [`Cancelled | `Msg of string]) Lwt_result.t
(** [with_build_context t ~log descr fn] runs [fn dir], where [dir] is a
    temporary directory containing the requested build context. *)

val ensure_opam_repository :
  t ->
  switch:Lwt_switch.t ->
  log:Log_data.t ->
  url:string ->
  commits:string list ->
  (string, [`Cancelled | `Msg of string]) Lwt_result.t
(** [ensure_opam_repository t ~switch ~log ~url ~commits] makes sure every
    commit in [commits] of [url] is present in the local Git mirror, fetching
    them if necessary but {e without} checking out a worktree, and returns the
    path to the mirror repository. The result is suitable for
    [day10 --opam-repository <path>:<commit>], which reads each commit directly
    from the Git object database. Several commits let one fetch cover both sides
    of a PR (base + head). *)
