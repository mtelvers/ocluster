(** Manager for a worker's day10 build cache.

    Dispatches day10 [Custom] jobs (see {!Day10_dispatch}) and, when configured,
    runs a background loop that prunes the cache with [day10 prune] whenever the
    cache partition's free space falls below a threshold.

    Modelled on {!Obuilder_build}: a prune pass gates only new day10 builds (via
    an internal flag/condition), so the worker keeps serving other jobs while a
    prune runs. Pruning is least-recently-used, so a build in progress keeps the
    layers it just touched. *)

type t

(** [create ?prune_threshold ?max_size cache_dir] is a manager for the day10
    cache at [cache_dir] (passed to day10 as [--cache-dir]).

    When [prune_threshold] is set, a background loop prunes the cache whenever
    the partition holding [cache_dir] has less than that percentage of blocks
    free. The prune keeps the cache within [max_size]
    ([day10 prune --max-size], e.g. ["40G"]) or, if [max_size] is unset, keeps
    the newest 90% by size ([--percent 90]). With no [prune_threshold],
    automatic pruning is disabled and the manager only dispatches jobs. *)
val create : ?prune_threshold:float -> ?max_size:string -> string -> t

(** [run t ~state_dir ~switch ~log ~src custom] waits for any in-progress prune
    to finish, then dispatches the day10 [Custom] job. *)
val run :
  t ->
  state_dir:string ->
  switch:Lwt_switch.t ->
  log:Log_data.t ->
  src:string ->
  Cluster_api.Custom.recv ->
  (string, [ `Cancelled | `Msg of string ]) Lwt_result.t
