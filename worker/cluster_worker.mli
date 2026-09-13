type job_spec = [
  | `Docker of [ `Contents of string | `Path of string ] * Cluster_api.Docker.Spec.options
  | `Obuilder of [ `Contents of string ]
  | `Custom of Cluster_api.Custom.recv
]

module Obuilder_config = Obuilder_build.Config

type build =
  switch:Lwt_switch.t ->
  log:Log_data.t ->
  src:string ->
  secrets:(string * string) list ->
  job_spec ->
  (string, [`Cancelled | `Msg of string]) Lwt_result.t

val default_build : ?obuilder:Obuilder_build.t -> ?day10:Day10.t -> state_dir:string -> build
(** The default build that is used if no [build] argument is given to {! run}.
    @param day10_cache When set, enables [Custom] jobs of kind ["day10"] and is
      used as day10's build-cache directory (passed as [--cache-dir]). When
      unset, day10 jobs are rejected. *)

val run :
  ?switch:Lwt_switch.t ->
  ?build:build ->
  ?day10_cache:string ->
  ?day10_prune_threshold:float ->
  ?day10_cache_max_size:string ->
  ?allow_push:string list ->
  ?healthcheck_period:float ->
  ?prune_threshold:float ->
  ?docker_max_df_size:float ->
  ?obuilder_prune_threshold:float ->
  ?obuilder_prune_limit:int ->
  ?obuilder:Obuilder_config.t ->
  ?additional_metrics:(string * Uri.t) list ->
  update:(unit -> (unit -> unit Lwt.t) Lwt.t) ->
  capacity:int ->
  name:string ->
  state_dir:string ->
  Cluster_api.Raw.Client.Registration.t Capnp_rpc_lwt.Sturdy_ref.t ->
  unit Lwt.t
(** [run ~capacity ~name ~state_dir registry] runs a builder that connects to registry and runs up
    to [capacity] jobs at once. The builder registers using the unique ID [name].
    @param switch Turning this off causes the builder to exit (for unit-tests).
    @param build Used to override the default build action (for unit-tests or custom job
      specifications).
    @param day10_cache When set, enables [Custom] jobs of kind ["day10"] and is used as
      day10's build-cache directory. When unset, day10 jobs are rejected.
    @param day10_prune_threshold When set (and [day10_cache] is set), a background loop runs
      [day10 prune] whenever the day10 cache partition's free space falls below this percentage
      (0 to 100). Prunes only gate new day10 builds, not the whole worker.
    @param day10_cache_max_size Target size for [day10 prune --max-size] (e.g. ["40G"]) used by
      the day10 prune loop; when unset the prune keeps the newest 90% by size.
    @param allow_push Docker repositories to which results can be pushed.
    @param healthcheck_period Time period, in seconds, at which the health of the worker is checked.
    @param prune_threshold Stop and run [docker system prune -af] if free-space is less than this
      percentage (0 to 100).
    @param docker_max_df_size Run [docker system df] to get the amount of memory being taken up by
      the images and if this is greater than [docker_max_df_size], run [docker system prune].
    @param obuilder_prune_threshold The threshold for OBuilder to prune the store of cached build
      steps.
    @param obuilder OBuilder's configuration.
    @param additional_metrics A list of additional prometheus endpoints with a descriptive name to
      collect.
    @param update Function to run on "selfUpdate" requests. It should do any preparation (such as
      downloading new images), then return a function to do the actual update. This is so that the
      first part can run while the node finishes its remaining jobs. The second part is called once
      all jobs are finished.  If the second function returns, the process will exit.
    @param capacity The number of builds that can run in parallel.
    @param name Unique builder name.
    @param state_dir A persistent directory for Git caches, etc. *)

module Process = Process
module Log_data = Log_data
