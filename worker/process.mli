type error = [
  | `Cancelled
  | `Exit_code of int
  | `Msg of string
]

(* How to stop the child on cancellation: [`Kill] sends SIGKILL immediately
   (default); [`Terminate_then_kill grace] sends SIGTERM, then SIGKILL only if
   still running after [grace] seconds (lets day10 release its own resources). *)
(* [?stderr] defaults to [`Stdout]: the child's stderr goes into the job log
   with its stdout, on every platform. *)
val exec :
  label:string ->
  log:Log_data.t ->
  switch:Lwt_switch.t ->
  ?env:string array ->
  ?stdin:string ->
  ?stderr:[ `Stdout | Lwt_process.redirection ] ->
  ?is_success:(int -> bool) ->
  ?on_cancel:[ `Kill | `Terminate_then_kill of float ] ->
  string list ->
  (unit, [> error]) Lwt_result.t

val check_call :
  label:string ->
  log:Log_data.t ->
  switch:Lwt_switch.t ->
  ?env:string array ->
  ?stdin:string ->
  ?stderr:[ `Stdout | Lwt_process.redirection ] ->
  ?is_success:(int -> bool) ->
  ?on_cancel:[ `Kill | `Terminate_then_kill of float ] ->
  string list ->
  (unit, [> `Cancelled | `Msg of string]) Lwt_result.t
