type error = [
  | `Cancelled
  | `Exit_code of int
  | `Msg of string
]

(* How to stop the child on cancellation: [`Kill] kills it immediately
   (default); [`Interrupt_then_kill grace] interrupts it (SIGINT on Unix,
   Ctrl-C on its own console on Windows), then kills it only if still running
   after [grace] seconds (lets day10 release its own resources). *)
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
  ?on_cancel:[ `Kill | `Interrupt_then_kill of float ] ->
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
  ?on_cancel:[ `Kill | `Interrupt_then_kill of float ] ->
  string list ->
  (unit, [> `Cancelled | `Msg of string]) Lwt_result.t

(* The body of [worker.exe console-interrupt PID WORKER] on Windows: send
   Ctrl-C to the processes on PID's console unless WORKER is on it too.
   Returns the exit code; see console_interrupt.c. *)
val console_interrupt_helper : int -> int -> int
