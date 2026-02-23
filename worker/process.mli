type error = [
  | `Cancelled
  | `Exit_code of int
  | `Msg of string
]

val exec :
  ?cwd:string ->
  ?env:string array ->
  Lwt_process.command ->
  Unix.process_status Lwt.t
(** Drop-in replacement for [Lwt_process.exec]. On Windows, bypasses
    Lwt_process (which has pipe EOF bugs) and uses [Unix.create_process]
    via [cmd.exe /c] with [Lwt_unix.waitpid]. On other platforms, delegates
    to [Lwt_process.exec]. *)

val pread_line :
  ?cwd:string ->
  ?env:string array ->
  Lwt_process.command ->
  string Lwt.t
(** Drop-in replacement for [Lwt_process.pread_line]. On Windows, uses
    manual pipes with cloexec for reliable EOF. On other platforms, delegates
    to [Lwt_process.pread_line]. *)

val run :
  label:string ->
  log:Log_data.t ->
  switch:Lwt_switch.t ->
  ?env:string array ->
  ?stdin:string ->
  ?stderr:Lwt_process.redirection ->
  ?is_success:(int -> bool) ->
  string list ->
  (unit, [> error]) Lwt_result.t

val check_call :
  label:string ->
  log:Log_data.t ->
  switch:Lwt_switch.t ->
  ?env:string array ->
  ?stdin:string ->
  ?stderr:Lwt_process.redirection ->
  ?is_success:(int -> bool) ->
  string list ->
  (unit, [> `Cancelled | `Msg of string]) Lwt_result.t
