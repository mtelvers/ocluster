open Lwt.Infix

type error = [
  | `Cancelled
  | `Exit_code of int
  | `Msg of string
]

let send_to ch contents =
  Lwt.try_bind
    (fun () ->
       Lwt_io.write ch contents >>= fun () ->
       Lwt_io.close ch
    )
    (fun () -> Lwt.return (Ok ()))
    (fun ex -> Lwt.return (Fmt.error_msg "%a" Fmt.exn ex))

(* How to stop the child when the job is cancelled.
   [`Kill] kills it at once (SIGKILL, or TerminateProcess on Windows): the
   default, for children that hold no resources of their own to release.
   [`Interrupt_then_kill grace] interrupts it, and kills it only if it is
   still running [grace] seconds later.  day10 uses the latter: the interrupt
   lets it tear down its own container, overlay mount and temp directory
   before exiting, which a kill prevents.

   The interrupt is the same on every platform as far as the child can tell:
   SIGINT.  On Unix it is sent as such.  On Windows it is Ctrl-C on the
   child's console, sent by a helper process (see console_interrupt.c), which
   the C runtime delivers as SIGINT; where that cannot be done the child is
   killed instead. *)
external console_interrupt_helper : int -> int -> int = "ocluster_console_interrupt_helper"

let interrupt (proc : #Lwt_process.process_out) =
  if Sys.win32 then
    Lwt.catch
      (fun () ->
         Lwt_process.exec ("", [| Sys.executable_name; "console-interrupt"; string_of_int proc#pid; string_of_int (Unix.getpid ()) |])
         >|= function
         | Unix.WEXITED 0 -> true
         | _ -> false)
      (fun ex ->
         Log.warn (fun f -> f "console-interrupt helper failed: %a" Fmt.exn ex);
         Lwt.return false)
  else (proc#kill Sys.sigint; Lwt.return true)

(* [`Stdout], the default, sends the child's stderr into the job log with its
   stdout, through one pipe given to the child as both.  The old default,
   [`FD_copy Unix.stdout], only did that on Unix, and by accident: Lwt applies
   redirections in the forked child in order, so by the time it copies fd 1 to
   fd 2, fd 1 is already the log pipe.  On Windows the handles are resolved in
   the parent, so it meant the worker's own stdout, and day10's verdict
   ([NOTE] success, [ERROR] failure) never reached the job log. *)
let spawn ?env ~stderr cmd =
  match stderr with
  | `Stdout ->
    let r, w = Lwt_unix.pipe_in ~cloexec:true () in
    let proc = Lwt_process.open_process_out ?env ~stdout:(`FD_copy w) ~stderr:(`FD_copy w) cmd in
    Unix.close w;
    (proc :> Lwt_process.process_out), Lwt_io.of_fd ~mode:Lwt_io.input r
  | #Lwt_process.redirection as stderr ->
    let proc = Lwt_process.open_process ?env ~stderr cmd in
    (proc :> Lwt_process.process_out), proc#stdout

let exec ~label ~log ~switch ?env ?(stdin="") ?(stderr=`Stdout) ?(is_success=((=) 0)) ?(on_cancel=`Kill) cmd =
  Log.info (fun f -> f "Exec(%s): %a" label Fmt.(list ~sep:sp (quote string)) cmd);
  let cmd = "", Array.of_list cmd in
  let proc, output = spawn ?env ~stderr cmd in
  Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
      if Lwt.state proc#status = Lwt.Sleep then (
        Log.info (fun f -> f "Cancelling %s job…" label);
        match on_cancel with
        | `Kill -> proc#terminate
        | `Interrupt_then_kill grace ->
          Lwt.async (fun () ->
              interrupt proc >>= function
              | false ->
                Log.info (fun f -> f "Cannot interrupt %s; killing it" label);
                proc#terminate;
                Lwt.return_unit
              | true ->
                Lwt_unix.sleep grace >|= fun () ->
                if Lwt.state proc#status = Lwt.Sleep then (
                  Log.info (fun f ->
                      f "%s still running %.0fs after the interrupt; killing it" label grace);
                  proc#terminate))
      );
      Lwt.return_unit
    )
  >>= fun () ->
  let copy_thread = Log_data.copy_from_stream log output >>= fun () -> Lwt_io.close output in
  send_to proc#stdin stdin >>= fun stdin_result ->
  copy_thread >>= fun () -> (* Ensure all data has been copied before returning *)
  proc#status >|= function
  | _ when not (Lwt_switch.is_on switch) -> Error `Cancelled
  | Unix.WEXITED n when is_success n ->
    begin match stdin_result with
      | Ok () -> Ok ()
      | Error (`Msg msg) -> Fmt.error_msg "Failed sending input to %s: %s" label msg
    end
  | Unix.WEXITED n -> Error (`Exit_code n)
  | Unix.WSIGNALED x -> Fmt.error_msg "%s failed with signal %a" label Fmt.Dump.signal x
  | Unix.WSTOPPED x -> Fmt.error_msg "%s stopped with signal %a" label Fmt.Dump.signal x

let check_call ~label ~log ~switch ?env ?stdin ?stderr ?is_success ?on_cancel cmd =
  exec ~label ~log ~switch ?env ?stdin ?stderr ?is_success ?on_cancel cmd >|= function
  | Ok () -> Ok ()
  | Error `Cancelled -> Error `Cancelled
  | Error (`Exit_code n) -> Fmt.error_msg "%s failed with exit-code %d" label n
  | Error (`Msg _) as e -> e
