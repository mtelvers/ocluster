open Lwt.Infix

type error = [
  | `Cancelled
  | `Exit_code of int
  | `Msg of string
]

(* On Windows, Lwt_process and Lwt pipes have issues with short-lived processes
   where EOF is never detected. Use obuilder's Os.win32_pread (temp file + polling
   waitpid) for reliable process execution, then write captured output to the log.
   On other platforms, use Lwt_process directly. *)

let win32_pread_cmd ?cwd ?env cmd =
  let argv = ["cmd"; "/c"] @
    (match cwd with Some dir -> ["cd"; "/d"; dir; "&&"] | None -> []) @
    cmd
  in
  match env with
  | Some env ->
    (* win32_pread doesn't support env, so use create_process_env directly *)
    let tmpfile = Filename.temp_file "ocluster-" ".out" in
    let dev_null_in = Unix.openfile "NUL" [Unix.O_RDONLY] 0 in
    let tmpfd = Unix.openfile tmpfile [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
    Lwt.catch (fun () ->
      let cmd_exe = {|C:\Windows\System32\cmd.exe|} in
      let cmd_args = Array.of_list argv in
      let pid = Unix.create_process_env cmd_exe cmd_args env dev_null_in tmpfd tmpfd in
      Unix.close dev_null_in;
      Unix.close tmpfd;
      Obuilder.Os.win32_poll_waitpid pid >>= fun status ->
      let output =
        let ic = open_in_bin tmpfile in
        Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
        really_input_string ic (in_channel_length ic)
      in
      (try Unix.unlink tmpfile with _ -> ());
      Lwt.return (status, output)
    ) (fun exn ->
      (try Unix.close dev_null_in with _ -> ());
      (try Unix.close tmpfd with _ -> ());
      (try Unix.unlink tmpfile with _ -> ());
      Lwt.fail exn)
  | None ->
    Obuilder.Os.win32_pread argv >>= function
    | Ok output -> Lwt.return (Unix.WEXITED 0, output)
    | Error (`Msg msg) ->
      (* Parse exit code from error message if possible *)
      Lwt.return (Unix.WEXITED 1, msg)

let exec ?cwd ?env cmd =
  if Sys.win32 then begin
    let _, argv = cmd in
    win32_pread_cmd ?cwd ?env (Array.to_list argv) >|= fun (status, _) -> status
  end else
    Lwt_process.exec ?cwd ?env cmd

let pread_line ?cwd ?env cmd =
  if Sys.win32 then begin
    let _, argv = cmd in
    win32_pread_cmd ?cwd ?env (Array.to_list argv) >>= fun (_, output) ->
    let line = match String.split_on_char '\n' (String.trim output) with
      | l :: _ -> String.trim l
      | [] -> ""
    in
    Lwt.return line
  end else
    Lwt_process.pread_line ?env cmd

let send_to ch contents =
  Lwt.try_bind
    (fun () ->
       Lwt_io.write ch contents >>= fun () ->
       Lwt_io.close ch
    )
    (fun () -> Lwt.return (Ok ()))
    (fun ex -> Lwt.return (Fmt.error_msg "%a" Fmt.exn ex))

let run ~label ~log ~switch ?env ?(stdin="") ?(stderr=`FD_copy Unix.stdout) ?(is_success=((=) 0)) cmd =
  Log.info (fun f -> f "Exec(%s): %a" label Fmt.(list ~sep:sp (quote string)) cmd);
  if Sys.win32 then begin
    (* Write stdin to a temp file if non-empty *)
    let stdin_result =
      if String.length stdin > 0 then begin
        let tmpfile = Filename.temp_file "ocluster-stdin-" ".tmp" in
        (try
           let oc = open_out_bin tmpfile in
           Fun.protect ~finally:(fun () -> close_out oc) (fun () ->
             output_string oc stdin);
           Ok (Some tmpfile)
         with ex ->
           (try Unix.unlink tmpfile with _ -> ());
           Error (`Msg (Printexc.to_string ex)))
      end else
        Ok None
    in
    match stdin_result with
    | Error _ as e ->
      Lwt.return (e :> (unit, [> error]) result)
    | Ok stdin_file ->
      let output_file = Filename.temp_file "ocluster-" ".log" in
      let out_fd = Unix.openfile output_file [Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC] 0o600 in
      let stdin_fd = match stdin_file with
        | Some f -> Unix.openfile f [Unix.O_RDONLY] 0
        | None -> Unix.openfile "NUL" [Unix.O_RDONLY] 0
      in
      let stderr_fd = match (stderr :> [> `FD_copy of Unix.file_descr]) with `FD_copy fd -> fd | _ -> Unix.stdout in
      let cmd_exe = {|C:\Windows\System32\cmd.exe|} in
      let cmd_args = Array.of_list (["cmd"; "/c"] @ cmd) in
      let pid = match env with
        | Some env -> Unix.create_process_env cmd_exe cmd_args env stdin_fd out_fd stderr_fd
        | None -> Unix.create_process cmd_exe cmd_args stdin_fd out_fd stderr_fd
      in
      Unix.close stdin_fd;
      Unix.close out_fd;
      Option.iter (fun f -> try Unix.unlink f with _ -> ()) stdin_file;
      (* Set up cancellation *)
      Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
          (match Unix.waitpid [Unix.WNOHANG] pid with
           | (0, _) ->
             Log.info (fun f -> f "Cancelling %s job…" label);
             (try Unix.kill pid Sys.sigkill with Unix.Unix_error _ -> ())
           | _ -> ()
           | exception Unix.Unix_error _ -> ());
          Lwt.return_unit
        )
      >>= fun () ->
      Obuilder.Os.win32_poll_waitpid ~sleep_interval:0.1 pid >>= fun status ->
      (* Read output from temp file and write to log *)
      let output =
        try
          let ic = open_in_bin output_file in
          Fun.protect ~finally:(fun () -> close_in ic) @@ fun () ->
          really_input_string ic (in_channel_length ic)
        with _ -> ""
      in
      (try Unix.unlink output_file with _ -> ());
      if String.length output > 0 then
        Log_data.write log output;
      Lwt.return @@ (begin
        match status with
        | _ when not (Lwt_switch.is_on switch) -> Error `Cancelled
        | Unix.WEXITED n when is_success n -> Ok ()
        | Unix.WEXITED n -> Error (`Exit_code n)
        | Unix.WSIGNALED x -> Fmt.error_msg "%s failed with signal %a" label Fmt.Dump.signal x
        | Unix.WSTOPPED x -> Fmt.error_msg "%s stopped with signal %a" label Fmt.Dump.signal x
      end :> (unit, [> error]) result)
  end else begin
    let cmd' = "", Array.of_list cmd in
    let proc = Lwt_process.open_process ?env ~stderr cmd' in
    Lwt_switch.add_hook_or_exec (Some switch) (fun () ->
        if Lwt.state proc#status = Lwt.Sleep then (
          Log.info (fun f -> f "Cancelling %s job…" label);
          proc#terminate;
        );
        Lwt.return_unit
      )
    >>= fun () ->
    let copy_thread = Log_data.copy_from_stream log proc#stdout in
    send_to proc#stdin stdin >>= fun stdin_result ->
    copy_thread >>= fun () ->
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
  end

let check_call ~label ~log ~switch ?env ?stdin ?stderr ?is_success cmd =
  run ~label ~log ~switch ?env ?stdin ?stderr ?is_success cmd >|= function
  | Ok () -> Ok ()
  | Error `Cancelled -> Error `Cancelled
  | Error (`Exit_code n) -> Fmt.error_msg "%s failed with exit-code %d" label n
  | Error (`Msg _) as e -> e
