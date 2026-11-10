let dir_exists d =
  match Unix.lstat d with
  | Unix.{ st_kind = S_DIR; _ } -> true
  | _ -> false
  | exception Unix.Unix_error(Unix.ENOENT, _, _) -> false

let ensure_dir path =
  if not (dir_exists path) then Unix.mkdir path 0o700

let run name state_dir ?style_renderer (main:?formatter:Format.formatter -> unit -> unit) =
  let stop_notification = Lwt_unix.make_notification ~once:true (fun () -> exit 0) in
  let module Svc = Winsvc.Make
    (struct
      let name = name
      let display = ""
      let text = ""
      let arguments = []
      let stop () = Lwt_unix.send_notification stop_notification
    end)
  in
  Random.self_init ();
  let name =
    Printf.sprintf "%s-%d.log" (Sys.executable_name |> Filename.basename |> Filename.remove_extension) (Random.bits ())
    |> Filename.concat state_dir
  in
  try
    ensure_dir state_dir;
    let log = open_out_bin name in
    (* A service has no console, so the process's standard handles are not
       valid, and every child inherits them: Process.exec gives a child a pipe
       for stdout but copies the worker's own stdout as its stderr, and day10
       passes that on again to ctr and hcn-namespace. Under the service on
       ltsc2025-day10 day10 silently went away before its first container
       while the same binary run with stdio redirected to a file worked. Point
       both at the worker's log: the children get real handles, and what they
       write to stderr is kept next to the worker's own messages. *)
    let fd = Unix.descr_of_out_channel log in
    Unix.dup2 fd Unix.stdout;
    Unix.dup2 fd Unix.stderr;
    let formatter = Fmt_tty.setup ?style_renderer log in
    Svc.run (main ~formatter)
  with Failure _ ->
    begin try Sys.remove name with Sys_error _ -> () end;
    main ()

let install name display text arguments =
  let module Svc = Winsvc.Make
    (struct
      let name = name
      let display = display
      let text = text
      let arguments = arguments
      let stop () = ()
    end)
  in
  Svc.install ()

let remove name =
  let module Svc = Winsvc.Make
    (struct
      let name = name
      let display = ""
      let text = ""
      let arguments = []
      let stop () = ()
    end)
  in
  Svc.remove ()
