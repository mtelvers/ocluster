open Lwt.Infix

let log_src = Logs.Src.create "cluster_worker.day10_cache" ~doc:"day10 cache manager"

module Log = (val Logs.src_log log_src : Logs.LOG)

type t = {
  cache_dir : string;
  max_size : string option;             (* [day10 prune --max-size] target; [--percent 50] if unset *)
  mutable pruning : bool;               (* true while a prune pass is running (new builds wait) *)
  cond : unit Lwt_condition.t;          (* fires when a prune pass finishes / space is fine *)
}

let prune t =
  let policy = match t.max_size with
    | Some size -> [ "--max-size"; size ]
    | None -> [ "--percent"; "90" ]
  in
  let argv = Array.of_list ("day10" :: "prune" :: "--cache-dir" :: t.cache_dir :: policy) in
  Log.info (fun f -> f "Pruning day10 cache: %s" (String.concat " " (Array.to_list argv)));
  Lwt_process.exec ("", argv) >|= function
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n -> Log.warn (fun f -> f "day10 prune exited with code %d" n)
  | Unix.WSIGNALED _ | Unix.WSTOPPED _ -> Log.warn (fun f -> f "day10 prune was killed by a signal")

let create ?prune_threshold ?max_size cache_dir =
  let t = { cache_dir; max_size; pruning = false; cond = Lwt_condition.create () } in
  begin match prune_threshold with
    | None ->
      Log.info (fun f -> f "day10 cache %S: no prune threshold set; automatic pruning disabled" cache_dir)
    | Some threshold ->
      Log.info (fun f ->
          f "day10 cache %S: pruning when partition free space < %.0f%% (%s)"
            cache_dir threshold
            (match max_size with Some s -> "to " ^ s | None -> "keep newest 90%"));
      Lwt.async (fun () ->
          let rec loop () =
            let free = Df.free_space_percent cache_dir in
            Log.info (fun f -> f "day10 cache partition: %.0f%% free" free);
            Prometheus.Gauge.set Metrics.day10_space_free free;
            if free > threshold then (
              t.pruning <- false;
              Lwt_condition.broadcast t.cond ();   (* release any waiting builds *)
              Lwt_unix.sleep 30.0 >>= loop
            ) else (
              t.pruning <- true;
              prune t >>= fun () ->
              Lwt_unix.sleep 30.0 >>= loop
            )
          in
          loop ())
  end;
  t

let run t ~state_dir ~switch ~log ~src custom =
  (if t.pruning then Lwt_condition.wait t.cond else Lwt.return_unit) >>= fun () ->
  Day10_dispatch.run ~cache_dir:t.cache_dir ~state_dir ~switch ~log ~src custom
