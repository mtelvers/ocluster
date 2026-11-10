open Lwt.Infix

let max_chunk_size = 10240L

type t = {
  data : Buffer.t;
  mutable cond : [ `Running of unit Lwt_condition.t
                 | `Finished ]
}

let create () =
  {
    data = Buffer.create 10240;
    cond = `Running (Lwt_condition.create ());
  }

let rec stream t ~start =
  let len = Int64.of_int (Buffer.length t.data) in
  let start = if start < 0L then max 0L (Int64.add len start) else start in
  let avail = Int64.sub len start in
  if avail < 0L then Fmt.failwith "Start value out of range!";
  if avail = 0L then (
    match t.cond with
    | `Running cond ->
      Lwt_condition.wait cond >>= fun () ->
      stream t ~start
    | `Finished ->
      Lwt.return ("", start)
  ) else (
    let chunk = min avail max_chunk_size in
    let next = Int64.add start chunk in
    let start = Int64.to_int start in
    let chunk = Int64.to_int chunk in
    Lwt.return (Buffer.sub t.data start chunk, next)
  )

let write t data =
  match t.cond with
  | `Running cond ->
    Buffer.add_string t.data data;
    Lwt_condition.broadcast cond ()
  | `Finished ->
    Fmt.failwith "Attempt to write to log after close: %S" data

(* Drop the '\r's that end lines as bytes are copied into the log.  Windows
   build tools (day10's own output and the subprocesses it forwards) end lines
   with "\r\n", and opam's progress display adds a '\r' of its own before
   that, so what arrives is "\r\r\n"; copied verbatim, a trailing '\r'
   renders as a blank line and is swallowed into log-matcher captures
   (patterns match [(.+)\n]).  Every run of '\r' that ends a line goes; a
   '\r' inside a line (a progress bar redrawn in place) stays, and Linux logs
   are unchanged. *)

(* [s] split into its prefix and the run of '\r' that ends it. *)
let trailing_cr s =
  let rec start i = if i > 0 && s.[i - 1] = '\r' then start (i - 1) else i in
  let i = start (String.length s) in
  (String.sub s 0 i, String.sub s i (String.length s - i))

(* [pending] is the run of '\r' that ended the previous chunk, whose '\n' may
   begin this one; the run ending this chunk is returned in its place. *)
let strip_cr ~pending data =
  match List.rev (String.split_on_char '\n' (pending ^ data)) with
  | [] -> ("", "") (* split_on_char never returns [] *)
  | last :: rev_lines ->
    let last, pending = trailing_cr last in
    let lines = List.rev_map (fun line -> fst (trailing_cr line)) rev_lines in
    (String.concat "\n" (lines @ [ last ]), pending)

let copy_from_stream t src =
  let rec aux pending =
    Lwt_io.read ~count:4096 src >>= function
    | "" ->
      (* A run of '\r' at the end of the stream: nothing follows, so emit it. *)
      if pending <> "" then write t pending;
      Lwt.return_unit
    | data ->
      let out, pending = strip_cr ~pending data in
      if out <> "" then write t out;
      aux pending
  in
  aux ""

let close t =
  match t.cond with
  | `Running cond ->
    t.cond <- `Finished;
    Lwt_condition.broadcast cond ()
  | `Finished ->
    Fmt.failwith "Log already closed!"

let info t fmt =
  Fmt.kstr (write t) (fmt ^^ "@.")
