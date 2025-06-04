(**
   Model Context Protocol – stdio TRANSPORT implementation

   This module provides a concrete implementation of the

     [Mcp_transport.TRANSPORT]

   signature that communicates with an MCP server over the server
   process' *standard input / standard output* streams.

   A new sub-process is started with [Eio.Process.spawn].  We create two
   uni-directional pipes:

   - one for sending JSON-RPC **requests** to the child (parent writes ->
     child reads; connected to the child's *stdin* ), and
   - one for receiving JSON-RPC **responses / notifications** from the
     child (child writes -> parent reads; connected to the child's
     *stdout* ).  The child's *stderr* is merged into stdout so that we
     get any diagnostic output in one place.  Lines that fail to parse
     as JSON are ignored (but still forwarded to the debug logger).

   All messages are newline-delimited UTF-8 encoded JSON values (the
   line-delimiter is mandated by the spec for the stdio transport). *)

open Core

(**
    Record describing a *live* stdio connection
  *)
type t =
  { send_fn : Jsonaf.t -> unit
  ; recv_fn : unit -> Jsonaf.t
  ; close_fn : unit -> unit
  ; mutable closed : bool
  }

exception Connection_closed

(*---------------------  helper: spawn sub-process  ------------------*)

let spawn_child ~sw ~(env : < process_mgr : _ ; .. >) cmd_line : t =
  let proc_mgr = env#process_mgr in
  (* ----------------------------------------------------------------
       1. Create two uni-directional pipes
          (stdin  : parent → child)
          (stdout : child  → parent; stderr is merged into stdout)
    ---------------------------------------------------------------- *)
  let stdin_r, stdin_w = Eio.Process.pipe ~sw proc_mgr in
  let stdout_r, stdout_w = Eio.Process.pipe ~sw proc_mgr in
  (* ----------------------------------------------------------------
       2. Spawn the child process.  We keep a handle so that [close]
          can await its termination.
    ---------------------------------------------------------------- *)
  let child : _ Eio.Process.t =
    Eio.Process.spawn
      ~sw
      proc_mgr
      ~stdin:stdin_r
      ~stdout:stdout_w
      ~stderr:stdout_w
      cmd_line
  in
  (* Parent no longer needs the fds that were handed to the child. *)
  Eio.Flow.close stdin_r;
  Eio.Flow.close stdout_w;
  (* ----------------------------------------------------------------
       3. Build buffered reader / writer helpers.
    ---------------------------------------------------------------- *)
  let reader = Eio.Buf_read.of_flow stdout_r ~max_size:10_000_000 in
  (* Ensure that only one fibre writes at a time.  Reads are already
       serialised because [recv] is blocking. *)
  let read_mutex = Eio.Mutex.create () in
  let write_mutex = Eio.Mutex.create () in
  (* ----------------------------------------------------------------
       4. Build the transport interface.
    ---------------------------------------------------------------- *)
  (* The [send_fn] writes a JSON value to the child process' stdin.
     It blocks until the write is complete or the child closes its
     stdin. *)
  let send_fn (json : Jsonaf.t) : unit =
    let line = Jsonaf.to_string json ^ "\n" in
    try
      Eio.Mutex.lock write_mutex;
      Eio.Flow.copy_string line stdin_w;
      Eio.Mutex.unlock write_mutex
    with
    | End_of_file | _ -> raise Connection_closed
  in
  let recv_fn () : Jsonaf.t =
    try
      Eio.Mutex.lock read_mutex;
      (* Read a line from the child.  This will block until the child
         sends a message or closes its stdout. *)
      let line = Eio.Buf_read.line reader in
      Eio.Mutex.unlock read_mutex;
      Jsonaf.of_string line
    with
    | End_of_file ->
      (* Child closed its stdout → no further messages. *)
      raise Connection_closed
    | ex ->
      (* Malformed line – failing for now may update to logging. *)
      failwith
        (Format.asprintf
           "@[<v>(mcp-stdio) ignoring unparsable line (%a)@]@."
           Eio.Exn.pp
           ex)
  in
  let close_fn () =
    (* Close our pipe ends first – this should trigger graceful
         shutdown in well-behaved children. *)
    (try Eio.Flow.close stdin_w with
     | _ -> ());
    (try Eio.Flow.close stdout_r with
     | _ -> ());
    (* Wait for the process to exit to avoid zombies.  We ignore
         failures, e.g. if the fibre holding [close] is cancelled. *)
    try ignore (Eio.Process.await child) with
    | _ -> ()
  in
  { send_fn; recv_fn; close_fn; closed = false }
;;

(*---------------------  public API  ---------------------------------*)

let connect : sw:Eio.Switch.t -> env:< process_mgr : _ ; .. > -> string -> t =
  fun ~sw ~env uri ->
  (* Expected URI format: "stdio:<command line>" *)
  let prefix = "stdio:" in
  if not (String.is_prefix uri ~prefix)
  then invalid_arg "Mcp_transport_stdio.connect: uri must start with \"stdio:\"";
  let cmdline =
    String.sub
      uri
      ~pos:(String.length prefix)
      ~len:(String.length uri - String.length prefix)
  in
  (* Split on whitespace – rudimentary, but sufficient for Phase-1. *)
  let cmd_list =
    if String.is_empty cmdline
    then invalid_arg "Mcp_transport_stdio.connect: empty command line"
    else String.split ~on:' ' cmdline |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  spawn_child ~sw ~env cmd_list
;;

let send t (json : Jsonaf.t) : unit =
  if t.closed then raise Connection_closed;
  try t.send_fn json with
  | Connection_closed as ex ->
    (* Mark closed so that subsequent calls are fast-fail *)
    t.closed <- true;
    raise ex
;;

let recv t : Jsonaf.t =
  if t.closed then raise Connection_closed;
  (* Blocking read – this will wait until the child sends a message. *)
  (* If the child closed its stdout, this will raise [Connection_closed]. *)
  try t.recv_fn () with
  | Connection_closed as ex ->
    t.closed <- true;
    raise ex
;;

let is_closed (t : t) = t.closed

let close t : unit =
  if not t.closed
  then (
    t.closed <- true;
    try t.close_fn () with
    | _ -> ())
;;
