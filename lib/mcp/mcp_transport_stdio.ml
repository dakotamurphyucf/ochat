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
  ; mutable disposed : bool
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
      Eio.Mutex.use_rw ~protect:false write_mutex (fun () ->
        Eio.Flow.copy_string line stdin_w)
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | _ -> raise Connection_closed
  in
  let recv_fn () : Jsonaf.t =
    try
      let line =
        Eio.Mutex.use_rw ~protect:false read_mutex (fun () -> Eio.Buf_read.line reader)
      in
      Jsonaf.of_string line
    with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
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
    let await_child =
      match Eio.Switch.check sw with
      | () -> true
      | exception Eio.Cancel.Cancelled _ -> false
      | exception Invalid_argument _ -> false
    in
    (* Close our pipe ends first – this should trigger graceful
         shutdown in well-behaved children. *)
    (try Eio.Flow.close stdin_w with
     | _ -> ());
    (try Eio.Flow.close stdout_r with
     | _ -> ());
    (* During switch release, Eio's process-reaper daemon has already stopped.
       Its earlier release hook will kill/reap the child after this hook returns;
       awaiting its promise here would prevent that hook from ever running. *)
    match await_child with
    | false -> ()
    | true ->
      (try ignore (Eio.Process.await child : Eio.Process.exit_status) with
       | Eio.Cancel.Cancelled _ as exn -> raise exn
       | _ -> ())
  in
  { send_fn; recv_fn; close_fn; closed = false; disposed = false }
;;

(*---------------------  public API  ---------------------------------*)

let connect : ?auth:bool -> sw:Eio.Switch.t -> env:< process_mgr : _ ; .. > -> string -> t
  =
  fun ?(auth = true) ~sw ~env uri ->
  let _auth = auth in
  (* Expected URI format: "stdio:<command line>" *)
  let prefix = "stdio:" in
  if not (String.is_prefix uri ~prefix)
  then invalid_arg "Mcp_transport_stdio.connect: uri must start with \"stdio:\"";
  let cmdline =
    String.sub
      uri
      ~pos:(String.length prefix)
      ~len:(String.length uri - String.length prefix)
    |> String.substr_replace_all ~pattern:"%20" ~with_:" "
  in
  (* Split on whitespace – rudimentary, but sufficient for Phase-1. *)
  let cmd_list =
    if String.is_empty cmdline
    then invalid_arg "Mcp_transport_stdio.connect: empty command line"
    else
      String.split_on_chars ~on:[ ' '; Char.of_int_exn 32 ] cmdline
      |> List.filter ~f:(fun s -> not (String.is_empty s))
  in
  (* Check that we have a command to run. *)
  (* Spawn the child process and return the transport interface. *)
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
  if not t.disposed
  then (
    t.disposed <- true;
    t.closed <- true;
    try t.close_fn () with
    | Eio.Cancel.Cancelled _ as exn -> raise exn
    | _ -> ())
;;
