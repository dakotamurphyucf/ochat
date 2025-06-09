(** Minimal transport abstraction for talking to an MCP server.  The
    design is intentionally tiny – just enough for the Phase-1 stdio
    client implementation.  Additional helpers (streaming, batching,
    HTTP, …) will be added in later milestones. *)

module type TRANSPORT = sig
  (** Abstract handle representing a *live* connection to a single MCP
            server.  The concrete type is transport-specific. *)

  type t

  (** [connect ~sw ~env uri] establishes a new connection.
            For the stdio transport [uri] is expected to have the form
      
            {v
              "stdio:<command line>"
            v}
      
            where the substring after the first ':' is the command that will
            be spawned (using [Eio.Process.spawn]).
      
            For transports that do not need to spawn a process (e.g. HTTP)
            the [uri] will be interpreted differently.
      
            The returned handle is valid until [close] is called *or* the
            enclosing switch [sw] finishes. *)
  val connect : sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> string -> t

  (** [send t msg] serialises [msg] to JSON and writes it to the
            underlying stream.  The function is *blocking* until all bytes
            have been handed to the OS. *)
  val send : t -> Jsonaf.t -> unit

  (** [recv t] blocks until a single complete JSON value has been read
            from the stream and returns it. *)
  val recv : t -> Jsonaf.t

  (** Raised by [send] and [recv] when the underlying connection has been
      closed (EOF or explicit [close]).  Callers can catch the exception
      and decide whether to re-connect. *)
  exception Connection_closed

  (** Query whether the connection is still alive.  Guaranteed to return
      [false] once [close] has been called or EOF was observed. *)
  val is_closed : t -> bool

  (** Close the connection, terminate any sub-processes and free all
            resources belonging to the transport.  Idempotent. *)
  val close : t -> unit
end
