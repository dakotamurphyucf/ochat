(** Transport-agnostic API for talking to an MCP server.

    The {!module-type:TRANSPORT} signature abstracts over *how* bytes are
    moved between the client and the server.  A concrete implementation can
    forward messages over a subprocess’ standard I/O (see
    {!module:Mcp_transport_stdio}) or via HTTP/S (see
    {!module:Mcp_transport_http}); future variants may support WebSockets or
    Unix domain sockets.

    The contract is intentionally minimal: {!val:send} and {!val:recv} move
    **whole JSON values** (type {!Jsonaf.t}) without imposing a higher-level
    request/response framing.  This makes the signature equally useful for
    the simple Phase-1 stdio transport and for richer streaming transports
    introduced later.

    {1 Usage}

    Connecting to a server over stdio:
    {[
      Eio_main.run @@ fun env ->
      Eio.Switch.run @@ fun sw ->
        let t =
          Mcp_transport_stdio.connect
            ~sw
            ~env
            "stdio:python3 mcp_server.py"
        in
        Mcp_transport_stdio.send t (`String "ping");
        match Mcp_transport_stdio.recv t with
        | `String "pong" -> print_endline "got pong"
        | _ -> print_endline "unexpected response";
        Mcp_transport_stdio.close t
    ]}
    The example compiles as-is and demonstrates the life-cycle:
    connect → send → recv → close.
*)

module type TRANSPORT = sig
  (** Opaque handle representing a *live* connection to exactly one MCP
      server.  The concrete representation depends on the transport
      implementation and must not leak outside the module. *)

  type t

  (** [connect ?auth ~sw ~env uri] returns a *live* handle linked to [sw].

      Required arguments
      • [sw] – lifetime switch that bounds every resource allocated by the
        transport.  When the switch finishes the implementation closes the
        connection automatically.
      • [env] – {!Eio_unix.Stdenv.base} giving access to the host’s process
        manager, network stack and entropy sources.
      • [uri] – endpoint description.  The scheme selects the concrete
        implementation, e.g. ["stdio:"], ["http:"], ["https:"].

      Optional arguments
      • [auth] – enable implementation-specific authentication helpers
        (default = [true]).  Ignored by the stdio transport; causes the HTTP
        variant to perform bearer-token OAuth 2.

      Behaviour
      • Blocks until the connection is ready to exchange data.
      • Raises [Invalid_argument] if the scheme is unknown or if the attempt
        fails.

      Performance is dominated by the network handshake or process-spawn
      latency, whichever applies. *)
  val connect : ?auth:bool -> sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> string -> t

  (** [send t msg] writes exactly one JSON value.

      • Serialises [msg] using {!Jsonaf.to_string}; implementation-specific
        framing (newline, HTTP chunk, …) is added afterwards.
      • Blocks until **all** bytes have been handed to the operating system
        or until the peer closes the connection.
      • Mutating [msg] after the call returns is safe; the transport keeps no
        reference.

      @raise Connection_closed If the underlying stream is closed.

      Performance: the stdio implementation is `O(length msg)` (single
      [Eio.Flow.copy_string]); other transports may buffer internally. *)
  val send : t -> Jsonaf.t -> unit

  (** [recv t] returns the next JSON value emitted by the server.

      • Blocks until a *full* value has been decoded.
      • Values are delivered in FIFO order w.r.t. the wire.

      @raise Connection_closed If EOF is observed before a full value has
        arrived. *)
  val recv : t -> Jsonaf.t

  (** Raised by {!val:send} or {!val:recv} if the underlying stream reached
      EOF or was closed explicitly.  Clients can catch the exception and try
      to re-connect. *)
  exception Connection_closed

  (** [is_closed t] is [true] after {!val:close} completes *or* after
      [Connection_closed] has been raised.  The flag never toggles back to
      [false]. *)
  val is_closed : t -> bool

  (** [close t] shuts the connection down and frees resources.

      Idempotent; concurrent or repeated invocations are allowed.  After the
      function returns {!val:is_closed} yields [true] and both {!val:send}
      and {!val:recv} raise [Connection_closed]. *)
  val close : t -> unit
end
