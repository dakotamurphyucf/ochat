(** High-level MCP client (non-blocking).

    {1 Design}

    The client opens **one underlying transport connection** (currently
    either {!Mcp_transport_stdio} or {!Mcp_transport_http}) and starts a
    dedicated *receiver fibre*.  The fibre continuously reads JSON-RPC
    packets, parses them with {!Mcp_types.Jsonrpc}, and then:

    • *Responses* – matched to a promise resolver stored in a per-request
      hash-table; the corresponding {!Eio.Promise.t} is then resolved.
    • *Notifications* – forwarded verbatim to {!notifications} so that
      callers can subscribe if needed.

    All helpers therefore come in two flavours:

    • [`*_async`] – fire-and-forget functions that return an
      [`('a, string) result Eio.Promise.t`] immediately.
    • *blocking wrappers* (`list_tools`, `call_tool`, …) that simply
      {!Eio.Promise.await} the asynchronous variant and expose a regular
      [('a, string) result].

    {1 Supported transports}

    The URI passed to {!connect} selects the transport:

    • "stdio:<command>" – spawn a local process and communicate over
      JSON lines on stdin/stdout.
    • Any `http` / `https` (or `mcp+http/https`) URI – experimental
      streamable HTTP transport.

    Additional transports can be added without changing the public API.

    {1 Concurrency model}

    • **Thread-safe** – all public functions may be called concurrently
      from multiple fibres.
    • **Single outstanding resolver per request id** – the client
      guarantees that at most one resolver is stored for any
      {!Mcp_types.Jsonrpc.Id.t}.
    • **Cancellation** – closing the underlying {!transport} will cause
      all awaiting fibres to fail with {!Connection_closed}.
*)

type t

(** [connect ?auth ~sw ~env uri] opens a new connection to the MCP
    server designated by [uri] and returns a fresh client handle.

    The function performs the mandatory
    [initialize]/[initialized] JSON-RPC handshake *synchronously* before
    it returns; once the promise is fulfilled the returned client is
    ready to accept requests.

    Parameters:
    • [?auth] – set to [false] to disable transport-level
      authentication (default = [true]).
    • [sw] – switch whose lifetime bounds the client; closing the
      switch closes the connection and cancels all pending promises.
    • [env] – {!Eio_unix.Stdenv.base} passed to the transport (used for
      spawning processes, opening sockets, …).
    • [uri] – transport-selecting identifier (see module doc).

    @raise Mcp_transport_stdio.Connection_closed if the stdio transport
           terminates before the handshake completes.
    @raise Mcp_transport_http.Connection_closed for the HTTP transport.
*)
val connect : ?auth:bool -> sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> string -> t

(** [close t] closes the underlying transport.  Further calls to any
    function except {!is_closed} will raise {!Connection_closed}.  Safe
    to call multiple times. *)
val close : t -> unit

(** [is_closed t] is [true] once {!close} has been called or the server
    has unexpectedly terminated. *)
val is_closed : t -> bool

(** [rpc_async t req] sends the JSON-RPC [req] and returns a promise
    that resolves with the *matching* result once it arrives.

    The promise is resolved with:
    • [Ok json] – the [result] field of the response.
    • [Error msg] – if the server responded with an [error] object or
      if the response could not be parsed. *)
val rpc_async : t -> Mcp_types.Jsonrpc.request -> (Jsonaf.t, string) result Eio.Promise.t

(** [rpc t req] is a blocking wrapper around {!rpc_async}. *)
val rpc : t -> Mcp_types.Jsonrpc.request -> (Jsonaf.t, string) result

(** [list_tools_async t] queries the server’s tool registry and returns
    a promise with the list of declared tools. *)
val list_tools_async : t -> (Mcp_types.Tool.t list, string) result Eio.Promise.t

(** [list_tools t] is the blocking version of {!list_tools_async}. *)
val list_tools : t -> (Mcp_types.Tool.t list, string) result

(** [call_tool_async t ~name ~arguments] invokes the remote tool [name]
    with the given JSON [arguments] and returns a promise that resolves
    to the decoded {!Mcp_types.Tool_result.t}. *)
val call_tool_async
  :  t
  -> name:string
  -> arguments:Jsonaf.t
  -> (Mcp_types.Tool_result.t, string) result Eio.Promise.t

(** [call_tool] is the blocking wrapper around {!call_tool_async}. *)
val call_tool
  :  t
  -> name:string
  -> arguments:Jsonaf.t
  -> (Mcp_types.Tool_result.t, string) result

(** [notifications t] is a bounded {!Eio.Stream.t} that receives every
    *raw* JSON-RPC notification sent by the server.  Callers can attach
    their own consumer fibres if they need side-band events. *)
val notifications : t -> Mcp_types.Jsonrpc.notification Eio.Stream.t
