(** HTTP/​HTTPS transport for the *Model-Context-Protocol*.

    Implements {!module-type:Mcp_transport_interface.TRANSPORT} on top of
    [Piaf], [Eio] and the “Streamable-HTTP” flavour of the MCP wire
    protocol (revision&nbsp;2025-03-26).

    {1 Supported content-types}

    • [application/json] – the response body is parsed once and all JSON
      values are enqueued immediately.  This is used for *single-shot*
      requests such as ["model.list"].

    • [text/event-stream] – the body is treated as a **Server-Sent
      Events (SSE)** stream.  A dedicated fibre decodes events line-by-line
      and enqueues each JSON payload as soon as it arrives.  The
      transport drops the special “[DONE]” message used by some servers
      to mark the end of a stream.

    {1  Authentication workflow}

    When [?auth] (default) is enabled, [connect] performs a best-effort
    OAuth&nbsp;2 flow:

    {ol
    {- Credentials are located in the following order of precedence:
       explicit URI query parameters –
       environment variables –
       client-store –
       dynamic client registration.}
    {- Once credentials are obtained, the transport fetches an access
       token from the issuer (= scheme + authority of the endpoint
       URI) and adds “Authorization: Bearer  …” headers to every
       request.}}

    The helper functions live in {!module:Oauth2_manager} and
    {!module:Oauth2_http}.  Failures are logged to [stderr] and the
    connection falls back to anonymous mode.

    {1  Session persistence}

    The server can return an *Mcp-Session-Id* header.  The transport stores
    the first value it sees and includes it in all subsequent requests so
    that the server can associate a series of HTTP requests with the same
    logical session.

    {1  URI schemes}

    • “http:” / “mcp+http:”  —  clear-text HTTP
    • “https:” / “mcp+https:” —  TLS-encrypted HTTP/2 or HTTP/1.1 (as
      negotiated by Piaf)

    Any other scheme triggers [Invalid_argument] in [connect].

    {1  Concurrency semantics}

    The implementation is **fully concurrent**:
    • [send] is non-blocking – the JSON payload is handed to a background
      fibre that performs the actual POST so that callers do not stall.
    • [recv] blocks until a value is available.  Multiple fibres can call
      [recv] concurrently; they will dequeue in FIFO order.

    {1  Exception   [Connection_closed]}

    Raised by any operation once the remote peer closed the TCP
    connection or after [close] has been called.  In that state
    [is_closed] is [true] and further [send]/[recv] invocations re-raise
    the exception.
    *)

include Mcp_transport_interface.TRANSPORT
