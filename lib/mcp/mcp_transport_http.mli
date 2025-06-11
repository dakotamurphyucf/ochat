(** Model Context Protocol – HTTP transport implementation

    This module provides a concrete implementation of the

      [Mcp_transport_interface.TRANSPORT]

    signature using the Streamable-HTTP transport defined by the MCP
    specification (2025-03-26).

    The implementation supports both classic JSON HTTP responses and
    Server-Sent Events (SSE) streams per the 2025-03-26 Streamable-HTTP
    transport:

    • If the response is `Content-Type: application/json` we parse the
      body once and enqueue the resulting JSON value(s).

    • If the response is `Content-Type: text/event-stream` we spin up a
      background fibre that decodes SSE events and enqueues each JSON
      payload as it arrives.

    Session handling – when the server returns the `Mcp-Session-Id`
    header it is stored and automatically included in all subsequent
    requests. This fulfils the remaining requirements for Phase-2 step
    2 of the client roadmap. *)

include Mcp_transport_interface.TRANSPORT
