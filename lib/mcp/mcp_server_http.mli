open! Core

(** [run ~env ~core ~port] starts a Streamable HTTP MCP server listening on
    [127.0.0.1:port].  The call blocks the current fibre forever.  One fibre
    is spawned per incoming connection / request.

    Implemented HTTP endpoints:

    • `POST /mcp` – accepts a single JSON-RPC message *or* a
      JSON-RPC batch (array).  The reply is returned either as
      `application/json` (default) *or* as a compact
      Server-Sent-Events (SSE) stream when the client sends an
      `Accept: text/event-stream` header.

    • `GET /mcp` – opens a long-lived SSE channel that the server
      uses for **notifications** (e.g. `list_changed`, structured
      logging).  A valid `Mcp-Session-Id` request header is
      required – the identifier is issued in the response to the
      initial `initialize` request.

    Error handling rules:
    – malformed JSON ⇒ HTTP 400 with a minimal JSON error payload;
    – unknown / missing session id ⇒ HTTP 404;
    – unsupported HTTP methods ⇒ HTTP 405.

    The function does not return. *)

val run : env:Eio_unix.Stdenv.base -> core:Mcp_server_core.t -> port:int -> unit
