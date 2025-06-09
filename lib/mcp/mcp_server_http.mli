open! Core

(** [run ~env ~core ~port] starts a Streamable HTTP MCP server listening on
    [127.0.0.1:port].  The call blocks the current fibre forever.  One fibre
    is spawned per incoming connection / request.  Only the subset of the MCP
    specification required by the client implementation is supported: the
    server understands HTTP POST requests to the single endpoint [/mcp].  The
    POST body must contain either a single JSON-RPC message or a batch (JSON
    array).  The response is returned synchronously as [application/json].

    GET requests as well as Server-Sent Events streaming are **not** supported
    in this MVP and result in HTTP 405.

    Error handling:
    – Malformed JSON yields HTTP 400 with a JSON-RPC error response.
    – All other internal failures return HTTP 500 with a plain-text body. *)

val run
  :  env:Eio_unix.Stdenv.base
  -> core:Mcp_server_core.t
  -> port:int
  -> unit

