(** Streamable HTTP transport for the MCP server.

    This module exposes the registry in {!Mcp_server_core} over HTTP.  It
    speaks JSON-RPC 2.0 on the request / response level and uses
    Server-Sent Events (SSE) for bidirectional streaming:

    - **Client → Server** – a `POST /mcp` request carries either a single
      JSON-RPC envelope or a batch (JSON array).  If the client advertises
      `Accept: text/event-stream` every response is streamed as its own SSE
      event.  Otherwise a regular `application/json` body is returned.

    - **Server → Client** – a long-lived `GET /mcp` request establishes an
      SSE channel used by the server to push asynchronous notifications such
      as `notifications/*/list_changed`, progress updates and structured log
      messages.

    {1 Endpoints}

    • `POST /mcp` – JSON-RPC request batch, optional SSE response.

    • `GET  /mcp` – SSE channel for server-initiated notifications (requires a
      valid `Mcp-Session-Id` request header).

    • Standard OAuth2 helper endpoints exposed by {!module:Oauth2_server_routes}
      are also registered when [require_auth] is [true].

    {1 Sessions}

    A successful [`initialize`] request creates a fresh, cryptographically
    random session identifier.  The value is returned in the
    `Mcp-Session-Id` response header and must be echoed by subsequent
    requests in the same header.

    {1 Authentication}

    Bearer-token validation is enforced by default.  Pass
    [~require_auth:false] to [run] to turn the check off during local
    development or automated tests.

    {1 Failure mapping}

    | Problem                          | HTTP | Body                                   |
    |----------------------------------|------|----------------------------------------|
    | Malformed JSON                   | 400  | {"error":"Invalid JSON"}               |
    | Missing / unknown session id     | 404  | {"error":"Missing or unknown session id"} |
    | Authentication failure           | 401  | {"error":"unauthorized"}               |
    | Unsupported HTTP method / path   | 405  | "Method Not Allowed" (plain-text)      |

    {1 Concurrency model}

    The HTTP server runs inside a single Eio domain.  Piaf spawns one fibre
    per incoming connection so state is protected by the OCaml runtime lock
    and no explicit synchronisation is required.
*)

open! Core

(** [run ?require_auth ~env ~core ~port] starts the HTTP listener on
    [127.0.0.1:port] and never returns.

    Parameters:
    - [env] – current Eio standard environment (obtained from
      [Eio_main.run]).
    - [core] – shared registry created via {!Mcp_server_core.create}.
    - [port] – TCP port to listen on (IPv4 loopback).
    - [?require_auth] – enforce bearer-token validation (default [true]).

    One fibre is forked for each incoming request.

    Example running a development server on port 8080 without authentication:
    {[ Eio_main.run @@ fun env ->
       let registry = Mcp_server_core.create () in
       Mcp_server_http.run ~require_auth:false ~env ~core:registry ~port:8080 ]}
*)

val run
  :  ?require_auth:bool
  -> env:Eio_unix.Stdenv.base
  -> core:Mcp_server_core.t
  -> port:int
  -> unit
