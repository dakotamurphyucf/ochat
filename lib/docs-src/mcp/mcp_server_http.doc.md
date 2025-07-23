# `Mcp_server_http` – Streamable HTTP transport

`Mcp_server_http` exposes the in-memory registry implemented by
[`Mcp_server_core`](./mcp_server_core.doc.md) over plain HTTP.  The design is
optimised for *streaming* in both directions: requests are standard
JSON-RPC 2.0 envelopes while responses and server-initiated messages are
encoded as [Server-Sent Events (SSE)](https://html.spec.whatwg.org/multipage/server-sent-events.html).

````text
┌──────────┐  HTTP POST    ┌────────────────┐
│          │  JSON-RPC     │                │   SSE push
│ Browser  │ ────────────►│   MCP Router   │────────────► stdout
│ or CLI   │              │  / Core        │◄────────────
│          │  HTTP GET    │                │   keep-alive
└──────────┘◄─────────────└────────────────┘
           SSE channel
````

## 1  Endpoints

| Path        | Method | Purpose |
|-------------|--------|---------|
| `/mcp`      | POST   | Submit a single JSON-RPC request or a batch.  The reply is sent back either as `application/json` (default) or as a compact SSE stream when the client advertises `Accept: text/event-stream`. |
| `/mcp`      | GET    | Open a long-lived SSE channel that the server uses for *notifications* such as `notifications/*/list_changed`, progress updates and structured logs.  Requires a valid `Mcp-Session-Id` header. |
| OAuth2 aux. | GET/POST | When `~require_auth:true` is passed to {!val:Mcp_server_http.run} the helper endpoints from `Oauth2_server_routes` are mounted under `/.well-known/oauth-authorization-server`, `/token`, `/authorize` and `/register`. |

### Session management

On the first `initialize` request the server creates a fresh 32-character
hexadecimal session identifier and returns it in the `Mcp-Session-Id`
response header.  All further requests **must** repeat the header; missing or
unknown identifiers produce HTTP 404.

## 2  Authentication

Bearer-token validation powered by `chatgpt.oauth2` is enabled by default.
Pass `~require_auth:false` when calling {!val:Mcp_server_http.run} to disable
the check – useful during local development and in unit tests where setting
up a full OAuth flow would be overkill.

## 3  Error mapping

| Failure                                    | Status | Body |
|--------------------------------------------|--------|------|
| malformed JSON                             | 400    | `{"error":"Invalid JSON"}` |
| missing / unknown session ID               | 404    | `{"error":"Missing or unknown session id"}` |
| authentication failed                      | 401    | `{"error":"unauthorized"}` + `WWW-Authenticate: Bearer` |
| unsupported HTTP method or path            | 405    | plain-text `Method Not Allowed` |

## 4  Starting a server

```ocaml
open Core

let () =
  Eio_main.run @@ fun env ->
    let registry = Mcp_server_core.create () in
    (* Development mode: no OAuth validation *)
    Mcp_server_http.run
      ~require_auth:false
      ~env
      ~core:registry
      ~port:8080
```

## 5  Consuming the API from JavaScript

```js
// 1. Submit an RPC request
const resp = await fetch("http://localhost:8080/mcp", {
  method: "POST",
  headers: {
    "Content-Type": "application/json",
    "Mcp-Session-Id": sessionId,
    "Accept": "text/event-stream" // opt-in for SSE responses
  },
  body: JSON.stringify({
    jsonrpc: "2.0",
    id: 1,
    method: "tools/list",
    params: {}
  })
});

// 2. Listen for server-initiated notifications
const source = new EventSource("http://localhost:8080/mcp", {
  headers: { "Mcp-Session-Id": sessionId }
});

source.onmessage = (ev) => {
  const msg = JSON.parse(ev.data);
  console.log("notification", msg);
};
```

## 6  Internals

* **Piaf** provides the lightweight HTTP/1 & HTTP/2 server, running inside a
  single Eio domain.
* Active SSE streams are stored in a mutable list guarded by the runtime
  lock → no additional synchronisation is required in the stock OCaml
  runtime.
* The implementation registers hooks with `Mcp_server_core` before starting
  the listener so that future changes (e.g. dynamic plugin loading) are also
  broadcast.

## 7  Limitations

* No back-pressure is applied when broadcasting events – a slow client can
  cause memory use to grow.
* The server binds only to `127.0.0.1`.  Expose it via a reverse proxy if it
  must be reachable from the outside.
* Replay of missed SSE events by `Last-Event-ID` is **not** implemented –
  clients are expected to tolerate duplicates.

