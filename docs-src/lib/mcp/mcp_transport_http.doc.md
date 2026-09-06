# `Mcp_transport_http` – Streamable HTTP/S transport

This is maintained MCP tool/client infrastructure. It is not deprecated by the
new agent server; only the separate ChatMD prompt-serving MCP host is legacy.
See [MCP tool configuration](../../overview/tools.md) and
[discovery identity/lifetime](../chat_response/tool.doc.md#cache-invalidation-strategy).

`Mcp_transport_http` connects an MCP client to a remote server over
plain **HTTP** or **HTTPS**.  The module speaks the *Streamable-HTTP*
variant of the protocol (spec rev. *2025-03-26*) and automatically
handles JSON bodies **and** Server-Sent-Event (SSE) streams.

The implementation is built on top of:

- [`Piaf`](https://github.com/anmonteiro/piaf) for the HTTP/1.1 + HTTP/2
  client engine.
- [`Eio`](https://github.com/ocaml-multicore/eio) for portable
  concurrency and fibres.
- [OAuth token management](../oauth/oauth2_manager.doc.md) for optional bearer-token
  authentication.

---

## 1  How it works

1. `connect` parses the endpoint URI.  If `?auth=true` (default) it runs a
   best-effort OAuth 2 client-credentials or PKCE flow via `Oauth2_manager` and stores the
   resulting *access token*.
2. A persistent `Piaf.Client.t` is created for the scheme & authority
   portion of the URI.  Each `send` call spawns **one fibre** that
   performs an HTTP `POST` to the *path* part with the JSON payload in the
   request body.
3. When the response comes back, the transport inspects its
   `Content-Type` header:
   * **`application/json`** → parse body once and enqueue all JSON values.
   * **`text/event-stream`** → spawn a reader fibre that decodes SSE
     events. Within each event, `data:` fields are joined with newlines and
     decoded as one JSON value; the special `[DONE]` marker is ignored. A blank
     line completes the event immediately, even when the response body stays open.
4. Values are delivered to callers via an `Eio.Stream.t` so that multiple
   fibres can call `recv` concurrently.

Session stickiness – if the server sets the **`Mcp-Session-Id`** header,
the transport stores the latest present value and includes it in subsequent
requests. A response without that header leaves the previous value unchanged.

---

## 2  Public API

The module instantiates the
[`Mcp_transport_interface.TRANSPORT`](../../../lib/mcp/mcp_transport_interface.mli)
signature.  Only HTTP-specific behaviour is highlighted below.

```ocaml
type t

val connect :
  ?auth:bool -> sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> string -> t

val send   : t -> Jsonaf.t -> unit
val recv   : t -> Jsonaf.t

val is_closed : t -> bool
val close     : t -> unit

exception Connection_closed
```

### 2.1  URI scheme

```
http://api.acme.com/mcp/v1           (HTTP/1.1)
https://api.acme.com/mcp/v1          (HTTP/1.1 or HTTP/2 via ALPN)
mcp+http://api.acme.com/mcp/v1       (alias for http)
mcp+https://api.acme.com/mcp/v1      (alias for https)
```

Any other scheme raises `Invalid_argument` in `connect`.

### 2.2  Authentication

When `?auth=true` the transport attempts to fetch an *access token* from
the **issuer** (scheme + authority of the endpoint URI).  Credentials are
looked up in the following order:

1. **URI query parameters** – `?client_id=…&client_secret=…`
2. **Environment variables** – `MCP_CLIENT_ID` / `MCP_CLIENT_SECRET`
3. **Client store** – previously persisted credentials
4. **Dynamic registration** – fetch metadata with `GET
   /.well-known/oauth-authorization-server`, then `POST` the registration payload
   to its `registration_endpoint`, or the conventional `/register` fallback.

Persisted confidential-client credentials use the client-credentials grant;
public-client entries use PKCE. If credential setup or token retrieval returns
an operational failure, connection setup continues without an `Authorization`
header. Eio cancellation propagates and does not trigger anonymous fallback.

Closing the transport wakes blocked receivers with `Connection_closed`.
POST/body errors, HTTP error responses, malformed JSON, and an SSE stream ending
before its matching RPC response terminate the transport as well. Normal SSE
completion after the matching response does not close the client. An empty
HTTP 202 acknowledgement remains valid for notifications. The high-level
`Mcp_client` drains pending requests when the transport terminates.

---

## 3  Examples

### 3.1  Listing MCP tools over HTTP

Use the high-level client to perform initialization and correlate replies. This
example targets a placeholder server that does not require OAuth.

```ocaml
open Core

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let client =
      Mcp_client.connect ~auth:false ~sw ~env "https://mcp.example/mcp"
    in
    Fun.protect
      (fun () ->
        let tools = Mcp_client.list_tools client |> Result.ok_or_failwith in
        List.iter tools ~f:(fun (tool : Mcp_types.Tool.t) ->
          Eio.Flow.copy_string (tool.name ^ "\n") (Eio.Stdenv.stdout env)))
      ~finally:(fun () -> Mcp_client.close client)
```

### 3.2  Sending a raw MCP request

For lower-level integration, after completing the `initialize` /
`notifications/initialized` handshake, send a JSON-RPC `tools/list` request.
The caller must allocate a unique ID and route responses and notifications from
`recv`. JSON and SSE responses share that same receive API.

```ocaml
let request_tools conn ~id =
  let request =
    Mcp_types.Jsonrpc.make_request
      ~id:(Mcp_types.Jsonrpc.Id.of_int id)
      ~method_:"tools/list"
      ~params:(`Object [])
      ()
  in
  Mcp_transport_http.send conn (Mcp_types.Jsonrpc.jsonaf_of_request request)
```

---

## 4  Behavioural contract

* **Non-blocking `send`** – schedules a background POST; returning does not
  guarantee that the remote server has received the request.
* **Blocking `recv`** – waits for the next *complete* JSON value.
* **Idempotent close** – `close` may be called multiple times and from
  any fibre.
* **Error surface** – once `Connection_closed` has been raised the handle
  is permanently unusable.

---

## 5  Known limitations

* **Limited 401 retry** – with credentials, the transport retries once. If an
  access token is already present, it resends that token; it does **not** force
  refresh or invalidate the cache on rejection. With no token it calls the token
  manager before retrying. This pre-existing limitation is separate from token
  acquisition/refresh decoding fixes. Network-level failures are not retried.
* **Back-pressure** – the in-memory queue is fixed at 64 messages.  If the
  client does not call `recv` fast enough the enqueue will block the SSE
  reader fibre and eventually the server.
* **No HTTP/3** – currently limited to HTTP/1.1 & HTTP/2 (whatever Piaf
  negotiates).

---

## 6  Extending / debugging

* Enable `EIO_TRACE=1` to diagnose low-level scheduling and I/O.
* Piaf’s own debug logs can be activated with the usual `Logs`
  configuration machinery.
