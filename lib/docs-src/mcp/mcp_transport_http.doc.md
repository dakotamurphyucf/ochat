# `Mcp_transport_http` – Streamable HTTP/S transport

`Mcp_transport_http` connects an MCP client to a remote server over
plain **HTTP** or **HTTPS**.  The module speaks the *Streamable-HTTP*
variant of the protocol (spec rev. *2025-03-26*) and automatically
handles JSON bodies **and** Server-Sent-Event (SSE) streams.

The implementation is built on top of:

- [`Piaf`](https://github.com/anmonteiro/piaf) for the HTTP/1.1 + HTTP/2
  client engine.
- [`Eio`](https://github.com/ocaml-multicore/eio) for portable
  concurrency and fibres.
- [`oauth2`](./oauth.md) helpers for optional bearer-token
  authentication.

---

## 1  How it works

1. `connect` parses the endpoint URI.  If `?auth=true` (default) it runs a
   best-effort OAuth 2 client-cred flow via `Oauth2_manager` and stores the
   resulting *access token*.
2. A persistent `Piaf.Client.t` is created for the scheme & authority
   portion of the URI.  Each `send` call spawns **one fibre** that
   performs an HTTP `POST` to the *path* part with the JSON payload in the
   request body.
3. When the response comes back, the transport inspects its
   `Content-Type` header:
   * **`application/json`** → parse body once and enqueue all JSON values.
   * **`text/event-stream`** → spawn a reader fibre that decodes SSE
     events and enqueues every `data:` line (except the special
     `[DONE]`).
4. Values are delivered to callers via an `Eio.Stream.t` so that multiple
   fibres can call `recv` concurrently.

Session stickiness – if the server sets the **`Mcp-Session-Id`** header,
the transport memorises the first value and includes it in every
subsequent request.

---

## 2  Public API

The module instantiates the
[`Mcp_transport_interface.TRANSPORT`](./mcp_transport_interface.mli)
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
4. **Dynamic registration** – if the issuer supports it (`POST
   /.well-known/oauth-authorization-server` or `/register`)

If no credentials can be found or token retrieval fails the connection is
still established but **no** `Authorization` header is sent.

---

## 3  Examples

### 3.1  Listing models

```ocaml
open Core

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let conn =
      Mcp_transport_http.connect
        ~sw ~env "https://api.acme.com/mcp/v1"
    in
    Mcp_transport_http.send conn (`Object [ "op", `String "model.list" ]);
    printf "%a\n" Jsonaf.pp (Mcp_transport_http.recv conn);
    Mcp_transport_http.close conn
```

### 3.2  Streaming completions

```ocaml
let stream_chat ?(model="llama-3") prompt =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let conn =
      Mcp_transport_http.connect
        ~sw ~env "https://chat.example.com/mcp/v1"
    in
    Mcp_transport_http.send conn (
      `Object [
        "op", `String "chat.completions";
        "model", `String model;
        "prompt", `String prompt;
        "stream", `Bool true
      ]
    );
    let rec loop () =
      match Mcp_transport_http.recv conn with
      | `Object [ "delta", `String chunk ] ->
          print_string chunk; flush stdout; loop ()
      | other ->
          eprintf "Unexpected: %a\n" Jsonaf.pp other
    in
    loop ()
```

---

## 4  Behavioural contract

* **Non-blocking `send`** – the call returns as soon as the JSON value has
  been copied into Piaf’s request buffer.
* **Blocking `recv`** – waits for the next *complete* JSON value.
* **Idempotent close** – `close` may be called multiple times and from
  any fibre.
* **Error surface** – once `Connection_closed` has been raised the handle
  is permanently unusable.

---

## 5  Known limitations

* **At-most-once retry** – on `401` the transport retries **once** after
  refreshing the access-token.  It does *not* retry on network-level
  failures.
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


