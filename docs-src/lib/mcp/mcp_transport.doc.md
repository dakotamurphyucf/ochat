# `Mcp_transport` – runtime-selectable wire-transport for the Model-Context-Protocol

> _Modules: `Mcp_transport`, `Mcp_transport_interface`, `Mcp_transport_stdio`, `Mcp_transport_http`_

`Mcp_transport` defines the **minimal surface needed to move raw JSON packets**
between an MCP client and server.  The abstraction is intentionally thin – it
exposes the *how* (JSON in, JSON out) and hides the *where* (pipes, TCP, …).

The crate ships two ready-to-use implementations:

* **`Mcp_transport_stdio`** – spawns the server process locally and exchanges
  newline-delimited JSON over `stdin`/`stdout`.  Ideal for unit-tests and the
  Phase-1 CLI client.
* **`Mcp_transport_http`** – talks to a remote MCP endpoint over HTTP or HTTPS.
  Supports streaming responses via Server-Sent Events (SSE) and optional OAuth
  2 bearer-token authentication.

Both modules satisfy the same
[`Mcp_transport_interface.TRANSPORT`](./mcp_transport_interface.mli) signature:

```ocaml
type t

val connect :
  ?auth:bool -> sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> string -> t
val send   : t -> Jsonaf.t -> unit
val recv   : t -> Jsonaf.t
val is_closed : t -> bool
val close  : t -> unit

exception Connection_closed
```

## 1  Choosing a transport at runtime

```ocaml
let transport_for_uri uri : (module Mcp_transport_interface.TRANSPORT) =
  match Uri.scheme (Uri.of_string uri) with
  | Some ("http" | "https" | "mcp+http" | "mcp+https") ->
      (module Mcp_transport_http)
  | Some "stdio" | None -> (module Mcp_transport_stdio)
  | Some s -> invalid_arg (Printf.sprintf "Unknown MCP scheme: %s" s)

let with_connection uri f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let (module T) = transport_for_uri uri in
    let conn = T.connect ~sw ~env uri in
    Fun.protect ~finally:(fun () -> T.close conn) (fun () -> f (module T) conn)
```

## 2  Talking over stdio

```ocaml
let demo_stdio () =
  let server = "stdio:python3 -m mcp.example_server" in
  with_connection server @@ fun (module T) conn ->
    T.send conn (`String "ping");
    match T.recv conn with
    | `String "pong" -> print_endline "✓ server responded"
    | other -> Format.eprintf "Unexpected: %a@." Jsonaf.pp other
```

## 3  Talking over HTTP/S

```ocaml
let demo_http () =
  let endpoint = "https://api.acme.com/mcp/v1" in
  with_connection endpoint @@ fun (module T) conn ->
    T.send conn (`Object [ "model", `String "acme-1" ]);
    let resp = T.recv conn in
    Format.printf "%a@." Jsonaf.pp resp
```

## 4  Behavioural contract

* **Blocking semantics** – `send` blocks until the whole JSON value has been
  handed off to the OS; `recv` blocks until exactly one full JSON value is
  available.  This keeps back-pressure handling deterministic and simplifies
  the client code.
* **Idempotent close** – calling `close` more than once is allowed and has no
  effect.
* **Error surface** – once `Connection_closed` is raised or `is_closed`
  becomes `true`, `send` and `recv` keep raising the same exception.  Clients
  should create a fresh connection instead of attempting to resuscitate the
  broken one.

## 5  Known limitations

* **No built-in request/response correlation** – the transport only moves raw
  JSON values.  The caller is responsible for tagging outgoing messages with
  an `id` and matching them with the corresponding responses.
* **Single-server per handle** – multiplexing is left to higher layers.
* **TLS configuration hooks** – the HTTP implementation relies on the default
  Piaf (cohttp-style) TLS stack.  Fine-grained control (pinning, mTLS, etc.)
  is not yet exposed.

## 6  Extending with new transports

Imitate the layout of `mcp_transport_stdio.ml`:

1. Define an internal state record.
2. Provide concrete implementations for `connect`, `send`, `recv`, `is_closed`
   and `close`.
3. Expose the module as `Mcp_transport_<name>` and list it in
   `lib/mcp/dune`.

Because user code is encouraged to depend on the signature instead of the
module name, adding a new transport is a backwards-compatible change.

