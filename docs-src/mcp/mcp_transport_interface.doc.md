# `Mcp_transport_interface` – contract for MCP wire-transports

`Mcp_transport_interface` contains a single **module type** –
`TRANSPORT`.  Every concrete wire-transport (stdio, HTTP/S, WebSocket, …)
implements this signature so that application code can stay agnostic of the
actual I/O mechanism.

The abstraction is intentionally thin: it moves **complete JSON values** (type
`Jsonaf.t`) and leaves higher-level concerns such as request/response
correlation or streaming token accumulation to the caller.

---

## 1  Signature overview

```ocaml
module type TRANSPORT = sig
  type t

  val connect :
    ?auth:bool -> sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> string -> t

  val send   : t -> Jsonaf.t -> unit
  val recv   : t -> Jsonaf.t

  val is_closed : t -> bool
  val close     : t -> unit

  exception Connection_closed
end
```

### Life-cycle

1. `connect` → obtain a fresh handle.
2. `send` / `recv` as many times as you like (possibly from multiple fibres).
3. `close` → resources released; `is_closed` becomes `true`.

Once `Connection_closed` has been raised, the handle is considered dead and
all subsequent `send` / `recv` operations will raise the same exception.

---

## 2  Choosing a transport at runtime

```ocaml
let pick_transport uri : (module Mcp_transport_interface.TRANSPORT) =
  match Uri.scheme (Uri.of_string uri) with
  | Some ("http" | "https" | "mcp+http" | "mcp+https") ->
      (module Mcp_transport_http)
  | Some "stdio" | None -> (module Mcp_transport_stdio)
  | Some other -> invalid_arg (Printf.sprintf "Unknown MCP scheme: %s" other)
```

Using the helper:

```ocaml
let with_connection uri f =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    let (module T) = pick_transport uri in
    let conn = T.connect ~sw ~env uri in
    Fun.protect ~finally:(fun () -> T.close conn) (fun () -> f (module T) conn)
```

---

## 3  Practical examples

### 3.1  Ping-pong over stdio

```ocaml
let ping_stdio () =
  let server = "stdio:python3 -m mcp.example_server" in
  with_connection server @@ fun (module T) conn ->
    T.send conn (`String "ping");
    match T.recv conn with
    | `String "pong" -> Format.printf "✓ pong@."
    | other -> Format.eprintf "unexpected: %a@." Jsonaf.pp other
```

### 3.2  Listing models over HTTP

```ocaml
let list_models () =
  let endpoint = "https://api.acme.com/mcp/v1" in
  with_connection endpoint @@ fun (module T) conn ->
    T.send conn (`Object [ "op", `String "model.list" ]);
    Format.printf "%a@." Jsonaf.pp (T.recv conn)
```

---

## 4  Behavioural contract

* **Blocking semantics** – `send` blocks until the value has been flushed to
  the OS; `recv` blocks until a full JSON token has been decoded.
* **Idempotent close** – repeated invocations of `close` are safe.
* **Error surface** – once `Connection_closed` is raised or `is_closed`
  becomes `true`, the handle is permanently unusable.

---

## 5  Extending with new transports

1. Create a new module `Mcp_transport_<name>` implementing `TRANSPORT`.
2. Add it to `lib/mcp/dune` so it is built and exposed.
3. That’s it – user code written against the interface requires no changes.

---

*Generated on 2025-07-20.*

