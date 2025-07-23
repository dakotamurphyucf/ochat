# `Mcp_client` – High-level client helper for the Model-Context-Protocol

`Mcp_client` hides the transport details of the **Model-Context-Protocol**
and provides a *concurrency-safe*, *non-blocking* wrapper around
JSON-RPC-style requests.

The current implementation supports two wire-transports:

* **`stdio:`** – spawn a local process and exchange `\n`-delimited JSON
  over standard I/O (see {!Mcp_transport_stdio}).
* **HTTP(S)** – stream requests and responses over an HTTP/2 connection
  (see {!Mcp_transport_http}, experimental).

The public API is intentionally small and centres around two concepts:

* *Promises* – every asynchronous helper returns an
  [`('a, string) result Eio.Promise.t`].  The promise is resolved when
  the *matching* response arrives on the wire.
* *Blocking wrappers* – convenience functions (`rpc`, `list_tools`,
  `call_tool`, …) that simply `Eio.Promise.await` the asynchronous
  variant and therefore fit nicely into code that is not promise-aware.

---

## 1  Quick start

```ocaml
open Core

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    (* 1.  Connect to a local Python reference implementation *)
    let client =
      Mcp_client.connect
        ~sw ~env "stdio:python3 -m mcp.reference_server"
    in

    (* 2.  Discover available tools *)
    let tools = Mcp_client.list_tools client |> Result.ok_or_failwith in
    List.iter tools ~f:(fun t -> printf "tool: %s\n" t.Mcp_types.Tool.name);

    (* 3.  Call the "echo" tool *)
    let args = `Assoc [ "text", `String "Hello" ] in
    match Mcp_client.call_tool client ~name:"echo" ~arguments:args with
    | Ok r -> printf "echo → %s\n" (Jsonaf.to_string r.output)
    | Error m -> eprintf "error: %s\n" m;

    Mcp_client.close client
```

---

## 2  API reference (friendly)

### `connect` – open a new client connection

```ocaml
val connect :
  ?auth:bool -> sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> string -> t
```

* Spawns the receiver fibre and performs the mandatory
  *initialize/initialized* handshake before returning.
* `auth = false` disables transport-level authentication.  The flag is
  ignored by the stdio transport.

### `rpc_async` / `rpc` – low-level JSON-RPC helpers

Send **any** pre-constructed JSON-RPC request.

```ocaml
val rpc_async :
  t -> Mcp_types.Jsonrpc.request -> (Jsonaf.t, string) result Eio.Promise.t

val rpc :
  t -> Mcp_types.Jsonrpc.request -> (Jsonaf.t, string) result
```

Use these when you need to talk to experimental server extensions that
are not yet exposed via first-class helpers.

### `list_tools_async` / `list_tools` – tool discovery

```ocaml
val list_tools_async :
  t -> (Mcp_types.Tool.t list, string) result Eio.Promise.t

val list_tools :
  t -> (Mcp_types.Tool.t list, string) result
```

Returns the server’s *runtime* registry – tools can theoretically be
added and removed dynamically.

### `call_tool_async` / `call_tool` – invoke a tool

```ocaml
val call_tool_async :
  t -> name:string -> arguments:Jsonaf.t
  -> (Mcp_types.Tool_result.t, string) result Eio.Promise.t

val call_tool :
  t -> name:string -> arguments:Jsonaf.t
  -> (Mcp_types.Tool_result.t, string) result
```

The arguments JSON must conform to the schema declared by the tool.

### `notifications` – raw push events

```ocaml
val notifications : t -> Mcp_types.Jsonrpc.notification Eio.Stream.t
```

Useful for progress updates or server-side logs that do not belong to a
particular request.

---

## 3  Implementation notes

* **Single receiver fibre** – avoids contention on the transport’s read
  endpoint and makes message routing trivial.
* **Hash-table of pending requests** – keys are `Jsonrpc.Id.t`; the value
  is the promise resolver of the *caller*.
* **Transport abstraction** – a private runtime union hides the concrete
  transport type while allowing allocation-free dispatch.

---

## 4  Error handling

`Mcp_client` itself never raises on normal operation.  All errors are
channelled through the [`('a, string) result`] surface.

Closing the underlying transport (either explicitly via {!close} or due
to server termination) cancels *all* in-flight promises with the error
`Connection_closed`.

---

## 5  Limitations

* **Back-pressure** – the notification stream is bounded (size 64).  If
  the consumer cannot keep up the server will be back-pressured once the
  buffer fills.
* **Only JSON-serialisable arguments** – advanced binary payloads must be
  base64-encoded by the caller.

---

## 6  Related modules

* {!Mcp_tool} – wrap a JSON schema into a first-class OCaml function.
* {!Mcp_transport_stdio} – spawn local process and exchange newline-delimited JSON.
* {!Mcp_transport_http} – experimental streaming HTTP transport.

