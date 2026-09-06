# `Mcp_client` – High-level client helper for the Model-Context-Protocol

This is maintained MCP tool/client infrastructure. It is not deprecated by the
new agent server; only the separate ChatMD prompt-serving MCP host is legacy.
See [MCP tool configuration](../../overview/tools.md) and
[discovery identity/lifetime](../chat_response/tool.doc.md#cache-invalidation-strategy).

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

RPC failures use the `('a, string) result` surface. EOF and explicit `close`
resolve every pending request with `Error "Connection_closed"`; other transport
failures use a generic diagnostic without copying payloads or exception text.
The pending table is drained before transport cleanup. Repeated close is safe,
late responses are ignored, and further RPCs return the terminal error.

A send failure terminates the client because the request may have been partially
written. Eio cancellation during send drains pending requests and propagates to
the sender. Cancelling a blocking `rpc`, `list_tools`, or `call_tool` removes that
request; cancelling only a waiter on an asynchronous promise leaves its request
active until a reply or client shutdown. Connection setup and Eio cancellation
can still raise; they are not converted into successful results.

Typed async results are resolved directly by the pending-request table, without
mapper fibers. Terminal results resolve synchronously, including calls made
during cancellation-protected teardown of an already-cancelled client switch.

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
