# `Mcp_server_router` – stateless JSON-RPC dispatcher

`Mcp_server_router` is the {b glue} between the wire-transport (stdio, HTTP
or WebSocket) and the in-memory registry exposed by
[`Mcp_server_core`](./mcp_server_core.doc.md).  It receives raw JSON values
from a transport, interprets them as JSON-RPC 2.0 requests or
notifications, delegates the work to the core registry and finally returns
the responses that the transport must emit back to the client.

````text
         ┌──────────┐  Jsonaf.t   ┌────────────────┐  Jsonaf.t
  stdin  │          │  --------► │                │  --------► stdout
  HTTP   │ Transport│            │ Router / Core  │
  WS     │          │  ◄-------- │                │  ◄--------
         └──────────┘  Jsonaf.t   └────────────────┘  Jsonaf.t
````

The router is **stateless** and **exception-free**: every error is converted
into a well-formed JSON-RPC error object so that the caller never has to
catch exceptions.

## 1  Public API

```ocaml
val handle :
  core:Mcp_server_core.t
  -> env:Eio_unix.Stdenv.base
  -> Jsonaf.t               (* request / notification / batch *)
  -> Jsonaf.t list          (* responses, empty for notifications *)
```

The function accepts either a single JSON-RPC envelope or a batch (JSON
array).  For each {i request} exactly one response is returned in the same
order.  {i Notifications} such as `notifications/cancelled` yield no
response.

### Supported methods (Phase-1)

| Method            | Description                                          |
|-------------------|------------------------------------------------------|
| `initialize`      | Capability negotiation                               |
| `tools/list`      | List registered tools                                |
| `tools/call`      | Invoke a tool and stream progress                    |
| `prompts/list`    | List registered prompts                              |
| `prompts/get`     | Fetch a single prompt                                |
| `roots/list`      | Enumerate project roots (CWD + `$MCP_ADDITIONAL_ROOTS`) |
| `resources/list`  | Non-recursive directory listing                      |
| `resources/read`  | Read (and optionally base64-encode) a single file    |
| `ping`            | Liveness probe                                       |

Unknown methods trigger a JSON-RPC error with code `-32601` (“Method not
found”).

## 2  Quick-start example

```ocaml
open Core

Eio_main.run @@ fun env ->
  (* 1. Create the shared registry *)
  let registry = Mcp_server_core.create () in

  (* 2. Build a JSON-RPC request – here: list tools *)
  let open Mcp_types.Jsonrpc in
  let request =
    make_request ~id:(Id.of_int 1) ~method_:"tools/list" ()
    |> jsonaf_of_request
  in

  (* 3. Route the request *)
  let responses = Mcp_server_router.handle ~core:registry ~env request in

  (* 4. Print the response *)
  List.iter responses ~f:(fun r -> Format.printf "%a@." Jsonaf.pp r)
```

Running the snippet prints (pretty-printed for readability):

```json
{
  "jsonrpc": "2.0",
  "id": 1,
  "result": {
    "tools": [],
    "nextCursor": null
  }
}
```

## 3  Internal flow

1. Parse the incoming JSON value via the auto-generated helpers from
   `Mcp_types.Jsonrpc`.
2. Dispatch on `method_` (or silently ignore notifications).
3. Build a JSON response with `Jsonrpc.ok` or `Jsonrpc.error`.
4. Serialize back to `Jsonaf.t` so the transport can send it as-is.

The implementation strives to avoid needless allocations: most responses are
constructed via `Jsonaf.of_string` on a small JSON blob, which is cheaper
than building nested variants manually.

## 4  Environment and filesystem helpers

Some RPCs access the filesystem (`roots/list`, `resources/*`).  Those
helpers rely on [`Eio_unix`](https://ocaml.eio/) for non-blocking I/O and
therefore require the caller to pass the current standard environment
[`Eio.Stdenv.t`].  Transports that do not care about resources (e.g. a pure
in-memory test harness) can construct a dummy environment with
`Eio.Stdenv.nop`.

## 5  Cancellation handling

The router listens for `notifications/cancelled` messages and marks the
referenced request ID as cancelled in the registry.  Long-running tool
handlers can query `Mcp_server_core.is_cancelled` to abort early.

## 6  Limitations

* Hooks are executed synchronously – a slow logging sink blocks the caller.
* Resource enumeration is non-recursive and capped at 1 MiB per file.
* The MIME type mapping depends on the tiny heuristic in [`Mime`](../mime.mli).

## 7  Extending the router

Adding a new RPC generally involves three changes:

1. Implement the handler in `mcp_server_router.ml` (or delegate to a helper
   module).
2. Document the JSON schema in the public MCP specification.
3. Update the client bindings in `mcp_types` if new fields are required.

A defensive default case makes unrecognised methods safe – clients receive a
standard “method not found” error until the feature rolls out on both ends.

