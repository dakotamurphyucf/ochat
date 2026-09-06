# `Mcp_tool` – Turn **remote** MCP tools into local `Ochat_function`s

This is maintained MCP tool/client infrastructure. It is not deprecated by the
new agent server; only the separate ChatMD prompt-serving MCP host is legacy.
See [MCP tool configuration](../../overview/tools.md) and
[discovery identity/lifetime](../chat_response/tool.doc.md#cache-invalidation-strategy).

The Model-Context-Protocol (MCP) allows a server to expose an *open-ended*
registry of tools.  The [`tools/list`] RPC returns a list of
`Mcp_types.Tool.t` descriptors that describe each tool’s name,
documentation and JSON-Schema input definition.

`Mcp_tool` provides a **single helper** –
[`ochat_function_of_remote_tool`](../../../lib/mcp/mcp_tool.ml) – that converts such a
descriptor into a ready-to-use `Ochat_function.t`.  The returned value can be
bundled with other local tools via `Ochat_function.functions` and submitted to
OpenAI’s *function-calling* API without the caller having to think about the
wire protocol.

---

## 1  Quick start

```ocaml
open Core

let () =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
    (* 1. Connect to a server (here: the Python reference impl) *)
    let client =
      Mcp_client.connect ~sw ~env "stdio:python3 -m mcp.reference_server"
    in

    (* 2. Discover tools and wrap the remote "echo" tool *)
    let echo_desc =
      Mcp_client.list_tools client
      |> Result.ok_or_failwith
      |> List.find_exn ~f:(fun t -> String.equal t.Mcp_types.Tool.name "echo")
    in
    let echo_fn =
      Mcp_tool.ochat_function_of_remote_tool ~sw ~client ~strict:true echo_desc
    in

    (* 3. Call it just like any other Ochat_function *)
    let args = {|{"text":"Hello world"}|} in
    match echo_fn.run args with
    | Openai.Responses.Tool_output.Output.Text text -> printf "%s\n" text
    | Openai.Responses.Tool_output.Output.Content _ ->
      failwith "Expected the MCP wrapper's flattened text output"
```

With a compatible echo server installed, the program prints its reply and exits.
The wrapper's background fiber currently consumes notifications silently; it
does not print them to stdout.

---

## 2  API reference (friendly)

### `string_of_content` – normalise a single result part

```ocaml
val string_of_content : Mcp_types.Tool_result.content -> string
```

* `Text`   → returned unchanged.
* `Json` / `Rich`   → serialised with `Jsonaf_ext.to_string`.

Rarely useful on its own but documented for completeness.

### `string_of_result` – flatten multi-part output

```ocaml
val string_of_result : Mcp_types.Tool_result.t -> string
```

Maps every part with `string_of_content` and joins the pieces with `"\n"`.
This yields a *single* string result that integrates seamlessly with the
`Chat_response` driver. The driver supports richer outputs too, but this MCP
wrapper returns text, including JSON serialization of rich MCP result parts.

### `ochat_function_of_remote_tool` – the star of the show

```ocaml
val ochat_function_of_remote_tool :
  sw:Eio.Switch.t ->
  client:Mcp_client.t ->
  strict:bool ->
  Mcp_types.Tool.t ->
  Ochat_function.t
```

• **Schema forwarding** – the function’s JSON-Schema is copied verbatim from
  the remote declaration and `strict` is forwarded in provider tool metadata.
  The wrapper parses JSON but does not perform local JSON-Schema validation;
  do not treat this flag as an authorization or local validation boundary.

• **Runtime call** – at invocation time the helper performs a synchronous
  [`tools/call`] RPC and returns whatever the server responded with (after
  flattening).

• **Notifications** – each wrapper starts a background consumer that silently
  discards notifications. This competes with the host's discovery listener.

---

## 3  Notifications and debugging

`Mcp_client.notifications` returns a shared queue, not an independent broadcast
subscription. The wrapper's print call is commented out, but its consumer still
runs. Adding another reader does not provide reliable observation: readers divide
messages among themselves. In particular, a wrapper can consume a catalog-change
notification before the host invalidates discovery. A single dispatcher with
explicit fan-out is needed to fix this; see the
[implementation audit](../../development/code-documentation-audit.md#mcp-discovery-and-notifications).

---

## 4  Known limitations / future work

* **Result formatting** – concatenating parts with newlines works well for
  text, but it may be surprising when the server returns multiple JSON parts.
  Consider enhancing `string_of_result` to emit a JSON array instead.

* **No automatic retries** – transient transport errors bubble up to the
  caller as plain strings.  A helper that retries idempotent calls could be
  added later.

* **Notification handling/catalog refresh** – shared-queue competition can lose
  invalidations, and constructed wrappers do not hot-reload schemas. Recreate
  the runtime after catalog changes; progress notification forwarding is not
  supplied by this wrapper.
