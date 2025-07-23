# `Mcp_tool` – Turn **remote** MCP tools into local `Gpt_function`s

The Model-Context-Protocol (MCP) allows a server to expose an *open-ended*
registry of tools.  The [`tools/list`] RPC returns a list of
`Mcp_types.Tool.t` descriptors that describe each tool’s name,
documentation and JSON-Schema input definition.

`Mcp_tool` provides a **single helper** –
[`gpt_function_of_remote_tool`](./mcp_tool.ml) – that converts such a
descriptor into a ready-to-use `Gpt_function.t`.  The returned value can be
bundled with other local tools via `Gpt_function.functions` and submitted to
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
      Mcp_tool.gpt_function_of_remote_tool ~sw ~client ~strict:true echo_desc
    in

    (* 3. Call it just like any other Gpt_function *)
    let args = `Assoc [ "text", `String "Hello world" ] in
    Printf.printf "%s\n" (Gpt_function.call echo_fn args)
```

Running the program prints `Hello world` and exits.  Any server-side
notifications received while the function call is in flight are printed to
`stdout` by the internal daemon fiber.

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
`Chat_response` driver, which currently expects plain text.

### `gpt_function_of_remote_tool` – the star of the show

```ocaml
val gpt_function_of_remote_tool :
  sw:Eio.Switch.t ->
  client:Mcp_client.t ->
  strict:bool ->
  Mcp_types.Tool.t ->
  Gpt_function.t
```

• **Schema forwarding** – the function’s JSON-Schema is copied verbatim from
  the remote declaration, ensuring that OpenAI validates user input on our
  behalf.

• **Runtime call** – at invocation time the helper performs a synchronous
  [`tools/call`] RPC and returns whatever the server responded with (after
  flattening).

• **Notifications** – a background daemon prints every JSON-RPC
  notification that arrives on the same connection.  Replace the
  `print_endline` with structured logging if you need more control.

---

## 3  Notifications and debugging

The helper subscribes to `Mcp_client.notifications` and prints each incoming
packet using `Jsonaf.to_string`.  This is invaluable during development when
you want to see progress updates streamed by the server, but can be noisy in
production.  Feel free to:

1. Apply a filter (e.g. only `tool/log` messages).
2. Replace the `print_endline` with your favourite logging library.
3. Disable the daemon entirely by removing the call to `Fiber.fork_daemon`.

---

## 4  Known limitations / future work

* **Result formatting** – concatenating parts with newlines works well for
  text, but it may be surprising when the server returns multiple JSON parts.
  Consider enhancing `string_of_result` to emit a JSON array instead.

* **No automatic retries** – transient transport errors bubble up to the
  caller as plain strings.  A helper that retries idempotent calls could be
  added later.

* **Notification handling** – currently hard-wired to `stdout`.  A more
  flexible hook system would allow callers to process progress events in a
  structured fashion.

