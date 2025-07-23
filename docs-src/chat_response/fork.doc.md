# Fork – drive a nested *assistant clone*

`lib/chat_response/Fork` provides the *runtime* implementation of the
`fork` GPT-4 tool defined in [`lib/definitions.ml`](../definitions.ml#module-Fork).

The tool allows the assistant to spawn a fully-featured **child agent**
that inherits the entire conversation context, available tools and file
system – but whose intermediate chatter never reaches the user.  Only
whatever the child writes in the `===PERSIST===` block is kept when the
fork terminates.

The parent UI receives live updates while the forked agent is running,
so users can monitor progress (or cancel a runaway fork) without waiting
for completion.

---

## Public interface

### `execute`

````ocaml
val execute :
  env:Eio_unix.Stdenv.base ->
  history:Openai.Responses.Item.t list ->
  call_id:string ->
  arguments:string ->
  tools:Openai.Responses.Request.Tool.t list ->
  tool_tbl:(string, string -> string) Base.Hashtbl.t ->
  on_event:(Openai.Responses.Response_stream.t -> unit) ->
  on_fn_out:(Openai.Responses.Function_call_output.t -> unit) ->
  ?temperature:float ->
  ?max_output_tokens:int ->
  ?reasoning:Openai.Responses.Request.Reasoning.t ->
  unit -> string
````

Blocking helper that clones the conversation and runs the fork to
completion.

Parameters (see the inline documentation in `fork.mli` for full
details):

* `env` – Eio standard environment, used for network and filesystem.
* `history` – messages **before** the tool call.
* `call_id` – identifier assigned by the driver; echoed back in all
  streamed items.
* `arguments` – raw JSON arguments of the tool, parsed with
  `Definitions.Fork.input_of_string`.
* `tools` / `tool_tbl` – capabilities available to the fork.
* `on_event` – forward each raw streaming event upstream.
* `on_fn_out` – forward each `function_call_output` event upstream.

Returns the concatenated assistant messages produced by the fork after
the initial history.

### `history`

```ocaml
val history :
  history:Openai.Responses.Item.t list ->
  arguments:string ->
  string -> (* call_id *)
  Openai.Responses.Item.t list
```

Utility used in unit-tests: inserts the synthetic instruction block that
informs the child agent of its *systems contract* and returns the new
message list.

---

## Usage example

The snippet below spawns a fork that greps all `.ml` files in the current
workspace for the identifier `todo`.  While the fork is running, the
parent UI receives progress events and partial deltas.

```ocaml
let grep_todo () =
  let open Fork in
  let output =
    execute
      ~env
      ~history:prev_messages
      ~call_id:"grep-todo"
      ~arguments:{|
        { "command": "rg", "arguments": ["-n", "todo", "*.ml"] }
      |}
      ~tools
      ~tool_tbl
      ~on_event:(fun ev -> Log.debug "fork-ev: %s" (Sexp.to_string (sexp_of_event ev)))
      ~on_fn_out:(fun out -> Log.info "fork-out: %s" out.output)
      ()
  in
  Console.print_string output
```

---

## Internal design notes

* **Streaming first**.  The heavy lifting happens in a small
  self-contained `run_stream` driver that speaks the *response-stream*
  protocol and forwards every event to the parent.  This avoids a
  compile-time dependency on the much larger `Driver` module and keeps
  the recursion footprint under control.

* **Progress echoing**.  Whenever the fork receives a new text delta it
  appends it to a local `Buffer.t` and immediately emits a
  `function_call_output` update up the stack.  The parent therefore sees
  coherent, incremental output in the tool-call block it already uses to
  display progress.

* **Recursive forks** are fully supported because `tool_tbl` must expose
  an entry named `fork` that points back to `Fork.execute` itself.  The
  helper detects such nested invocation and delegates again rather than
  spawning a fresh process.

* A deliberately **tiny cache** (1 000 entries) is instantiated for each
  level of recursion.  This keeps memory usage in check even for deep
  fork trees.

---

## Known limitations

1. The function is blocking.  Long-running forks will stall the parent
   fiber unless the caller runs `execute` in its own domain.
2. Each fork adds one level of OpenAI completion overhead: streaming
   events must traverse the stack from the model to the fork, then to
   the parent UI.
3. The current implementation does **not** propagate cancellation from
   the parent to the nested request.  Adding a cancellation token is on
   the roadmap.


