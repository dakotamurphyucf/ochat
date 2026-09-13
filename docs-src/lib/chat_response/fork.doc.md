# Fork – drive a nested *assistant clone*

Fork progress is classified onto the Agent page and keeps child history/input consumption separate. Completed tool output belongs to the root transcript; active-call summaries are bounded projections, not persisted continuations.

See [agent-host integration](../../agent-server/embedding.md) and
[orchestration semantics](../../agent-server/chatml-orchestration.md).

`lib/chat_response/Fork` provides the *runtime* implementation of the
`fork` tool defined in [`lib/definitions.ml`](../../../lib/definitions.ml).

The tool runs a child over the supplied parent history and tools. Parent
entries retain their IDs; new child entries use a separate invocation-scoped
allocator. Child history is not merged into parent history. Instead, all new
assistant-message text is returned as the completed parent tool output.
`===RESULT===` and `===PERSIST===` are prompt conventions, not extraction
boundaries: text outside `===PERSIST===` is returned too.

The parent UI receives live updates while the forked agent is running,
so users can monitor progress (or cancel a runaway fork) without waiting
for completion.

The daemon and local agent-session host use the shared
[in-memory streaming driver](in_memory_stream.doc.md) for their built-in
`<tool name="fork"/>`. That path inherits the current model and selected tools
and returns the child's final assistant message. Its temporary children can call
native tools, standalone ChatML tools, moderator-handled tools, and `fork` again.
Each call checks current permissions and parent moderation and records an
invocation linked to its immediate fork parent. Child calls and outputs stay out
of the root conversation history. The child acquires no independent persisted
session or authority to outlive its parent. The lower-level helper APIs described
below are also retained for older integrations.

---

## Public interface

### `execute_entries`

````ocaml
val execute_entries :
  env:Eio_unix.Stdenv.base ->
  allocator:History_entry.Allocator.t ->
  history:History_entry.t list ->
  invocation_id:Invocation_id.t ->
  call_id:string ->
  arguments:string ->
  tools:Openai.Responses.Request.Tool.t list ->
  tool_tbl:(string, Ochat_function.runner) Base.Hashtbl.t ->
  on_event:(Openai.Responses.Response_stream.t -> unit) ->
  ?on_sourced_event:(Sourced_response_event.t -> unit) ->
  ?on_tool_execution:(Tool_execution_event.t -> unit) ->
  on_fn_out:(Openai.Responses.Function_call_output.t -> unit) ->
  ?temperature:float ->
  ?max_output_tokens:int ->
  ?reasoning:Openai.Responses.Request.Reasoning.t ->
  unit -> string
````

Blocking helper that runs the fork to completion in the caller's Eio fiber.
Errors or cancellation can terminate the call without a returned reply.

Parameters (see the inline documentation in `fork.mli` for full
details):

* `env` – Eio standard environment, used for network and filesystem.
* `history` – identity-bearing parent entries supplied to the child.
* `invocation_id` – fresh identity from `Invocation_id.create ()`, independent
  of provider IDs and reusable tool-call correlation IDs.
* `allocator` – child allocator made with `Fork.allocator ~parent_namespace
  invocation_id`; parent allocation is not advanced by child entries.
* `call_id` – parent tool-call correlation ID used for cumulative text
  progress and as the parent ID on sourced events. Raw provider events
  retain their own IDs.
* `arguments` – raw JSON arguments of the tool, parsed with
  `Definitions.Fork.input_of_string`.
* `tools` / `tool_tbl` – supplied tool definitions and invocation-aware
  runners. Recursive fork dispatch needs no self-entry in the table.
* `on_event` – forward each raw streaming event upstream.
* `on_sourced_event` – optionally observe response events tagged with the
  fork invocation and parent call ID.
* `on_tool_execution` – optionally observe child tool activity.
* `on_fn_out` – cumulative assistant-text progress under the parent call ID,
  plus nested function-call outputs under their own call IDs.

Returns all assistant-message text produced after the initial child history,
joining content parts with spaces and messages with newlines. It does not
extract either named section from the reply.

### `history_entries`

```ocaml
val history_entries :
  allocator:History_entry.Allocator.t ->
  history:History_entry.t list ->
  arguments:string ->
  call_id:string ->
  History_entry.t list
```

Builds child input without making a model request. It preserves parent
entries and appends one child-owned synthetic function-call output containing
the fork instructions. This is also the offline prompt-test entrypoint.

---

## Usage example

This offline example constructs the child input for a search task without
running tools or making a model request:

```ocaml
let search_input () =
  let invocation_id = Chat_response.Fork.Invocation_id.create () in
  let allocator =
    Chat_response.Fork.allocator ~parent_namespace:"example" invocation_id
  in
  Chat_response.Fork.history_entries
    ~allocator
    ~history:[]
    ~call_id:"grep-todo"
    ~arguments:{|{"command":"rg","arguments":["-n","todo","-g","*.ml"]}|}
```

---

## Internal design notes

* **Streaming first**.  The heavy lifting happens in a small
  self-contained `run_stream` driver that speaks the *response-stream*
  protocol and forwards every event to the parent.  This avoids a
  compile-time dependency on the much larger `Driver` module and keeps
  the recursion footprint under control.

* **Progress echoing**. Whenever the fork receives a new text delta it
  appends it to a local `Buffer.t` and immediately emits a
  `function_call_output` update up the stack. Hosts can display this progress
  on the Agent page without merging child history into the root transcript.

* **Recursive forks** use `Fork.execute_entries` with an invocation-scoped
  allocator. Parent entries retain their IDs, child-created entries remain
  isolated, and recursive dispatch does not require a legacy self-reference
  in `tool_tbl`.

* A deliberately **tiny cache** (1 000 entries) is instantiated for each
  level of recursion.  This keeps memory usage in check even for deep
  fork trees.

---

## Known limitations

1. The function is blocking. Long-running forks should run in a dedicated
   fiber managed by the caller.
2. Each fork adds one level of OpenAI completion overhead: streaming
   events must traverse the stack from the model to the fork, then to
   the parent UI.
3. Local nested execution inherits the caller's Eio cancellation context:
   the request and response reader run under a nested switch. Cancelling
   that context stops local work, but does not guarantee that a remote
   provider stops generation or billing. An embedding must connect its
   user-facing cancel action to that context.

## Nested shell runtimes

A nested ChatMD agent compiles and authorizes its effective shell manifest
through the same `Agent_runtime` constructor as a top-level agent. Host
administrative ceilings always apply. Manifest and command grants match only
when their complete source/runtime/session identity permits reuse; opening a
nested prompt does not implicitly inherit broader parent shell authority.
