# Chat_tui.Types

Shared data types for the Ochat terminal UI.

`Chat_tui.Types` centralises the small set of types that the rest of the
TUI depends on:

- **Chat schema** – roles and messages (`role`, `message`)
- **Tool-output classification** – coarse tags for tool responses
  (`tool_output_kind`)
- **Streaming buffers** – per-call accumulators used while deltas arrive
  (`msg_buffer`)
- **Commands and patches** – interaction between the pure controller and the
  impure runtime (`cmd`, `patch`)
- **Runtime settings** – small record of user-togglable flags (`settings`)

Keeping these definitions in a tiny library layer avoids circular
dependencies between the model, controller, renderer, and IO-heavy pieces of
the system.

---

## Chat-schema primitives

### `type role = string`

`role` is the textual role attached to each message in the transcript. In
practice this is expected to be one of the OpenAI roles:

- `"system"`
- `"user"`
- `"assistant"`
- `"function"`
- `"tool"` (for assistant tool calls and their outputs)

The type is intentionally just `string`: the module does **not** validate or
normalise roles. Callers are free to introduce project-specific roles if they
also teach the renderer how to display them.

### `type message = role * string`

A `message` is a `(role, content)` pair:

```ocaml
type message = role * string
```

- The `role` field is the sender role as above.
- The `string` is the UTF-8 text shown in the history pane and sent to the
  OpenAI API.

Messages are **renderable** items only – they do not carry OpenAI IDs or any
tool-call metadata. Higher layers keep a separate canonical history in
`Openai.Responses.Item.t list` and derive `message` values from it.

Example: building a simple transcript for initial rendering:

```ocaml
let messages : Chat_tui.Types.message list =
  [ "assistant", "Welcome to ochat!";
    "user", "How do I use this TUI?";
    "assistant", "Type your question at the bottom and hit Enter." ]
```

This list can be passed directly to `Chat_tui.Model.create` as the
`~messages` argument.

---

## Tool-output classification

```ocaml
type tool_output_kind =
  | Apply_patch
  | Read_file      of { path : string option }
  | Read_directory of { path : string option }
  | Other          of { name : string option }
```

`tool_output_kind` is **view-only metadata** attached to certain messages.
It lets the renderer distinguish between different classes of tool outputs
without exposing the full JSON protocol at the UI boundary.

The mapping from OpenAI function calls to `tool_output_kind` values is
implemented in `Chat_tui.Model`:

- Tool calls named `"apply_patch"` are tagged as `Apply_patch`.
- Tool calls named `"read_file"` or `"read_directory"` are tagged as
  `Read_file { path }` or `Read_directory { path }`, where `path` is parsed
  from the tool arguments when possible.
- All other tools are classified as
  `Other { name = Some tool_name }` (or `None` if the name cannot be
  recovered).

The classification is stored in
`Model.tool_output_by_index : (int, tool_output_kind) Hashtbl.t`, keyed by
the **message index** in `Model.messages`. This allows the renderer to:

- choose syntax-highlighting grammars (for example, detecting `read_file`
  output containing OCaml or JSON code),
- apply specialised layouts for known tools (e.g. patches vs. directory
  listings), and
- fall back to generic rendering for unknown tools.

Because this metadata is derived purely from the existing history, it is not
persisted in session files and can always be recomputed via
`Chat_tui.Model.rebuild_tool_output_index`.

---

## Streaming buffers

```ocaml
type msg_buffer =
  { buf   : Buffer.t
  ; index : int
  }
```

`msg_buffer` is a small helper record used while assistant deltas are
streaming in.

- `buf` – mutable `Buffer.t` accumulating the current textual content for a
  single in-flight assistant or tool message.
- `index` – zero-based index into `Chat_tui.Model.messages` that identifies
  the renderable message associated with this buffer.

The typical lifecycle is:

1. When the first delta for a given `call_id` arrives,
   `Chat_tui.Model.apply_patch` handles an `Ensure_buffer` patch by
   allocating a `msg_buffer` and pushing an empty placeholder message at the
   end of `Model.messages`.
2. Subsequent deltas trigger `Append_text` patches that append to `buf` and
   update `messages.(index)` with the new text.
3. Once the tool or assistant finishes, the placeholder has been replaced by
   the fully-streamed message. The buffer may optionally be dropped.

Because `index` is captured at buffer creation time, code that mutates
`Model.messages` outside the patch machinery must take care not to reorder or
remove messages in a way that would invalidate existing indices.

Example: manually emulating streaming for a single message (simplified):

```ocaml
let simulate_streaming (model : Chat_tui.Model.t) ~(id : string)
    ~(role : string) ~(chunks : string list) : unit =
  let open Chat_tui.Types in
  let patches =
    Ensure_buffer { id; role }
    :: List.map (fun text -> Append_text { id; role; text }) chunks
  in
  ignore (Chat_tui.Model.apply_patches model patches)
```

---

## Commands (`cmd`)

```ocaml
type cmd =
  | Persist_session of (unit -> unit)
  | Start_streaming of (unit -> unit)
  | Cancel_streaming of (unit -> unit)
```

`cmd` values are **requests** emitted by the pure controller and interpreted
by a small, side-effecting runner (usually in `Chat_tui.App`). They decouple
key handling and state updates from IO-heavy operations such as persistence
and network calls.

Each constructor carries a thunk `unit -> unit`:

- `Persist_session f` – run `f ()` to save the current session.
- `Start_streaming f` – run `f ()` to start an OpenAI streaming request.
- `Cancel_streaming f` – run `f ()` to abort the in-flight streaming
  request, if any.

The controller typically returns a list of patches plus a list of commands
for each input event:

```ocaml
type reaction =
  { patches : Chat_tui.Types.patch list;
    cmds    : Chat_tui.Types.cmd   list;
  }
```

An interpreter applies the patches to the model and then executes the
commands by calling their thunks.

Example: simple interpreter for `cmd` (error handling omitted):

```ocaml
let run_cmd (c : Chat_tui.Types.cmd) : unit =
  match c with
  | Persist_session f
  | Start_streaming f
  | Cancel_streaming f -> f ()

let run_cmds (cmds : Chat_tui.Types.cmd list) : unit =
  List.iter run_cmd cmds
```

---

## Patches (`patch`)

```ocaml
type patch =
  | Ensure_buffer of { id : string; role : string }
  | Append_text   of { id : string; role : string; text : string }
  | Set_function_name   of { id : string; name : string }
  | Set_function_output of { id : string; output : string }
  | Update_reasoning_idx of { id : string; idx : int }
  | Add_user_message       of { text : string }
  | Add_placeholder_message of { role : string; text : string }
```

`patch` values are **high-level edits** to `Chat_tui.Model.t`. They are
interpreted by `Chat_tui.Model.apply_patch`, which currently mutates the
model in place but is designed to evolve towards an immutable
model-plus-patches style.

Role of each constructor:

- `Ensure_buffer { id; role }` – guarantee that a streaming buffer exists
  for the given `id`. Creates a fresh `msg_buffer` and appends an empty
  `(role, "")` placeholder message when necessary.
- `Append_text { id; role; text }` – append `text` to the buffer `id`,
  allocating it first via `Ensure_buffer` when `id` is unseen. Updates the
  corresponding entry in `Model.messages` and invalidates renderer caches for
  that message.
- `Set_function_name { id; name }` – remember the function name associated
  with streaming buffer `id`. Used to classify the eventual tool output.
- `Set_function_output { id; output }` – store the (JSON-encoded) `output`
  returned by the tool call for `id`. The model sanitises and possibly
  truncates the text before updating the visible message and
  `tool_output_by_index`.
- `Update_reasoning_idx { id; idx }` – track how much of the model's
  reasoning has been rendered for a streaming tool call. The value is stored
  in a mutable `int ref` per `id` and read by the renderer.
- `Add_user_message { text }` – append a user message `("user", text)` to
  `Model.messages`. The canonical OpenAI history remains under the caller's
  control (e.g. via `Model.add_history_item`).
- `Add_placeholder_message { role; text }` – append a **non-persisted**
  placeholder message `(role, text)` that lives only in the renderable
  transcript.

Patches compose naturally via `Chat_tui.Model.apply_patches`, which folds a
list of patches over a model.

Example: building patches for a simple question–answer exchange:

```ocaml
let patches_for_prompt ~(prompt : string) ~(answer : string)
  : Chat_tui.Types.patch list =
  let open Chat_tui.Types in
  [ Add_user_message { text = prompt };
    Add_placeholder_message { role = "assistant"; text = answer } ]
```

In a real application the assistant answer would arrive via streaming
patches instead of a single placeholder.

---

## Runtime settings

```ocaml
type settings =
  { parallel_tool_calls : bool }

val default_settings : unit -> settings
```

`settings` is a tiny record of flags that influence runtime behaviour of the
TUI and tool execution.

- `parallel_tool_calls` – when `true`, the assistant is allowed to request
  multiple tool calls in a single turn. The runtime executes them
  concurrently. When `false`, tool calls are forced to run sequentially,
  which can simplify debugging or accommodate models that do not yet support
  parallel tool calls.

`default_settings ()` returns the default configuration. At the time of
writing it enables `parallel_tool_calls`:

```ocaml
let cfg = Chat_tui.Types.default_settings () in
assert cfg.parallel_tool_calls
```

Callers are encouraged to start from `default_settings ()` and override only
the fields they care about. This keeps them resilient to future additions:

```ocaml
let serial_tools () : Chat_tui.Types.settings =
  let open Chat_tui.Types in
  let cfg = default_settings () in
  { cfg with parallel_tool_calls = false }
```

---

## Known issues and limitations

- **Stringly-typed roles** – `role` is just `string`, so invalid or
  misspelled roles are not caught at compile time. The renderer is
  defensive, but callers should prefer the standard OpenAI roles when
  possible.

- **Mutable streaming indices** – `msg_buffer.index` assumes messages are
  only appended. If `Model.messages` is mutated in other ways (insertion in
  the middle, removal), existing indices may no longer point at the intended
  messages. Prefer to evolve the model via patches.

- **Heuristic tool classification** – `tool_output_kind` is best-effort. New
  tools default to the `Other` case until the classifier is taught about
  them. This affects only rendering, not correctness.

- **In-place patch application (for now)** – `Chat_tui.Model.apply_patch`
  currently mutates the model record. While convenient, this makes it harder
  to reason about sharing between fibers. The API is designed so that a
  future refactor to an immutable model plus patches will not change the
  shape of `patch` itself.

---

## Related modules

- `Chat_tui.Model` – mutable snapshot of the TUI state; exposes
  `apply_patch`, `apply_patches`, and helpers that work with `patch` and
  `msg_buffer`.
- `Chat_tui.Renderer` – pure view layer that renders `Model.t` into Notty
  images. Uses `tool_output_kind` and `message` heavily.
- `Chat_tui.Controller` – key handling, cursor movement, and high-level
  reactions that typically emit `patch list * cmd list`.
- `Chat_tui.Stream` – translates OpenAI streaming events into `patch`
  sequences applied via the model.
- `Chat_tui.App` – orchestration glue that wires the controller, model,
  renderer, and command interpreter together.

