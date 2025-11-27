# Chat_tui.App

Event loop, streaming, and persistence for the terminal chat UI.

`Chat_tui.App` is the orchestration layer that wires together:

- `Chat_tui.Model` (mutable UI state),
- `Chat_tui.Controller` (key handling),
- `Chat_tui.Renderer` (full-screen view),
- `Notty_eio.Term` (terminal IO), and
- `Chat_response.Driver` / `Context_compaction.Compactor` (OpenAI
  integration and optional context-compaction).

If you want to embed the Ochat TUI in your own executable, this is the
module you call.

---

## Overview

At a high level `Chat_tui.App`:

1. Starts a full-screen `Notty_eio.Term` session bound to the current
   `Eio_unix.Stdenv.base`.
2. Parses a `*.chatmd` prompt file, discovers tools, and builds an initial
   `Chat_tui.Model.t` from the static ChatMarkdown content and an optional
   persisted `Session.t`.
3. Runs a **single-threaded event loop** that reads from an
   `Eio.Stream.t` of UI events:
   - key and mouse events decoded by Notty,
   - `Redraw` requests,
   - streaming events from the OpenAI API,
   - function-call output notifications, and
   - history replacement events.
4. Delegates input handling to `Chat_tui.Controller`, which mutates the
   model and returns high-level `reaction` values.
5. Applies streaming updates via `Chat_tui.Stream` and throttles redraws
   with `Chat_tui.Redraw_throttle` to reach a steady frame rate.
6. On shutdown, optionally
   - exports the conversation as ChatMarkdown, and
   - persists a `Session.t` snapshot to disk.

The public surface is kept intentionally small. Typical callers only need
`run_chat`; the other helpers are exposed primarily to support white-box
unit and integration tests.

---

## Event loop and streaming architecture

`run_chat` creates a bounded `Eio.Stream.t` (`ev_stream`) shared between
the Notty callback and the pure event loop:

- `Notty_eio.Term.run` installs an `on_event` callback that pushes
  `Notty.Unescape.event` values and `\`Resize` notifications into
  `ev_stream`.
- The main loop repeatedly `Eio.Stream.take`'s from `ev_stream` and
  dispatches on a closed event type:

  ```ocaml
  [ `Resize
  | `Redraw
  | Notty.Unescape.event
  | `Stream of Openai.Responses.Response_stream.t
  | `Stream_batch of Openai.Responses.Response_stream.t list
  | `Replace_history of Openai.Responses.Item.t list
  | `Function_output of Openai.Responses.Function_call_output.t ]
  ```

Key paths:

- **Keyboard input** – forwarded to `Chat_tui.Controller.handle_key`. The
  returned `reaction` determines whether to redraw, submit the prompt,
  cancel streaming, start context compaction, or quit.

- **Streaming events** – `handle_submit` starts
  `Chat_response.Driver.run_completion_stream_in_memory_v1` in a fresh
  `Eio.Switch.t`. All OpenAI streaming events and tool outputs are pushed
  into a _local_ `Eio.Stream.t` which is consumed by a background batching
  fiber:

  - `Output_*` deltas are coalesced into `Stream_batch` events when they
    arrive within a small window (12 ms by default, configurable via
    `OCHAT_STREAM_BATCH_MS`).
  - Each `Stream` / `Stream_batch` is converted into a list of
    `Chat_tui.Types.patch` values by `Chat_tui.Stream.handle_event` and
    folded into the model via `Model.apply_patches`.
  - Completed items are appended to `Model.history_items` so the canonical
    transcript stays in sync with what was rendered.

- **Function-call outputs** – `Function_call_output.t` values are forwarded
  to `Chat_tui.Stream.handle_fn_out`, which updates the model's
  `tool_output_by_index` and clears fork bookkeeping. The full output item
  is also appended to `Model.history_items`.

- **Redraw throttling** – frequent updates (especially streaming) request a
  redraw via `Redraw_throttle.request_redraw`. A separate scheduler fiber
  calls `Redraw_throttle.tick` at the configured FPS and enqueues a single
  `\`Redraw` event when the UI is dirty. `\`Redraw` events then re-render
  the model using `Chat_tui.Renderer.render_full`.

Context compaction is handled separately: when the user triggers the
`Compact_context` reaction (via `:` commands such as `:c` or `:compact`),
`run_chat` spawns a background fiber that:

1. Optionally archives the current `Session.t` via `Session_store`.
2. Invokes `Context_compaction.Compactor.compact_history` on the current
   `Model.history_items`.
3. Replaces the model's history, messages, and tool-output index with the
   compacted version and requests a redraw.

---

## Public API

### `type prompt_context`

```ocaml
type prompt_context = {
  cfg      : Chat_response.Config.t;
  tools    : Openai.Responses.Request.Tool.t list;
  tool_tbl : (string, string -> string) Base.Hashtbl.t;
}
```

Runtime artefacts derived from the static ChatMarkdown prompt:

- `cfg` – behavioural settings such as model, temperature, max tokens and
  optional reasoning configuration. Built from `Chat_response.Config`.
- `tools` – list of tool descriptors passed to
  `Chat_response.Driver.run_completion_stream_in_memory_v1`.
- `tool_tbl` – mapping from tool name to an OCaml implementation
  (`string -> string`) produced by `Ochat_function.functions`. When the
  model issues a tool call, its JSON payload is dispatches through this
  table.

`prompt_context` is computed once in `run_chat` and threaded into
`handle_submit`.

### `type persist_mode`

```ocaml
type persist_mode = [ `Always | `Never | `Ask ]
```

Persistence policy for the **session snapshot** at the end of `run_chat`:

- `` `Always `` – always persist the derived `Session.t` snapshot.
- `` `Never `` – never write a snapshot.
- `` `Ask `` – (default) prompt the user with `Save session snapshot? [Y/n]`
  when a session is present. An empty response defaults to `yes`.

The policy is ignored when `?session` is `None`.

### Placeholder helpers

These helpers are small convenience functions used by the app to surface
state to the user. They all operate on `Chat_tui.Model.t` via the
`Chat_tui.Types.Add_placeholder_message` patch.

#### `add_placeholder_thinking_message`

```ocaml
val add_placeholder_thinking_message : Model.t -> unit
```

Append a transient `("assistant", "(thinking…)")` message to the model so
the user gets immediate visual feedback after submitting a prompt. The
placeholder is overwritten once the first streaming tokens arrive.

#### `add_placeholder_stream_error`

```ocaml
val add_placeholder_stream_error : Model.t -> string -> unit
```

`add_placeholder_stream_error model msg` appends a one-shot `("error",
msg)` message. It is used when streaming fails so that fatal conditions are
visible in the transcript instead of only appearing in logs.

#### `add_placeholder_compact_message`

```ocaml
val add_placeholder_compact_message : Model.t -> unit
```

Append a temporary `("assistant", "(compacting…)")` message while
background context compaction is running. Once compaction finishes (either
successfully or with an error), this stub is effectively replaced by a more
informative status message.

### Snapshot persistence

#### `persist_snapshot`

```ocaml
val persist_snapshot : Eio_unix.Stdenv.base -> Session.t option -> Model.t -> unit
```

`persist_snapshot env session model` copies the live `model` back into a
`Session.t` and saves it to disk via `Session_store.save`:

- `history_items` → `Session.history`,
- `Model.tasks`   → `Session.tasks`,
- `Model.kv_store` (hashtable) → `Session.kv_store` (list of pairs).

When `session` is `None` the function is a no-op. `run_chat` uses this
helper from all quit branches so that conversation state is not lost even
when the user skips ChatMarkdown export.

### Submitting prompts

#### `apply_local_submit_effects`

```ocaml
val apply_local_submit_effects :
  dir:Eio.Fs.dir_ty Eio.Path.t ->
  env:Eio_unix.Stdenv.base ->
  cache:Chat_response.Cache.t ->
  model:Model.t ->
  ev_stream:'ev Eio.Stream.t ->
  term:Notty_eio.Term.t ->
  unit
```

Immediate (synchronous) UI updates performed **after** the user hits
Meta+Enter but **before** the OpenAI request is started.

Responsibilities:

- Snapshot the current draft buffer and interpret it according to
  `Model.draft_mode`:
  - `Plain` – treat the buffer as ordinary user text.
  - `Raw_xml` – treat the buffer as ChatMarkdown-style XML describing
    tool invocations; it is parsed with `Prompt.Chat_markdown` and
    converted to `Openai.Responses.Item.t` values via
    `Chat_response.Converter`.
- Append a user message to `Model.messages` and a corresponding history
  item to `Model.history_items`.
- Clear `Model.input_line`, reset the caret, and enable
  `Model.auto_follow`.
- Scroll the history viewport so the submitted message is visible (using
  the current terminal height from `Notty_eio.Term.size`).
- Inject a transient `("assistant", "(thinking…)")` placeholder via
  `add_placeholder_thinking_message`.
- Enqueue a `\`Redraw` event on `ev_stream` so the renderer can refresh
  the screen.

This function performs **no network IO**. The heavy lifting (constructing
and running the OpenAI request, handling streaming events) is delegated to
`handle_submit`.

#### `handle_submit`

```ocaml
val handle_submit :
  env:Eio_unix.Stdenv.base ->
  model:Model.t ->
  ev_stream:'ev Eio.Stream.t ->
  system_event:string Eio.Stream.t ->
  prompt_ctx:prompt_context ->
  datadir:Eio.Fs.dir_ty Eio.Path.t ->
  parallel_tool_calls:bool ->
  history_compaction:bool ->
  unit
```

Start an **asynchronous** OpenAI Chat Completions stream and feed the
results back into the UI.

Key details:

- A fresh `Eio.Switch.t` is created and stored in `Model.fetch_sw` so that
  `Esc` can cancel the in-flight request via `Eio.Switch.fail`.
- Streaming is driven by
  `Chat_response.Driver.run_completion_stream_in_memory_v1`, which operates
  entirely on an in-memory chat history and a `tools` list derived from the
  prompt.
- Two callbacks are installed:
  - `on_event` – receives `Response_stream.t` events and pushes them into a
    local `Eio.Stream.t` as `` `Stream ``.
  - `on_fn_out` – receives `Function_call_output.t` values and pushes them
    as `` `Function_output ``.
- A background batching fiber drains the local stream, grouping adjacent
  token events into `Stream_batch` bursts using a short time window
  (default 12 ms, configurable via `OCHAT_STREAM_BATCH_MS`). Function-call
  outputs are forwarded immediately.
- The outer UI loop consumes `Stream` / `Stream_batch` / `Function_output`
  events from `ev_stream`, turns them into `Types.patch` values via
  `Chat_tui.Stream`, and applies them to the model.
- When the driver finishes successfully, `handle_submit` emits a single
  `` `Replace_history `` event with the updated
  `Openai.Responses.Item.t list`, and `run_chat` replaces the model's
  `history_items` and `messages` accordingly.

Error handling:

- On exception from the OpenAI client or tool functions, the helper:
  - clears `Model.fetch_sw`,
  - prunes trailing reasoning and partial function calls from
    `Model.history_items`,
  - rebuilds `Model.messages` from the pruned history,
  - logs a message to `stdout`, and
  - appends an error placeholder via `add_placeholder_stream_error` before
    requesting a redraw.

The `history_compaction` flag is forwarded to
`Chat_response.Driver.run_completion_stream_in_memory_v1`. When `true`, the
driver collapses redundant file-read entries in the history before sending
requests to the model, reducing token usage on long conversations that
repeatedly read the same documents.

The separate user-triggered context compaction (via
`Context_compaction.Compactor.compact_history`) is handled entirely inside
`run_chat`.

### High-level entry point

#### `run_chat`

```ocaml
val run_chat :
  env:Eio_unix.Stdenv.base ->
  prompt_file:string ->
  ?session:Session.t ->
  ?export_file:string ->
  ?persist_mode:persist_mode ->
  ?parallel_tool_calls:bool ->
  unit ->
  unit
```

Boot the TUI and block until the user terminates the program.

Parameters:

- `env` – standard environment from `Eio_main.run`.
- `prompt_file` – path to the `*.chatmd` prompt. The document seeds the
  chat history, declares tools and configures default model settings.
- `session` – optional `Session.t` to resume. When provided and non-empty,
  its history, tasks, and `kv_store` override the defaults from
  `prompt_file`.
- `export_file` – optional override for the ChatMarkdown export path.
  Defaults to `prompt_file` when omitted.
- `persist_mode` – snapshot persistence policy (see above). Default is
  `` `Ask ``.
- `parallel_tool_calls` – whether tool calls are allowed to run in
  parallel. Passed through to `Chat_response.Driver`.

On exit, `run_chat`:

1. Releases the `Notty_eio.Term` so subsequent messages appear cleanly in
   the user's shell.
2. Optionally exports the conversation as ChatMarkdown:
   - If the user quit via `Esc`, they are asked
     `Export conversation to promptmd file? [y/N]` and may override the
     target path.
   - For other quit paths (e.g. `q`, `Ctrl-C`), export happens
     automatically using `export_file` or `prompt_file`.
3. Depending on `persist_mode` and `session`, optionally persists the
   snapshot via `persist_snapshot`.

---

## Examples

### Minimal CLI using `run_chat`

```ocaml
open Core

let main (env : Eio_unix.Stdenv.base) (prompt_file : string) : unit =
  Chat_tui.App.run_chat ~env ~prompt_file ()

let () =
  let prompt_file = Sys.get_argv ().(1) in
  Io.run_main (fun env -> main env prompt_file)
```

This tiny program starts the TUI for the given `prompt.chatmd` file,
handling streaming, tools, export, and session persistence according to the
defaults.

### Custom persistence policy

```ocaml
open Core

let () =
  let prompt_file = Sys.get_argv ().(1) in
  Io.run_main (fun env ->
    Chat_tui.App.run_chat
      ~env
      ~prompt_file
      ~persist_mode:`Never
      ~parallel_tool_calls:false
      ())
```

This variant disables snapshot persistence and forces tool calls to execute
sequentially, which can be useful when debugging tools or running under a
restricted environment.

---

## Known issues and limitations

- **Terminal-only UI** – `Chat_tui.App` is tightly coupled to
  `Notty_eio.Term`. Embedding it in non-terminal environments (e.g. web
  UIs) requires factoring out the event loop and renderer.

- **Byte-based editing and selections** – underlying editing semantics come
  from `Chat_tui.Controller` / `Chat_tui.Model` and are byte-based, not
  grapheme-aware. Some Unicode input may behave unexpectedly when deleting
  or moving by character.

- **Long-running compaction** – context compaction runs in the background
  but still depends on the OpenAI API when `env` is provided to
  `Context_compaction.Compactor`. On slow or flaky networks the
  "(compacting…)" placeholder may stay visible for a while.

- **Streaming batch size tuning** – the `OCHAT_STREAM_BATCH_MS` knob is
  meant for experimentation and benchmarking. Extremely small values can
  increase CPU usage; very large values may make the UI feel sluggish.

---

## Related modules

- `Chat_tui.Model` – mutable UI state and helpers for manipulating it.
- `Chat_tui.Controller` – translates `Notty.Unescape.event` into model
  updates and high-level reactions.
- `Chat_tui.Renderer` – renders a `Model.t` and terminal size into a Notty
  image plus cursor position.
- `Chat_tui.Stream` – converts OpenAI streaming events into declarative
  `Types.patch` values.
- `Chat_tui.Redraw_throttle` – coalesces redraw requests to reach a steady
  frame rate.
- `Chat_response.Driver` – high-level ChatMarkdown driver used for
  streaming conversations and tools.
- `Context_compaction.Compactor` – conversation-history compactor used by
  the explicit context-compaction command.
- `Session` / `Session_store` – persistent session representation and
  filesystem wiring.

