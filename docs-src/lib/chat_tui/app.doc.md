# `Chat_tui.App` — start and run the Ochat terminal UI

`Chat_tui.App` boots the full-screen Notty terminal UI, initializes the chat
model from a `*.chatmd` prompt (and optionally an existing session), then runs
the main event loop until the user quits.

The public API is intentionally small:

- `run_chat` is the supported entry point for executables.
- Lower-level event-loop helpers live in internal modules (documented below)
  and are primarily useful for maintenance and white-box tests.

## Table of contents

1. [Boot sequence — `run_chat`](#run_chat)
2. [Sessions, export, and persistence](#shutdown)
3. [How streaming and events fit together](#architecture)
4. [Internal modules](#internal-modules)

## Boot sequence — `run_chat` <a id="run_chat"></a>

```ocaml
val run_chat :
  env:Eio_unix.Stdenv.base ->
  prompt_file:string ->
  ?session:Session.t ->
  ?export_file:string ->
  ?persist_mode:Chat_tui.App.persist_mode ->
  ?parallel_tool_calls:bool ->
  unit ->
  unit
```

Call `run_chat` from your executable:

```ocaml
let () =
  Eio_main.run @@ fun env ->
  Chat_tui.App.run_chat ~env ~prompt_file:"prompt.chatmd" ()
```

At startup, `run_chat`:

1. loads and parses `prompt_file` as ChatMarkdown,
2. constructs an initial `Model.t`:
   - from the prompt history, or
   - from `~session` if it was supplied and has a non-empty history,
3. builds the tool runtime from `<tool .../>` declarations in the prompt, and
4. runs the main event loop until the user quits.

## Sessions, export, and persistence <a id="shutdown"></a>

`run_chat` supports two independent “write back” mechanisms:

1. **Export to ChatMarkdown** (a consolidated `*.chatmd` file)
2. **Persist a session snapshot** (for resuming via `~session`)

Export behaviour:

- If the UI quits via `Esc` while idle, Ochat prompts whether to export and
  optionally asks for an output path (defaulting to `prompt_file`).
- If the UI quits via other means (e.g. `q`/Ctrl-C), export happens
  automatically, targeting `export_file` if provided, otherwise `prompt_file`.

Snapshot persistence is controlled by `persist_mode`:

```ocaml
type persist_mode = [ `Always | `Never | `Ask ]
```

When `persist_mode = `Ask` and a session is active, Ochat prompts
`Save session snapshot? [Y/n]`.

## How streaming and events fit together <a id="architecture"></a>

The event loop is single-threaded with respect to the shared `Model.t`.
Background fibres communicate by pushing `internal_event`s into a queue.

```text
Notty_eio.Term ───────► input_stream  ┐
                                     │
  streaming / compaction fibres ─► internal_stream ─► App_reducer.run
                                     │
                                     └──► Model mutations + redraw requests
```

Streaming in particular flows like this:

1. `App_reducer` turns a keypress into a submit request.
2. `App_submit` applies local submit effects and forks a streaming worker.
3. `App_streaming` runs the OpenAI streaming request and emits:
   `Streaming_started`, `Stream` / `Stream_batch`, `Tool_output`, and
   `Streaming_done` events.
4. `App_stream_apply` applies streaming events to the model using
   `Chat_tui.Stream` to translate low-level events into `Types.patch` lists.

## Internal modules <a id="internal-modules"></a>

These modules are part of the `ochat.chat_tui` library but should be treated
as internal implementation detail:

- [`Chat_tui.App_reducer`](app_reducer.doc.md) — main event loop and concurrency policy
- [`Chat_tui.App_runtime`](app_runtime.doc.md) — mutable runtime container shared by helpers
- [`Chat_tui.App_events`](app_events.doc.md) — event types exchanged between fibres and reducer
- [`Chat_tui.App_submit`](app_submit.doc.md) — local submit effects and spawning streaming
- [`Chat_tui.App_streaming`](app_streaming.doc.md) — OpenAI streaming worker
- [`Chat_tui.App_stream_apply`](app_stream_apply.doc.md) — apply stream/tool output events to the model
- [`Chat_tui.App_compaction`](app_compaction.doc.md) — spawn history compaction worker

If you are extending the UI, start by reading `App.run_chat` and
`App_reducer.run` to understand how events are sequenced.

