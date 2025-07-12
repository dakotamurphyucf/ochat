# Chat-TUI – Code Context (June 2025)

This document provides a compact yet precise overview of the **Chat-TUI**
sub-library that lives in `lib/chat_tui/`.  It mirrors the style of the
top-level *ocamlgpt* project context file so that developers can very
quickly understand how the interactive terminal UI is organised and how the
individual modules collaborate.

The information is **authoritative** for the current code base — please keep
it in sync whenever you add/rename modules or change larger internal
interfaces.

---

## Notation

• All paths are relative to `lib/chat_tui/` unless stated otherwise.  
• `Module_name` refers to the corresponding `module_name.ml` / `.mli` pair.  
• `*` in a path matches all variants (e.g. `foo*` = `foo.ml` + `foo.mli`).  
• Italic words (e.g. *renderer*) denote the dune **library** stanza of the
  same name – here there is a single library called *chat_tui*.

---

## Module Overview

```
chat_tui/
│ app.ml/.mli            – glue module that starts the Notty/Eio event loop
│ cmd.ml/.mli            – side-effectful command interpreter (IO, network)
│ controller.ml/.mli     – key-event → state-patch reducer (pure)
│ conversation.ml/.mli   – helpers to turn OpenAI history → display tuples
│ model.ml/.mli          – mutable container holding the entire UI state
│ renderer.ml/.mli       – pure functions turning Model → Notty images
│ stream.ml/.mli         – OpenAI streaming delta → Types.patch list (pure)
│ persistence.ml/.mli    – (de)serialise ChatMarkdown + transcript files
│ snippet.ml             – simple `/expand` snippet table
│ util.ml/.mli           – misc string & wrapping helpers (pure)
│ types.ml/.mli          – shared type definitions (role, message, patch…)
│ command_mode_plan.md   – design doc for upcoming “command mode”
│ path_completion.md     – notes on shell-style path completion for the input box
```

### 1. Types (types.ml/.mli)
Central, *side-effect free* module shared by all others.  Defines:

• `role`, `message` – primitive chat concepts.  
• `msg_buffer` – incremental per-message buffer used while streaming so that
  partial tokens can be shown immediately.  
• Open variant `patch` – describes **pure** modifications to the `Model.t`
  record.  Current constructors cover:
  – `Ensure_buffer` (prepare an empty buffer & placeholder row)  
  – `Append_text` (append delta to buffer + visible message)  
  – `Set_function_name` / `Set_function_output` (tool call meta)  
  – `Update_reasoning_idx` (track which reasoning summary we are on)  
  – `Add_user_message` (immediately after submit)  
  – `Add_placeholder_message` ("(thinking…)" or error placeholder).  
• Open variant `cmd` – describes **effectful** actions triggered by the UI.
  Implemented constructors are `Persist_session`, `Start_streaming` and
  `Cancel_streaming`; more are expected to land once tool-invocation from the
  UI is surfaced.

### 2. Model (model.ml/.mli)
Owns all *mutable* state required by the UI:

• `history_items` – full OpenAI item list (for persistence).  
• `messages` – derived `(role * text)` list shown on screen.  
• Input editor state (`input_line`, `cursor_pos`, `selection_anchor`).  
• Scroll-box (`Notty_scroll_box.t`) for history viewport.  
• Auxiliary hash-tables for streaming buffers, function names, reasoning
  indices.  

`Model.apply_patch{,es}` performs the low-level in-place updates demanded by
`Types.patch`.  A future refactor will migrate to an *immutable* model that
rebuilds a fresh record every time.

### 3. Controller (controller.ml/.mli)
Pure key-event handler that consumes `Notty.Unescape.event` values and returns
one of `reaction = Redraw | Submit_input | Cancel_or_quit | Quit | Unhandled`.

Highlights of the current Insert-mode keymap (2025-06):
• Standard ASCII insertion and Backspace.  
• Line editing à-la Readline:  Ctrl-A/E, Meta-F/B, Ctrl-K/U/W, Ctrl-Y, kill
  buffer with yank, word-wise deletion, etc.  
• Multi-line helpers: Ctrl-↑/↓ moves cursor vertically within draft, Meta+↑/↓
  duplicates current line, Meta+Shift+←/→ indents/unindents line, Enter inserts
  literal newline.  
• Scrolling the conversation viewport: Arrow ↑/↓ (without modifiers) scroll a
  single line; PgUp/PgDown, Home/End move by page or jump extremes.  
• Selection: Meta-V (or Meta-S / ß) toggles selection anchor; copy/cut with
  Ctrl-C / Ctrl-X when selection active, yank with Ctrl-Y.  
• High-level actions: Meta-Enter → Submit, ESC → Cancel/quit, Ctrl-C / `q`
  → immediate quit.

The implementation is completely side-effect free – every change is expressed
as mutations on the passed‐in `Model.t` or reading its fields.

### 4. Renderer (renderer.ml/.mli)
Collection of *pure* helpers that compute the terminal image and cursor
position from the immutable snapshot of a `Model.t` value.  Key details:
• Per-role colour palette (`attr_of_role`) – assistant cyan, user yellow, tool
  magenta, reasoning blue, system grey, … errors red-reverse.  
• `message_to_image` draws each message inside a light box using Unicode line
  drawing; text is UTF-8 word-wrapped via Util.wrap_line.  
• A `Notty_scroll_box.t` (stored in the model) renders the conversation and
  remembers the scroll offset; renderer only sets the content + dimensions.  
• The lower input editor reuses the same box style, adds reverse-video
  selection highlight and reports the accurate cursor location.

### 5. Stream (stream.ml/.mli)
Pure translation layer that turns the incremental
`Openai.Responses.Response_stream.t` (token deltas, function-call chunks,
reasoning summaries, …) into a list of `Types.patch` ready for
`Model.apply_patches`.  `handle_event` deals with a single event, while
`handle_events` batches lists for efficiency.  A small helper
`handle_fn_out` covers the separate SSE channel that delivers the final tool
outputs.

### 6. Cmd (cmd.ml/.mli)
Tiny interpreter that executes the *effectful* `Types.cmd` values on behalf
of the pure layers.  All variants are just thin wrappers around a captured
closure – this decouples the caller from IO without inventing a heavyweight
effect system.

Implemented today:
• `Persist_session`  – flush conversation to disk (ChatMarkdown + JSON blobs)  
• `Start_streaming`  – kick off an OpenAI completion request in a background fibre  
• `Cancel_streaming` – abort the running HTTP stream via `Switch.fail`.

### 7. Persistence (persistence.ml/.mli)
File-system persistence for both draft *and* final conversation:

• `write_user_message` – in-place update that appends the user’s last input
  to the original prompt file so the next run starts with the same draft.  
• `persist_session`   – exports new history items to the `.chatmd` side-car
  directory, optionally splitting bulky tool call arguments / outputs into
  separate JSON documents to keep the main prompt readable.

All operations use the synchronous Eio FS API and are therefore expected to
run inside dedicated fibres.

### 8. Conversation (conversation.ml/.mli)
Convenience helpers that convert the rich DAG-style OpenAI history
(`Openai.Responses.Item.t list`) into a flat list of `(role * text)` tuples
usable by the renderer and controller.  Items without visible text are
filtered out.  Long tool outputs are sanitised & truncated so they never blow
up the UI.

### 9. Snippet (snippet.ml)
Tiny hard-coded table of `/expand` snippets.  Pure functions `find` and
`available` so unit tests can exercise them easily.

### 10. Util (util.ml/.mli)
Pure helpers:
• `sanitize` – strip control characters, expand TABs, optional whitespace
  trimming.  Used everywhere before text is inserted into the model.  
• `truncate` – safe shorten with ellipsis.
• `wrap_line` – byte-wise UTF-8 aware word-wrapper (guarantees valid UTF-8 and
  progress even on malformed sequences).  The Renderer builds on it.

### 11. App (app.ml/.mli)
High-level entry point used by `bin/chat_tui.ml`:

1. Parses command-line flags, opens files and Eio main switch.  
2. Instantiates `Notty_eio.Term`.  
3. Builds initial `Model.t` from ChatMarkdown history.  
4. Constructs a dedicated in-memory event queue – `ev_stream` – of variant
   type

     [`Resize | `Redraw | Notty.Unescape.event
      | `Stream of Openai.Responses.Response_stream.t
      | `Stream_batch of _ list
      | `Function_output of Res.Function_call_output.t
      | `Replace_history of Res.Item.t list ].

   All producers (Notty back-end and network fibres) push to this queue; the
   *main_loop* is the only consumer which keeps the whole program single-
   threaded from the model’s point of view.

5. Initialisation sequence inside `run_chat` (simplified):

   a. Resolve and parse the prompt file → `Chat_markdown` elements.  
   b. Derive configuration (`Config.of_elements`).  
   c. Instantiate tools declared in the prompt via `Tool.of_declaration`.  
   d. Build initial `history_items` through `Converter.to_items`.  
   e. Convert to `messages` with `Conversation.of_history`.  
   f. `Model.create` – packs all mutable state refs plus freshly created
      `Notty_scroll_box`.  

   All heavy work (file IO, embedding look-ups) already happened during this
   step so the UI can show the first frame instantaneously.

6. Notty integration: `Notty_eio.Term.run` spawns the low-level terminal
   backend which calls an *on_event* callback in **a separate fibre**.  That
   callback just enqueues the received `Notty.Unescape.event` or resize marker
   into `ev_stream` and returns, guaranteeing that raw key polling never
   blocks the renderer.

7. Submission path in detail

   • User hits Meta-Enter → `Controller.Submit_input` reaction.  
   • `apply_local_submit_effects` (main fibre):
       – strip & read current draft  
       – append `Add_user_message` patch  
       – reset draft buffer / cursor  
       – enable auto-follow and scroll to bottom  
       – insert UI placeholder “(thinking…)”.  
   • A new fibre is forked under a fresh `streaming_sw` switch and stored in
     `Model.fetch_sw` so it can be cancelled later.  Inside that fibre
     `handle_submit` calls
     `Chat_response.Driver.run_completion_stream_in_memory_v1` with callbacks
     `on_event` / `on_fn_out` that forward every SSE chunk into a *second*
     internal `stream` queue.  Another helper daemon folds that queue into
     either single `Stream` or aggregated `Stream_batch` messages and enqueues
     them onto `ev_stream`.  Batching improves rendering performance on
     high-latency terminals without compromising interactivity.

   • When the Driver completes it enqueues a `Replace_history` with the full
     authoritative history list so the model cannot drift from the backend.

8. Main event loop

   Pseudocode (exactly mirrors the `match` in app.ml):

   ```text
   loop():
     ev ← Stream.take ev_stream
     match ev with
       | #Notty.Unescape.event → Controller.handle_key …
       | `Resize | `Redraw     → redraw()
       | `Stream s             → apply_patches(handle_event s); maybe add item; redraw()
       | `Stream_batch lst     → iterate; redraw()
       | `Function_output out  → apply_patches(handle_fn_out out); redraw()
       | `Replace_history his  → Model.set_history_items …; redraw()
   ```

   The “maybe add item” cases correspond to `Output_item_done` and similar end
   markers which append the finished assistant/tool/reasoning block to
   `history_items` so it persists.

9. Cancellation & error handling

   • ESC press → `Cancel_or_quit`.  If a fetch is active `Switch.fail sw
     Cancelled` aborts the HTTP request; otherwise the UI quits.  
   • Streaming errors or exceptions in the background fibre enqueue a red
     placeholder `(error)` message via `add_placeholder_stream_error`.

10. Shutdown & persistence

   After the event loop exits `run_chat` calls
   `Persistence.persist_session` through a `Persist_session` command unless
   the user quit via ESC and refused the “export conversation?” prompt.  The
   autosave logic thus lives entirely outside the interactive phase.

11. Redraw strategy

   `redraw()` grabs `(w,h)` from `Notty_eio.Term.size`, calls
   `Renderer.render_full`, writes the image and cursor, nothing else.  The
   function is cheap enough to be called after *every* state change; scroll
   calculations are handled by the scroll-box object.

Hot-reload: on `Resize` or manual `Redraw` events the same function is invoked
without touching any model fields, ensuring the UI instantly adapts to the
new window size.

A rough mental model is therefore “one big imperative loop around an
otherwise purely functional core”; by funnelling every external stimulus into
`ev_stream` the code keeps thread-safety without relying on locks.

The loop exits when the user chooses to quit; on shutdown `persistence.ml`
is invoked (optionally after confirmation when quitting with ESC) so that
all newly received messages are written back to disk.

---

## Runtime Flow (Text → Assistant Response)

1. User types in the composer; `Controller` updates `Model.input_line`.  
2. On Meta-Enter → `Submit_input` is emitted.  The main loop
   – queues a placeholder “(thinking…)” assistant message,  
   – empties the draft editor, and  
   – spawns a background fibre that runs `Chat_response.Driver.run_completion_stream_in_memory_v1`.
3. The driver turns the static prompt+history into an HTTP request and feeds
   each SSE chunk back into the TUI via the shared event queue (`Stream` /
   `Function_output`).
4. Each event is turned into a list of patches via `Stream.handle_event`; the
   model is updated and the screen redrawn incrementally – tokens appear as
   they arrive.
5. Completed items are appended to both `Model.history_items` and the visible
   `messages` list so the state is self-contained when exported.
6. Once the stream finishes (or the user aborts with ESC) a `Persist_session`
   command is enqueued; `Cmd.run` calls `Persistence.persist_session` which
   updates the original prompt file and writes side-car JSON blobs for
   function calls.

---

## Concurrency Model

The UI runs in a single **main fibre** responsible for key polling and
rendering.  Everything that touches the network or filesystem runs in very
short-lived **auxiliary fibres** created off the current `Eio.Switch.t`:
• HTTP streaming fibre (per submit) – cancelled by `Switch.fail` on ESC.  
• Persistence fibre – spawned by `Cmd.run`.  

The shared event queue serialises all incoming messages so the main loop is
never accessed concurrently, eliminating races despite the mutable model.

---

## Planned Refactors (tracked by design docs)

• *command_mode_plan.md* – introduces a vim-style command bar and refines
  the controller.  
• Incremental migration to an **immutable** `Model.t` with diff-patch
  updates (already prepared by `Types.patch`).  
• Path completion & raw-XML draft helpers described in *path_completion.md*.  
• More `Types.cmd` variants for streaming control & tool invocation.

---

## Guidelines

• Keep **side-effects** out of controller/renderer/stream – those must stay
  unit-testable.  
• Use `Types.patch` + `Model.apply_patch{,es}` for *all* state changes so
  we can later swap the in-place mutability for pure copies.  
• All public modules must have an `.mli`; helpers that should not leak are
  `.ml` only.  
• Respect Jane Street style (no polymorphic compare, explicit `Module.compare`).

---

This document should be updated whenever the Chat-TUI architecture changes
or new modules are added.  Think of it as the “map” that lets future
contributors navigate the UI code quickly.

