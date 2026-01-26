# Chat_tui.Controller

Translate low-level terminal events into updates of `Chat_tui.Model.t`.

`Chat_tui.Controller` is the central **event controller** for the Ochat
terminal UI. It receives a decoded `Notty.Unescape.event`, mutates the
in-memory model accordingly, and returns a small `reaction` value telling the
caller what to do next (redraw, submit the prompt, cancel, compact context,
quit, or ignore the event).

All logic in this module is **pure with respect to IO**: it never talks to
the network, disk, or timers. Side-effects are performed by higher layers
such as `Chat_tui.App` once they have interpreted the returned reaction.

---

## Overview

The controller exposes a single public function:

```ocaml
val handle_key :
  model:Chat_tui.Model.t ->
  term:Notty_eio.Term.t ->
  Notty.Unescape.event ->
  Chat_tui.Controller.reaction
```

`handle_key ~model ~term ev` inspects `model.editor_mode` and dispatches
`ev` to one of three keymaps:

- **Insert** – free-form text editing (implemented in this module)
- **Normal** – Vim-style modal navigation and message manipulation
  (`Chat_tui.Controller_normal`)
- **Cmdline** – `:` command prompt (`Chat_tui.Controller_cmdline`)

The function mutates the `model` in place and returns a
`Chat_tui.Controller.reaction` describing the next high-level step for the
caller.

---

## Editor modes and dispatch

Editor mode is stored on the model as `Model.editor_mode`:

- `Insert` – default mode. Printable keys edit `Model.input_line` and move
  the caret. Cursor position and selections are stored in byte offsets
  (`Model.cursor_pos`, `Model.selection_anchor`).
- `Normal` – Vim-inspired command mode. Keys operate on messages, selections
  and the input buffer. See `Chat_tui.Controller_normal` for details.
- `Cmdline` – `:` prompt at the bottom of the screen. The contents live in
  `Model.cmdline` / `Model.cmdline_cursor`. See
  `Chat_tui.Controller_cmdline`.

`handle_key` simply looks at `Model.mode model` and forwards the event to the
appropriate handler. Insert-mode logic is implemented locally; Normal and
Cmdline controllers live in their own modules but share the same `reaction`
type.

---

## Reactions

The `reaction` type is defined in `Chat_tui.Controller_types` and re-exported
here:

```ocaml
type reaction =
  | Redraw
  | Submit_input
  | Cancel_or_quit
  | Compact_context
  | Quit
  | Unhandled
```

Typical responsibilities of the main loop for each case:

- `Redraw` – re-render the UI by calling `Chat_tui.Renderer.render_full` and
  pushing the resulting image and cursor position into `Notty_eio.Term`.
- `Submit_input` – assemble an OpenAI request from the draft in
  `Model.input_line`, append a pending entry to the history, clear the
  prompt, and start streaming the response (Ochat uses
  `Chat_tui.App.apply_local_submit_effects` and `Chat_tui.App.handle_submit`
  for this).
- `Cancel_or_quit` – when a request is in flight, cancel it via
  `Eio.Switch.fail` or an equivalent mechanism and return to idle. When
  idle, treat it as `Quit`.
- `Compact_context` – trigger conversation compaction via
  `Context_compaction.Compactor`, replace elided messages with the summary,
  then redraw.
- `Quit` – terminate the Notty session, release resources, and exit the
  program.
- `Unhandled` – leave the event to outer layers (e.g. a global key-binding
  handler) or ignore it.

The variant is deliberately small and closed; adding a new reaction is a
conscious API change that forces downstream code to update its pattern
matches.

---

## Insert-mode behaviour

Insert mode implements a pragmatic subset of desktop-editor shortcuts. All
operations are **in-place updates** of `Model.t` fields.

### Text insertion and basic movement

- Printable ASCII characters (`Key (`ASCII c, []` with `0x20 ≤ c`) insert
  `c` at the cursor and advance `Model.cursor_pos`.
- `Enter` (with no modifiers) inserts a literal newline into
  `Model.input_line`.
- `Left` / `Right` arrows move the caret by one byte.
- `Ctrl-A` / `Ctrl-E` move to the beginning / end of the current line.
- `Ctrl+Home` / `Ctrl+End` move to the beginning / end of the entire prompt.
- `Ctrl+Up` / `Ctrl+Down` move the caret by one **visual line** within the
  multi-line prompt while trying to preserve the visual column.

All cursor positions are stored as **byte indices** into the UTF‑8 string,
mirroring `String.get` / `String.length` from Core. See
[Known issues](#known-issues-and-limitations) for implications.

### Word navigation

Word-wise navigation is intentionally forgiving about modifier choice so it
works across terminals with slightly different keymaps:

- `Ctrl+←` / `Meta+←` – move to the beginning of the previous word
  (skipping whitespace first).
- `Ctrl+→` / `Meta+→` – move to the beginning of the next word (and skip any
  following whitespace).
- `Meta-b` – move to the beginning of the previous word.
- `Meta-f` – move to the beginning of the next word.

### Deletion, kill-ring, and yank

Insert mode maintains a small **kill buffer** (a single string) that stores
the most recent deletion made by a kill command:

- `Backspace` – delete the character before the cursor (does not touch the
  kill buffer).
- `Ctrl-K` – kill from the cursor to end-of-line (including the newline when
  present).
- `Ctrl-U` – kill from beginning-of-line to the cursor.
- `Ctrl-W` or `Meta+Backspace` – kill the previous word.
- `Ctrl-Y` – yank (re-insert) the last killed text at the cursor position.

The kill buffer is shared between these operations; there is no multi-level
kill ring.

### Selection and clipboard-like operations

The controller supports a simple selection model driven by a **selection
anchor** stored on the model (`Model.selection_anchor`):

- `Meta+v` (or Alt+`s`, often reported as the Unicode character `ß`) toggles
  the selection anchor at the current cursor position.
- When a selection is active, `Ctrl-C` copies the selected range into the
  kill buffer without modifying the prompt.
- `Ctrl-X` cuts the selected range: it copies into the kill buffer and
  removes the text from the prompt.

Selections are tracked in byte indices and may not align perfectly with
grapheme clusters for non-ASCII input.

### History scrolling and auto-follow

While in Insert mode, vertical movement keys operate on the **history
viewport** rather than recalling input history:

- `Up` / `Down` arrows scroll the history by one line.
- `PageUp` / `PageDown` scroll by a page. The effective page size is
  computed from the terminal height (via `Notty_eio.Term.size`) minus the
  number of lines occupied by the input editor.
- `Home` scrolls to the top of the history; `End` scrolls to the bottom.

All scrolling operations update the `Notty_scroll_box.t` held in the model
and disable `Model.auto_follow` so that incoming messages do not snap the
viewport back to the bottom. When the scroll position naturally reaches the
bottom again, `auto_follow` is re-enabled.

### Draft mode and submission

The input buffer can be interpreted in two draft modes (`Model.draft_mode`):

- `Plain` – regular markdown that is sent directly to the OpenAI API.
- `Raw_xml` – low-level XML used by the command palette to express tool
  invocations.

Draft mode can be toggled in both Insert and Normal modes with `Ctrl-R`. The
controller simply flips the enum on the model; it does not validate or
rewrite the buffer.

Submitting the prompt is bound to **Meta+Enter**. In Insert mode this returns
`Submit_input` without touching the model. In Normal mode, `Enter` also
returns `Submit_input` as a convenience fallback for terminals that encode
`Meta+Enter` as an `Esc` prefix sequence. The main loop is responsible for
moving the draft into the history, clearing the prompt, scheduling the
network request, and streaming the response.

### Escape, cancel, and quit

Escape handling is intentionally layered:

- In Insert mode, a **bare** `Esc` switches to Normal mode and returns
  `Redraw` so the status bar can be updated.
- In Normal mode (and for `Esc` pressed with modifiers in Insert mode), the
  controller returns `Cancel_or_quit`. The main loop should cancel an
  in-flight request when there is one, otherwise fall back to `Quit`.
- Pressing `q` or `Ctrl-C` always produces `Quit`, independent of the editor
  mode.

Other `Notty.Unescape.event` variants (`Mouse`, `Paste`, etc.) are currently
left as `Unhandled` and can be dealt with by outer layers if desired.

---

## Example: wiring handle_key into an event loop

The snippet below shows how `handle_key` can be integrated into a simplified
event handler. Error handling and OpenAI integration are omitted for
clarity.

```ocaml
open Core

let handle_event
    (model : Chat_tui.Model.t)
    (term  : Notty_eio.Term.t)
    (ev    : Notty.Unescape.event)
  : unit =
  match Chat_tui.Controller.handle_key ~model ~term ev with
  | Chat_tui.Controller.Redraw ->
      let size = Notty_eio.Term.size term in
      let image, (cx, cy) =
        Chat_tui.Renderer.render_full ~size ~model
      in
      Notty_eio.Term.image term image;
      Notty_eio.Term.cursor term (Some (cx, cy))
  | Submit_input ->
      (* Turn [model] into an OpenAI request and start streaming.
         In Ochat this is handled by [Chat_tui.App.handle_submit]. *)
      ()
  | Cancel_or_quit ->
      (* Cancel an in-flight request, or treat as [Quit] when idle. *)
      ()
  | Compact_context ->
      (* Trigger background context compaction. *)
      ()
  | Quit ->
      raise Exit
  | Unhandled ->
      ()
```

In a real application this helper would be called from the Notty event
callback used with `Notty_eio.Term.run`, alongside handling of resize events
(`` `Resize ``) that also result in a `Redraw`.

---

## Known issues and limitations

- **Byte-based editing** – cursor positions, selections, and word motions are
  expressed in bytes, not Unicode scalar values or grapheme clusters. For
  non-ASCII input, deleting or moving by "character" may cut through a
  multi-byte code point. Full Unicode-aware editing is left for a future
  iteration.

- **Terminal key encoding quirks** – the available modifier combinations are
  limited by what terminals and `Notty.Unescape.event` can represent.
  Certain key combinations may be indistinguishable or unavailable on some
  platforms. The controller favours robustness over exact Vim parity.

- **Partial Vim emulation** – Normal and Cmdline modes implement only a
  subset of Vim commands. Insert mode also provides a curated subset of
  familiar shortcuts rather than a full readline clone.

- **No mouse support in the controller** – mouse and paste events are
  currently returned as `Unhandled`. Higher layers are free to intercept and
  act on them if needed.

---

## Related modules

- `Chat_tui.Model` – mutable state of the UI, including editor mode,
  selection, scroll box, and render caches.
- `Chat_tui.Renderer` – renders a `Model.t` and terminal size into a Notty
  image plus cursor position.
- `Chat_tui.Controller_normal` – Normal-mode key handling (Vim-style
  commands).
- `Chat_tui.Controller_cmdline` – `:` command-line controller.
- `Chat_tui.App` – orchestrates the event loop, streaming, persistence, and
  mapping of `reaction` values to side-effects.
- `ochat.Notty_scroll_box` – scrolling helper that backs the history
  viewport.
- `Notty`, `Notty_eio` – terminal drawing and IO primitives used throughout
  the UI.

