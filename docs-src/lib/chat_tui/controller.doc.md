# `Chat_tui.Controller` – event dispatcher & Insert-mode handler

This document complements the inline **odoc** comments in
`controller.mli` / `controller.ml`.  It answers the “How do I use it?” and
“Which shortcuts are available?” questions that tend to pop up when hacking
on the TUI or writing integration tests.

---

## 1  Purpose & Architecture

`Controller` is the *central* event handler of the terminal UI.  The main loop
receives raw [`Notty.Unescape.event`][notty-event] values from the Notty
backend and forwards each one to

```ocaml
val Chat_tui.Controller.handle_key :
  model:Model.t ->
  term:Notty_eio.Term.t ->
  Notty.Unescape.event ->
  Controller_types.reaction
```

The function inspects `model.mode` and chooses the correct key map:

| Mode           | Handler                               | Responsibility            |
|----------------|---------------------------------------|---------------------------|
| `Insert` (default) | *local implementation* (this file) | Free-form text editing     |
| `Normal`          | `Controller_normal.handle_key_normal` | Vim-like navigation        |
| `Cmdline`         | `Controller_cmdline.handle_key_cmdline` | `:` command prompt         |

All three handlers are **pure** with respect to side-effects: they only mutate
the in-memory record stored in `model` and return a `reaction` telling the
caller what to do next.

[notty-event]: https://pqwy.github.io/notty/doc/notty/Notty/Unescape/index.html#TYPEevent

---

## 2  Reactions

```ocaml
type reaction =
  | Redraw          (* Visible state changed – rerender *)
  | Submit_input    (* Meta+Enter – send prompt to assistant *)
  | Cancel_or_quit  (* ESC – cancel streaming or quit if idle *)
  | Compact_context (* Trigger context compaction request *)
  | Quit            (* Immediate termination (Ctrl-C / q) *)
  | Unhandled       (* Let caller try the next fallback *)
```

Returning `Unhandled` is not an error – it merely signals that the controller
did not care about the event.  The main loop can then fall back to
application-global shortcuts (e.g. `Ctrl-L` for hard redraw) before giving up.

---

## 3  Insert-mode key bindings

The table lists the recognised commands.  All indices refer to **bytes**
inside `Model.input_line`; the implementation is *UTF-8-agnostic* for now.

| Key(s)                              | Action                                         |
|-------------------------------------|------------------------------------------------|
| `Arrow ← / →`                       | Move caret one byte left / right               |
| `Arrow ↑ / ↓`                       | Scroll history (auto-follow off)               |
| `Ctrl-↑ / Ctrl-↓`                   | Move caret one *visual line* up / down         |
| `Home` / `End`                      | Beginning / end of current line                |
| `Ctrl-A` / `Ctrl-E` *(fallback)*    | idem – for ttys that do not report `Home/End`  |
| `Meta-← / Meta-→` <br/>`Ctrl-← / →` | Previous / next word                           |
| `Backspace`                         | Delete character to the left                   |
| `Ctrl-K`, `Ctrl-U`, `Ctrl-W`        | Kill to *EOL*, *BOL*, previous word            |
| `Ctrl-Y`                            | Yank (paste) last killed region                |
| `Meta-v`, `Meta-s`                  | Toggle selection anchor / clear selection      |
| `Ctrl-c` *(while selection active)* | Copy selection to kill-ring                    |
| `Ctrl-x` *(while selection active)* | Cut selection                                  |
| `Meta-Shift-↑ / Meta-Shift-↓`       | Duplicate current line above / below           |
| `Meta-Shift-← / Meta-Shift-→`       | Unindent / indent current line by two spaces   |
| `Page Up / Page Down`               | Scroll history by one page                     |
| `Tab` *(type-ahead)*                | Accept the full type-ahead completion          |
| `Shift-Tab` *(type-ahead)*          | Accept one line of the completion              |
| `Ctrl-Space` *(type-ahead)*         | Toggle the completion preview popup            |
| `Ctrl-Shift-↑ / Ctrl-Shift-↓` *(preview open)* | Scroll preview by one line            |
| `Page Up / Page Down` *(preview open)* | Scroll preview (fallback on terminals that can’t encode Ctrl-Shift-↑/↓) |
| `Meta-Enter`                        | Submit the draft (→ `Submit_input`)            |
| `Enter`                             | Insert literal newline into the prompt         |
| `Escape`                            | Layered (see below): close preview → dismiss completion → switch to Normal / cancel-or-quit |
| `Ctrl-C`, `q`                       | Immediate quit (→ `Quit`)                      |

Bindings were chosen to work across macOS, Linux and Windows terminal
emulators, many of which under-report modifier keys.  When both *Meta* and
*Ctrl* variants exist they perform the {i same} motion, ensuring that at
least one of them works on a given tty.

---

## Type-ahead completion (Insert mode)

When a type-ahead completion is available and *relevant* (see
`Model.typeahead_is_relevant`), Insert mode recognises additional editor keys:

- `Tab` accepts the entire completion.
- `Shift+Tab` accepts a single line of the completion (up to and including the
  first newline, when present).
- `Ctrl+Space` toggles the preview popup open/closed.

`Tab` and `Shift+Tab` always close the preview as part of accepting the
completion.

While the preview is open, the preview body can be scrolled:

- `Ctrl+Shift+Up` / `Ctrl+Shift+Down` scroll by one line.
- Fallback: `PageUp` / `PageDown` scroll the preview while it is open (useful
  on terminals that cannot encode the `Ctrl+Shift+Up/Down` combinations).

Any typing/edit that mutates `Model.input_line` closes the preview and dismisses
the current completion so it cannot linger across edits.

Note: different terminals may encode “Ctrl+Space” differently. The controller
handles both a space key with the `Ctrl` modifier and the `NUL` character
(`'\000'`) as preview toggles. Some terminals report `Ctrl+Space` as `Ctrl+@`
(i.e. `Key (ASCII '@', mods=[Ctrl])`); the controller treats that as
equivalent.

In the full application (via `Chat_tui.App_reducer`), pressing `Ctrl+Space`
when no relevant completion exists also triggers an immediate type-ahead request
so the preview can open with fresh content.

## Escape, cancel, and quit

Escape handling is intentionally layered:

- In Insert mode, a **bare** `Esc` follows this priority order:
  1. if the type-ahead preview is open: close it and return `Redraw`
  2. else if a type-ahead completion is relevant: dismiss it and return
     `Redraw`
  3. else: switch to Normal mode and return `Redraw`
- In Normal mode (and for `Esc` pressed with modifiers in Insert mode), the
  controller returns `Cancel_or_quit`. The main loop should cancel an
  in-flight request when there is one, otherwise fall back to `Quit`.

---

## Manual verification checklist (type-ahead UX / key encoding)

The available modifier combinations depend on terminal emulation and what
`Notty.Unescape` can decode. Before relying on a key combination, confirm what
your terminal delivers.

1. Run `bin/key_dump.ml` and confirm the event shapes:
   - `Shift+Tab` appears as `Key Tab mods=[Shift]`
   - `Ctrl+Space` is delivered consistently (many terminals send NUL; Notty may
     decode this as `Key (ASCII '\000', [])` or as `Key (ASCII '@', mods=[Ctrl])`)
   - `Ctrl+Shift+Up/Down` are distinguishable events (on some terminals this
     combination is not encodable; in that case use `PageUp/PageDown` while the
     preview is open)
2. Layout stability: show/hide a completion and confirm the transcript region
   does not jump (no extra rows appear/disappear).
3. Preview: open the preview (`Ctrl+Space`), then type a printable character:
   - preview closes immediately (typing closes preview)
4. Esc priority in Insert mode (bare Esc only):
   - Esc closes preview first
   - then dismisses the completion
   - only then (when nothing to dismiss) switches to Normal mode
5. Undo:
   - accept a completion (Tab or Shift+Tab)
   - switch to Normal mode
   - press `u` to undo the acceptance (draft and cursor should revert)

---

## 4  Usage example

Launching the TUI without the full `Chat_tui.App` orchestration is as simple
as wiring the controller and the renderer:

```ocaml
let rec main_loop term model =
  match Notty_eio.Term.event term with
  | None -> ()
  | Some ev ->
      (match Chat_tui.Controller.handle_key ~model ~term ev with
       | Redraw -> Renderer.draw ~model ~term; main_loop term model
       | Submit_input -> (* send draft to assistant *)
       | Cancel_or_quit -> (* cancel running request or exit *)
       | Quit -> exit 0
       | Unhandled -> main_loop term model)
```

---

## 5  Implementation notes

* **Kill-ring** – a single global `string ref` stores the most recently
  deleted region.  Multiple kill-buffers à la Emacs would be overkill here.
* **Scrolling** – history and prompt share the screen.  The controller
  computes the available viewport height dynamically using
  `Notty_eio.Term.size` and `String.split_lines`.
* **Unicode** – cursor positions are byte indices.  The limitation is
  acceptable because the terminal sends ASCII for shortcuts.  Full grapheme
  awareness may be added later via `uutf`.

---

## 6  Known limitations / future work

1. **UTF-8 granularity** — word movement and column counts operate on bytes.
2. **Undo / redo** — not yet implemented for Insert mode; Normal mode offers
   a prototype based on a simple ring buffer.
3. **Context compaction shortcut** — the dedicated *Compact_context* reaction
   is only exposed via the Normal-mode key map (currently mapped to `F2`).
   A binding for Insert mode will be added once the workflow has stabilised.
4. **Limited key coverage** — only popular shortcuts are included by default.
   Adding more is straightforward once a clear demand arises.

---


