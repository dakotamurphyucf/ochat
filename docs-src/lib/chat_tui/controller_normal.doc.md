# Chat_tui.Controller_normal — Normal-mode draft and history commands

## Purpose and dispatch

Handle Normal-mode draft motions, selection, operators and history navigation
by mutating the UI-owned model and returning a typed reaction. No provider,
persistence or daemon calls run here.

Applications must dispatch through [Controller.handle_key](controller.doc.md),
not invoke this handler as a complete event loop. The shared controller owns
page/dialog priority, Insert entry, submission, undo/redo and cancel-or-quit.
Both legacy and native/daemon TUI runners use that shared route.

## Public API

```ocaml
val handle_key_normal
  :  model:Model.t
  -> term:Notty_eio.Term.t
  -> Notty.Unescape.event
  -> Controller_types.reaction

val cancel_pending : unit -> unit
```

`cancel_pending ()` clears partial counts/operators, `g` and find prefixes
while retaining the repeatable last-find command. Motion helpers are private,
not separately callable APIs.

## Key semantics

| Keys | Behavior through the shared controller |
|---|---|
| `h/l` | Move one extended grapheme cluster left/right, with counts |
| `j/k` | Move one visual draft row, with counts |
| `w/b/e` | Whitespace-based word motions |
| `0/^/$` | Draft line start / first nonblank / end |
| `gg/G` | First/last **draft line**; `5gg` or `5G` selects draft line five |
| `Home/End` | Earlier/latest conversation viewport destination |
| `↑/↓`, `Ctrl-f/b`, `Ctrl-d/u` | History line/page/half-page scrolling |
| `[/]` | Select previous/next displayed history row |
| `a`, `o/O` | Append or open a draft line, entering Insert |
| `v` | Toggle character-wise Visual selection |
| Bare `Esc`, selection active | Clear selection and pending command/count; remain Normal |
| `y/d/c`, selection active | Yank/delete/change selected draft text |
| `x` | Delete and register-yank the grapheme under the cursor |
| `p/P`, `yy/dd/cc` | Register paste and line operators |
| `u` / `Ctrl-r` | Draft undo/redo |
| Bare `r` | Toggle Plain/Raw XML, not Vim replace-character |
| `:`, `/ ?`, `n/N` | Command prompt, history search, repeat search |
| `Enter` | Submit draft |
| `Esc`, no selection | Return cancel-or-quit for the host to interpret |

Selection clearing neither changes draft text/cursor nor cancels active work.
A subsequent Escape without a selection follows the ordinary cancellation/quit
path. Shell dialogs and non-Chat pages retain their own Escape handling.

The [complete user keymap](../../guide/chat_tui.md) also lists supported
operator/find combinations. This is a partial Vim implementation, not a full
Vim parser; register contents are local editor state, not the OS clipboard.

## Example

Return the full reaction to the application host:

```ocaml
let dispatch ~model ~term event =
  Chat_tui.Controller.handle_key ~model ~term event
```

For a complete executable use [App](app.doc.md). A renderer-only loop cannot
correctly implement submission, authorized history deletion, background layout,
approvals or cancellation. There is no `Renderer.draw` API.

## Editing and geometry

Cursor offsets remain UTF-8 byte positions, but horizontal movement and
character deletion use `Utf8_edit` extended-grapheme boundaries.
Visual rows use terminal-cell layout. Word motions remain whitespace-based;
terminal glyph widths may differ for complex Unicode sequences.

Ordinary controller edits are checkpointed once per action. Undo restores
draft text/cursor, not transcript mutations or tool effects. New edits clear
redo. Tests for selection Escape go through the public shared controller.

Sources: [interface](../../../lib/chat_tui/controller_normal.mli),
[implementation](../../../lib/chat_tui/controller_normal.ml),
[shared-controller regression](../../../test/chat_tui_normal_mode_cursor_test.ml).
