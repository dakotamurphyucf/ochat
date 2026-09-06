# Chat_tui.Controller_cmdline

Interpret the Chat editor's colon commands. The handler mutates local UI state
and returns a [reaction](controller_types.doc.md); the host performs semantic
work. See the [user guide](../../guide/chat_tui.md#cmdline-after-typing--in-normal-mode).

## Overview

```ocaml
val handle_key_cmdline
  :  model:Model.t
  -> term:Notty_eio.Term.t
  -> Notty.Unescape.event
  -> Controller_types.reaction
```

Commands are stripped and matched case-insensitively. Only explicit aliases
below are supported, not arbitrary first-letter abbreviation.

| Command | Effect / reaction |
|---|---|
| q, quit, wq | Quit; wq does **not** submit first |
| w | Submit_input |
| c, cmp, compact | Compact_context |
| shell, security | Open Shell Security; generation-tagged management refresh |
| d, delete | Return Delete_history with the selected canonical occurrence ID |
| e, edit | Copy the selected canonical row's displayed text into Plain Insert mode |
| noh, nohlsearch | Clear search highlight; Redraw |

Delete/edit reject missing selection, moderator-projected replacements/insertions,
streaming, approval and placeholder rows. Rejection is a transient UI notice.
Edit does not replace the original entry or re-run a tool. Native/daemon deletion
uses `session.delete_history`, a writable attachment and expected revision.
Legacy deletion updates local history. Both reject active work and remove a
matching tool call/result pair together; neither reverses tool effects.

Unknown commands clear the prompt and redraw without an error. Executing a
command resets command text/cursor and returns to Normal before applying the
command-specific action. Editing a message then enters Insert.

## Function reference

`insert_text`, `backspace`, and `execute_command` implement local editing
and dispatch. Printable unmodified ASCII and Unicode, Backspace, and Left/Right
edit the command buffer at grapheme boundaries; Enter executes; Escape leaves
command mode. Command names themselves remain the explicit ASCII aliases above.

The terminal parameter is unused by this handler. Do not use it to perform
network or persistence effects inside the input reducer.

## Usage example

```ocaml
let dispatch_cmdline ~model ~term event =
  Chat_tui.Controller_cmdline.handle_key_cmdline ~model ~term event
```

The caller must interpret the complete reaction variant. See
[App](app.doc.md) and [the shared controller](controller.doc.md) rather than a
partial renderer-only event loop.

## Limitations and source

Editor offsets are grapheme-aligned byte positions. Unknown commands have no
error notice. A local model mutation is not a daemon mutation.
[Implementation](../../../lib/chat_tui/controller_cmdline.ml).
