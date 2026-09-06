# Chat_tui.Controller — input and page routing

Translate decoded Notty events into mutations of the UI-owned model and typed
[reactions](controller_types.doc.md). The controller itself does not perform
provider calls, persistence, or daemon mutations. Use the
[TUI guide](../../guide/chat_tui.md) for the complete user keymap.

## Overview

```ocaml
val handle_key
  :  model:Model.t
  -> term:Notty_eio.Term.t
  -> Notty.Unescape.event
  -> reaction
```

Source: [implementation](../../../lib/chat_tui/controller.ml),
[interface](../../../lib/chat_tui/controller.mli). Some older interface prose
describes intended shortcuts; the implemented ordering and the notes below
describe current behavior.

## Editor modes and dispatch

Active shell/moderator interaction and non-Chat pages route before editor keys.
Chat has four editor modes: Insert, Normal, Cmdline, and Search. Agent and
Shell Security are pages, not editor modes. Ctrl+G opens Agent only while calls
are active; switching pages does not stop execution.

Normal operations live in `Controller_normal`; search input in
`Controller_search`; command execution in
[Controller_cmdline](controller_cmdline.doc.md). Partial Normal-mode
operators/counts are cleared when opening Agent.

## Reactions

Use the complete [reaction contract](controller_types.doc.md), including history
refresh, asynchronous destination preparation, approvals, and security management.
A renderer-only loop is not a complete TUI host. Native/daemon actions must pass
through `Agent_session_client` and actor authorization, not local-model mutation.

## Insert-mode behaviour

### Text insertion and basic movement

| Keys | Current behavior |
|---|---|
| Printable unmodified ASCII or Unicode | Insert at a grapheme-aligned byte cursor |
| Enter | Insert newline |
| Left/Right | Move one extended grapheme cluster |
| Ctrl+A / Ctrl+E | Current line beginning/end; raw C0 aliases accepted |
| Ctrl+Home / Ctrl+End | Entire draft beginning/end |
| Meta+Up/Down or Shift+Up/Down | Move one visual editor row |
| Ctrl+Shift+Up/Down, preview closed | Move by visible editor page height |
| Ctrl+Shift+Left/Right, preview closed | Entire draft beginning/end |
| Meta+Shift+Left/Right | Unindent/indent current line by two spaces |

Plain and Ctrl+Up/Down scroll history, not the editor. Meta+Shift+Up/Down
duplicates the current line above/below; these events are distinct from
Meta-arrow movement. Unicode input uses grapheme-aligned byte offsets, while
layout measures terminal cells. Unmodified `ß` inserts that character.

### Word navigation

Ctrl/Meta+Left/Right and Meta+b/f perform word-wise movement. Their helper
definition is whitespace-based, not a language tokenizer.

### Deletion, kill-ring, and yank

Backspace deletes one extended grapheme cluster. Ctrl+K kills to end-of-line including its newline;
Ctrl+U kills back to line beginning; Ctrl+W or Meta+Backspace kills the previous
word; Ctrl+Y inserts the last killed text. The kill buffer is one process-global
string, not a multi-entry clipboard. At an empty deletion range the shared
helper falls back to backspace, so boundary behavior is not identical to readline.

### Selection and clipboard-like operations

Meta+v or Meta+s toggles the selection anchor.
With selection active, Ctrl+C copies and Ctrl+X cuts it into the kill buffer.
This is local editor state, not the OS clipboard. Normal-mode registers/motions
are documented in the [user keymap](../../guide/chat_tui.md#normal-mode-vim-ish-commands-over-the-draft--history-tools).

### History scrolling and auto-follow

Plain/Ctrl+Up/Down scroll history one row. PageUp/Down scroll history unless a
typeahead preview is open. Home/End request an asynchronous earlier/latest
conversation destination; Ctrl+Home/End instead move the draft cursor.
Mouse-scroll events have handlers, but stock terminal creation disables mouse
reporting. History scrolling and editor cursor movement are separate.

### Draft mode and submission

Insert Ctrl+R toggles Plain/Raw XML. It accepts lowercase/uppercase Ctrl events
and raw DC2 (`0x12`); actual Notty terminal decoding yields uppercase Ctrl+R.
Normal Ctrl+R is redo instead. Normal bare `r` toggles draft mode.

Meta+Enter submits from Insert; Enter also submits from Normal; `:w` is the
command-line equivalent. Submission interpretation belongs to the host.
Raw XML converts a user message, not arbitrary tool/transcript records.
`:e` copies a canonical row's display text into **Plain** Insert mode.

### Type-ahead completion (Insert mode)

The shared [UI adapter/coordinator](type_ahead_provider.doc.md) handles manual
and automatic requests in all hosts when enabled and eligible. Ctrl+Space with
no relevant completion requests immediately and opens the preview on arrival;
with an existing completion it toggles the preview. Ctrl+@ and NUL are aliases.
Controller-only use does not start model requests.

- Tab accepts all remaining text; Shift+Tab accepts one logical line including
  its newline and retains any remainder. Both close the preview.
- Each acceptance saves a draft/cursor undo snapshot and clears redo history.
  Acceptance itself never schedules another request.
- Preview Ctrl+Shift+Up/Down scrolls one line; PageUp/Down five lines;
  Ctrl+Shift+Left/Right jumps to its beginning/end.
- The popup is at most ten rows, constrained by the history viewport. Long
  lines are cropped, not wrapped.
- Edits/cursor changes invalidate relevance. Full-app lifecycle checks also
  invalidate work on history, attachment, permission, and connection changes.
- Manual and automatic requests show an independent `[suggesting]` status.
  Both application adapters request redraw when this status changes.

### Escape, cancel, and quit

Bare Insert Escape closes the preview, then dismisses a relevant suggestion,
then switches to Normal. Closing a still-pending preview alone need not cancel
its request; dismissal/leaving eligibility does. Bare Normal Escape first
clears any Visual selection and pending command/count, remains Normal and
returns Redraw without cancelling work. Without a selection, Normal Escape
requests cancel-or-quit; the host decides from active work/permissions.

In Insert, Ctrl+C copies an active selection or otherwise requests quit.
Bare `q` is caught by printable insertion and types a letter. Prefer `:q`
or `:quit` for explicit exit. `:wq` quits without submitting first.
On Agent/Shell Security, Escape returns to Chat when no dialog owns input.

## Manual verification checklist (type-ahead UX / key encoding)

Use the [recorded fixture/live verification](../../development/typeahead-verification.md)
and its repeatable checklist. Verify actual Notty events with
`dune exec bin/key_dump.exe --`; a synthetic lowercase event is not sufficient
evidence for a real Ctrl-key binding. Check preview bounds, acceptance,
undo/redo, independent status, pending cancellation, reconnect, and terminal
restoration. Automated fixtures do not prove every terminal emulator behaves alike.

## Example: wiring handle_key into an event loop

For a real integration follow [App](app.doc.md),
[App_reducer](app_reducer.doc.md), and [agent embedding](../../agent-server/embedding.md).
Handle every reaction and own worker cancellation/redraws explicitly. Do not
copy an old six-constructor toy loop or invoke nonexistent `Renderer.draw`.

## Known issues and limitations

`Model.with_edit_checkpoint` wraps controller dispatch: a changed draft gets
one text/cursor snapshot unless the action already managed its own undo stack.
This covers ordinary Insert edits without double-recording typeahead acceptance
or Normal edits. New edits invalidate redo; undo/redo clears stale selections.
It is not transcript undo or tool-effect rollback. Byte offsets remain the
storage representation, clamped to grapheme boundaries by `Utf8_edit`.

Native/daemon `:delete` sends an actor-authorized history mutation. Home/End
and search destinations use [background layout](agent_history_layout.doc.md).
Compaction is available through `:compact`/`:cmp`/`:c`; there is no F2 binding.

## Related modules

- [Model](model.doc.md), [normal controller](controller_normal.doc.md)
- [Command-line controller](controller_cmdline.doc.md)
- [Reaction types](controller_types.doc.md)
- [Shell controller](controller_shell_security.doc.md)
- [Typeahead provider and lifecycle](type_ahead_provider.doc.md)
- [Renderer](renderer.doc.md)
