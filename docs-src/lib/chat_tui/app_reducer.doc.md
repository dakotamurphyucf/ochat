# `Chat_tui.App_reducer` — main event loop and concurrency policy

`Chat_tui.App_reducer` contains the main event loop for the terminal UI.
It consumes:

- terminal `input_event`s (keypresses, paste events), and
- `internal_event`s from background fibres (streaming, compaction, redraw).

The loop mutates the shared `Model.t` and requests redraws through
`Redraw_throttle`.

## Concurrency policy (important)

The reducer enforces a simple invariant:

- at most one operation is active at a time (`Streaming` or `Compacting`)

Additional user actions are queued (`App_runtime.pending`) and started in
FIFO order once the current operation finishes.

## Cancellation and quit

`Esc` is interpreted as:

- cancel when streaming/compaction is active (or starting), and
- quit when idle (in which case `run` returns `true` so shutdown logic can
  prompt about exporting the conversation).

Tool output and stream events are tagged with an operation id so the reducer
can ignore stale events that arrive after cancellation.

## Type-ahead completion (debounced background work)

In addition to the main `op` (streaming/compaction), the reducer tracks a
separate `App_runtime.typeahead_op` for background type-ahead completion.

Key properties:

- It is **independent** of streaming/compaction and may run while an assistant
  response is streaming.
- It is **debounced** after input edits (currently ~200ms) to avoid spamming
  the provider while the user is typing.
- Results are applied only when they are still **applicable**:
  - the `op_id` must match the current `typeahead_op`, and
  - the completion snapshot (`generation`, `base_input`, `base_cursor`) must
    still match the current editor state.

Triggering behaviour (high level):

- Cursor-only motion in Insert mode clears any existing completion and schedules
  a debounced request for a fresh completion at the new cursor position (as long
  as the draft is non-empty).
- Input edits in Insert mode schedule debounced completion requests unless an
  existing completion remains relevant.
- Pressing `Ctrl+Space` in Insert mode:
  - toggles the preview when a relevant completion exists (handled in
    `Chat_tui.Controller`), and
  - when no relevant completion exists, the reducer treats it as “open preview
    and fetch now”, triggering an immediate completion request.

