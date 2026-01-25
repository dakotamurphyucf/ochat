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

