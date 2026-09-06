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

The reducer delegates to the same `Type_ahead_ui` and `Type_ahead_controller`
used by `App.Agent_mode`. Suggestions default off, remain independent of the
foreground turn, and never mutate session history. The coordinator replaces
pending work, joins cancellation, and reports immutable snapshots to the UI.
See [configuration, privacy and lifecycle](type_ahead_provider.doc.md).

## Shell events and UI ownership

The reducer is the only owner that applies shell approval changes, management
refreshes, audit pages, grant revocations, and Shell Security snapshot updates
to the model. Blocking I/O runs in Eio workers and reports generation-tagged
events. Stale results cannot overwrite newer state.

Approval responses never enter the normal submit/history path. Grant
revocation updates typed session state and appends a management audit event.
