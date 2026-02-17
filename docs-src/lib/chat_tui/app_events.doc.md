# `Chat_tui.App_events` — event types for the TUI event loop

`Chat_tui.App_events` contains the event types exchanged between:

- the Notty terminal callback (`input_event`), and
- background fibres (`internal_event`) such as streaming, compaction, and
  redraw scheduling.

The app uses two separate `Eio.Stream.t` queues so terminal input is not
backpressured by internal traffic.

## `input_event`

`input_event` is an alias for `Notty.Unescape.event` and is fed into the
controller (`Chat_tui.Controller.handle_key`) by the reducer.

## `internal_event`

`internal_event` includes:

- `Resize` / `Redraw`
- streaming events (`Streaming_started`, `Stream`, `Stream_batch`,
  `Tool_output`, `Streaming_done`, `Streaming_error`)
- type-ahead events (`Typeahead_started`, `Typeahead_done`, `Typeahead_error`)
- submit/compaction requests (`Submit_requested`, `Compact_requested`)
- compaction lifecycle events (`Compaction_started`, `Compaction_done`,
  `Compaction_error`)

All streaming/compaction lifecycle events carry an `op_id` allocated via
`Chat_tui.App_runtime.alloc_op_id`. The reducer uses that id to ignore
events from cancelled operations.

### Type-ahead completion events

Type-ahead follows the same “lifecycle event” pattern as streaming/compaction:

- `Typeahead_started (op_id, sw)` announces the worker and publishes its
  cancellation switch.
- `Typeahead_done (op_id, payload)` reports a successful completion.
- `Typeahead_error (op_id, exn)` reports a failure (often cancellation).

The `payload : Chat_tui.App_events.typeahead_done` contains:

- `generation` — the `Model.typeahead_generation` value at request time
- `base_input` / `base_cursor` — the exact editor snapshot the completion
  applies to
- `text` — the suffix to insert at `base_cursor`

The reducer applies the completion only when the operation id matches the
current `App_runtime.typeahead_op` *and* the snapshot still matches the
current model state. This prevents stale completions from “landing” after
further edits or cursor movement.

