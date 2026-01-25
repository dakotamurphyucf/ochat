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
- submit/compaction requests (`Submit_requested`, `Compact_requested`)
- compaction lifecycle events (`Compaction_started`, `Compaction_done`,
  `Compaction_error`)

All streaming/compaction lifecycle events carry an `op_id` allocated via
`Chat_tui.App_runtime.alloc_op_id`. The reducer uses that id to ignore
events from cancelled operations.

