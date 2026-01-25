# `Chat_tui.App_stream_apply` — apply streaming events to the model

`Chat_tui.App_stream_apply` centralises the “apply patches + request redraw”
logic for streaming events.

The reducer (`Chat_tui.App_reducer`) remains focused on control flow:

- start/cancel/queue operations
- dispatch event variants
- decide when to replace history and when to redraw

…while this module handles the mechanical steps of:

- translating `Response_stream.t` events into `Types.patch` lists using
  [`Chat_tui.Stream`](stream.doc.md),
- applying those patches to the model (`Model.apply_patches`), and
- requesting redraws via `Redraw_throttle`.

## Coalescing

For batched stream events (`Stream_batch`), the helper coalesces adjacent
`Append_text` patches targeting the same buffer. This reduces patch volume
without losing incremental updates.

