# `Chat_tui.App_streaming` — OpenAI Responses streaming worker

`Chat_tui.App_streaming` runs a single OpenAI “Responses API” request in
streaming mode and forwards progress back to the UI via
[`Chat_tui.App_events.internal_event`](app_events.doc.md).

This module is the only part of the TUI that should need to know about:

- `Openai.Responses.Response_stream.t` events
- tool output callbacks
- batching of rapid token events

## Cancellation

The worker defines an internal `Cancelled` exception. The reducer cancels an
in-flight request by failing the streaming switch with that exception.

Cancellation is treated like any other error: the worker catches exceptions
and emits a `Streaming_error` event, and the reducer performs rollback and
UI updates.

## Batching strategy

The stream can emit token events at very high frequency. To avoid excessive
UI churn, the worker batches consecutive stream events into a `Stream_batch`
event after a small time window (default ~12ms; configurable via
`OCHAT_STREAM_BATCH_MS`).

Tool outputs are forwarded without batching.

