# `Chat_tui.App_submit` — local submit effects and spawning streaming

`Chat_tui.App_submit` owns the submit-specific logic that happens when the
user hits enter:

1. Synchronous local effects (mutate `Model.t` immediately):
   - capture the draft buffer as a user message (plain text or raw XML),
   - append to the transcript/history,
   - clear the editor and scroll to bottom (including clearing any type-ahead
     completion/preview state),
   - mark assistant activity as thinking, and
   - request a redraw.
2. Spawn the streaming worker (asynchronously):
   - allocate an operation id and mark runtime as `Starting_streaming`,
   - fork a fibre that runs the OpenAI streaming request and reports back via
     `internal_stream`.

The asynchronous worker itself is supplied as a callback (`start_streaming`)
so that `Chat_tui.App` can partially apply configuration and tool runtime.

## Where it is used

- `Chat_tui.App_reducer` calls `capture_request` and `clear_editor` when a
  controller action requests submission.
- `Chat_tui.App_reducer` calls `start` when it decides a submit should begin
  (either immediately when idle, or later when drained from the pending
  queue).

## Notes on raw-XML drafts

When `Model.draft_mode = Raw_xml`, this legacy path parses ChatMD and converts
the first `<user>` message, including its inline helpers. It does not append
arbitrary tool calls or an entire transcript. A draft not starting with `<` is
wrapped in `<user>`. Malformed/no-user input becomes a recoverable local error:
canonical history remains unchanged, no turn starts, and the rejected draft and
its Plain/Raw mode are restored when the editor is empty. If a newer draft is
already present, it is retained and the notice includes the rejected text.
The same recovery applies to deferred submissions. Cancellation propagates
instead of being displayed as validation failure. Native/daemon admission uses
the actor's typed error contract.
