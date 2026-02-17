# `Chat_tui.App_submit` — local submit effects and spawning streaming

`Chat_tui.App_submit` owns the submit-specific logic that happens when the
user hits enter:

1. Synchronous local effects (mutate `Model.t` immediately):
   - capture the draft buffer as a user message (plain text or raw XML),
   - append to the transcript/history,
   - clear the editor and scroll to bottom (including clearing any type-ahead
     completion/preview state),
   - insert a `(thinking…)` placeholder, and
   - request a redraw.
2. Spawn the streaming worker (asynchronously):
   - allocate an operation id and mark runtime as `Starting_streaming`,
   - fork a fibre that runs the OpenAI streaming request and reports back via
     `internal_stream`.

The asynchronous worker itself is supplied as a callback (`handle_submit`)
so that `Chat_tui.App` can partially apply configuration and tool runtime.

## Where it is used

- `Chat_tui.App_reducer` calls `capture_request` and `clear_editor` when a
  controller action requests submission.
- `Chat_tui.App_reducer` calls `start` when it decides a submit should begin
  (either immediately when idle, or later when drained from the pending
  queue).

## Notes on raw-XML drafts

When `Model.draft_mode = Raw_xml`, the draft is parsed with the ChatMarkdown
parser. This supports rich inputs and can produce history items that include
tool calls.

