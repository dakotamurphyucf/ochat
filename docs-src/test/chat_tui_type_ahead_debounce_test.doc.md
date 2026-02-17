# `chat_tui_type_ahead_debounce_test` — expect tests for reducer policies (async)

This module runs a real `Chat_tui.App_reducer` loop under Eio and validates
type-ahead behaviour that depends on the reducer’s concurrency/cancellation
policy:

- `Typeahead_done` applies only when:
  - the op id matches the current `App_runtime.typeahead_op`, and
  - the completion snapshot (`generation`, `base_input`, `base_cursor`) still
    matches the current editor state.
- Stale op ids and stale snapshots are ignored, and the reducer still clears the
  running `typeahead_op`.
- Cursor-only movement in Insert mode clears any existing completion and closes
  the preview popup (so suggestions cannot “stick” across cursor motion).

