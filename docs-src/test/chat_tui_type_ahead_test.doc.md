# `chat_tui_type_ahead_test` — expect tests for type-ahead UX (sync)

This module contains expect tests that validate the user-visible type-ahead
behaviour without running any async background workers:

- Status bar hint text appears only when a completion is relevant.
- Inline “ghost” text is rendered at the cursor line and does not change cursor
  geometry.
- Multi-line completions do not add extra editor rows; they render an indicator
  (`… (+N more lines)`).
- The preview popup is an overlay and does not move the cursor.
- Controller key handling:
  - `Tab` accepts the whole completion.
  - `Shift+Tab` accepts one line and keeps the remainder as a new completion.
  - Typing dismisses completion and closes preview.
  - Bare `Esc` closes preview → dismisses completion → switches to Normal.

The tests directly populate `Chat_tui.Model.typeahead_completion` to avoid
network calls.

