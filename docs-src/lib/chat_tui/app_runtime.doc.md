# `Chat_tui.App_runtime` — shared mutable state for the app reducer

`Chat_tui.App_runtime` defines the mutable runtime record that is threaded
through the main event loop in [`Chat_tui.App_reducer`](app_reducer.doc.md).

It is internal plumbing: it exists to keep `Chat_tui.App` small and to
factor the reducer, submit, streaming, and compaction logic into separate
modules.

## Key types

- `op` — the currently active operation:
  - `Streaming { sw; id }` / `Starting_streaming { id }`
  - `Compacting { sw; id }` / `Starting_compaction { id }`
- `typeahead_op` — the currently active type-ahead completion operation:
  - `Typeahead { sw; id }` / `Starting_typeahead { id }`
- `submit_request` — a snapshot of the editor at submit time:
  - `text` and `Model.draft_mode`
- `queued_action` — work queued while another operation is running:
  - `Submit of submit_request` or `Compact`

## Invariants and intended use

- At most one `op` is active at a time.
- Every started operation is tagged with a fresh integer id from
  `alloc_op_id`; the id is attached to streaming/compaction events so the
  reducer can ignore stale events.
- Cancellation during the `Starting_*` phase is recorded in
  `cancel_*_on_start` because the worker switch is not yet available.

Type-ahead completion uses the same “starting” pattern, but is tracked
independently in `typeahead_op` so it can run while streaming/compaction is
active.

This module is not intended to be consumed by applications directly.

