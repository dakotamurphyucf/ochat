# `Chat_tui.App_compaction` — start history compaction in the background

`Chat_tui.App_compaction` starts semantic history compaction via
`Context_compaction.Compactor.compact_history` and reports results back to
the reducer using `internal_event` messages.

Compaction is triggered by the controller action bound to “compact context”.

## Behaviour

When started, the module:

1. snapshots the current history from `runtime.model`,
2. injects a `(compacting…)` placeholder and requests a redraw,
3. optionally saves a session snapshot if a `~session` is active, and
4. runs `compact_history` in a worker fibre, emitting:
   - `Compaction_started`, then
   - `Compaction_done` with the compacted history, or
   - `Compaction_error` on failure/cancellation.

The reducer is responsible for installing the new history into the model.

