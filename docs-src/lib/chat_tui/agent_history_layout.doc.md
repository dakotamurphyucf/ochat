# Agent_history_layout — native/daemon history preparation

Used by `App.run_agent_session` for both embedded-local and daemon-connected
TUI modes. Execution still belongs to the session actor; this module owns only
UI preparation. See [App](app.doc.md) and the [TUI guide](../../guide/chat_tui.md).

Initial history and uncached width changes capture immutable render snapshots
and jobs. One worker runs an aggregate render through the existing two-domain
`Chat_startup_render` machinery. At most one request executes and one newer
request waits; a new request cancels/supersedes older work. This is coalesced
aggregate preparation, not the legacy progressive corridor algorithm.

Results return to the UI event queue. Before installing caches or publishing
exact geometry, `accept` checks generation, current snapshot, width, result
count and each job/result identity. Stale results cannot change the display.
Successful publication also restores normal-input eligibility, including
typeahead; marking the layout warm alone is insufficient.
Current render failure uses the synchronous fallback. `close` cancels work and
suppresses late publication; the host switch owns worker lifetime.

Home/End and search-reveal destinations wait for exact geometry when loading;
when warm they apply immediately. Home disables auto-follow; End follows the
bottom. At a warm unchanged width, streaming updates synchronously warm only
dirty chunks, avoiding restarting whole-history preparation for every token.
Geometry publication still happens on the UI fiber; this does not promise
constant-time layout or identical performance to the legacy host.

Regression coverage lives in
[initial-render lifecycle tests](../../../test/chat_tui_initial_rendering/chat_tui_initial_render_lifecycle_test.ml):
coalesced resize, stale completion rejection, Home/End, current-width caches,
and closing before late completion. See [source](../../../lib/chat_tui/agent_history_layout.ml)
and [interface](../../../lib/chat_tui/agent_history_layout.mli).
