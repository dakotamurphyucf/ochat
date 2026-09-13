# Attached-session work overview

`Agent_work_view.of_snapshot` derives display metadata from a principal-projected
`Agent_protocol.Snapshot.t`. The TUI applies the current snapshot after each client
update and opens the overview with `:work` or `:jobs`.

Jobs show execution state separately from completion handoff. A successful job
may still await delivery, and an authority change may discard delivery while
preserving the successful result. A published Pending invocation is labelled
**Background work acknowledged**; the invocation's initial outcome is never
rewritten to infer the job's eventual outcome. Subscription, timer, moderator-event
and notification statuses have separate rows.

The projection retains IDs, fixed display labels, state strings and counts. It
does not retain arguments, output values, arbitrary error messages, free-form
progress text, or credentials. Progress sequence numbers are shown only if included
in the received job projection; this component does not poll or subscribe to an
additional progress stream. Jobs/timers from another session or generation and
extension rows from another generation are excluded. A narrowed snapshot replaces
the previous visible collection instead of merging inaccessible old entries.

`Model.update_session_work` preserves a scrolled row's ID when the list changes.
Active rows appear first; jobs are ordered by creation time with stable ID ties.
`Renderer_page_work` renders only the visible slice, and `Controller_work` supports
arrows, j/k, paging, Home/End, mouse scrolling and Escape. A new foreground turn
does not close the page. Disconnected clients label the retained state as last
known. The legacy file-backed TUI explains that it has no attached work projection.

Work updates do not invalidate Chat history layout. The existing attached-session
controller already redraws on each projection, so `Agent_event_apply.apply`
continues returning history damage only. Navigation does not submit inputs,
acknowledge results or control execution.

The offline `test/chat_tui_work_test.ml` suite runs job events through the real
client reducer and TUI projection/application/rendering path. It checks completion
retirement, acknowledgement separation, snapshot reconnect, new-turn continuity,
small viewports, draft/history preservation, stable scrolling and actual
principal-projection narrowing. Shared fixture setup lives in
`test/chat_tui_projection_support.ml`.
