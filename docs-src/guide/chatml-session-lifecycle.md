# Session lifecycle and workflow recovery

Persisted records let a workflow recover its identity and results. They do not
preserve live processes, closures or a suspended program counter. Design recovery
around retained jobs, subscriptions, receipts and moderator checkpoints, using
the [background contract](chatml-authoring-background.md) and the
[child-session tools](chatml-authoring-children.md).

## Distinguish the lifecycle changes

| Change | What a workflow should expect |
|---|---|
| Runtime unload/reload | Restore captured configuration and retained state; runtime initialization may run again, so mutable globals are not once-only workflow records. |
| Host restart | Reconcile persisted attempts, receipts and dependencies; interrupted active execution is not silently made safe to retry. |
| Graceful stop | Let admitted active work settle; retain owned timers/subscriptions and history for a later start. |
| Cancel stop | Request cancellation and retire unfinished owned work/callbacks; preserve already committed terminal winners and history. |
| History compaction | Replace model-visible history with its compacted representation while retaining workflow and delivery records. |
| Reset/rebuild | Advance the session generation and retire its live extension work; retained historical IDs do not authorize new work. |
| Prompt upgrade | Use an explicitly selected source revision; replacing a moderator does not transfer the old source's pending events or authority. |

These are host/session operations, not extra ChatML builtins. Ordinary script
capabilities and native child-management tools expose only their selected
operations. A workflow can observe a change initiated by a user or another
authorized controller without possessing the authority to initiate it itself.

## Stop, start and restart

A stop acknowledgement means the stop was admitted. Cleanup may still be in
progress: use the stop result/status instead of treating the acknowledgement as
a join of every child and process. Replaying an old stop key cannot stop a newly
started lifetime. See `runtime.delegation.stop-helper` for modes and receipts.

Graceful stop preserves source-owned subscriptions and timers. Their deadlines
do not pause or renew just because the conversation stopped; retained timers can
become due and subscriptions can expire. A queued timer is not proof its moderator
handler has run. Cancel stop cancels unfinished owned subscriptions/timers and
retires unclaimed callbacks together with the applicable job/lifecycle changes.
An already claimed handler may instead retain an explicit interruption record.
Neither mode rewrites an already committed terminal result as a new cancellation.

Stopping a parent is also subject to its children's admitted lifetime policy.
Do not assume every independent child stops with its parent, or that a linked
subscription automatically controls the child it watches. Independent children
still retain their delegated authority/resource relationship. See
`runtime.delegation.creation` before choosing attached or independent lifetime.

After host restart, inspect retained work before launching replacements. A job
waiting on an owned pending dependency can retain that dependency and its original
deadline instead of calling the target again. Interrupted external effects may
already have happened; use application reconciliation or an explicitly safe retry.
Notifications and wakes have separate receipts, so recovering a pending wake does
not require republishing data that was already committed. Use the timer's declared
misfire policy for overdue work rather than inventing a fresh timer on every load.

## Compaction changes context, not work identity

Compaction has its own history generation; it does not itself create a new session
generation. Moderator state, jobs, subscriptions and delivery receipts are separate
from the model's retained text. A previously delivered notification can disappear
from effective context after compaction while its committed delivery remains.
Do not republish it merely because its original wording is absent from the summary.

Pending notification data can still enter history once at a valid boundary after
compaction. A saved wake can be admitted without reinserting an archived frame.
The host rejects stale delivery proposals and prepares against current state;
compaction does not reset automatic-turn budgets or grant an extra model turn.

Managed message receipts retain adopted/assigned correlation even when compaction
removes the original input text. An assistant output or idle status still does not
prove that the receipt's operation has completed. Output cursors bind the history
and compaction generation, so compaction can expire them. On cursor-expired or
snapshot-required responses, obtain a fresh bounded snapshot and reconcile stable
entry IDs; do not interpret the error as an empty successful read. A receipt cannot
restore output removed by retention.

Authoring references also track effective context. A remembered topic ID is a
rediscovery pointer, not evidence that the full guide remains in the next model
request. Retrieve the needed topic again after compaction when its text is absent.
Automatic refresh follows the author's auto/manual/preload policy; manual does not
silently become automatic. See `authoring.reference` for retrieval and continuation.

## Reset and rebuild retire a generation

Reset removes live jobs, schedules, invocations, subscriptions, ingress registrations
and deliveries, and resets moderator state for the new generation. With `keep_history`, old
notification entries remain historical data. They neither schedule work nor restore
the authority behind their IDs. Without retained history, current history returns
to the initial prompt; rebuild prepares a fresh history using the selected prompt.

The reset option `keep_tasks` refers to the conversation's task/KV data, not the
extension job/subscription tables. It cannot preserve an old job's live authority.
Managed submission reconciliation retains terminal receipt identities and marks
unfinished old-generation receipts invalidated. Reusing a pre-reset send key does
not silently submit the message again. Start a new intended operation with a new
key only after inspecting the old receipt and current session state.

Administrative archives can retain the pre-change state, including results and
delivery receipts. An authorized export of that retained revision can inspect it;
an artifact descriptor still requires authorized access to materialize. Archives
are history, not a way for the new moderator to claim the old generation's work.
Retention policy may later remove historical data, so do not promise permanent
recovery merely because a receipt or ID was once returned.

## Source replacement is not workflow migration

Editing a file does not rewrite a session's captured source. A prompt upgrade
selects an explicit revision through authorized stopped-session administration.
Replacing existing moderator state requires the host's explicit migration approval;
that approval does not implement an application-specific state migration. The
replacement initializes its own state, and the upgrade archive retains the old
checkpoint and queue.

A terminal job whose publisher or selected dependencies are no longer valid can
retain its result while delivery is discarded for authority change. The result
is not delivered to an unrelated replacement moderator. A timer already queued
under the old source stays in the old archive; a not-yet-enqueued timer fails its
source admission when due. Its historical delivered marker means the event was
enqueued, not that the replacement handled it. Existing subscriptions keep their
original deadlines, and obsolete ingress registrations cannot submit into the
replacement source.

If a handler failed or was interrupted after an external effect, source replacement
is not permission to replay that effect. Reconcile the retained result/failure and
establish an explicitly authorized new workflow. Preserve source/generation/epoch
checks in application polling logic rather than accepting an ID alone.

The offline suites cover [reset/rebuild and historical artifacts](../../test/chatml_composition/notification_administration_tests.ml),
[compaction and delivery](../../test/agent_session/notification_compaction_tests.ml),
[submission receipt recovery](../../test/agent_session/managed_submission_recovery_tests.ml),
[moderator replacement](../../test/chatml_composition/moderator_upgrade_tests.ml), and
[old timers/ingress after upgrade](../../test/chatml_composition/timer_upgrade_tests.ml).
These contracts are qualified at their implemented host boundaries; none implies
exactly-once execution of an arbitrary shell command or external API.
