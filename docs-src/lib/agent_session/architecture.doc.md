# agent_session

Single-writer session actors, immutable prompt/workspace revisions, runtime builders, workers, subscriptions, quotas and transitions. Commit state through the actor; keep blocking work outside it.

An unexpected request-handler exception terminates the actor. Its in-flight
caller receives the original exception and backtrace, while queued and later
requests receive `Server_shutting_down`. Cancellation of the actor owner follows
the same termination boundary; cancelling an individual waiting caller does not
terminate the actor. Owner exit closes the mailbox and retires process-local
execution proofs, including retained initialization proofs. This cleanup does
not persist new state or run effectful shutdown callbacks. A publication failure
after a successful commit leaves that commit intact: an exceptional reply is not
rollback evidence or permission to replay the request. Ordinary `shutdown`
retains its existing cleanup and return behavior.

Actual callback cleanup has an opaque factual receipt in `Callback_cleanup`.
A receipt preserves the pre-finish producer-phase outcome separately from the
original releasing invocation, lexical finish reply and independent capture
failure. Producer work includes claim/setup/refresh and may fail before user code
enters; Stop or bookkeeping release is not producer exit. Only the same actual
handler invocation's projected reply is coalesced. Other failures retain their
original exceptions and backtraces even when exception objects match.

Init and foreground Operation parents register a callback only after its actual
claim ACK installs, before publication. A publication exception can therefore
leave a real acquired claim even without a successful reply. The optional
`capture_cleanup` on the common Actor claim transport runs outside the mailbox,
before native setup and refresh, with the acquired finish bracket in place.
It must not yield. This transport also serves custom Job claim callers without
creating another Job parent. Rejected staging produces no acquired receipt.
These producer wrappers require the creating domain before acquisition;
general Actor RPC transport remains unchanged.

Custom workers obtain their exact original Operation receipt from
`Operation_worker.Capabilities.callback_cleanup` before execution; no later lookup
of the current operation slot reconstructs it. Actual serialized Init End and
operation terminal entry close callback registration. Operation observations
remain pending until the actual terminal handler returns, whose raw result stays
separate from normalized worker cancellation/failure. Synchronous domain-checked
snapshots survive expiry, replacement and Actor death. Successful cleanup can
compact parent references even after a handled producer error; an individually
retained Completion still preserves its primary. Pending or cleanup-negative
facts remain. These receipts do not execute cleanup or certify whole runtime,
source or Owner retirement; a host reporting those lifetimes must retain and
consume the actual receipts at its own boundary.

First-party Factory initializers retain the actual Init parent and the full
original End result on their original source, administration or lease cleanup
holder. Standalone cold loading has its own original constructor cleanup bracket;
`Runtime_owner.ensure_loaded` and `ensure_loaded_authenticated` can capture that
exact holder before work. A loaded request with no Init has no invented callback
duties. The advanced Factory constructor receives an opaque initialization
capture port for this invocation; the port grants no permission or new receipt.

An acknowledged install records the same binding before fallible baseline
release. A constructor primary and independent callback/End cleanup failures stay
separate, including an abort before any Product returns. Repeated nested views
coalesce only the same actual Completion or original End boundary, preserving
distinct failures even when exception objects match. Original End failures are
immutable observations; recovery does not replay End or the initializer. Physical
row/resource repair cannot erase an independent original cleanup diagnostic.

The server records stable original callback and End diagnostic segments on that
same initialization boundary. Its original End result is retained before the
read-only diagnostic observation. An unexplained original finish await without
an actual handler result remains incomplete, even if later release settles current
custody. Normal live child cleanup is different: fresh immutable server views can
include the child's later original outcome without changing an earlier view or
replaying the child. Diagnostic completeness alone grants no release permission.

Server disposal consumes these exact receipts alongside the actual producer,
physical-resource, factual and original End duties. A positive runtime disposal
report can therefore coexist with retained callback diagnostics; reported reads
do not delete those failures or change earlier snapshots. Missing or unfinished
duties keep custody retained. A callback receipt alone does not certify whole
session disposal: the enclosing owner must also finish its remaining cleanup
stages, including release of the Actor, writer and Handle. Cleanup does not replay the
callback, initializer or original End invocation.

A Product binding stores one actual reported closer. Advanced Factory bindings
use `Runtime_builder.Product.Binding.attach_reported`; existing `attach` adapts
its single raw closer to the same implementation. `Attachment.close_reported`
invokes that closer once, while the attachment runtime's `close` resumes its
exact observed return or exception. Actual Cell close callers joining the same
in-flight cleanup receive the same diagnostic carriers. A separately executed
retained retry owns a new producer, even when its failure payload is identical.
Known independent physical, constructor and common cleanup failures remain in
actual order before the legacy first-error projection.

`Cleanup_diagnostic` and `Close_observation` expose abstract immutable report
data, including validated primary membership. Their constructors grant no
custody, admission or disposal authority. The creating-domain checked legacy
delivery operation is a presentation effect separate from reported payloads;
reported reads and raw resumption do not consume that delivery. These attachment
receipts alone do not integrate whole runtime/Registry error-origin transport.

Each actual Product attachment retains its original foreground Operation receipts
before the worker runs. Its read-only `Runtime_builder.Product_callbacks` observer
keeps callback facts for the same installed Manager, registration state and raw
terminal outcome separate. Stable Completion/slot and terminal diagnostic carriers
survive later operations and physical bindings. A parent compacts only after its
registration closes, callback cleanup is positive and its actual raw terminal
handler succeeds; a handled user failure alone does not retain successful history.
Frozen snapshots remain unchanged, and refreshing a selected Operation never
selects a newer operation from the Product. Enrollment is a private trusted worker
handoff; receipt IDs and domain checks do not independently prove Actor affinity.

Installed idle, resume and queue adapters forward the current invocation's optional
Completion capture through the actual Actor claim bracket. They do not retain a
first caller's capture for later calls. Hosts associate those no-operation claims
with the actual invocation lifetime; foreground claims already have their original
Operation parent. Init/End observations remain at their constructor boundary.
Custom workers and Job hosts retain their explicit receipt responsibilities; a
runtime with no actual callback has no invented callback duty.

Ordinary Turn admission uses typed preparation, acknowledged installation and
suffix stages within the same Actor request. The existing command audit, input
ownership, writer-attempt marker, acknowledgement and worker ordering stay in
that handler. `Turn_admission_staging` records those phases; it does not certify a
Store write. `Preparation_tracking` shares actual revision, closure and constructor
subset checks without granting execution authority. `Run_preparation` keeps its
required original principal. Ordinary delegated loading has separate
`Delegated_runtime_load` custody and still requires the actual Factory/Actor/Store
association, current source, selection and initialization checks.

`Session_actor.observe_resource_state` captures the original accepted State
through the Actor mailbox. Its request-local observation keeps that snapshot
and can check the same Actor from its creating domain without yielding. A
successful check returns the current accepted State after comparing the session,
configuration and source identity, initialization, lease and Stop fields.
Unrelated metadata and usage commits can pass. A lifecycle reservation, runtime
retirement or closed Actor blocks the check. A pending write can still install
after a check against the previously accepted State.

This observation supplies facts for source-aware admission. The host still checks
its current Actor association and physical source, uses the returned State for its
source and policy checks, and retains the original guards on each effect.

`Administration` constructs detached reset/rebuild/upgrade candidates. Commit
prepared candidates through `Session_actor.commit_administration`, which rechecks
authority/revision/lifecycle and retains the previous state before replacement.
See [archive storage](compaction_archive.doc.md) and
[operator initializer restrictions](../../agent-server/operations.md#reset-rebuild-and-upgrade-initializers).

The optional `recovery_windows` state field stores ordered context-window
membership and physical source coordinates. Each ledger has one host-qualified
owner and a single open window at the end of its chain. A sealed window locates
each canonical member in a checksummed summary archive by exact reference and
source ordinal. Live locations are checked against the selected occurrence and
content revision in canonical state. Archive bytes still require an
authorized, fully verified read. A citation and its membership never confer
read authority.

The named-field codec preserves unknown window, member and source fields.
A change that would move an unknown field to a different source kind fails
with an extension conflict. Enabled documents mark the required recovery
semantics, and the absent field keeps legacy and default-off sessions on their
existing path. The codec admits up to 64 windows, 8,192 members and 2 MiB of
encoded ledger data including retained fields. This storage contract carries
canonical evidence. Actor transitions, logical
visibility and admission of exact effective model-input sources are separate
runtime capabilities. [Ledger interface](../../../lib/agent_session/recovery_window_ledger.mli),
[local context recovery design](../../design/local-context-recovery.md).

Original source access has its own retained directory owner, shared revocable
catalog policy and runtime loan. Captured prompt revisions remain immutable when
that access is unavailable or revoked. See [original source ownership](../../../lib/agent_session/original_sources.md)
for admission, reload and borrower lifetimes.

Nested authored resources capture and select each child's delegated revision once
before native setup. `Authored_source_preparation` retains that immutable pair;
recursive planning and resource construction use the same parser/revision owner.
The caller lends one startup deadline and text/graph allowance across the whole
graph. The prepared source retains no startup context or live native authority.

See [embedding](../../agent-server/embedding.md), [protocol](../../agent-server/protocol.md)
and the [implementation specification](../../design/ochat-agent-server-implementation-spec.md).
Public interfaces are the exact callable API; the table below is the complete
module inventory for this library. Follow each interface for preconditions,
resource ownership, return types and cancellation/error behavior.

| Module | Interface | Implementation |
|---|---|---|
| `active_calls` | [contract](../../../lib/agent_session/active_calls.mli) | [source](../../../lib/agent_session/active_calls.ml) |
| `administration` | [contract](../../../lib/agent_session/administration.mli) | [source](../../../lib/agent_session/administration.ml) |
| `authored_source_preparation` | [contract](../../../lib/agent_session/authored_source_preparation.mli) | [source](../../../lib/agent_session/authored_source_preparation.ml) |
| `callback_cleanup` | [contract](../../../lib/agent_session/callback_cleanup.mli) | [source](../../../lib/agent_session/callback_cleanup.ml) |
| `callback_cleanup_data` | [contract](../../../lib/agent_session/callback_cleanup_data.mli) | [source](../../../lib/agent_session/callback_cleanup_data.ml) |
| `product_callback_data` (private) | [contract](../../../lib/agent_session/product_callback_data.mli) | [source](../../../lib/agent_session/product_callback_data.ml) |
| `cleanup_diagnostic` | [contract](../../../lib/agent_session/cleanup_diagnostic.mli) | [source](../../../lib/agent_session/cleanup_diagnostic.ml) |
| `close_observation` | [contract](../../../lib/agent_session/close_observation.mli) | [source](../../../lib/agent_session/close_observation.ml) |
| `chatmd_export` | [contract](../../../lib/agent_session/chatmd_export.mli) | [source](../../../lib/agent_session/chatmd_export.ml) |
| `compaction_archive` | [contract](../../../lib/agent_session/compaction_archive.mli) | [source](../../../lib/agent_session/compaction_archive.ml) |
| `delegated_runtime_load` | [contract](../../../lib/agent_session/delegated_runtime_load.mli) | [source](../../../lib/agent_session/delegated_runtime_load.ml) |
| `durable_event_log` | [contract](../../../lib/agent_session/durable_event_log.mli) | [source](../../../lib/agent_session/durable_event_log.ml) |
| `history_codec` | [contract](../../../lib/agent_session/history_codec.mli) | [source](../../../lib/agent_session/history_codec.ml) |
| `history_id_source` | [contract](../../../lib/agent_session/history_id_source.mli) | [source](../../../lib/agent_session/history_id_source.ml) |
| `live_event_buffer` | [contract](../../../lib/agent_session/live_event_buffer.mli) | [source](../../../lib/agent_session/live_event_buffer.ml) |
| `mailbox` | [contract](../../../lib/agent_session/mailbox.mli) | [source](../../../lib/agent_session/mailbox.ml) |
| `memory_backend` | [contract](../../../lib/agent_session/memory_backend.mli) | [source](../../../lib/agent_session/memory_backend.ml) |
| `operation_worker` | [contract](../../../lib/agent_session/operation_worker.mli) | [source](../../../lib/agent_session/operation_worker.ml) |
| `permission_policy` | [contract](../../../lib/agent_session/permission_policy.mli) | [source](../../../lib/agent_session/permission_policy.ml) |
| `permission_reviewer` | [contract](../../../lib/agent_session/permission_reviewer.mli) | [source](../../../lib/agent_session/permission_reviewer.ml) |
| `preparation_tracking` | [contract](../../../lib/agent_session/preparation_tracking.mli) | [source](../../../lib/agent_session/preparation_tracking.ml) |
| `prompt_catalog` | [contract](../../../lib/agent_session/prompt_catalog.mli) | [source](../../../lib/agent_session/prompt_catalog.ml) |
| `prompt_definition` | [contract](../../../lib/agent_session/prompt_definition.mli) | [source](../../../lib/agent_session/prompt_definition.ml) |
| `prompt_revision` | [contract](../../../lib/agent_session/prompt_revision.mli) | [source](../../../lib/agent_session/prompt_revision.ml) |
| `prompt_revision_builder` | [contract](../../../lib/agent_session/prompt_revision_builder.mli) | [source](../../../lib/agent_session/prompt_revision_builder.ml) |
| `prompt_source_lease` | [contract](../../../lib/agent_session/prompt_source_lease.mli) | [source](../../../lib/agent_session/prompt_source_lease.ml) |
| `prompt_source_owner` | [contract](../../../lib/agent_session/prompt_source_owner.mli) | [source](../../../lib/agent_session/prompt_source_owner.ml) |
| `quota_key` | [contract](../../../lib/agent_session/quota_key.mli) | [source](../../../lib/agent_session/quota_key.ml) |
| `quota_manager` | [contract](../../../lib/agent_session/quota_manager.mli) | [source](../../../lib/agent_session/quota_manager.ml) |
| `runtime_builder` | [contract](../../../lib/agent_session/runtime_builder.mli) | [source](../../../lib/agent_session/runtime_builder.ml) |
| `runtime_paths` | [contract](../../../lib/agent_session/runtime_paths.mli) | [source](../../../lib/agent_session/runtime_paths.ml) |
| `security_grant` | [contract](../../../lib/agent_session/security_grant.mli) | [source](../../../lib/agent_session/security_grant.ml) |
| `session_actor` | [contract](../../../lib/agent_session/session_actor.mli) | [source](../../../lib/agent_session/session_actor.ml) |
| `session_delta` | [contract](../../../lib/agent_session/session_delta.mli) | [source](../../../lib/agent_session/session_delta.ml) |
| `session_persistence` | [contract](../../../lib/agent_session/session_persistence.mli) | [source](../../../lib/agent_session/session_persistence.ml) |
| `session_state` | [contract](../../../lib/agent_session/session_state.mli) | [source](../../../lib/agent_session/session_state.ml) |
| `session_transition` | [contract](../../../lib/agent_session/session_transition.mli) | [source](../../../lib/agent_session/session_transition.ml) |
| `start_queue` | [contract](../../../lib/agent_session/start_queue.mli) | [source](../../../lib/agent_session/start_queue.ml) |
| `subscriber` | [contract](../../../lib/agent_session/subscriber.mli) | [source](../../../lib/agent_session/subscriber.ml) |
| `turn_admission_staging` | [contract](../../../lib/agent_session/turn_admission_staging.mli) | [source](../../../lib/agent_session/turn_admission_staging.ml) |
| `turn_worker` | [contract](../../../lib/agent_session/turn_worker.mli) | [source](../../../lib/agent_session/turn_worker.ml) |
| `workspace_catalog` | [contract](../../../lib/agent_session/workspace_catalog.mli) | [source](../../../lib/agent_session/workspace_catalog.ml) |
| `workspace_cleanup` | [contract](../../../lib/agent_session/workspace_cleanup.mli) | [source](../../../lib/agent_session/workspace_cleanup.ml) |
| `workspace_definition` | [contract](../../../lib/agent_session/workspace_definition.mli) | [source](../../../lib/agent_session/workspace_definition.ml) |
| `workspace_instance` | [contract](../../../lib/agent_session/workspace_instance.mli) | [source](../../../lib/agent_session/workspace_instance.ml) |
| `workspace_lease` | [contract](../../../lib/agent_session/workspace_lease.mli) | [source](../../../lib/agent_session/workspace_lease.ml) |
| `workspace_resolver` | [contract](../../../lib/agent_session/workspace_resolver.mli) | [source](../../../lib/agent_session/workspace_resolver.ml) |
| `extension_invariants` | [contract](../../../lib/agent_session/extension_invariants.mli) | [source](../../../lib/agent_session/extension_invariants.ml) |
