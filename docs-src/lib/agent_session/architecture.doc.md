# agent_session

Single-writer session actors, immutable prompt/workspace revisions, runtime builders, workers, subscriptions, quotas and transitions. Commit state through the actor; keep blocking work outside it.

`Administration` constructs detached reset/rebuild/upgrade candidates. Commit
prepared candidates through `Session_actor.commit_administration`, which rechecks
authority/revision/lifecycle and retains the previous state before replacement.
See [archive storage](compaction_archive.doc.md) and
[operator initializer restrictions](../../agent-server/operations.md#reset-rebuild-and-upgrade-initializers).

See [embedding](../../agent-server/embedding.md), [protocol](../../agent-server/protocol.md)
and the [implementation specification](../../design/ochat-agent-server-implementation-spec.md).
Public interfaces are the exact callable API; the table below is the complete
module inventory for this library. Follow each interface for preconditions,
resource ownership, return types and cancellation/error behavior.

| Module | Interface | Implementation |
|---|---|---|
| `active_calls` | [contract](../../../lib/agent_session/active_calls.mli) | [source](../../../lib/agent_session/active_calls.ml) |
| `administration` | [contract](../../../lib/agent_session/administration.mli) | [source](../../../lib/agent_session/administration.ml) |
| `chatmd_export` | [contract](../../../lib/agent_session/chatmd_export.mli) | [source](../../../lib/agent_session/chatmd_export.ml) |
| `compaction_archive` | [contract](../../../lib/agent_session/compaction_archive.mli) | [source](../../../lib/agent_session/compaction_archive.ml) |
| `durable_event_log` | [contract](../../../lib/agent_session/durable_event_log.mli) | [source](../../../lib/agent_session/durable_event_log.ml) |
| `history_codec` | [contract](../../../lib/agent_session/history_codec.mli) | [source](../../../lib/agent_session/history_codec.ml) |
| `history_id_source` | [contract](../../../lib/agent_session/history_id_source.mli) | [source](../../../lib/agent_session/history_id_source.ml) |
| `live_event_buffer` | [contract](../../../lib/agent_session/live_event_buffer.mli) | [source](../../../lib/agent_session/live_event_buffer.ml) |
| `mailbox` | [contract](../../../lib/agent_session/mailbox.mli) | [source](../../../lib/agent_session/mailbox.ml) |
| `memory_backend` | [contract](../../../lib/agent_session/memory_backend.mli) | [source](../../../lib/agent_session/memory_backend.ml) |
| `operation_worker` | [contract](../../../lib/agent_session/operation_worker.mli) | [source](../../../lib/agent_session/operation_worker.ml) |
| `permission_policy` | [contract](../../../lib/agent_session/permission_policy.mli) | [source](../../../lib/agent_session/permission_policy.ml) |
| `permission_reviewer` | [contract](../../../lib/agent_session/permission_reviewer.mli) | [source](../../../lib/agent_session/permission_reviewer.ml) |
| `prompt_catalog` | [contract](../../../lib/agent_session/prompt_catalog.mli) | [source](../../../lib/agent_session/prompt_catalog.ml) |
| `prompt_definition` | [contract](../../../lib/agent_session/prompt_definition.mli) | [source](../../../lib/agent_session/prompt_definition.ml) |
| `prompt_revision` | [contract](../../../lib/agent_session/prompt_revision.mli) | [source](../../../lib/agent_session/prompt_revision.ml) |
| `prompt_revision_builder` | [contract](../../../lib/agent_session/prompt_revision_builder.mli) | [source](../../../lib/agent_session/prompt_revision_builder.ml) |
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
| `turn_worker` | [contract](../../../lib/agent_session/turn_worker.mli) | [source](../../../lib/agent_session/turn_worker.ml) |
| `workspace_catalog` | [contract](../../../lib/agent_session/workspace_catalog.mli) | [source](../../../lib/agent_session/workspace_catalog.ml) |
| `workspace_cleanup` | [contract](../../../lib/agent_session/workspace_cleanup.mli) | [source](../../../lib/agent_session/workspace_cleanup.ml) |
| `workspace_definition` | [contract](../../../lib/agent_session/workspace_definition.mli) | [source](../../../lib/agent_session/workspace_definition.ml) |
| `workspace_instance` | [contract](../../../lib/agent_session/workspace_instance.mli) | [source](../../../lib/agent_session/workspace_instance.ml) |
| `workspace_lease` | [contract](../../../lib/agent_session/workspace_lease.mli) | [source](../../../lib/agent_session/workspace_lease.ml) |
| `workspace_resolver` | [contract](../../../lib/agent_session/workspace_resolver.mli) | [source](../../../lib/agent_session/workspace_resolver.ml) |
| `extension_invariants` | [contract](../../../lib/agent_session/extension_invariants.mli) | [source](../../../lib/agent_session/extension_invariants.ml) |
