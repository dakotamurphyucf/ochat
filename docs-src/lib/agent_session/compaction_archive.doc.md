# Compaction_archive — independently retained pre-change history

Agent hosts archive the previous `Session_state` before committing compaction,
reset, rebuild, prompt upgrade, history edit or deletion, and pending-input
replacement or cancellation. The module and persisted field retain their
historical names for compatibility. Legacy TUI snapshots and session deletion
have separate archive policies. See [session history](../../agent-server/sessions-and-workspaces.md#history-and-synchronization).

## Commit and storage

`Session_actor` places a `Compaction_archived` reference in the terminal
compaction delta alongside history replacement and lifecycle updates.
`Session_persistence` writes the previous state before committing that delta.
The reference includes operation ID, pre-replacement revision and SHA-256 of
the serialized state. References are newest-first in conversation state; old
serialized states without this additive field decode as an empty list.

Administrative replacement uses a `Created` state delta with a new retained
reference. The same persistence boundary writes the previous state first; a
failed archive or journal write does not install the candidate state. References
have a typed `kind`. Older references without it decode as `Compaction`.

Files live in the session's private `archive/<kind>-<operation-id>.frame`.
The lowercase prefixes are `compaction`, `reset`, `rebuild`, `upgrade`,
`edit`, `delete` and `pending-input`. The writer and retained-history scanner
use the same rules for prefixes, typed operation IDs and the frame suffix.
Previously written pending-input frames participate in the same reference
proof as the other kinds. This uses existing storage paths and requires no
migration.
They use the store's checksummed frame format and configured snapshot payload
limit, with Eio durable replacement and file/directory flush. A failed write
prevents replacement history from committing. The actor then attempts a normal
compaction-failed transition, retaining old history. If the whole store is
unwritable, that failure transition cannot itself be guaranteed durable.

A crash after archive creation but before journal commit can leave an
unreferenced file. The retained-history scanner checks complete orphan frames
for possible blob references. It skips temporary writes and fails the entire
proof on an unknown name, invalid operation ID or inconsistent committed
reference. A reference proof checks storage custody. API access still needs an
authorized committed reference. Referenced archives survive ordinary event,
journal and fallback snapshot pruning. Session removal and transient-store
cleanup remove them.

## Retrieval and boundaries

Fetch `session.get.archived_revisions`, then `session.export` with the desired
revision. The archive reader checks framing, full-file length, SHA-256, decoded
state validity, session identity and revision. Missing/corrupt files return a
persistence error, never current-history fallback. Export then applies current
principal permissions, requested canonical/effective history windows and normal
principal-bound blob authorization. Internal native paths and full serialized
state are not the exported API payload.

The archive captures state immediately before replacement; operation metadata
may still describe compaction in progress. It is a history-export source, not
an executable continuation, rollback command or external-effect backup. Later
history deletion/reset does not rewrite an existing archive.

The in-memory backend retains equivalent prior states by revision for actor
tests. Production storage and restart/corruption checks are exercised by the
opt-in `administration-idempotency / admin.compact` daemon scenario.

See [implementation](../../../lib/agent_session/compaction_archive.ml),
[interface](../../../lib/agent_session/compaction_archive.mli),
[retained-history scan](../../../lib/agent_session/retained_history.mli),
[protocol](../../agent-server/protocol.md), and [testing](../../agent-server/testing.md).
