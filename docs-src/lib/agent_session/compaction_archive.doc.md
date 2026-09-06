# Compaction_archive — independently retained pre-change history

Agent hosts archive the previous `Session_state` before committing compaction,
reset, rebuild or prompt upgrade. The module and persisted field retain their
historical names for compatibility. This is distinct from legacy TUI snapshots and from session deletion's
archive policy. See [session history](../../agent-server/sessions-and-workspaces.md#history-and-synchronization).

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
have an additive `kind` (`Compaction`, `Reset`, `Rebuild`, `Upgrade`); old
references without it decode as `Compaction`. Existing snapshots remain readable.

Files live in the session's private `archive/<kind>-<operation-id>.frame`, with
lowercase `compaction`, `reset`, `rebuild` or `upgrade` prefixes. Old compaction
paths are unchanged.
They use the store's checksummed frame format and configured snapshot payload
limit, with Eio durable replacement and file/directory flush. A failed write
prevents replacement history from committing. The actor then attempts a normal
compaction-failed transition, retaining old history. If the whole store is
unwritable, that failure transition cannot itself be guaranteed durable.

A crash after archive creation but before journal commit can leave an
unreferenced file. It is not exposed through the API. Referenced archives are
not pruned with events, journals or fallback snapshots. Session removal and
transient-store cleanup remove them; disk use grows with retained operations.

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
[protocol](../../agent-server/protocol.md), and [testing](../../agent-server/testing.md).
