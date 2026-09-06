# agent_store

Eio-backed durable files, journals, snapshots, transaction commits, artifacts, blobs, audit, locks and recovery. Preserve typed missing/corrupt/I/O distinctions and exclusive ownership.

See [embedding](../../agent-server/embedding.md), [protocol](../../agent-server/protocol.md)
and the [implementation specification](../../design/ochat-agent-server-implementation-spec.md).
Public interfaces are the exact callable API; the table below is the complete
module inventory for this library. Follow each interface for preconditions,
resource ownership, return types and cancellation/error behavior.

| Module | Interface | Implementation |
|---|---|---|
| `audit_store` | [contract](../../../lib/agent_store/audit_store.mli) | [source](../../../lib/agent_store/audit_store.ml) |
| `blob_store` | [contract](../../../lib/agent_store/blob_store.mli) | [source](../../../lib/agent_store/blob_store.ml) |
| `commit_writer` | [contract](../../../lib/agent_store/commit_writer.mli) | [source](../../../lib/agent_store/commit_writer.ml) |
| `data_root` | [contract](../../../lib/agent_store/data_root.mli) | [source](../../../lib/agent_store/data_root.ml) |
| `durable_file` | [contract](../../../lib/agent_store/durable_file.mli) | [source](../../../lib/agent_store/durable_file.ml) |
| `frame` | [contract](../../../lib/agent_store/frame.mli) | [source](../../../lib/agent_store/frame.ml) |
| `idempotency_store` | [contract](../../../lib/agent_store/idempotency_store.mli) | [source](../../../lib/agent_store/idempotency_store.ml) |
| `journal` | [contract](../../../lib/agent_store/journal.mli) | [source](../../../lib/agent_store/journal.ml) |
| `journal_segment` | [contract](../../../lib/agent_store/journal_segment.mli) | [source](../../../lib/agent_store/journal_segment.ml) |
| `lock` | [contract](../../../lib/agent_store/lock.mli) | [source](../../../lib/agent_store/lock.ml) |
| `migration` | [contract](../../../lib/agent_store/migration.mli) | [source](../../../lib/agent_store/migration.ml) |
| `prompt_artifact_store` | [contract](../../../lib/agent_store/prompt_artifact_store.mli) | [source](../../../lib/agent_store/prompt_artifact_store.ml) |
| `recovery` | [contract](../../../lib/agent_store/recovery.mli) | [source](../../../lib/agent_store/recovery.ml) |
| `session_index` | [contract](../../../lib/agent_store/session_index.mli) | [source](../../../lib/agent_store/session_index.ml) |
| `session_store` | [contract](../../../lib/agent_store/session_store.mli) | [source](../../../lib/agent_store/session_store.ml) |
| `snapshot` | [contract](../../../lib/agent_store/snapshot.mli) | [source](../../../lib/agent_store/snapshot.ml) |
| `store_error` | [contract](../../../lib/agent_store/store_error.mli) | [source](../../../lib/agent_store/store_error.ml) |
| `transaction` | [contract](../../../lib/agent_store/transaction.mli) | [source](../../../lib/agent_store/transaction.ml) |
