# agent_client

Typed connection ownership, projection, reconnect and downloads. Keep local drafts separate; replace snapshots and deduplicate stable IDs. Always close owned connections.

`Projection.apply_event` installs a validated administrative
`replacement_snapshot` before applying its `session.updated` payload. Replacement
clears absent/empty child collections and transient/terminal caches, so attached
clients converge after reset without waiting for reconnect. Legacy peers that
ignore the additive field must explicitly refresh their snapshot.

See [embedding](../../agent-server/embedding.md), [protocol](../../agent-server/protocol.md)
and the [implementation specification](../../design/ochat-agent-server-implementation-spec.md).
Public interfaces are the exact callable API; the table below is the complete
module inventory for this library. Follow each interface for preconditions,
resource ownership, return types and cancellation/error behavior.

| Module | Interface | Implementation |
|---|---|---|
| `admin` | [contract](../../../lib/agent_client/admin.mli) | [source](../../../lib/agent_client/admin.ml) |
| `blob_download` | [contract](../../../lib/agent_client/blob_download.mli) | [source](../../../lib/agent_client/blob_download.ml) |
| `catalog` | [contract](../../../lib/agent_client/catalog.mli) | [source](../../../lib/agent_client/catalog.ml) |
| `connection` | [contract](../../../lib/agent_client/connection.mli) | [source](../../../lib/agent_client/connection.ml) |
| `in_memory` | [contract](../../../lib/agent_client/in_memory.mli) | [source](../../../lib/agent_client/in_memory.ml) |
| `projection` | [contract](../../../lib/agent_client/projection.mli) | [source](../../../lib/agent_client/projection.ml) |
| `reconnect` | [contract](../../../lib/agent_client/reconnect.mli) | [source](../../../lib/agent_client/reconnect.ml) |
| `session_handle` | [contract](../../../lib/agent_client/session_handle.mli) | [source](../../../lib/agent_client/session_handle.ml) |
| `transport` | [contract](../../../lib/agent_client/transport.mli) | [source](../../../lib/agent_client/transport.ml) |
