# agent_client

Typed connection ownership, projection, reconnect and downloads. Keep local drafts separate; replace snapshots and deduplicate stable IDs. Always close owned connections.

`Projection.apply_event` installs a validated administrative
`replacement_snapshot` before applying its `session.updated` payload. Replacement
clears absent/empty child collections and transient/terminal caches, so attached
clients converge after reset without waiting for reconnect. Legacy peers that
ignore the additive field must explicitly refresh their snapshot.

`Completion_view` observes the existing subscribed handle while waiting for a
specific admitted operation to finish. It does not own another event reader or
copy of the session projection. Reader failure, replacement and callback races
are resolved against that handle before delivering a terminal result.

`Developer_repl_connection` holds at most 64 pending REPL mutations and 16 MiB
of their original request params across Create, Submit, Task, Reset and Close.
Fresh Task submission takes a Create/Reset View and checks its instance and
epoch before retaining or sending. An authenticated reconnect may copy pending
custody from the same host, principal and authentication kind; explicit retry
uses the original request. Its requests share one serialized connection lane,
so a blocked Task await needs a separate initialized control connection for
concurrent cancellation. The caller owns the connection's one notification lease.

`Developer_trace_connection` uses that same initialized physical connection
for bounded read-only trace pages. It checks the selected trace feature,
installed methods and limits, the exact requested locator/cursor and the
returned event count. Reconnect requires the same server, principal and
authentication kind; page positions remain caller-owned, and each request
reenters the host's current read authorization. No second notification reader,
mutation custody or source bytes are retained here.

`Canonical_export.read` retrieves the authorized JSON export of an explicit
revision through the same handle and validates its authenticated blob. Supply
`Document_schema.Limits` to bound the download and decoded document. Success
requires a complete, fully disclosed canonical history, matching session and
position metadata, and agreement between the snapshot and duplicate history.
The returned snapshot is suitable for presentation or transcript rendering;
it does not grant import, edit or execution authority. Restricted public views
remain available through the ordinary export API. The helper does not change
the live projection, choose a save path, or detach the handle.

See [embedding](../../agent-server/embedding.md), [protocol](../../agent-server/protocol.md)
and the [implementation specification](../../design/ochat-agent-server-implementation-spec.md).
Public interfaces are the exact callable API; the table below is the complete
module inventory for this library. Follow each interface for preconditions,
resource ownership, return types and cancellation/error behavior.

| Module | Interface | Implementation |
|---|---|---|
| `admin` | [contract](../../../lib/agent_client/admin.mli) | [source](../../../lib/agent_client/admin.ml) |
| `blob_download` | [contract](../../../lib/agent_client/blob_download.mli) | [source](../../../lib/agent_client/blob_download.ml) |
| `canonical_export` | [contract](../../../lib/agent_client/canonical_export.mli) | [source](../../../lib/agent_client/canonical_export.ml) |
| `catalog` | [contract](../../../lib/agent_client/catalog.mli) | [source](../../../lib/agent_client/catalog.ml) |
| `completion_view` | [contract](../../../lib/agent_client/completion_view.mli) | [source](../../../lib/agent_client/completion_view.ml) |
| `connection` | [contract](../../../lib/agent_client/connection.mli) | [source](../../../lib/agent_client/connection.ml) |
| `developer_connection` | [contract](../../../lib/agent_client/developer_connection.mli) | [source](../../../lib/agent_client/developer_connection.ml) |
| `developer_repl_connection` | [contract](../../../lib/agent_client/developer_repl_connection.mli) | [source](../../../lib/agent_client/developer_repl_connection.ml) |
| `developer_trace_connection` | [contract](../../../lib/agent_client/developer_trace_connection.mli) | [source](../../../lib/agent_client/developer_trace_connection.ml) |
| `in_memory` | [contract](../../../lib/agent_client/in_memory.mli) | [source](../../../lib/agent_client/in_memory.ml) |
| `projection` | [contract](../../../lib/agent_client/projection.mli) | [source](../../../lib/agent_client/projection.ml) |
| `reconnect` | [contract](../../../lib/agent_client/reconnect.mli) | [source](../../../lib/agent_client/reconnect.ml) |
| `session_handle` | [contract](../../../lib/agent_client/session_handle.mli) | [source](../../../lib/agent_client/session_handle.ml) |
| `transport` | [contract](../../../lib/agent_client/transport.mli) | [source](../../../lib/agent_client/transport.ml) |
