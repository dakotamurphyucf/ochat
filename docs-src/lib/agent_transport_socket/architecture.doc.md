# agent_transport_socket

Unix NDJSON client/server and same-user peer credentials. Private socket directory permissions and platform peer checks both apply; connections do not own detached sessions.

See [embedding](../../agent-server/embedding.md), [protocol](../../agent-server/protocol.md)
and the [implementation specification](../../design/ochat-agent-server-implementation-spec.md).
Public interfaces are the exact callable API; the table below is the complete
module inventory for this library. Follow each interface for preconditions,
resource ownership, return types and cancellation/error behavior.

| Module | Interface | Implementation |
|---|---|---|
| `client` | [contract](../../../lib/agent_transport_socket/client.mli) | [source](../../../lib/agent_transport_socket/client.ml) |
| `peer_credentials` | [contract](../../../lib/agent_transport_socket/peer_credentials.mli) | [source](../../../lib/agent_transport_socket/peer_credentials.ml) |
| `server` | [contract](../../../lib/agent_transport_socket/server.mli) | [source](../../../lib/agent_transport_socket/server.ml) |
