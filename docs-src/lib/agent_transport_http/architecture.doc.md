# agent_transport_http

HTTP RPC, logical connections, connection notifications, per-session replay SSE, snapshots and blobs. Enforce auth/limits/projection and parse frame boundaries independently of network chunks.

See [embedding](../../agent-server/embedding.md), [protocol](../../agent-server/protocol.md)
and the [implementation specification](../../design/ochat-agent-server-implementation-spec.md).
Public interfaces are the exact callable API; the table below is the complete
module inventory for this library. Follow each interface for preconditions,
resource ownership, return types and cancellation/error behavior.

| Module | Interface | Implementation |
|---|---|---|
| `client` | [contract](../../../lib/agent_transport_http/client.mli) | [source](../../../lib/agent_transport_http/client.ml) |
| `client_lifetime` | [contract](../../../lib/agent_transport_http/client_lifetime.mli) | [source](../../../lib/agent_transport_http/client_lifetime.ml) |
| `request_contract` | [contract](../../../lib/agent_transport_http/request_contract.mli) | [source](../../../lib/agent_transport_http/request_contract.ml) |
| `rpc_body` | [contract](../../../lib/agent_transport_http/rpc_body.mli) | [source](../../../lib/agent_transport_http/rpc_body.ml) |
| `server` | [contract](../../../lib/agent_transport_http/server.mli) | [source](../../../lib/agent_transport_http/server.ml) |
