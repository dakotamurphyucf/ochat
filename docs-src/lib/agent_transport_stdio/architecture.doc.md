# agent_transport_stdio

Local stdio server and full-duplex daemon gateway. Stdout carries envelopes only; EOF ends the connection and local process-bound host, not a detached daemon session.

See [embedding](../../agent-server/embedding.md), [protocol](../../agent-server/protocol.md)
and the [implementation specification](../../design/ochat-agent-server-implementation-spec.md).
Public interfaces are the exact callable API; the table below is the complete
module inventory for this library. Follow each interface for preconditions,
resource ownership, return types and cancellation/error behavior.

| Module | Interface | Implementation |
|---|---|---|
| `gateway` | [contract](../../../lib/agent_transport_stdio/gateway.mli) | [source](../../../lib/agent_transport_stdio/gateway.ml) |
| `server` | [contract](../../../lib/agent_transport_stdio/server.mli) | [source](../../../lib/agent_transport_stdio/server.ml) |
