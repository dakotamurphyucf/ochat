# agent_transport_client

Common Unix/HTTP endpoint selection and private token-file loading. Endpoint descriptions omit credentials; transport handles remain owned by their Eio switch.

See [embedding](../../agent-server/embedding.md), [protocol](../../agent-server/protocol.md)
and the [implementation specification](../../design/ochat-agent-server-implementation-spec.md).
Public interfaces are the exact callable API; the table below is the complete
module inventory for this library. Follow each interface for preconditions,
resource ownership, return types and cancellation/error behavior.

| Module | Interface | Implementation |
|---|---|---|
| `endpoint` | [contract](../../../lib/agent_transport_client/endpoint.mli) | [source](../../../lib/agent_transport_client/endpoint.ml) |
