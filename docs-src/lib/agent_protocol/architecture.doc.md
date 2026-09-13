# agent_protocol

Closed protocol envelopes, identifiers, requests/results, events, pagination, history and security projections. Codecs own wire validation; transports do not add methods.

Administrative `session.updated` payloads may include an additive
`replacement_snapshot`. `Event.Durable.replacement_snapshot` validates its session
identity, revision and cursor against that individual event. Older payloads
without the field remain valid; it does not introduce a new event kind.

See [embedding](../../agent-server/embedding.md), [protocol](../../agent-server/protocol.md)
and the [implementation specification](../../design/ochat-agent-server-implementation-spec.md).
Public interfaces are the exact callable API; the table below is the complete
module inventory for this library. Follow each interface for preconditions,
resource ownership, return types and cancellation/error behavior.

| Module | Interface | Implementation |
|---|---|---|
| `audit` | [contract](../../../lib/agent_protocol/audit.mli) | [source](../../../lib/agent_protocol/audit.ml) |
| `blob` | [contract](../../../lib/agent_protocol/blob.mli) | [source](../../../lib/agent_protocol/blob.ml) |
| `command` | [contract](../../../lib/agent_protocol/command.mli) | [source](../../../lib/agent_protocol/command.ml) |
| `envelope` | [contract](../../../lib/agent_protocol/envelope.mli) | [source](../../../lib/agent_protocol/envelope.ml) |
| `error` | [contract](../../../lib/agent_protocol/error.mli) | [source](../../../lib/agent_protocol/error.ml) |
| `event` | [contract](../../../lib/agent_protocol/event.mli) | [source](../../../lib/agent_protocol/event.ml) |
| `grant` | [contract](../../../lib/agent_protocol/grant.mli) | [source](../../../lib/agent_protocol/grant.ml) |
| `health` | [contract](../../../lib/agent_protocol/health.mli) | [source](../../../lib/agent_protocol/health.ml) |
| `history` | [contract](../../../lib/agent_protocol/history.mli) | [source](../../../lib/agent_protocol/history.ml) |
| `id` | [contract](../../../lib/agent_protocol/id.mli) | [source](../../../lib/agent_protocol/id.ml) |
| `idempotency_key` | [contract](../../../lib/agent_protocol/idempotency_key.mli) | [source](../../../lib/agent_protocol/idempotency_key.ml) |
| `initialize` | [contract](../../../lib/agent_protocol/initialize.mli) | [source](../../../lib/agent_protocol/initialize.ml) |
| `job` | [contract](../../../lib/agent_protocol/job.mli) | [source](../../../lib/agent_protocol/job.ml) |
| `json_codec` | [contract](../../../lib/agent_protocol/json_codec.mli) | [source](../../../lib/agent_protocol/json_codec.ml) |
| `method_result` | [contract](../../../lib/agent_protocol/method_result.mli) | [source](../../../lib/agent_protocol/method_result.ml) |
| `mutation_result` | [contract](../../../lib/agent_protocol/mutation_result.mli) | [source](../../../lib/agent_protocol/mutation_result.ml) |
| `operation` | [contract](../../../lib/agent_protocol/operation.mli) | [source](../../../lib/agent_protocol/operation.ml) |
| `page` | [contract](../../../lib/agent_protocol/page.mli) | [source](../../../lib/agent_protocol/page.ml) |
| `permission` | [contract](../../../lib/agent_protocol/permission.mli) | [source](../../../lib/agent_protocol/permission.ml) |
| `ping` | [contract](../../../lib/agent_protocol/ping.mli) | [source](../../../lib/agent_protocol/ping.ml) |
| `principal` | [contract](../../../lib/agent_protocol/principal.mli) | [source](../../../lib/agent_protocol/principal.ml) |
| `prompt` | [contract](../../../lib/agent_protocol/prompt.mli) | [source](../../../lib/agent_protocol/prompt.ml) |
| `protocol_error` | [contract](../../../lib/agent_protocol/protocol_error.mli) | [source](../../../lib/agent_protocol/protocol_error.ml) |
| `schedule` | [contract](../../../lib/agent_protocol/schedule.mli) | [source](../../../lib/agent_protocol/schedule.ml) |
| `scope` | [contract](../../../lib/agent_protocol/scope.mli) | [source](../../../lib/agent_protocol/scope.ml) |
| `session` | [contract](../../../lib/agent_protocol/session.mli) | [source](../../../lib/agent_protocol/session.ml) |
| `snapshot` | [contract](../../../lib/agent_protocol/snapshot.mli) | [source](../../../lib/agent_protocol/snapshot.ml) |
| `timestamp` | [contract](../../../lib/agent_protocol/timestamp.mli) | [source](../../../lib/agent_protocol/timestamp.ml) |
| `version` | [contract](../../../lib/agent_protocol/version.mli) | [source](../../../lib/agent_protocol/version.ml) |
| `workspace` | [contract](../../../lib/agent_protocol/workspace.mli) | [source](../../../lib/agent_protocol/workspace.ml) |
| `invocation` | [contract](../../../lib/agent_protocol/invocation.mli) | [source](../../../lib/agent_protocol/invocation.ml) |
| `completion` | [contract](../../../lib/agent_protocol/completion.mli) | [source](../../../lib/agent_protocol/completion.ml) |
| `subscription` | [contract](../../../lib/agent_protocol/subscription.mli) | [source](../../../lib/agent_protocol/subscription.ml) |
| `delivery` | [contract](../../../lib/agent_protocol/delivery.mli) | [source](../../../lib/agent_protocol/delivery.ml) |
| `extension_status` | [contract](../../../lib/agent_protocol/extension_status.mli) | [source](../../../lib/agent_protocol/extension_status.ml) |
| `extension_capabilities` | [contract](../../../lib/agent_protocol/extension_capabilities.mli) | [source](../../../lib/agent_protocol/extension_capabilities.ml) |
