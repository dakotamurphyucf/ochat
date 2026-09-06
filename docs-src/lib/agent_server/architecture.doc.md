# agent_server

Validated catalogs, instance authentication/authorization, principal projection, dispatcher, daemon/embedded ownership, schedulers and maintenance. Starting a daemon core does not bind listeners.

`Session_factory.prepare_administration` prepares runtime state without a live
actor, using private cache/response storage and a cancelled-and-joined child
switch. `Runtime_owner.with_administration` serializes preparation/commit against
runtime loading and retires the old runtime only after successful commit.
`Principal_projection` filters nested administrative replacement snapshots on
both live and replay delivery, including for security readers without grant
visibility. See [initializer restrictions](../../agent-server/operations.md#reset-rebuild-and-upgrade-initializers).

See [embedding](../../agent-server/embedding.md), [protocol](../../agent-server/protocol.md)
and the [implementation specification](../../design/ochat-agent-server-implementation-spec.md).
Public interfaces are the exact callable API; the table below is the complete
module inventory for this library. Follow each interface for preconditions,
resource ownership, return types and cancellation/error behavior.

| Module | Interface | Implementation |
|---|---|---|
| `authenticator` | [contract](../../../lib/agent_server/authenticator.mli) | [source](../../../lib/agent_server/authenticator.ml) |
| `authorization` | [contract](../../../lib/agent_server/authorization.mli) | [source](../../../lib/agent_server/authorization.ml) |
| `catalog_builder` | [contract](../../../lib/agent_server/catalog_builder.mli) | [source](../../../lib/agent_server/catalog_builder.ml) |
| `catalog_identity` | [contract](../../../lib/agent_server/catalog_identity.mli) | [source](../../../lib/agent_server/catalog_identity.ml) |
| `catalog_projection` | [contract](../../../lib/agent_server/catalog_projection.mli) | [source](../../../lib/agent_server/catalog_projection.ml) |
| `command_handler` | [contract](../../../lib/agent_server/command_handler.mli) | [source](../../../lib/agent_server/command_handler.ml) |
| `config` | [contract](../../../lib/agent_server/config.mli) | [source](../../../lib/agent_server/config.ml) |
| `config_diff` | [contract](../../../lib/agent_server/config_diff.mli) | [source](../../../lib/agent_server/config_diff.ml) |
| `config_parser` | [contract](../../../lib/agent_server/config_parser.mli) | [source](../../../lib/agent_server/config_parser.ml) |
| `config_validator` | [contract](../../../lib/agent_server/config_validator.mli) | [source](../../../lib/agent_server/config_validator.ml) |
| `config_watcher` | [contract](../../../lib/agent_server/config_watcher.mli) | [source](../../../lib/agent_server/config_watcher.ml) |
| `connection_context` | [contract](../../../lib/agent_server/connection_context.mli) | [source](../../../lib/agent_server/connection_context.ml) |
| `daemon` | [contract](../../../lib/agent_server/daemon.mli) | [source](../../../lib/agent_server/daemon.ml) |
| `dispatcher` | [contract](../../../lib/agent_server/dispatcher.mli) | [source](../../../lib/agent_server/dispatcher.ml) |
| `embedded` | [contract](../../../lib/agent_server/embedded.mli) | [source](../../../lib/agent_server/embedded.ml) |
| `job_capacity` | [contract](../../../lib/agent_server/job_capacity.mli) | [source](../../../lib/agent_server/job_capacity.ml) |
| `job_scheduler` | [contract](../../../lib/agent_server/job_scheduler.mli) | [source](../../../lib/agent_server/job_scheduler.ml) |
| `maintenance` | [contract](../../../lib/agent_server/maintenance.mli) | [source](../../../lib/agent_server/maintenance.ml) |
| `operator_manifest_grant` | [contract](../../../lib/agent_server/operator_manifest_grant.mli) | [source](../../../lib/agent_server/operator_manifest_grant.ml) |
| `pagination` | [contract](../../../lib/agent_server/pagination.mli) | [source](../../../lib/agent_server/pagination.ml) |
| `permission_review_service` | [contract](../../../lib/agent_server/permission_review_service.mli) | [source](../../../lib/agent_server/permission_review_service.ml) |
| `permission_scheduler` | [contract](../../../lib/agent_server/permission_scheduler.mli) | [source](../../../lib/agent_server/permission_scheduler.ml) |
| `principal_projection` | [contract](../../../lib/agent_server/principal_projection.mli) | [source](../../../lib/agent_server/principal_projection.ml) |
| `runtime_owner` | [contract](../../../lib/agent_server/runtime_owner.mli) | [source](../../../lib/agent_server/runtime_owner.ml) |
| `schedule_scheduler` | [contract](../../../lib/agent_server/schedule_scheduler.mli) | [source](../../../lib/agent_server/schedule_scheduler.ml) |
| `session_capacity` | [contract](../../../lib/agent_server/session_capacity.mli) | [source](../../../lib/agent_server/session_capacity.ml) |
| `session_factory` | [contract](../../../lib/agent_server/session_factory.mli) | [source](../../../lib/agent_server/session_factory.ml) |
| `session_registry` | [contract](../../../lib/agent_server/session_registry.mli) | [source](../../../lib/agent_server/session_registry.ml) |
| `start_scheduler` | [contract](../../../lib/agent_server/start_scheduler.mli) | [source](../../../lib/agent_server/start_scheduler.ml) |
