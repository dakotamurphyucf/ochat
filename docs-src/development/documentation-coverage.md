# Documentation coverage ledger

Generated inventory for the current tree, not a claim that every historical code
example was executed. Regenerate with `docs_check --refresh ROOT`. Read the
[worklog](documentation-worklog.md) for actual verification and qualifications.
The [code/documentation audit](code-documentation-audit.md) records scope and boundaries.

## Specification sections

| Section | Source | Current owner | Disposition |
|---|---|---|---|
| Architecture: 1. Purpose | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 2. Normative language | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 3. Goals | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 4. Non-goals | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 5. Design principles | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 6. Terminology | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 7. System architecture | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 8. Execution modes | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 9. Workspace model | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/sessions-and-workspaces.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 10. Prompt catalog and revisions | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/sessions-and-workspaces.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 11. Server configuration | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 12. Session identity and persisted specification | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/operations.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 13. Session actor | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/sessions-and-workspaces.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 14. Lifecycle operations | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 15. Runtime construction | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 16. History model | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/sessions-and-workspaces.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 17. Foreground turn behavior | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 18. ChatML host behavior | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/chatml-orchestration.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 19. Permissions and approvals | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/permissions-and-security.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 20. Durable jobs and schedules | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/chatml-orchestration.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 21. Persistence architecture | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/operations.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 22. Event model | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 23. Command protocol | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/protocol.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 24. Attachments, ownership, and multi-client behavior | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/protocol.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 25. HTTP transport | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/protocol.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 26. Stdio transport | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/protocol.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 27. TUI integration | [spec](../design/ochat-agent-server-spec.md) | [guide](../guide/chat_tui.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 28. Common client library | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/protocol.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 29. MCP relationship | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 30. Authentication and authorization | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/permissions-and-security.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 31. Security boundaries | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/permissions-and-security.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 32. Resource limits and fairness | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 33. Observability and audit | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 34. Shutdown behavior | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 35. Failure behavior summary | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 36. End-to-end behavior examples | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 37. Proposed module organization | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 38. Implementation phases | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 39. Testing requirements | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 40. Acceptance criteria | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Architecture: 41. Required invariants summary | [spec](../design/ochat-agent-server-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 1. Purpose | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 2. Implementation rules | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 3. Current codebase baseline | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 4. Target library graph | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 5. Protocol foundation | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/protocol.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 6. Configuration implementation | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 7. Prompt catalog and revision artifacts | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/sessions-and-workspaces.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 8. Workspace implementation | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/sessions-and-workspaces.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 9. Quotas and workspace leases | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/sessions-and-workspaces.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 10. Durable session data model | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/sessions-and-workspaces.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 11. Store layout and filesystem ownership | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/operations.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 12. Journal format and transaction protocol | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/protocol.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 13. Snapshots, replay, and recovery | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/operations.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 14. Store indexes and list operations | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/operations.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 15. Idempotency implementation | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 16. Blob storage | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 17. Session actor implementation | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/sessions-and-workspaces.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 18. Lifecycle transition implementation | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 19. Runtime builder | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 20. Extracting the shared session controller | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/sessions-and-workspaces.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 21. Foreground turn worker | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 22. User message submission | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 23. Compaction worker | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 24. Cancellation repair | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 25. ChatML moderator integration | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/chatml-orchestration.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 26. Follow-up budgets and scheduling | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 27. Generic permission gate | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/permissions-and-security.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 28. Shell runtime adapters | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../guide/chatmd-shell-host-integration.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 29. Durable job service | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/chatml-orchestration.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 30. Durable schedules | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/chatml-orchestration.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 31. Session registry | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/sessions-and-workspaces.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 32. Attachments and owner leases | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 33. Event implementation | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 34. Protocol dispatcher | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/protocol.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 35. Exact command-handler behavior | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 36. Unix-socket transport | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/protocol.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 37. Stdio transport | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/protocol.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 38. HTTP transport | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/protocol.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 39. Common client library | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/protocol.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 40. Embedded server mode | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 41. TUI implementation and migration | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/operations.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 42. Daemon composition | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 43. Authentication | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/permissions-and-security.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 44. Authorization | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/permissions-and-security.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 45. Security implementation | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/permissions-and-security.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 46. Observability | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 47. Resource limits and fairness | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 48. Cache and artifact handling | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 49. Legacy session migration | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/operations.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 50. Configuration and protocol compatibility | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/protocol.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 51. Failure mapping | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 52. Testing architecture | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 53. Implementation phases | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 54. File-by-file change inventory | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 55. Operational CLI specification | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 56. Documentation deliverables | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 57. Architecture traceability | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 58. Implementation acceptance checklist | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 59. Non-negotiable implementation invariants | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |
| Implementation: 60. Final delivery artifacts | [spec](../design/ochat-agent-server-implementation-spec.md) | [guide](../agent-server/concepts.md) | Cross-reference; exact subcontracts also in protocol/interfaces. |

## Acceptance criteria

The 35 implementation acceptance criteria remain normative in
[section 58](../design/ochat-agent-server-implementation-spec.md#58-implementation-acceptance-checklist).
Documentation ownership follows: 1–4 concepts/path and shell host guides; 5–13
sessions/workspaces and operations; 14–19 orchestration and permissions; 20–21
operations; 22–27 protocol and transports; 28–29 TUI; 30–31 tools/MCP compatibility;
32–34 embedding, security and operations; 35 testing. This is documentation
coverage, not a fresh assertion that every historical acceptance gate ran here.

## Contract inventories

All 38 methods, scopes, events and typed payloads are indexed in the
[protocol](../agent-server/protocol.md) and generated [types](../agent-server/protocol-types.md).
All eight routes are in [HTTP](../agent-server/transports/http.md).
[Configuration](../agent-server/configuration.md), [environment](../agent-server/environment.md),
and [executable references](../bin/chat_tui.doc.md) own operator settings.


## Public agent interfaces

| Surface | Source | Guide | Coverage |
|---|---|---|---|
| `lib/agent_client/admin.mli` | [contract](../../lib/agent_client/admin.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_client/blob_download.mli` | [contract](../../lib/agent_client/blob_download.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_client/catalog.mli` | [contract](../../lib/agent_client/catalog.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_client/connection.mli` | [contract](../../lib/agent_client/connection.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_client/in_memory.mli` | [contract](../../lib/agent_client/in_memory.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_client/projection.mli` | [contract](../../lib/agent_client/projection.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_client/reconnect.mli` | [contract](../../lib/agent_client/reconnect.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_client/session_handle.mli` | [contract](../../lib/agent_client/session_handle.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_client/transport.mli` | [contract](../../lib/agent_client/transport.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/audit.mli` | [contract](../../lib/agent_protocol/audit.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/blob.mli` | [contract](../../lib/agent_protocol/blob.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/command.mli` | [contract](../../lib/agent_protocol/command.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/completion.mli` | [contract](../../lib/agent_protocol/completion.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/delivery.mli` | [contract](../../lib/agent_protocol/delivery.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/envelope.mli` | [contract](../../lib/agent_protocol/envelope.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/error.mli` | [contract](../../lib/agent_protocol/error.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/event.mli` | [contract](../../lib/agent_protocol/event.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/extension_capabilities.mli` | [contract](../../lib/agent_protocol/extension_capabilities.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/extension_status.mli` | [contract](../../lib/agent_protocol/extension_status.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/grant.mli` | [contract](../../lib/agent_protocol/grant.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/health.mli` | [contract](../../lib/agent_protocol/health.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/history.mli` | [contract](../../lib/agent_protocol/history.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/id.mli` | [contract](../../lib/agent_protocol/id.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/idempotency_key.mli` | [contract](../../lib/agent_protocol/idempotency_key.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/initialize.mli` | [contract](../../lib/agent_protocol/initialize.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/invocation.mli` | [contract](../../lib/agent_protocol/invocation.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/job.mli` | [contract](../../lib/agent_protocol/job.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/json_codec.mli` | [contract](../../lib/agent_protocol/json_codec.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/method_result.mli` | [contract](../../lib/agent_protocol/method_result.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/moderator_execution.mli` | [contract](../../lib/agent_protocol/moderator_execution.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/mutation_result.mli` | [contract](../../lib/agent_protocol/mutation_result.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/operation.mli` | [contract](../../lib/agent_protocol/operation.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/page.mli` | [contract](../../lib/agent_protocol/page.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/permission.mli` | [contract](../../lib/agent_protocol/permission.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/ping.mli` | [contract](../../lib/agent_protocol/ping.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/principal.mli` | [contract](../../lib/agent_protocol/principal.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/prompt.mli` | [contract](../../lib/agent_protocol/prompt.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/protocol_error.mli` | [contract](../../lib/agent_protocol/protocol_error.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/schedule.mli` | [contract](../../lib/agent_protocol/schedule.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/scope.mli` | [contract](../../lib/agent_protocol/scope.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/session.mli` | [contract](../../lib/agent_protocol/session.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/snapshot.mli` | [contract](../../lib/agent_protocol/snapshot.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/subscription.mli` | [contract](../../lib/agent_protocol/subscription.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/timestamp.mli` | [contract](../../lib/agent_protocol/timestamp.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/version.mli` | [contract](../../lib/agent_protocol/version.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_protocol/workspace.mli` | [contract](../../lib/agent_protocol/workspace.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_server/authenticator.mli` | [contract](../../lib/agent_server/authenticator.mli) | [integration](../agent-server/permissions-and-security.md) | Public interface + current host guide. |
| `lib/agent_server/authorization.mli` | [contract](../../lib/agent_server/authorization.mli) | [integration](../agent-server/permissions-and-security.md) | Public interface + current host guide. |
| `lib/agent_server/catalog_builder.mli` | [contract](../../lib/agent_server/catalog_builder.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/catalog_identity.mli` | [contract](../../lib/agent_server/catalog_identity.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/catalog_projection.mli` | [contract](../../lib/agent_server/catalog_projection.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/command_handler.mli` | [contract](../../lib/agent_server/command_handler.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/config.mli` | [contract](../../lib/agent_server/config.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/config_diff.mli` | [contract](../../lib/agent_server/config_diff.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/config_parser.mli` | [contract](../../lib/agent_server/config_parser.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/config_validator.mli` | [contract](../../lib/agent_server/config_validator.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/config_watcher.mli` | [contract](../../lib/agent_server/config_watcher.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/connection_context.mli` | [contract](../../lib/agent_server/connection_context.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/daemon.mli` | [contract](../../lib/agent_server/daemon.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/dispatcher.mli` | [contract](../../lib/agent_server/dispatcher.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/embedded.mli` | [contract](../../lib/agent_server/embedded.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/job_capacity.mli` | [contract](../../lib/agent_server/job_capacity.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/agent_server/job_scheduler.mli` | [contract](../../lib/agent_server/job_scheduler.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/agent_server/maintenance.mli` | [contract](../../lib/agent_server/maintenance.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/operator_manifest_grant.mli` | [contract](../../lib/agent_server/operator_manifest_grant.mli) | [integration](../agent-server/permissions-and-security.md) | Public interface + current host guide. |
| `lib/agent_server/pagination.mli` | [contract](../../lib/agent_server/pagination.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/permission_review_service.mli` | [contract](../../lib/agent_server/permission_review_service.mli) | [integration](../agent-server/permissions-and-security.md) | Public interface + current host guide. |
| `lib/agent_server/permission_scheduler.mli` | [contract](../../lib/agent_server/permission_scheduler.mli) | [integration](../agent-server/permissions-and-security.md) | Public interface + current host guide. |
| `lib/agent_server/principal_projection.mli` | [contract](../../lib/agent_server/principal_projection.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/runtime_owner.mli` | [contract](../../lib/agent_server/runtime_owner.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/agent_server/schedule_scheduler.mli` | [contract](../../lib/agent_server/schedule_scheduler.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/agent_server/session_capacity.mli` | [contract](../../lib/agent_server/session_capacity.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_server/session_factory.mli` | [contract](../../lib/agent_server/session_factory.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_server/session_registry.mli` | [contract](../../lib/agent_server/session_registry.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_server/start_scheduler.mli` | [contract](../../lib/agent_server/start_scheduler.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/agent_session/active_calls.mli` | [contract](../../lib/agent_session/active_calls.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/administration.mli` | [contract](../../lib/agent_session/administration.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/chatmd_export.mli` | [contract](../../lib/agent_session/chatmd_export.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/compaction_archive.mli` | [contract](../../lib/agent_session/compaction_archive.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/durable_event_log.mli` | [contract](../../lib/agent_session/durable_event_log.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/extension_invariants.mli` | [contract](../../lib/agent_session/extension_invariants.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/history_codec.mli` | [contract](../../lib/agent_session/history_codec.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/history_id_source.mli` | [contract](../../lib/agent_session/history_id_source.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/invocation_history.mli` | [contract](../../lib/agent_session/invocation_history.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/invocation_recovery.mli` | [contract](../../lib/agent_session/invocation_recovery.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_session/live_event_buffer.mli` | [contract](../../lib/agent_session/live_event_buffer.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/mailbox.mli` | [contract](../../lib/agent_session/mailbox.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/memory_backend.mli` | [contract](../../lib/agent_session/memory_backend.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/moderator_event.mli` | [contract](../../lib/agent_session/moderator_event.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/agent_session/moderator_observation.mli` | [contract](../../lib/agent_session/moderator_observation.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/agent_session/moderator_tool_dispatch.mli` | [contract](../../lib/agent_session/moderator_tool_dispatch.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/agent_session/native_tool_dispatch.mli` | [contract](../../lib/agent_session/native_tool_dispatch.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/native_tool_invocation.mli` | [contract](../../lib/agent_session/native_tool_invocation.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/native_tool_moderation.mli` | [contract](../../lib/agent_session/native_tool_moderation.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/observation_follow_up.mli` | [contract](../../lib/agent_session/observation_follow_up.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/one_off_execution.mli` | [contract](../../lib/agent_session/one_off_execution.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/operation_worker.mli` | [contract](../../lib/agent_session/operation_worker.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/permission_policy.mli` | [contract](../../lib/agent_session/permission_policy.mli) | [integration](../agent-server/permissions-and-security.md) | Public interface + current host guide. |
| `lib/agent_session/permission_reviewer.mli` | [contract](../../lib/agent_session/permission_reviewer.mli) | [integration](../agent-server/permissions-and-security.md) | Public interface + current host guide. |
| `lib/agent_session/prompt_catalog.mli` | [contract](../../lib/agent_session/prompt_catalog.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/prompt_definition.mli` | [contract](../../lib/agent_session/prompt_definition.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/prompt_revision.mli` | [contract](../../lib/agent_session/prompt_revision.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/prompt_revision_builder.mli` | [contract](../../lib/agent_session/prompt_revision_builder.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/queued_moderator_event.mli` | [contract](../../lib/agent_session/queued_moderator_event.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/agent_session/quota_key.mli` | [contract](../../lib/agent_session/quota_key.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/quota_manager.mli` | [contract](../../lib/agent_session/quota_manager.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/run_chatml_tool.mli` | [contract](../../lib/agent_session/run_chatml_tool.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/agent_session/runtime_builder.mli` | [contract](../../lib/agent_session/runtime_builder.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/runtime_paths.mli` | [contract](../../lib/agent_session/runtime_paths.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/script_tool_calls.mli` | [contract](../../lib/agent_session/script_tool_calls.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/security_grant.mli` | [contract](../../lib/agent_session/security_grant.mli) | [integration](../agent-server/permissions-and-security.md) | Public interface + current host guide. |
| `lib/agent_session/session_actor.mli` | [contract](../../lib/agent_session/session_actor.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/session_delta.mli` | [contract](../../lib/agent_session/session_delta.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/session_persistence.mli` | [contract](../../lib/agent_session/session_persistence.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_session/session_state.mli` | [contract](../../lib/agent_session/session_state.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/session_transition.mli` | [contract](../../lib/agent_session/session_transition.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/standalone_tool_dispatch.mli` | [contract](../../lib/agent_session/standalone_tool_dispatch.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/start_queue.mli` | [contract](../../lib/agent_session/start_queue.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/stream_invocation.mli` | [contract](../../lib/agent_session/stream_invocation.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/subscriber.mli` | [contract](../../lib/agent_session/subscriber.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/turn_worker.mli` | [contract](../../lib/agent_session/turn_worker.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/workspace_catalog.mli` | [contract](../../lib/agent_session/workspace_catalog.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/workspace_cleanup.mli` | [contract](../../lib/agent_session/workspace_cleanup.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/workspace_definition.mli` | [contract](../../lib/agent_session/workspace_definition.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/workspace_instance.mli` | [contract](../../lib/agent_session/workspace_instance.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/workspace_lease.mli` | [contract](../../lib/agent_session/workspace_lease.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_session/workspace_resolver.mli` | [contract](../../lib/agent_session/workspace_resolver.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/agent_store/audit_store.mli` | [contract](../../lib/agent_store/audit_store.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/blob_store.mli` | [contract](../../lib/agent_store/blob_store.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/commit_writer.mli` | [contract](../../lib/agent_store/commit_writer.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/data_root.mli` | [contract](../../lib/agent_store/data_root.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/durable_file.mli` | [contract](../../lib/agent_store/durable_file.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/frame.mli` | [contract](../../lib/agent_store/frame.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/idempotency_store.mli` | [contract](../../lib/agent_store/idempotency_store.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/journal.mli` | [contract](../../lib/agent_store/journal.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/journal_segment.mli` | [contract](../../lib/agent_store/journal_segment.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/lock.mli` | [contract](../../lib/agent_store/lock.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/migration.mli` | [contract](../../lib/agent_store/migration.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/prompt_artifact_store.mli` | [contract](../../lib/agent_store/prompt_artifact_store.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/recovery.mli` | [contract](../../lib/agent_store/recovery.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/session_index.mli` | [contract](../../lib/agent_store/session_index.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/session_store.mli` | [contract](../../lib/agent_store/session_store.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/snapshot.mli` | [contract](../../lib/agent_store/snapshot.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/store_error.mli` | [contract](../../lib/agent_store/store_error.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_store/transaction.mli` | [contract](../../lib/agent_store/transaction.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/agent_transport_client/endpoint.mli` | [contract](../../lib/agent_transport_client/endpoint.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_transport_http/client.mli` | [contract](../../lib/agent_transport_http/client.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_transport_http/client_lifetime.mli` | [contract](../../lib/agent_transport_http/client_lifetime.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_transport_http/request_contract.mli` | [contract](../../lib/agent_transport_http/request_contract.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_transport_http/rpc_body.mli` | [contract](../../lib/agent_transport_http/rpc_body.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_transport_http/server.mli` | [contract](../../lib/agent_transport_http/server.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_transport_socket/client.mli` | [contract](../../lib/agent_transport_socket/client.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_transport_socket/peer_credentials.mli` | [contract](../../lib/agent_transport_socket/peer_credentials.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_transport_socket/server.mli` | [contract](../../lib/agent_transport_socket/server.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_transport_stdio/gateway.mli` | [contract](../../lib/agent_transport_stdio/gateway.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/agent_transport_stdio/server.mli` | [contract](../../lib/agent_transport_stdio/server.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/chat_response/agent_response_loop.mli` | [contract](../../lib/chat_response/agent_response_loop.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/agent_runtime.mli` | [contract](../../lib/chat_response/agent_runtime.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/agent_trace.mli` | [contract](../../lib/chat_response/agent_trace.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/authoring_policy.mli` | [contract](../../lib/chat_response/authoring_policy.mli) | [integration](../agent-server/permissions-and-security.md) | Public interface + current host guide. |
| `lib/chat_response/authoring_registration.mli` | [contract](../../lib/chat_response/authoring_registration.mli) | [integration](../agent-server/permissions-and-security.md) | Public interface + current host guide. |
| `lib/chat_response/chatml_moderation.mli` | [contract](../../lib/chat_response/chatml_moderation.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chat_response/chatml_moderator.mli` | [contract](../../lib/chat_response/chatml_moderator.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chat_response/chatml_turn_driver.mli` | [contract](../../lib/chat_response/chatml_turn_driver.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chat_response/compact_history.mli` | [contract](../../lib/chat_response/compact_history.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/chat_response/ctx.mli` | [contract](../../lib/chat_response/ctx.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/driver.mli` | [contract](../../lib/chat_response/driver.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/execution_gate.mli` | [contract](../../lib/chat_response/execution_gate.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/extension_compiler.mli` | [contract](../../lib/chat_response/extension_compiler.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/fetch.mli` | [contract](../../lib/chat_response/fetch.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/fork.mli` | [contract](../../lib/chat_response/fork.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/generated_admission.mli` | [contract](../../lib/chat_response/generated_admission.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/history_stream_event.mli` | [contract](../../lib/chat_response/history_stream_event.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/chat_response/in_memory_stream.mli` | [contract](../../lib/chat_response/in_memory_stream.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/managed_tool_registry.mli` | [contract](../../lib/chat_response/managed_tool_registry.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/mcp_discovery_cache.mli` | [contract](../../lib/chat_response/mcp_discovery_cache.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/model_executor.mli` | [contract](../../lib/chat_response/model_executor.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/moderation.mli` | [contract](../../lib/chat_response/moderation.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/moderator_invocation.mli` | [contract](../../lib/chat_response/moderator_invocation.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chat_response/moderator_manager.mli` | [contract](../../lib/chat_response/moderator_manager.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chat_response/one_off_request.mli` | [contract](../../lib/chat_response/one_off_request.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/one_off_script.mli` | [contract](../../lib/chat_response/one_off_script.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/response_loop.mli` | [contract](../../lib/chat_response/response_loop.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/runtime_request_scope.mli` | [contract](../../lib/chat_response/runtime_request_scope.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/runtime_semantics.mli` | [contract](../../lib/chat_response/runtime_semantics.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/shell_tool.mli` | [contract](../../lib/chat_response/shell_tool.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chat_response/sourced_response_event.mli` | [contract](../../lib/chat_response/sourced_response_event.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/tool.mli` | [contract](../../lib/chat_response/tool.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/tool_call.mli` | [contract](../../lib/chat_response/tool_call.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/tool_capability.mli` | [contract](../../lib/chat_response/tool_capability.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/tool_execution_event.mli` | [contract](../../lib/chat_response/tool_execution_event.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/tool_executor.mli` | [contract](../../lib/chat_response/tool_executor.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_response/utf8_ingest.mli` | [contract](../../lib/chat_response/utf8_ingest.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chat_tui/agent_event_apply.mli` | [contract](../../lib/chat_tui/agent_event_apply.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/agent_history_layout.mli` | [contract](../../lib/chat_tui/agent_history_layout.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/agent_page_layout.mli` | [contract](../../lib/chat_tui/agent_page_layout.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/agent_page_selector.mli` | [contract](../../lib/chat_tui/agent_page_selector.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/agent_permission_view.mli` | [contract](../../lib/chat_tui/agent_permission_view.mli) | [integration](../agent-server/permissions-and-security.md) | Public interface + current host guide. |
| `lib/chat_tui/agent_projection.mli` | [contract](../../lib/chat_tui/agent_projection.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/agent_security_projection.mli` | [contract](../../lib/chat_tui/agent_security_projection.mli) | [integration](../agent-server/permissions-and-security.md) | Public interface + current host guide. |
| `lib/chat_tui/agent_session_client.mli` | [contract](../../lib/chat_tui/agent_session_client.mli) | [integration](../agent-server/protocol.md) | Public interface + current host guide. |
| `lib/chat_tui/app.mli` | [contract](../../lib/chat_tui/app.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/app_compaction.mli` | [contract](../../lib/chat_tui/app_compaction.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/app_context.mli` | [contract](../../lib/chat_tui/app_context.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/app_events.mli` | [contract](../../lib/chat_tui/app_events.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/app_reducer.mli` | [contract](../../lib/chat_tui/app_reducer.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/app_runtime.mli` | [contract](../../lib/chat_tui/app_runtime.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/app_stream_apply.mli` | [contract](../../lib/chat_tui/app_stream_apply.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/app_streaming.mli` | [contract](../../lib/chat_tui/app_streaming.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/app_submit.mli` | [contract](../../lib/chat_tui/app_submit.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/attachments.mli` | [contract](../../lib/chat_tui/attachments.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/chat_message_render_job.mli` | [contract](../../lib/chat_tui/chat_message_render_job.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chat_tui/chat_page_layout.mli` | [contract](../../lib/chat_tui/chat_page_layout.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/chat_render_worker.mli` | [contract](../../lib/chat_tui/chat_render_worker.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/chat_render_worker_runtime.mli` | [contract](../../lib/chat_tui/chat_render_worker_runtime.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/chat_startup_render.mli` | [contract](../../lib/chat_tui/chat_startup_render.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/cmd.mli` | [contract](../../lib/chat_tui/cmd.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/connection_status.mli` | [contract](../../lib/chat_tui/connection_status.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/controller.mli` | [contract](../../lib/chat_tui/controller.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/controller_agent.mli` | [contract](../../lib/chat_tui/controller_agent.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/controller_normal.mli` | [contract](../../lib/chat_tui/controller_normal.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/controller_shell_security.mli` | [contract](../../lib/chat_tui/controller_shell_security.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chat_tui/controller_types.mli` | [contract](../../lib/chat_tui/controller_types.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/conversation.mli` | [contract](../../lib/chat_tui/conversation.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/highlight_grammar_discovery.mli` | [contract](../../lib/chat_tui/highlight_grammar_discovery.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/highlight_grammars.mli` | [contract](../../lib/chat_tui/highlight_grammars.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/highlight_registry.mli` | [contract](../../lib/chat_tui/highlight_registry.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/highlight_styles.mli` | [contract](../../lib/chat_tui/highlight_styles.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/highlight_theme.mli` | [contract](../../lib/chat_tui/highlight_theme.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/highlight_tm_engine.mli` | [contract](../../lib/chat_tui/highlight_tm_engine.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/highlight_tm_loader.mli` | [contract](../../lib/chat_tui/highlight_tm_loader.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/history_chunk.mli` | [contract](../../lib/chat_tui/history_chunk.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/history_materialization.mli` | [contract](../../lib/chat_tui/history_materialization.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/input_display.mli` | [contract](../../lib/chat_tui/input_display.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/live_scroll_trace.mli` | [contract](../../lib/chat_tui/live_scroll_trace.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/markdown_fences.mli` | [contract](../../lib/chat_tui/markdown_fences.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/model.mli` | [contract](../../lib/chat_tui/model.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/moderator_session_controller.mli` | [contract](../../lib/chat_tui/moderator_session_controller.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chat_tui/path_completion.mli` | [contract](../../lib/chat_tui/path_completion.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/persistence.mli` | [contract](../../lib/chat_tui/persistence.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/chat_tui/prepared_corridor.mli` | [contract](../../lib/chat_tui/prepared_corridor.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/projected_message.mli` | [contract](../../lib/chat_tui/projected_message.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/redraw_throttle.mli` | [contract](../../lib/chat_tui/redraw_throttle.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer.mli` | [contract](../../lib/chat_tui/renderer.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_component_history.mli` | [contract](../../lib/chat_tui/renderer_component_history.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_component_input_box.mli` | [contract](../../lib/chat_tui/renderer_component_input_box.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_component_loader.mli` | [contract](../../lib/chat_tui/renderer_component_loader.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_component_message.mli` | [contract](../../lib/chat_tui/renderer_component_message.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_component_status_bar.mli` | [contract](../../lib/chat_tui/renderer_component_status_bar.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_highlight_engine.mli` | [contract](../../lib/chat_tui/renderer_highlight_engine.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_json_highlight.mli` | [contract](../../lib/chat_tui/renderer_json_highlight.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_lang.mli` | [contract](../../lib/chat_tui/renderer_lang.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_page_agent.mli` | [contract](../../lib/chat_tui/renderer_page_agent.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_page_chat.mli` | [contract](../../lib/chat_tui/renderer_page_chat.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_page_shell_security.mli` | [contract](../../lib/chat_tui/renderer_page_shell_security.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_pages.mli` | [contract](../../lib/chat_tui/renderer_pages.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_shell_approval.mli` | [contract](../../lib/chat_tui/renderer_shell_approval.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_shell_border.mli` | [contract](../../lib/chat_tui/renderer_shell_border.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_shell_security_palette.mli` | [contract](../../lib/chat_tui/renderer_shell_security_palette.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chat_tui/renderer_virtual_list.mli` | [contract](../../lib/chat_tui/renderer_virtual_list.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/shell_management_service.mli` | [contract](../../lib/chat_tui/shell_management_service.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chat_tui/shell_security_page_state.mli` | [contract](../../lib/chat_tui/shell_security_page_state.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chat_tui/shell_security_snapshot.mli` | [contract](../../lib/chat_tui/shell_security_snapshot.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chat_tui/stream.mli` | [contract](../../lib/chat_tui/stream.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/type_ahead_config.mli` | [contract](../../lib/chat_tui/type_ahead_config.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/type_ahead_controller.mli` | [contract](../../lib/chat_tui/type_ahead_controller.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/type_ahead_provider.mli` | [contract](../../lib/chat_tui/type_ahead_provider.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/type_ahead_ui.mli` | [contract](../../lib/chat_tui/type_ahead_ui.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/types.mli` | [contract](../../lib/chat_tui/types.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/ui_helpers.mli` | [contract](../../lib/chat_tui/ui_helpers.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/utf8_edit.mli` | [contract](../../lib/chat_tui/utf8_edit.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chat_tui/util.mli` | [contract](../../lib/chat_tui/util.mli) | [integration](../guide/chat_tui.md) | Public interface + current host guide. |
| `lib/chatmd/chatmd_attributes.mli` | [contract](../../lib/chatmd/chatmd_attributes.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chatmd/chatmd_extension_declaration.mli` | [contract](../../lib/chatmd/chatmd_extension_declaration.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chatmd/chatmd_import_expansion.mli` | [contract](../../lib/chatmd/chatmd_import_expansion.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chatmd/chatmd_moderator_runtime_declaration.mli` | [contract](../../lib/chatmd/chatmd_moderator_runtime_declaration.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatmd/chatmd_read_file_declaration.mli` | [contract](../../lib/chatmd/chatmd_read_file_declaration.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chatmd/chatmd_read_file_spec.mli` | [contract](../../lib/chatmd/chatmd_read_file_spec.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chatmd/chatmd_script_declaration.mli` | [contract](../../lib/chatmd/chatmd_script_declaration.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chatmd/chatmd_shell_declaration.mli` | [contract](../../lib/chatmd/chatmd_shell_declaration.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd/chatmd_shell_serialization.mli` | [contract](../../lib/chatmd/chatmd_shell_serialization.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd/chatmd_source_bundle.mli` | [contract](../../lib/chatmd/chatmd_source_bundle.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chatmd/prompt.mli` | [contract](../../lib/chatmd/prompt.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/chatmd/source_loader.mli` | [contract](../../lib/chatmd/source_loader.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/authoring_metadata.mli` | [contract](../../lib/chatmd_shell_spec/authoring_metadata.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/builtin_profile.mli` | [contract](../../lib/chatmd_shell_spec/builtin_profile.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/chatmd_script_spec.mli` | [contract](../../lib/chatmd_shell_spec/chatmd_script_spec.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/diagnostic.mli` | [contract](../../lib/chatmd_shell_spec/diagnostic.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/duration.mli` | [contract](../../lib/chatmd_shell_spec/duration.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/extension_spec.mli` | [contract](../../lib/chatmd_shell_spec/extension_spec.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/feature.mli` | [contract](../../lib/chatmd_shell_spec/feature.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/manifest.mli` | [contract](../../lib/chatmd_shell_spec/manifest.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/manifest_compiler.mli` | [contract](../../lib/chatmd_shell_spec/manifest_compiler.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/manifest_defaults.mli` | [contract](../../lib/chatmd_shell_spec/manifest_defaults.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/manifest_merge.mli` | [contract](../../lib/chatmd_shell_spec/manifest_merge.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/path_expr.mli` | [contract](../../lib/chatmd_shell_spec/path_expr.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/shell_element.mli` | [contract](../../lib/chatmd_shell_spec/shell_element.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/shell_spec.mli` | [contract](../../lib/chatmd_shell_spec/shell_spec.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/shell_tool_spec.mli` | [contract](../../lib/chatmd_shell_spec/shell_tool_spec.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/source_ref.mli` | [contract](../../lib/chatmd_shell_spec/source_ref.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatmd_shell_spec/tool_schema.mli` | [contract](../../lib/chatmd_shell_spec/tool_schema.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_builtin_modules.mli` | [contract](../../lib/chatml/chatml_builtin_modules.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_builtin_spec.mli` | [contract](../../lib/chatml/chatml_builtin_spec.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_builtin_surface.mli` | [contract](../../lib/chatml/chatml_builtin_surface.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_compilation.mli` | [contract](../../lib/chatml/chatml_compilation.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_debug_log.mli` | [contract](../../lib/chatml/chatml_debug_log.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_eval.mli` | [contract](../../lib/chatml/chatml_eval.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_execution.mli` | [contract](../../lib/chatml/chatml_execution.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_extension_surface.mli` | [contract](../../lib/chatml/chatml_extension_surface.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_host_runtime.mli` | [contract](../../lib/chatml/chatml_host_runtime.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_lexer.mli` | [contract](../../lib/chatml/chatml_lexer.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_moderator_runtime.mli` | [contract](../../lib/chatml/chatml_moderator_runtime.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_parse.mli` | [contract](../../lib/chatml/chatml_parse.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_resolver.mli` | [contract](../../lib/chatml/chatml_resolver.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_runtime.mli` | [contract](../../lib/chatml/chatml_runtime.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/chatml/chatml_value_codec.mli` | [contract](../../lib/chatml/chatml_value_codec.mli) | [integration](../agent-server/chatml-orchestration.md) | Public interface + current host guide. |
| `lib/history_entry.mli` | [contract](../../lib/history_entry.mli) | [integration](../agent-server/sessions-and-workspaces.md) | Public interface + current host guide. |
| `lib/openai/responses.mli` | [contract](../../lib/openai/responses.mli) | [integration](../agent-server/concepts.md) | Public interface + current host guide. |
| `lib/session_store.mli` | [contract](../../lib/session_store.mli) | [integration](../agent-server/operations.md) | Public interface + current host guide. |
| `lib/shell_access/shell_access.mli` | [contract](../../lib/shell_access/shell_access.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_access/shell_access_v2.mli` | [contract](../../lib/shell_access/shell_access_v2.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/admin_policy.mli` | [contract](../../lib/shell_runtime/admin_policy.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/admin_policy_loader.mli` | [contract](../../lib/shell_runtime/admin_policy_loader.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/approval_broker.mli` | [contract](../../lib/shell_runtime/approval_broker.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/approval_store.mli` | [contract](../../lib/shell_runtime/approval_store.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/audit_replay.mli` | [contract](../../lib/shell_runtime/audit_replay.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/audit_sink.mli` | [contract](../../lib/shell_runtime/audit_sink.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/chatml_approval_value.mli` | [contract](../../lib/shell_runtime/chatml_approval_value.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/chatml_audit_value.mli` | [contract](../../lib/shell_runtime/chatml_audit_value.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/chatml_codec.mli` | [contract](../../lib/shell_runtime/chatml_codec.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/chatml_context_value.mli` | [contract](../../lib/shell_runtime/chatml_context_value.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/chatml_effect_value.mli` | [contract](../../lib/shell_runtime/chatml_effect_value.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/chatml_extension.mli` | [contract](../../lib/shell_runtime/chatml_extension.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/chatml_interceptor_value.mli` | [contract](../../lib/shell_runtime/chatml_interceptor_value.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/chatml_policy_value.mli` | [contract](../../lib/shell_runtime/chatml_policy_value.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/chatml_result_value.mli` | [contract](../../lib/shell_runtime/chatml_result_value.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/chatml_value.mli` | [contract](../../lib/shell_runtime/chatml_value.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/environment.mli` | [contract](../../lib/shell_runtime/environment.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/executable_analyzer.mli` | [contract](../../lib/shell_runtime/executable_analyzer.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/executable_audit_filter.mli` | [contract](../../lib/shell_runtime/executable_audit_filter.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/executable_interceptor.mli` | [contract](../../lib/shell_runtime/executable_interceptor.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/executable_reviewer.mli` | [contract](../../lib/shell_runtime/executable_reviewer.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/hook_payload.mli` | [contract](../../lib/shell_runtime/hook_payload.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/hook_protocol.mli` | [contract](../../lib/shell_runtime/hook_protocol.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/hook_worker.mli` | [contract](../../lib/shell_runtime/hook_worker.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/host.mli` | [contract](../../lib/shell_runtime/host.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/interrupted_store.mli` | [contract](../../lib/shell_runtime/interrupted_store.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/lowering.mli` | [contract](../../lib/shell_runtime/lowering.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/manifest_authorizer.mli` | [contract](../../lib/shell_runtime/manifest_authorizer.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/manifest_grant_store.mli` | [contract](../../lib/shell_runtime/manifest_grant_store.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/manifest_security.mli` | [contract](../../lib/shell_runtime/manifest_security.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/manifest_signature.mli` | [contract](../../lib/shell_runtime/manifest_signature.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/model_reviewer.mli` | [contract](../../lib/shell_runtime/model_reviewer.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/moderator_process_adapter.mli` | [contract](../../lib/shell_runtime/moderator_process_adapter.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/registry.mli` | [contract](../../lib/shell_runtime/registry.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/result.mli` | [contract](../../lib/shell_runtime/result.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/runtime.mli` | [contract](../../lib/shell_runtime/runtime.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |
| `lib/shell_runtime/trusted_source.mli` | [contract](../../lib/shell_runtime/trusted_source.mli) | [integration](../guide/chatmd-shell-host-integration.md) | Public interface + current host guide. |

## Document inventory

This includes retained historical and unrelated documentation; retention is not
a claim that all examples apply to native/daemon hosts. Changed host semantics
are qualified in entry points, with complete current tutorials in agent-server.

| Document | Path | Disposition |
|---|---|---|
| [page](../README.md) | `docs-src/README.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../agent-server/README.md) | `docs-src/agent-server/README.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/chatml-orchestration.md) | `docs-src/agent-server/chatml-orchestration.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/concepts.md) | `docs-src/agent-server/concepts.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/configuration.md) | `docs-src/agent-server/configuration.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/embedding.md) | `docs-src/agent-server/embedding.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/environment.md) | `docs-src/agent-server/environment.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/extensibility-foundations.md) | `docs-src/agent-server/extensibility-foundations.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/operations.md) | `docs-src/agent-server/operations.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/operator-contracts.md) | `docs-src/agent-server/operator-contracts.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/permissions-and-security.md) | `docs-src/agent-server/permissions-and-security.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/protocol-types.md) | `docs-src/agent-server/protocol-types.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/protocol.md) | `docs-src/agent-server/protocol.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/quickstart.md) | `docs-src/agent-server/quickstart.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/sessions-and-workspaces.md) | `docs-src/agent-server/sessions-and-workspaces.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/testing.md) | `docs-src/agent-server/testing.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/transports/http.md) | `docs-src/agent-server/transports/http.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/transports/stdio.md) | `docs-src/agent-server/transports/stdio.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/transports/unix.md) | `docs-src/agent-server/transports/unix.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/troubleshooting.md) | `docs-src/agent-server/troubleshooting.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/tutorials/background-agent.md) | `docs-src/agent-server/tutorials/background-agent.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/tutorials/http-client.md) | `docs-src/agent-server/tutorials/http-client.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/tutorials/local-tui.md) | `docs-src/agent-server/tutorials/local-tui.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/tutorials/shell-agent.md) | `docs-src/agent-server/tutorials/shell-agent.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/tutorials/stdio-client.md) | `docs-src/agent-server/tutorials/stdio-client.md` | Current reference/tutorial; offline checker applies. |
| [page](../agent-server/tutorials/unix-daemon.md) | `docs-src/agent-server/tutorials/unix-daemon.md` | Current reference/tutorial; offline checker applies. |
| [page](../applications/README.md) | `docs-src/applications/README.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../applications/background-workflow.md) | `docs-src/applications/background-workflow.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../applications/change-review.md) | `docs-src/applications/change-review.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../applications/documentation-review.md) | `docs-src/applications/documentation-review.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../applications/headless-report.md) | `docs-src/applications/headless-report.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../applications/repository-onboarding.md) | `docs-src/applications/repository-onboarding.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../applications/research-brief.md) | `docs-src/applications/research-brief.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/README.md) | `docs-src/bin/README.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/chat_tui.doc.md) | `docs-src/bin/chat_tui.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/developer-utilities.md) | `docs-src/bin/developer-utilities.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/dsl_script.doc.md) | `docs-src/bin/dsl_script.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/eio_get.doc.md) | `docs-src/bin/eio_get.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/gpt.doc.md) | `docs-src/bin/gpt.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/key_dump.doc.md) | `docs-src/bin/key_dump.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/main.doc.md) | `docs-src/bin/main.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/mcp_server.doc.md) | `docs-src/bin/mcp_server.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/md_index.doc.md) | `docs-src/bin/md_index.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/md_search.doc.md) | `docs-src/bin/md_search.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/mp_prompt.doc.md) | `docs-src/bin/mp_prompt.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/mp_refine_run.doc.md) | `docs-src/bin/mp_refine_run.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/ochat_agent_server.doc.md) | `docs-src/bin/ochat_agent_server.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/ochat_agent_stdio.doc.md) | `docs-src/bin/ochat_agent_stdio.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/ochat_shell_resource_runner.doc.md) | `docs-src/bin/ochat_shell_resource_runner.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/odoc_index.doc.md) | `docs-src/bin/odoc_index.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../bin/odoc_search.doc.md) | `docs-src/bin/odoc_search.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chat_tui/app.doc.md) | `docs-src/chat_tui/app.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chat_tui/controller.doc.md) | `docs-src/chat_tui/controller.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chat_tui/highlight_grammars.doc.md) | `docs-src/chat_tui/highlight_grammars.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chat_tui/highlight_registry.doc.md) | `docs-src/chat_tui/highlight_registry.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chat_tui/highlight_theme.doc.md) | `docs-src/chat_tui/highlight_theme.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chat_tui/highlight_tm_engine.doc.md) | `docs-src/chat_tui/highlight_tm_engine.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chat_tui/model.doc.md) | `docs-src/chat_tui/model.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chat_tui/renderer.doc.md) | `docs-src/chat_tui/renderer.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chat_tui/types.doc.md) | `docs-src/chat_tui/types.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chat_tui_renderer2_whitepaper.md) | `docs-src/chat_tui_renderer2_whitepaper.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chatmd/README.md) | `docs-src/chatmd/README.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chatml/README.md) | `docs-src/chatml/README.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chatml-async-completion-lifecycle.md) | `docs-src/chatml-async-completion-lifecycle.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chatml-budget-policy.md) | `docs-src/chatml-budget-policy.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chatml-host-session-controller-contract.md) | `docs-src/chatml-host-session-controller-contract.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chatml-safe-point-and-effective-history.md) | `docs-src/chatml-safe-point-and-effective-history.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../chatml-ui-host-capabilities.md) | `docs-src/chatml-ui-host-capabilities.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../cli/chat-completion.md) | `docs-src/cli/chat-completion.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../cli/shell-runtime-management.md) | `docs-src/cli/shell-runtime-management.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../context_compaction/compactor.doc.md) | `docs-src/context_compaction/compactor.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../context_compaction/config.doc.md) | `docs-src/context_compaction/config.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../context_compaction/relevance_judge.doc.md) | `docs-src/context_compaction/relevance_judge.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../design/ochat-agent-server-implementation-spec.md) | `docs-src/design/ochat-agent-server-implementation-spec.md` | Canonical design; conceptual/compatibility qualifications retained. |
| [page](../design/ochat-agent-server-spec.md) | `docs-src/design/ochat-agent-server-spec.md` | Canonical design; conceptual/compatibility qualifications retained. |
| [page](../development/admin-remediation-notes.md) | `docs-src/development/admin-remediation-notes.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../development/code-documentation-audit.md) | `docs-src/development/code-documentation-audit.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../development/documentation-coverage.md) | `docs-src/development/documentation-coverage.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../development/documentation-worklog.md) | `docs-src/development/documentation-worklog.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../development/final-audit-remediation.md) | `docs-src/development/final-audit-remediation.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../development/library-reference-remediation.md) | `docs-src/development/library-reference-remediation.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../development/readme-content-audit.md) | `docs-src/development/readme-content-audit.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../development/typeahead-verification.md) | `docs-src/development/typeahead-verification.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../examples/README.md) | `docs-src/examples/README.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../examples/agent-server/README.md) | `docs-src/examples/agent-server/README.md` | Current reference/tutorial; offline checker applies. |
| [page](../examples/agent-server/config/README.md) | `docs-src/examples/agent-server/config/README.md` | Current reference/tutorial; offline checker applies. |
| [page](../examples/prompt-patterns.md) | `docs-src/examples/prompt-patterns.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/build-troubleshooting.md) | `docs-src/guide/build-troubleshooting.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/chat_tui.md) | `docs-src/guide/chat_tui.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/chatmd-shell-examples.md) | `docs-src/guide/chatmd-shell-examples.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/chatmd-shell-extensions.md) | `docs-src/guide/chatmd-shell-extensions.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/chatmd-shell-host-integration.md) | `docs-src/guide/chatmd-shell-host-integration.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/chatmd-shell-persistence-and-audit.md) | `docs-src/guide/chatmd-shell-persistence-and-audit.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/chatmd-shell-runtime-internals.md) | `docs-src/guide/chatmd-shell-runtime-internals.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/chatmd-shell-security.md) | `docs-src/guide/chatmd-shell-security.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/chatml-implementation-architecture.md) | `docs-src/guide/chatml-implementation-architecture.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/chatml-language-spec.md) | `docs-src/guide/chatml-language-spec.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/chatml-match-semantics.md) | `docs-src/guide/chatml-match-semantics.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/chatml-moderator-runtime.md) | `docs-src/guide/chatml-moderator-runtime.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/chatml-parsing-and-diagnostics.md) | `docs-src/guide/chatml-parsing-and-diagnostics.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/general-agent-workflow.md) | `docs-src/guide/general-agent-workflow.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/search-and-indexing.md) | `docs-src/guide/search-and-indexing.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/search-examples/README.md) | `docs-src/guide/search-examples/README.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/search-examples/md-search.md) | `docs-src/guide/search-examples/md-search.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/search-examples/ochat-query.md) | `docs-src/guide/search-examples/ochat-query.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../guide/search-examples/odoc-search.md) | `docs-src/guide/search-examples/odoc-search.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/Io.doc.md) | `docs-src/lib/Io.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/README.md) | `docs-src/lib/README.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/agent_client/architecture.doc.md) | `docs-src/lib/agent_client/architecture.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/agent_protocol/architecture.doc.md) | `docs-src/lib/agent_protocol/architecture.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/agent_server/architecture.doc.md) | `docs-src/lib/agent_server/architecture.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/agent_session/architecture.doc.md) | `docs-src/lib/agent_session/architecture.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/agent_session/compaction_archive.doc.md) | `docs-src/lib/agent_session/compaction_archive.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/agent_store/architecture.doc.md) | `docs-src/lib/agent_store/architecture.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/agent_transport_client/architecture.doc.md) | `docs-src/lib/agent_transport_client/architecture.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/agent_transport_http/architecture.doc.md) | `docs-src/lib/agent_transport_http/architecture.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/agent_transport_socket/architecture.doc.md) | `docs-src/lib/agent_transport_socket/architecture.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/agent_transport_stdio/architecture.doc.md) | `docs-src/lib/agent_transport_stdio/architecture.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/apply_patch.doc.md) | `docs-src/lib/apply_patch.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/apply_patch_error.doc.md) | `docs-src/lib/apply_patch_error.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/bin_prot_utils_eio.doc.md) | `docs-src/lib/bin_prot_utils_eio.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/bm25.doc.md) | `docs-src/lib/bm25.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/agent_runtime.doc.md) | `docs-src/lib/chat_response/agent_runtime.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/cache.doc.md) | `docs-src/lib/chat_response/cache.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/chatml_moderation.doc.md) | `docs-src/lib/chat_response/chatml_moderation.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/config.doc.md) | `docs-src/lib/chat_response/config.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/converter.doc.md) | `docs-src/lib/chat_response/converter.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/ctx.doc.md) | `docs-src/lib/chat_response/ctx.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/driver.doc.md) | `docs-src/lib/chat_response/driver.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/fetch.doc.md) | `docs-src/lib/chat_response/fetch.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/fork.doc.md) | `docs-src/lib/chat_response/fork.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/history_stream_event.doc.md) | `docs-src/lib/chat_response/history_stream_event.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/in_memory_stream.doc.md) | `docs-src/lib/chat_response/in_memory_stream.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/mcp_discovery_cache.doc.md) | `docs-src/lib/chat_response/mcp_discovery_cache.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/moderation.doc.md) | `docs-src/lib/chat_response/moderation.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/response_loop.doc.md) | `docs-src/lib/chat_response/response_loop.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_response/tool.doc.md) | `docs-src/lib/chat_response/tool.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/agent_event_apply.doc.md) | `docs-src/lib/chat_tui/agent_event_apply.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/agent_history_layout.doc.md) | `docs-src/lib/chat_tui/agent_history_layout.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/agent_permission_view.doc.md) | `docs-src/lib/chat_tui/agent_permission_view.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/agent_projection.doc.md) | `docs-src/lib/chat_tui/agent_projection.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/agent_security_projection.doc.md) | `docs-src/lib/chat_tui/agent_security_projection.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/agent_session_client.doc.md) | `docs-src/lib/chat_tui/agent_session_client.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/app.doc.md) | `docs-src/lib/chat_tui/app.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/app_compaction.doc.md) | `docs-src/lib/chat_tui/app_compaction.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/app_events.doc.md) | `docs-src/lib/chat_tui/app_events.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/app_reducer.doc.md) | `docs-src/lib/chat_tui/app_reducer.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/app_runtime.doc.md) | `docs-src/lib/chat_tui/app_runtime.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/app_stream_apply.doc.md) | `docs-src/lib/chat_tui/app_stream_apply.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/app_streaming.doc.md) | `docs-src/lib/chat_tui/app_streaming.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/app_submit.doc.md) | `docs-src/lib/chat_tui/app_submit.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/cmd.doc.md) | `docs-src/lib/chat_tui/cmd.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/connection_status.doc.md) | `docs-src/lib/chat_tui/connection_status.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/controller.doc.md) | `docs-src/lib/chat_tui/controller.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/controller_cmdline.doc.md) | `docs-src/lib/chat_tui/controller_cmdline.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/controller_normal.doc.md) | `docs-src/lib/chat_tui/controller_normal.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/controller_shared.doc.md) | `docs-src/lib/chat_tui/controller_shared.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/controller_shell_security.doc.md) | `docs-src/lib/chat_tui/controller_shell_security.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/controller_types.doc.md) | `docs-src/lib/chat_tui/controller_types.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/conversation.doc.md) | `docs-src/lib/chat_tui/conversation.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/highlight_grammars.doc.md) | `docs-src/lib/chat_tui/highlight_grammars.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/highlight_registry.doc.md) | `docs-src/lib/chat_tui/highlight_registry.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/highlight_styles.doc.md) | `docs-src/lib/chat_tui/highlight_styles.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/highlight_theme.doc.md) | `docs-src/lib/chat_tui/highlight_theme.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/highlight_tm_engine.doc.md) | `docs-src/lib/chat_tui/highlight_tm_engine.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/highlight_tm_loader.doc.md) | `docs-src/lib/chat_tui/highlight_tm_loader.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/markdown_fences.doc.md) | `docs-src/lib/chat_tui/markdown_fences.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/model.doc.md) | `docs-src/lib/chat_tui/model.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/path_completion.doc.md) | `docs-src/lib/chat_tui/path_completion.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/persistence.doc.md) | `docs-src/lib/chat_tui/persistence.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/renderer.doc.md) | `docs-src/lib/chat_tui/renderer.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/renderer2.doc.md) | `docs-src/lib/chat_tui/renderer2.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/renderer_component_history.doc.md) | `docs-src/lib/chat_tui/renderer_component_history.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/renderer_component_input_box.doc.md) | `docs-src/lib/chat_tui/renderer_component_input_box.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/renderer_component_message.doc.md) | `docs-src/lib/chat_tui/renderer_component_message.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/renderer_component_status_bar.doc.md) | `docs-src/lib/chat_tui/renderer_component_status_bar.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/renderer_highlight_engine.doc.md) | `docs-src/lib/chat_tui/renderer_highlight_engine.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/renderer_lang.doc.md) | `docs-src/lib/chat_tui/renderer_lang.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/renderer_page_chat.doc.md) | `docs-src/lib/chat_tui/renderer_page_chat.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/renderer_page_shell_security.doc.md) | `docs-src/lib/chat_tui/renderer_page_shell_security.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/renderer_pages.doc.md) | `docs-src/lib/chat_tui/renderer_pages.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/renderer_shell_approval.doc.md) | `docs-src/lib/chat_tui/renderer_shell_approval.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/renderer_shell_security_palette.doc.md) | `docs-src/lib/chat_tui/renderer_shell_security_palette.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/shell_management_service.doc.md) | `docs-src/lib/chat_tui/shell_management_service.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/shell_security_page_state.doc.md) | `docs-src/lib/chat_tui/shell_security_page_state.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/shell_security_snapshot.doc.md) | `docs-src/lib/chat_tui/shell_security_snapshot.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/snippet.doc.md) | `docs-src/lib/chat_tui/snippet.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/stream.doc.md) | `docs-src/lib/chat_tui/stream.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/type_ahead_provider.doc.md) | `docs-src/lib/chat_tui/type_ahead_provider.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/types.doc.md) | `docs-src/lib/chat_tui/types.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/ui_helpers.doc.md) | `docs-src/lib/chat_tui/ui_helpers.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/utf8_edit.doc.md) | `docs-src/lib/chat_tui/utf8_edit.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chat_tui/util.doc.md) | `docs-src/lib/chat_tui/util.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatmd/chatmd_ast.doc.md) | `docs-src/lib/chatmd/chatmd_ast.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatmd/chatmd_import_expansion.doc.md) | `docs-src/lib/chatmd/chatmd_import_expansion.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatmd/chatmd_lexer.doc.md) | `docs-src/lib/chatmd/chatmd_lexer.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatmd/chatmd_parser.doc.md) | `docs-src/lib/chatmd/chatmd_parser.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatmd/chatmd_script_declaration.doc.md) | `docs-src/lib/chatmd/chatmd_script_declaration.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatmd/prompt.doc.md) | `docs-src/lib/chatmd/prompt.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatmd/source_loader.doc.md) | `docs-src/lib/chatmd/source_loader.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatmd_shell_spec/architecture.doc.md) | `docs-src/lib/chatmd_shell_spec/architecture.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatml/chatml_builtin_modules.doc.md) | `docs-src/lib/chatml/chatml_builtin_modules.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatml/chatml_lang.doc.md) | `docs-src/lib/chatml/chatml_lang.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatml/chatml_lexer.doc.md) | `docs-src/lib/chatml/chatml_lexer.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatml/chatml_parser.doc.md) | `docs-src/lib/chatml/chatml_parser.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatml/chatml_resolver.doc.md) | `docs-src/lib/chatml/chatml_resolver.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatml/chatml_typechecker.doc.md) | `docs-src/lib/chatml/chatml_typechecker.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/chatml/frame_env.doc.md) | `docs-src/lib/chatml/frame_env.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/context_compaction/summarizer.doc.md) | `docs-src/lib/context_compaction/summarizer.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/definitions.doc.md) | `docs-src/lib/definitions.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/dune_describe.doc.md) | `docs-src/lib/dune_describe.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/embed_service.doc.md) | `docs-src/lib/embed_service.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/embedding.md) | `docs-src/lib/embedding.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/environment.doc.md) | `docs-src/lib/environment.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/functions.doc.md) | `docs-src/lib/functions.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/github.doc.md) | `docs-src/lib/github.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/gpt_function.doc.md) | `docs-src/lib/gpt_function.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/indexer.doc.md) | `docs-src/lib/indexer.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/jsonaf_ext.doc.md) | `docs-src/lib/jsonaf_ext.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/log.doc.md) | `docs-src/lib/log.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/lru_cache.doc.md) | `docs-src/lib/lru_cache.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/markdown_crawler.doc.md) | `docs-src/lib/markdown_crawler.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/markdown_indexer.doc.md) | `docs-src/lib/markdown_indexer.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/markdown_snippet.doc.md) | `docs-src/lib/markdown_snippet.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/mcp/mcp_client.doc.md) | `docs-src/lib/mcp/mcp_client.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/mcp/mcp_prompt_agent.doc.md) | `docs-src/lib/mcp/mcp_prompt_agent.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/mcp/mcp_server_core.doc.md) | `docs-src/lib/mcp/mcp_server_core.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/mcp/mcp_server_http.doc.md) | `docs-src/lib/mcp/mcp_server_http.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/mcp/mcp_server_router.doc.md) | `docs-src/lib/mcp/mcp_server_router.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/mcp/mcp_tool.doc.md) | `docs-src/lib/mcp/mcp_tool.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/mcp/mcp_transport.doc.md) | `docs-src/lib/mcp/mcp_transport.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/mcp/mcp_transport_http.doc.md) | `docs-src/lib/mcp/mcp_transport_http.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/mcp/mcp_transport_interface.doc.md) | `docs-src/lib/mcp/mcp_transport_interface.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/mcp/mcp_transport_stdio.doc.md) | `docs-src/lib/mcp/mcp_transport_stdio.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/mcp/mcp_types.doc.md) | `docs-src/lib/mcp/mcp_types.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/md_index_catalog.doc.md) | `docs-src/lib/md_index_catalog.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/merlin.doc.md) | `docs-src/lib/merlin.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/meta_prompting/aggregator.doc.md) | `docs-src/lib/meta_prompting/aggregator.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/meta_prompting/context.doc.md) | `docs-src/lib/meta_prompting/context.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/meta_prompting/evaluator.doc.md) | `docs-src/lib/meta_prompting/evaluator.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/meta_prompting/meta_prompting.doc.md) | `docs-src/lib/meta_prompting/meta_prompting.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/meta_prompting/mp_flow.doc.md) | `docs-src/lib/meta_prompting/mp_flow.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/meta_prompting/preprocessor.doc.md) | `docs-src/lib/meta_prompting/preprocessor.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/meta_prompting/prompt_factory.doc.md) | `docs-src/lib/meta_prompting/prompt_factory.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/meta_prompting/prompt_factory_online.doc.md) | `docs-src/lib/meta_prompting/prompt_factory_online.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/meta_prompting/prompt_intf.doc.md) | `docs-src/lib/meta_prompting/prompt_intf.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/meta_prompting/prompting_guides.doc.md) | `docs-src/lib/meta_prompting/prompting_guides.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/meta_prompting/prompts.doc.md) | `docs-src/lib/meta_prompting/prompts.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/meta_prompting/recursive_mp.doc.md) | `docs-src/lib/meta_prompting/recursive_mp.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/meta_prompting/task_intf.doc.md) | `docs-src/lib/meta_prompting/task_intf.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/meta_prompting.doc.md) | `docs-src/lib/meta_prompting.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/mime.doc.md) | `docs-src/lib/mime.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/notty-eio/notty_eio.doc.md) | `docs-src/lib/notty-eio/notty_eio.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/notty_scroll_box.doc.md) | `docs-src/lib/notty_scroll_box.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/oauth/oauth2_client_credentials.doc.md) | `docs-src/lib/oauth/oauth2_client_credentials.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/oauth/oauth2_client_store.doc.md) | `docs-src/lib/oauth/oauth2_client_store.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/oauth/oauth2_http.doc.md) | `docs-src/lib/oauth/oauth2_http.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/oauth/oauth2_manager.doc.md) | `docs-src/lib/oauth/oauth2_manager.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/oauth/oauth2_pkce.doc.md) | `docs-src/lib/oauth/oauth2_pkce.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/oauth/oauth2_pkce_flow.doc.md) | `docs-src/lib/oauth/oauth2_pkce_flow.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/oauth/oauth2_server_client_storage.doc.md) | `docs-src/lib/oauth/oauth2_server_client_storage.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/oauth/oauth2_server_routes.doc.md) | `docs-src/lib/oauth/oauth2_server_routes.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/oauth/oauth2_server_storage.doc.md) | `docs-src/lib/oauth/oauth2_server_storage.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/oauth/oauth2_server_types.doc.md) | `docs-src/lib/oauth/oauth2_server_types.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/oauth/oauth2_types.doc.md) | `docs-src/lib/oauth/oauth2_types.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/ocaml_parser.doc.md) | `docs-src/lib/ocaml_parser.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/odoc_crawler.doc.md) | `docs-src/lib/odoc_crawler.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/odoc_indexer.doc.md) | `docs-src/lib/odoc_indexer.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/odoc_snippet.doc.md) | `docs-src/lib/odoc_snippet.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/openai/completions.doc.md) | `docs-src/lib/openai/completions.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/openai/embeddings.doc.md) | `docs-src/lib/openai/embeddings.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/openai/responses.doc.md) | `docs-src/lib/openai/responses.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/package_index.doc.md) | `docs-src/lib/package_index.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/parallel_tool_calls.doc.md) | `docs-src/lib/parallel_tool_calls.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/prompt_session.doc.md) | `docs-src/lib/prompt_session.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/session.doc.md) | `docs-src/lib/session.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/session_store.doc.md) | `docs-src/lib/session_store.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/shell_access/architecture.doc.md) | `docs-src/lib/shell_access/architecture.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/shell_runtime/architecture.doc.md) | `docs-src/lib/shell_runtime/architecture.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/source.doc.md) | `docs-src/lib/source.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/template.doc.md) | `docs-src/lib/template.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/tikitoken.doc.md) | `docs-src/lib/tikitoken.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/ttl_lru_cache.doc.md) | `docs-src/lib/ttl_lru_cache.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/vector_db.doc.md) | `docs-src/lib/vector_db.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/webpage_markdown/driver.doc.md) | `docs-src/lib/webpage_markdown/driver.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/webpage_markdown/fetch.doc.md) | `docs-src/lib/webpage_markdown/fetch.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/webpage_markdown/html_to_md.doc.md) | `docs-src/lib/webpage_markdown/html_to_md.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/webpage_markdown/md_render.doc.md) | `docs-src/lib/webpage_markdown/md_render.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../lib/webpage_markdown/tool.doc.md) | `docs-src/lib/webpage_markdown/tool.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../meta_prompting/evaluator.doc.md) | `docs-src/meta_prompting/evaluator.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../meta_prompting/recursive_mp.doc.md) | `docs-src/meta_prompting/recursive_mp.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../meta_prompting/templates.doc.md) | `docs-src/meta_prompting/templates.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../notty_examples_research.md) | `docs-src/notty_examples_research.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../notty_examples_research.md.report.md) | `docs-src/notty_examples_research.md.report.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../openai_responses_tool_output.md) | `docs-src/openai_responses_tool_output.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../overview/chatmd-language.md) | `docs-src/overview/chatmd-language.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../overview/chatmd-shell-runtime.md) | `docs-src/overview/chatmd-shell-runtime.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../overview/chatmd-shell-tools.md) | `docs-src/overview/chatmd-shell-tools.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../overview/project.md) | `docs-src/overview/project.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../overview/tools.md) | `docs-src/overview/tools.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../response_api_tool_output_image_support.md) | `docs-src/response_api_tool_output_image_support.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../shell/README.md) | `docs-src/shell/README.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../test/chat_tui_type_ahead_debounce_test.doc.md) | `docs-src/test/chat_tui_type_ahead_debounce_test.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../test/chat_tui_type_ahead_test.doc.md) | `docs-src/test/chat_tui_type_ahead_test.doc.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../tools/README.md) | `docs-src/tools/README.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../tutorials/README.md) | `docs-src/tutorials/README.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../tutorials/file-tool.md) | `docs-src/tutorials/file-tool.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../tutorials/specialist.md) | `docs-src/tutorials/specialist.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
| [page](../tutorials/workflow.md) | `docs-src/tutorials/workflow.md` | Retained reference; host-sensitive entry points reconciled; unrelated algorithms not rewritten. |
