# Ochat agent protocol 1.1

Use the same method/envelope contract over [Unix](transports/unix.md),
[stdio](transports/stdio.md), or [HTTP](transports/http.md). Transport framing and
authentication differ; session semantics do not. This is JSON-RPC-style Ochat
protocol, not an MCP endpoint.

The [ChatML extension foundations](extensibility-foundations.md) describe additive
status and host-capability metadata, storage guarantees and the current execution
feature availability.

Protocol 1.1 adds `ingress.submit` and its dedicated permission scope. Servers also
negotiate 1.0 for existing clients; those initialization responses omit the new
scope, preserving the older closed permission vocabulary. Ingress submission
requires negotiation of at least 1.1. A supported protocol method does not enable
ChatML features on a host where their qualified runtime service is unavailable.

## Initialize and correlate

Send this complete request before other work:

```json
{"jsonrpc":"2.0","id":"initialize","method":"protocol.initialize","params":{"implementation":{"name":"tutorial","version":"1"},"protocol_min":{"major":1,"minor":0},"protocol_max":{"major":1,"minor":1},"features":[],"event_encodings":["json"],"max_inbound_event_bytes":16777216}}
```

The response identifies the negotiated protocol, server/features/limits. Reject an
unsupported version; do not silently assume a future version is compatible.
Request IDs may be strings or numbers and are returned exactly. Notifications have
no request ID. Response envelopes contain `result` or `error`; notifications use
`method` and `params`. The [validated discovery stream](../examples/agent-server/clients/discover.ndjson)
shows complete requests. List methods require a positive `limit` even when other
filters are absent. Server limits may be narrower than the codec's encoded range.

The [generated type reference](protocol-types.md) includes every public request,
result, event, nested content shape, enum, error and scope, with links to the
actual JSON encoders/decoders. It is regenerated from public interfaces by the
documentation check. OCaml `option` denotes optional data, not permission to add
unknown wire fields; enum/tag encoding is defined by the linked codec.

## Method reference

All methods below use the corresponding `Command` variant and `Method_result`
variant of the same name in the [type reference](protocol-types.md). Request type
names identify the complete field schema there. All mutations require applicable
actor state and authorization, not merely passing JSON validation.

| Method | Request type | Minimum method scope | Result / behavior |
|---|---|---|---|
| `protocol.initialize` | `Initialize.Request` | Authentication only | Negotiate protocol; must precede other methods. |
| `protocol.ping` | `Ping.Request` | Authentication only | Ping response; not proof of a model completing work. |
| `server.info` | Empty object | Authentication only | Implementation/version/features/transports/limits and unsafe-development-auth indicator. |
| `server.health` | `Health.Request` | Authentication only; details scoped | Current health projection. |
| `prompt.list` | `Prompt.List_request` | `prompt.list` | Paged catalog. |
| `prompt.get` | `Prompt.Get_request` | `prompt.list` | Prompt definition/revision projection. |
| `workspace.list` | `Workspace.List_request` | `workspace.list` | Paged catalog. |
| `workspace.get` | `Workspace.Get_request` | `workspace.list` | Workspace projection. |
| `blob.read` | `Blob.Read_request` | `session.transcript.read` plus blob ownership | Bounded chunk with cursor and metadata. |
| `session.create` | `Session.Create_request` | `session.create` plus requested attachment scopes | Session, mutation acknowledgement and optional attachment/replay. |
| `session.list` | `Session.List_request` | `session.transcript.read` | Paged visible sessions with filters. |
| `session.get` | `Session.Get_request` | `session.transcript.read` | Scoped snapshot; optional history window. |
| `session.attach` | `Session.Attach_request` | `session.transcript.read` plus requested mode | Attachment, replay decision, sequence and optional reclaim token. |
| `session.detach` | `Session.Detach_request` | `session.transcript.read` | Detach supplied attachment; idempotent mutation acknowledgement. |
| `session.renew_owner` | `Session.Renew_owner_request` | `session.transcript.read` plus valid owner lease | Renew matching generation; owner lease and mutation result. |
| `session.start` | `Session.Start_request` | `session.message.send` | Writable attachment; start or queue if permitted. |
| `session.stop` | `Session.Stop_request` | `session.stop` | Writable attachment; graceful/cancel stop. |
| `session.cancel_operation` | `Session.Cancel_operation_request` | `session.message.send` | Writable attachment; target current operation ID. |
| `session.send_message` | `Session.Send_message_request` | `session.message.send` | Writable attachment; history ID, started/deferred disposition and optional operation ID. |
| `session.compact` | `Session.Compact_request` | `session.message.send` | Writable attachment; optional expected revision; starts compaction. |
| `session.delete_history` | `Session.Delete_history_request` | `session.message.send` | Writable attachment; required expected revision; remove a canonical occurrence and matching tool pair while idle/stopped. |
| `session.export` | `Session.Export_request` | `session.transcript.read` | Authorized attachment; format/revision/window, principal-bound blob. |
| `session.reset` | `Session.Reset_request` | `session.own` | Writable attachment; required expected revision and explicit preservation flags. |
| `session.rebuild` | `Session.Rebuild_request` | `session.own` | Writable attachment; expected revision, pinned/current-catalog choice. |
| `session.upgrade_prompt` | `Session.Upgrade_prompt_request` | `session.own` | Writable attachment; expected revision, target revision, migration flag. |
| `session.delete` | `Session.Delete_request` | `session.delete` | Writable attachment; expected revision, archive/remove policy; confirmation equals session ID. |
| `permission.list` | `Permission.List_request` | `security.read` | Paged permission state. |
| `permission.respond` | `Permission.Respond_request` | `permission.respond` | Writable attachment; offered decision, identity and compare-and-set checks. |
| `grant.list` | `Grant.List_request` | `grant.manage` | Paged grant state. |
| `grant.revoke` | `Grant.Revoke_request` | `grant.manage` | Writable attachment; revoke matching grant. |
| `audit.read` | `Audit.Read_request` | `audit.read` | Redacted audit records; not executable replay. |
| `job.list` | `Job.List_request` | `session.message.send` | Paged jobs; no generic external job-create method. |
| `job.get` | `Job.Get_request` | `session.message.send` | Job state/delivery projection. |
| `job.cancel` | `Job.Cancel_request` | `session.message.send` | Writable attachment checked inside actor; cancel job. |
| `schedule.list` | `Schedule.List_request` | `session.message.send` | Paged schedules. |
| `schedule.get` | `Schedule.Get_request` | `session.message.send` | Schedule state. |
| `schedule.create` | `Schedule.Create_request` | `session.message.send` | Writable attachment; persist timer/delivery intent. |
| `schedule.cancel` | `Schedule.Cancel_request` | `session.message.send` | Writable attachment; cancel schedule. |
| `ingress.submit` | `Ingress.Submit_request` | `ingress.submit` | Protocol 1.1; exact producer-bound registration; durable data acceptance acknowledgement. |

Read-only attachments cannot authorize writer operations or permission answers.
Ingress has separate authority: it needs its dedicated scope and matching
registration, without an attachment or transcript permission. Read scopes are
not stripped by read-only mode. Additional session,
blob, owner-lease and revision checks still apply beyond this minimum-scope table.

## Requests and results

### Registered external data

`ingress.submit` uses a closed version-1 request containing `session_id`,
`registration_id`, `namespace`, `idempotency_key` and `payload`. The authenticated
connection supplies producer identity; a caller-supplied `producer` field is rejected.
The principal needs `ingress.submit`, session visibility and the exact registration's
producer identity. Administrative visibility alone does not authorize another
producer's registration. No writer attachment or transcript access is required.

The host validates namespace/schema, bounded payload, rate/storage limits, current
source/generation and registration lifetime/revocation. Accepted data is saved with
its queue frame before acknowledgement. The version-1 result has `status: "accepted"`,
session/registration/event IDs, idempotency key, payload digest and acceptance time.
It does not claim handler execution, subscription completion or a model response.

Retry the same key and payload to recover the same acknowledgement after reconnect.
Changing its payload conflicts. Every retry rechecks current authority, including
explicit revocation; the general command response cache is not used for this method.
Already accepted data can remain runnable after producer revocation; cancelling or
advancing the subscription invalidates queued delivery. See the
[ingress execution contract](extensibility-foundations.md#external-data-ingress-foundation).

### Sessions and messages

Obtain catalog IDs from `prompt.list`/`workspace.list`, not by converting friendly
configuration names yourself. `Session.Spec` chooses host, prompt reference,
workspace request, liveness, persistence, start intent, optional profile/display
name and labels. Daemon sessions select configured catalogs; local-path host
options belong to embedded execution, not arbitrary remote filesystem access.

Create can return an attachment; otherwise attach explicitly. Keep session,
attachment, operation, history, permission and blob IDs distinct. Never use IDs
as paths. An attach response can report current state, replayed durable events or
a snapshot. Apply it before treating live notifications as an initialized view.

Message content supports plain text or ChatMD plus blob attachments. Validate
content parts, tool-output/image encodings and size constraints against the
history/blob codecs. Untrusted tool result text is data, not a protocol command.

ChatMD message admission converts the first `<user>` element (including its
inline helpers), rather than appending arbitrary transcript roles or tool-call
records. Text not starting with `<` is wrapped in a user element; missing-user
or parse failures are returned as errors. Root prompt declarations still come
from the configured, pinned source, not from a client message.

`session.delete_history` requires `session_id`, `attachment_id`, `history_id`,
nonnegative `expected_revision`, and `idempotency_key`. Obtain the stable history
ID from a current snapshot, not a row index or provider item ID. It rejects
stale revisions, read-only attachments, active operations and borrowed idle
moderator execution. The result is a `Session_mutation`; committed
`history.replaced` events update subscribers. It removes the nearest matching
function/custom call-result pair without crossing another same-direction
occurrence with the same call ID. It neither executes nor reverses tools.

`Snapshot.archived_revisions` lists pre-change archive revisions from compaction,
reset, rebuild and prompt upgrade, newest
first (older peers may omit it; the decoder defaults to an empty list).
`session.export.revision` may be absent/current or one of those archived
revisions, not an arbitrary old journal revision. Historical export uses the
current principal projection and blob authorization. Missing/corrupt archive
files fail instead of falling back to current history. See
[history and archives](sessions-and-workspaces.md#history-and-synchronization).

Administrative methods carry explicit expected revisions where specified.
`session.reset` preservation flags are independent; inspect the current snapshot
before choosing them. `session.rebuild` is not an alias for restart. Session deletion
requires `confirmation` exactly equal to the session ID and can remove data;
prefer archive when recovery is needed. See [operations](operations.md).

## Errors, idempotency, and acknowledgement

Errors carry a typed code, message, retryable flag and data. The complete code set
is in `Protocol_error` in the [type reference](protocol-types.md). Typical classes:
invalid request/method/version, authentication/permission denial, unknown IDs,
attachment/lease conflicts, stale revision, capacity/resource limit, snapshot
required, persistence failure and server draining. Treat `retryable` as guidance,
not permission to retry a changed or irreversible operation blindly.

For methods with `idempotency_key`, generate a new key for each new mutation and
retain it until the outcome is resolved. Retry the same payload/key after an
uncertain reply. A different payload under the same key is a conflict. The store
uses a fixed one-day expiry for Standard receipts; maintenance prunes them after
expiry. Protected receipts (including message submission, compaction, history
deletion, reset/rebuild/upgrade, session deletion and job/schedule mutations) do
not expire through that pruning. This is not a configurable retention interval
or an exactly-once external-execution guarantee. Keys are nonempty, up to 256 characters, using alphanumeric
characters or `-_.:/`. Revision preconditions and idempotency solve different
problems. Acknowledgement is not terminal model/tool completion, and durability
depends on selected flush mode. No exactly-once external side-effect guarantee.

## Events and client projection

`session.event` carries a durable event with session ID, sequence, revision,
timestamp, kind, visibility and payload. `session.live_event` carries recoverable
operation-scoped data with operation sequence and durable anchor. Do not advance
the durable cursor with an operation sequence.

Durable families include session creation/state/update/error; owner changes;
deferred/appended/replaced history; moderator overlays/notices; requested/resolved
permissions; created/revoked grants; started/completed/failed/cancelled/interrupted
operations; job transitions; schedule creation/transitions/cancellation; prompt
upgrade; workspace state. Recoverable families include provider/sourced/history-
correlated streams, tool start/progress/trace/finish, agent classification/progress,
activity and compaction progress. Exact payload shapes are in `Event`.

Client synchronization:

1. Initialize and establish an attachment/projection.
2. Apply snapshot or bounded replay in sequence, then process notifications.
3. Track stable history and operation IDs; preserve local drafts separately.
4. Redacted/hidden durable events still advance sequence. Do not infer missing
   protected data by treating hidden payloads as deserialization failures.
5. Protocol 1.0/1.1 does not expose replay of recoverable deltas. They are live-only
   notifications; their operation sequence is not an accepted reconnect cursor.
   Recover through durable replay or a replacement snapshot, then resume live
   notifications. Finalized canonical state remains authoritative.
6. On replay loss, replace from a scoped snapshot, including active-call summaries;
   do not append duplicate rows or preserve a terminal stale loading indicator.
7. On reconnect, re-establish identity/connection/attachment as required, then
   replay after the last durable position or request replacement.

Transcript-only principals receive finalized text/redacted tool placeholders.
Recoverable streams and tool/permission summaries require `security.read`, grants
require `grant.manage`, jobs/schedules require `session.message.send`. Export and
snapshot caching use the same principal projection; no unscoped cache reuse.

History entry `role` is a coarse protocol classification: both system and
developer input messages have outer `role: "system"`. The exact role remains
in `payload.role` (`"developer"` for developer instructions). Model reconstruction
uses the payload, not the coarse classification; this does not convert developer
instructions into system messages. ChatML `Item.role` also reports the exact role.

## Pagination and history windows

List requests carry `limit` and optional `cursor`, plus method-specific filters.
Signed cursors bind identity/scopes, query, collection and host; changing any may
invalidate them. Restart the listing on `invalid_request` rather than editing a
cursor. History windows support bounded before/after/tail/cursor selectors and
canonical/effective views. Partial windows advertise structural incompleteness;
complete them before using them as model context.

All types and generated inventories are refreshed with `@agent-docs-check`;
see [testing](testing.md). The [wire implementations](../../lib/agent_protocol/command.ml)
remain authoritative for exact encoding and validation.
