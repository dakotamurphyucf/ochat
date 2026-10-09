# Ochat agent protocol 2.0

Use the same method/envelope contract over [Unix](transports/unix.md),
[stdio](transports/stdio.md), or [HTTP](transports/http.md). Transport framing and
authentication differ; session semantics do not. This is JSON-RPC-style Ochat
protocol, not an MCP endpoint.

The [ChatML extension foundations](extensibility-foundations.md) describe additive
status and host-capability metadata, storage guarantees and the current execution
feature availability.

Protocol 2.0 introduces [neutral transcript projections](../neutral-transcript-protocol.md)
and typed live activity. Version-1 clients must upgrade; this server does not
negotiate their former response shape. The existing `ingress.submit` method and
its dedicated permission scope remain available. Protocol support does not enable
ChatML features on a host where their runtime service is unavailable.

## Initialize and correlate

Send this complete request before other work:

```json
{"jsonrpc":"2.0","id":"initialize","method":"protocol.initialize","params":{"implementation":{"name":"tutorial","version":"1"},"protocol_min":{"major":2,"minor":0},"protocol_max":{"major":2,"minor":0},"features":[],"event_encodings":["json"],"max_inbound_event_bytes":16777216}}
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
| `command.receipt` | `Command_receipt.Request` | Original request authorization plus current session visibility | Read-only bounded committed summary or unresolved/unavailable status; never retries execution. |
| `provider.setup` | `Provider_operator.Setup_request` | `provider.manage` | Explicit create-only trusted host bootstrap; returns host incarnation, never a credential. |
| `provider.status` | `Provider_operator.Status_request` | `provider.view` plus profile policy | Bounded authorized nonsecret profiles, owner flows and current selection/CAS revision. |
| `provider.login.begin` | `Provider_operator.Login_request` | `provider.manage` plus profile policy | Explicit browser/device acquisition; receipt contains only a host-qualified flow reference. |
| `provider.login.challenge` | `Provider_operator.Challenge_request` | `provider.manage` plus exact live flow ownership | Owner-private authorized transport result; no receipt, event, history or audit payload. |
| `provider.login.cancel` | `Provider_operator.Cancel_request` | `provider.manage` plus flow ownership | Cancel/join only the original owned flow; preserve a working login. |
| `provider.logout` | `Provider_operator.Logout_request` | `provider.manage` plus profile policy | Local disable epoch first, then bounded drain and owned cleanup; no provider revocation claim. |
| `provider.select` | `Provider_operator.Select_request` | `provider.select` plus profile policy | Selection revision CAS changes future default captures. Existing targets/plans remain pinned. |
| `provider.configure_environment` | `Provider_operator.Environment_request` | `provider.manage` plus profile policy | Enroll a predeclared host source ID; no arbitrary environment name, key or path. |
| `protocol.ping` | `Ping.Request` | Authentication only | Ping response; not proof of a model completing work. |
| `server.info` | Empty object | Authentication only | Implementation/version/features/transports/limits and unsafe-development-auth indicator. |
| `server.health` | `Health.Request` | Authentication only; details scoped | Current health projection. |
| `prompt.list` | `Prompt.List_request` | `prompt.list` | Paged catalog. |
| `prompt.get` | `Prompt.Get_request` | `prompt.list` | Prompt definition/revision projection. |
| `workspace.list` | `Workspace.List_request` | `workspace.list` | Paged catalog. |
| `workspace.get` | `Workspace.Get_request` | `workspace.list` | Workspace projection. |
| `project.create` | `Organization_request.Create` | `organization.manage` plus owner/admin visibility | Host-qualified logical project create. |
| `project.get` | `Organization_request.Project.Get` | `organization.view` plus owner/admin visibility | Host-qualified logical project get. |
| `project.list` | `Organization_request.List` | `organization.view` plus owner/admin visibility | Host-qualified logical project list. |
| `project.update` | `Organization_request.Project.Update` | `organization.manage` plus owner/admin visibility | Host-qualified logical project update. |
| `project.delete` | `Organization_request.Project.Delete` | `organization.manage` plus owner/admin visibility | Host-qualified logical project delete. |
| `collection.create` | `Organization_request.Create` | `organization.manage` plus owner/admin visibility | Host-qualified logical collection create. |
| `collection.get` | `Organization_request.Collection.Get` | `organization.view` plus owner/admin visibility | Host-qualified logical collection get. |
| `collection.list` | `Organization_request.List` | `organization.view` plus owner/admin visibility | Host-qualified logical collection list. |
| `collection.update` | `Organization_request.Collection.Update` | `organization.manage` plus owner/admin visibility | Host-qualified logical collection update. |
| `collection.delete` | `Organization_request.Collection.Delete` | `organization.manage` plus owner/admin visibility | Host-qualified logical collection delete. |
| `blob.read` | `Blob.Read_request` | `session.transcript.read` plus blob ownership | Bounded chunk with cursor and metadata. |
| `session.create` | `Session.Create_request` | `session.create` plus requested attachment scopes | Session, mutation acknowledgement and optional attachment/replay. |
| `session.list` | `Session.List_request` | `session.transcript.read` | Paged visible sessions with filters. |
| `session.get` | `Session.Get_request` | `session.transcript.read` | Scoped snapshot; optional history window. |
| `session.configuration_get` | `Session_configuration.Get_request` | `session.transcript.read` | Safe selected/captured configuration and independent configuration revision; profile identity requires `diagnostics.read`. |
| `session.configuration_update` | `Session_configuration.Update_request` | `session.message.send`; profile patches also require `provider.select` | Writable attachment, expected generation/configuration revision and nonempty model/profile/settings patch; next root capture selection. |
| `session.inference_summary` | `Inference_query.Summary_request` | Authentication plus session visibility | Safe retained totals, coverage and host turns; not lifetime totals or pricing. |
| `session.inference_observations` | `Inference_query.Request` | `session.transcript.read`; configuration and diagnostics also require `diagnostics.read` | Ordered, paginated retained attempts, with scoped disclosure. |
| `session.attach` | `Session.Attach_request` | `session.transcript.read` plus requested mode | Attachment, replay decision, sequence and optional reclaim token. |
| `session.detach` | `Session.Detach_request` | `session.transcript.read` | Detach supplied attachment; idempotent mutation acknowledgement. |
| `session.renew_owner` | `Session.Renew_owner_request` | `session.transcript.read` plus valid owner lease | Renew matching generation; owner lease and mutation result. |
| `session.start` | `Session.Start_request` | `session.message.send` | Writable attachment; start or queue if permitted. |
| `session.update_metadata` | `Session_metadata.Request` | `session.message.send` | Writable attachment; expected metadata revision; rename and label patch. |
| `session.update_organization` | `Session_organization.Request` | `session.message.send` and `organization.manage` | Current writable attachment; shared expected metadata revision; project set/clear and collection add/remove. |
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

Inference queries read persisted observations without activating a session. Cursors
bind the authenticated principal, scopes and accounting revision; changes require
a fresh query. See [inference observations](../lib/inference-observations.md) for
retention, response limits, disclosure and the OCaml client contract.

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
upgrade; workspace state. Recoverable payloads contain typed `Transcript.Stream`
observations or `Activity.Tool` start/progress/finish observations with actual
source/attempt identity and optional parent attribution. Public durable payloads
are defined by `Public.Durable`; `Event.Recoverable` defines the live envelope.

Client synchronization:

1. Initialize and establish an attachment/projection.
2. Apply snapshot or bounded replay in sequence, then process notifications.
3. Track stable history and operation IDs; preserve local drafts separately.
4. Redacted/hidden durable events still advance sequence. Do not infer missing
   protected data by treating hidden payloads as deserialization failures.
5. Protocol 2.0 does not expose replay of recoverable deltas. They are live-only
   notifications; their operation sequence is not an accepted reconnect cursor.
   Recover through durable replay or a replacement snapshot, then resume live
   notifications. Finalized canonical state remains authoritative.
6. On replay loss, replace from a scoped snapshot, including active-call summaries;
   do not append duplicate rows or preserve a terminal stale loading indicator.
7. On reconnect, re-establish identity/connection/attachment as required, then
   replay after the last durable position or request replacement.

Transcript-only principals receive whitelisted messages and reasoning, with
explicitly redacted tool/unknown bodies. Full history and recoverable live content
require both transcript and security scopes. Permission summaries require
`security.read`, grants require `grant.manage`, and jobs/schedules require
`session.message.send`. Export and snapshot caching use the same principal
projection; no unscoped cache reuse.

Public history rows carry stable IDs and provenance with `Full`, `Visible`, or
`Redacted` bodies. Developer remains distinct from System in both canonical
semantics and visible message views. Readable or redacted views cannot become
canonical input. The older coarse role fields belong only to private persistence
codecs; clients use `Public.History`.

A subscription that cannot deliver an admitted durable event emits
`session.stream_error` with its session and attachment IDs. Clients mark that
attachment stale and recover through a fresh snapshot; this notification does
not consume a durable sequence.

## Pagination and history windows

List requests carry `limit` and optional `cursor`, plus method-specific filters.
Signed cursors bind identity/scopes, query, collection and host; changing any may
invalidate them. Session lists preserve the requested created/updated/name sort,
with ascending session-ID ties. Default order is created time ascending; default
archive selection is active records. Existing `owner_principal_id` is a deprecated
creator filter; `creator_principal_id` names that role explicitly, while
`active_owner_principal_id` filters the current unexpired owner lease. Conflicting
creator aliases are invalid. Catalog results extend session JSON with `archived`
and optional `active_owner_principal_id`; absent legacy fields mean false/none.

Session cursors whose signed result data changed return `conflict` with
`data.refresh_required=true`; query/principal/restart changes return an expired
or invalid cursor. Streaming updates can change `updated_at` and invalidate a
catalog cursor. Explicitly restart the listing after choosing to refresh; do not
edit a cursor or silently combine pages from different catalog observations.
`Admin.list_sessions_page` returns one rich catalog page, and
`Admin.enumerate_sessions` completes all pages within explicit session/page bounds
or returns an error. Legacy `Admin.list_sessions` maps entries to sessions and
uses documented bounds of 100000 sessions and 100 pages.

Metadata edits carry `expected_metadata_revision`, independently of streaming
transaction revisions. Patches can set/clear names and set/remove labels, reject
ambiguous duplicate/overlapping keys, and commit both persisted identity and
public spec mirrors together. A metadata no-op preserves that metadata revision
while recording its command receipt. Membership implementations share this
organization revision. Names/labels never change the execution workspace, provider
selection or tool grants. Archived records are listed without runtime activation;
restoration remains a lifecycle operation.

Configuration reads and updates do not start a stopped session or dispatch a
provider request. An update supplies `expected_generation`,
`expected_revision` (the independent configuration revision), a writable
`attachment_id` and its original `idempotency_key`. The patch can supply a
nonempty model, an explicitly authorized compatible profile, and validated
named settings with omitted/null/value distinctions. Unknown retained target
fields survive updates. Credentials are resolved only by the runtime host and
never appear in these views or patches; profile selection cannot switch paid
account, endpoint or credential binding.

Each accepted fresh update advances configuration revision once, including a
repeated identical intent. Retrying the exact original command replays its
original result; `command.receipt` returns the committed session ID and
configuration revision. A fresh command with stale generation/revision returns
`conflict`. Profile patches require `provider.select` on fresh admission, replay
and original receipt lookup; ordinary model/settings reads do not require broad
diagnostics permission.

The returned view separates selected intent from a root capture. `preparing`
means resolution/preparation is in progress; `effective` identifies a currently
dispatched immutable root request; `retained` is historical capture evidence.
`pending` flags selected intent that differs from an existing retained capture;
a stopped session with no capture has no pending flag. An admitted request,
nested execution or job keeps its original context. Changes take effect at the
next root request preparation boundary, including after an active native tool;
they do not rewrite an already dispatched request. Failed/cancelled preparation
clears preparing ownership and does not invent a dispatch. Restored sessions
retain selection but have no process-local currently effective capture.

Restart other list methods on `invalid_request` rather than editing a cursor. History windows support bounded before/after/tail/cursor selectors and
canonical/effective views. Partial windows advertise structural incompleteness;
complete them before using them as model context.

All types and generated inventories are refreshed with `@agent-docs-check`;
see [testing](testing.md). The [wire implementations](../../lib/agent_protocol/command.ml)
remain authoritative for exact encoding and validation.

## Reconcile an uncertain command

`command.receipt` is a read-only lookup of an original generic idempotent request.
Supply its original method and bounded original parameter object. The server
decodes the original command, rechecks its current requested-mode permissions,
computes the original canonical digest and looks up the current authenticated
principal's receipt. It never executes the original command. Current session
visibility is checked before any stored outcome is disclosed, including the
session created by a lost create reply.

Outcomes are `missing`, `pending`, `failed`, `committed` or `unavailable`.
Missing/expired receipts do not establish noncommit. Unavailable discloses no
deleted session identity when current visibility cannot be proved. Committed
results contain narrow nonsecret effect references, not a replay of the original
history/result. Create returns the session identity. Attach returns its session
identity with `reattach_required`, never an old lease or reclaim token.

Client connections retain original uncertain intents before submission, with
64-intent and 16 MiB aggregate bounds. Capacity exhaustion rejects before effects;
unknown intents are never silently evicted. An equivalent fresh-key request is
blocked until reconciliation or explicit operator abandonment, while independent
requests remain possible. Reconnect transfers intents only after confirming the
same server and principal. Malformed successful transport responses remain
uncertain even if their decoder reports invalid input; authenticated server
errors retain their separate meaning. No exactly-once guarantee is implied.

Host-qualified references pair the persisted server ID with a session ID.
Client-owned named connection profiles persist nonsecret endpoint descriptions,
optional server pins and daemon credential-file references in versioned documents.
They contain no provider credentials or authority. Profile connection initializes
and checks its server pin before sensitive operations. Provider profile selection
is a separate host service.

## Provider operator privacy and recovery

Provider methods require an explicitly installed trusted host service. Protocol
support alone does not provision a registry or discover a login. The host owns
credentials; requests never forward keys, filesystem roots or arbitrary environment
bindings. View, management and selection scopes are independent of session ownership
and configuration administration. Per-profile policy and exact authenticated flow
ownership apply in addition to the method scope. An incoming flow reference is
untrusted; the service must compare its host/profile/ID/expiry with its owner record.

A login begin result and every command receipt contain nonsecret references only.
Fetch a live challenge through the separate owner-private method after current
scope, ownership and expiry checks. `Public.Result.Private_provider_challenge` has
an authorized transport codec and redacted sexp. Generic internal `Method_result`
codecs and `Non_history` consumers refuse it. Do not serialize a challenge into
receipts, events, history, audit or debug output; render it only to the authorized
interactive operator sink. Restart reports interrupted acquisition rather than
resuming code exchange. An uncertain mutation reply must reconcile the original
operation; a missing receipt does not prove noncommit and must not trigger a fresh
key or exchange retry.

`Setup_result.revision` identifies the host registry incarnation. Selection CAS
uses the distinct revision returned in `Status_result.selection`; it is not an
auth epoch or secret revision. Status never includes an authorization URI, device
code or raw provider response, and it does not infer a failure from readiness.


Logical projects and collections are private organization owned by their creator
on one initialized host. They carry names and revisions; they configure no execution
workspace, provider, prompt, grants or session authority. Organization reads require
`organization.view`; mutations and receipt reconciliation require `organization.manage`.
Each operation also requires creator ownership or the existing host configuration
administrator authority. Trusted local principals explicitly carry both scopes;
configured network principals must receive deliberate grants. Names need not be unique.

Create generates a distinct opaque ID. Update/delete require the current nonnegative
revision; a successful name change/delete advances it. A no-op rename preserves that
revision and still records its receipt. Delete retains a permanent tombstone, never
reuses the ID, and performs no session cascade. Membership is a separate service.
Lists exclude tombstones, sort by creation time then typed ID ascending, expose every
page and offer explicitly bounded client enumeration. Signed cursors bind current
principal/scopes, query and visible data; changed data returns refresh-required conflict.

The authoritative host organization document commits mutation and exact terminal
idempotency receipt together. Same key/params replays the original result after current
authorization; changed params conflict. Standard receipts expire after 24 hours;
4096 unexpired receipts and 4096 retained IDs per kind are maximums, with a shared
16 MiB complete-document limit. Capacity exhaustion rejects rather than evicting retry
evidence. `command.receipt` returns narrow group ID/revision references and never executes
or retries the original command. Root schema 2 requires organization authority. An older
schema 1 host installs or preserves that document before publishing schema 2, retaining
unknown fields and creation time. A schema 2 host missing authority fails admission;
uncertain replacement acknowledgements make organization reads unavailable until reopen.

The cross-transport conformance scenario exercises all ten organization methods
over Unix, HTTP, and both stdio gateway routes. It checks exact idempotent
results, conflicting key reuse, compare-and-swap conflicts, ordered continuation
pages, cursor invalidation after rename, deletion visibility and retained
historical receipts. A separate daemon restart scenario retains project and
collection identity and original create receipts. A principal granted only
organization view/manage cannot inspect another creator's groups or read
sessions; these operations leave the session catalog empty.

### Session project and collection membership

`session.update_organization` changes logical organization IDs without changing
execution workspace, prompt, provider, content permissions or running operations.
Its request includes `host_id`, `session_id`, `attachment_id`,
`expected_metadata_revision`, `patch` and `idempotency_key`. In the patch,
`project_id` omitted means keep, null means clear and an ID means set;
`add_collections` and `remove_collections` are optional distinct ID lists.
Each list and the resulting collection membership are bounded to 128 IDs.
A collection can contain sessions from different projects.

The host checks current writer authority and `organization.manage` before an
ordinary retry returns its original receipt. Every requested set/add group must
belong to this host and be owned by the principal or accessible to a configuration
administrator. Retained tombstoned IDs can be repeated without changing state;
new references must be live when admitted with the session commit. Membership
shares the metadata revision with rename/label updates. A changed membership
increments it once; a no-op preserves it; a stale revision conflicts. An exact
idempotency retry returns the original result rather than applying the patch again.

`command.receipt` permits reconciliation from a fresh authenticated transport after
a lost reply. It checks current principal, scopes, session visibility, retained
group authorization and the original request digest, without requiring the old
connection's attachment. Revoked scopes or group access deny this read.

The session's `organization` contains historical project and collection IDs.
Deleting a group leaves these durable references intact and never deletes a
session. Catalog entries additionally expose `effective_organization`: live groups
visible to the caller. Without `organization.view`, this projection is empty;
no organization scope grants access to session content. Explicit `project_filter`
(`unassigned` or a project ID) and `collection_all_of` filters require
`organization.view` and visibility of referenced groups. Catalog cursors bind the
checked host organization revision as well as query, principal authority and
session data; an organization mutation invalidates a cursor with an explicit
refresh conflict. Listing does not activate stopped or archived sessions.
