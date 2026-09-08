# Permissions and security

## Identity is not attachment mode

A principal carries scopes. A session attachment carries read-only, read/write,
or exclusive owner-read-write mode. Both checks apply, followed by actor-owned
state, lease and revision checks. A read-only attachment cannot mutate; a writable
attachment cannot widen a transcript-only credential. Read-only admin attachments
may still view security data allowed by their token.

Session visibility normally requires the creating principal, or
`configuration.admin`; transcript scope alone does not share another principal's
session. To give a second client restricted access to the same user's session,
issue a narrower token for the same principal and initialize its own connection.
HTTP connection IDs bind scopes, authentication kind and attributes as well as
principal ID, so a restricted token cannot borrow an admin connection's authority.

Unix clients use same-user peer credentials. HTTP supports hashed static tokens
and explicitly trusted direct reverse-proxy peers. An OAuth validator is an
embedding hook selected by ID, not a bundled authorization service. See
[configuration](configuration.md) and the [method/scope matrix](protocol.md).

## Server tool policy

| Setting | Behavior |
|---|---|
| `tool_default deny` | Reject generic tool invocations. |
| `tool_default allow` | Trusted-automation allowance; does not remove shell/runtime safety checks. |
| `tool_default ask` | Route through permission state and available approvers; unattended handling follows timeout/fallback rules. |
| `tool_default policy` | Use the installed deterministic evaluator; missing integration does not invent policy. |
| `approval_timeout none` | No automatic expiry; an interactive pending request can remain waiting. |
| `approval_timeout_ms N` | Apply configured fallback when the pending request expires. |
| `approval_fallback deny` | Conservative default. |
| `approval_fallback allow` | Explicit unattended trust; review before enabling. |
| `approval_fallback allow_if_policy` | Require installed policy to authorize. |
| `(model_reviewer ID)` / `(external_reviewer ID)` | Resolve a named host implementation and execute a redacted, durable reviewer job. |

For a **new generic invocation**, `ask` with no available responder applies the
fallback immediately; it does not silently create an indefinite wait. A request
already offered to a client can remain pending after that client disconnects when
no expiry is configured. Shell reviewer unavailability follows the shell runtime's
own declared rules. Keep these cases separate when designing headless behavior.

Reviewers have security-relevant revision identities. Exceptions, malformed
responses and unavailable implementations fail closed. Cancellation interrupts
the job and propagates. The stock daemon supplies no arbitrary model/external
reviewer from its name alone; custom embedding must install it.

Permission requests persist identity, offered choices and resolution. Multiple
approvers race through an actor compare-and-set boundary: only a valid first
resolution wins. A stale answer cannot approve another invocation. Grant scope
cannot exceed offered/administratively permitted scopes. Disconnect is not
approval, and a read-only client cannot answer on a writer's behalf.

Requests identify either their model operation (`operation_id`) or an actual
persisted tool invocation (`invocation_id`), never both. Invocation ownership lets
internally installed v1 moderator tools wait for approval while the session has
no model operation. The actor requires a live invocation callback and resolves
outstanding requests on stop or callback cancellation. Resolution restores the
remaining permission wait, or the prior execution state when none remain.
The `call_id` field remains a request correlation key; an invocation-owned script
request may use its invocation ID without creating a provider tool-call item.

Native execution carries its invocation identity through a scoped fiber binding
for shell approval/reviewer adapters. Expired or foreign bindings are rejected;
only an unbound legacy caller falls back to its active model operation. These
internal services do not enable public v1 ChatMD declarations by themselves.

## Shell is a separate authorization layer

Manifest authorization modes are `deny`, `require_grant` and explicitly trusted
`assume_authorized`. Exact grants bind source and canonical manifest hashes,
prompt/workspace and eligible principal identities. Source/authority edits can
invalidate them. Shell per-command policy then checks capabilities, executable
identity, effects, administrative ceilings and reviewers.

The generic gate delegates shell tool decisions to the shell runtime so the same
call does not generate duplicate generic and shell approval prompts. A generic
allow never overrides a shell hard deny or OS backend requirement. See
[shell host integration](../guide/chatmd-shell-host-integration.md).

## Choose an unattended policy deliberately

For a narrow known workflow, prefer explicit tool declarations, limited workspace
admission, a reviewed manifest, deterministic permitted commands, bounded jobs
and timeouts, and denial for unexpected work. For human review, provide an
authorized writable client and choose timeout/fallback behavior when it is absent.
For automated review, install and version the reviewer; verify failures deny.
Do not use YOLO merely to make headless approval prompts disappear.

Transcript-only clients receive scoped snapshots/history/events. Security,
grant, job/schedule, audit and export data are projected independently. Hidden
durable events retain positions but not protected payload. Recoverable provider
streams require security scope because deltas can contain tool arguments.
Export blobs bind their creator's exact projection. Protect the entire store and
backups; UI redaction is not at-rest encryption.

The operator remains responsible for trusted prompts, credentials, workspace
conflicts, external tool servers and sandbox deployment. Workspaces are logical
roots, not mutually untrusted tenants. Private Unix and authenticated loopback
HTTP are the exercised deployment baseline; broader exposure needs deployment
security appropriate to the environment.

Outbound provider transport is another boundary: the existing `Io.Net` provider
plumbing uses a development null TLS authenticator. This is not certificate
verification and is separate from authenticating clients to the agent daemon.
Review the [provider transport warning](../lib/openai/responses.doc.md#security-note-tls)
before deploying on an untrusted network; configuring an incoming proxy does not
automatically correct outbound TLS validation.
