# Agent-server configuration

The [source appendix](operator-contracts.md) contains the complete current config
record, scope names and HTTP validation contract. It is checked against source;
the annotations below explain operator-facing behavior and defaults.

Use an absolute `-config` path. Relative paths within the file resolve against
its directory, not the workspace or shell cwd. Unknown fields and duplicate
sections fail closed. Config validation reads referenced sources; it does not
execute tools or start a listener.

Start with the [generated private example](../examples/agent-server/README.md).
The annotated example below uses deployment placeholders and is not runnable
until those paths exist.

## Complete version 1 example

```lisp
(version 1)

(server
 ((data_dir /var/lib/ochat)
  (unix_socket /run/ochat/agent.sock)
  (shutdown_grace_ms 30000)
  (max_attachments_per_session 1024)
  (subscriber_queue_capacity 512)
  (http
   ((enabled true)
    (address 127.0.0.1)
    (port 8787)
    (require_auth true)
    (static_tokens_file /etc/ochat/http-tokens.sexp)
    (max_connections 1024)
    (idle_connection_timeout_ms 300000)))
  (event_retention
   ((completed_stream_ms 3600000)
    (response_artifact_ms 3600000)
    (max_events_per_session 100000)))
  (durability
   ((journal_flush each)
    (journal_flush_ms 50)
    (snapshot_every_events 100)
    (snapshot_every_ms 5000)))
  (job_limits
   ((daemon_total 16)
    (per_principal 8)
    (per_prompt 8)
    (per_workspace 8)
    (per_session 4)
    (per_kind 16)
    (max_nested_depth 8)))
  (unsafe_allow_unauthenticated_remote_http false)))

(workspaces
 (((id project)
   (source (physical /srv/projects/example))
   (access shared_write)
   (conflict_domain example-project)
   (prompt_limits
    (((prompt coding)
      (max_root_agents 4)
      (overflow queue)))))
  ((id disposable)
   (source
    (temporary
     ((location session_dir)
      (cleanup on_session_delete))))
   (access exclusive)
   (prompt_limits
    (((prompt coding)
      (max_root_agents 1)
      (overflow reject)))))))

(prompts
 (((id coding)
   (path /etc/ochat/prompts/coding.chatmd)
   (description "Coding agent")
   (allowed_workspaces (project disposable))
   (permission_profile interactive)
   (enabled true))))

(permission_profiles
 (((id interactive)
   (tool_default ask)
   (approval_timeout none)
   (approval_fallback deny)
   (manifest_authorization require_grant))))

(manifest_grants ())
```

Unknown sections and fields fail validation. Prompt files, imported ChatMD
sources, physical workspace directories, cross-references, authentication
files, and configured identifiers are validated before startup.

## Server configuration

`data_dir` is the daemon-owned durable store. It contains server identity,
session metadata, journals, snapshots, prompt artifacts, blobs, audit data,
exports, and indexes. Only one daemon process may own it. Store and session
lock contention returns a typed error; library code does not terminate the
process to resolve contention.

`unix_socket` is always enabled. Its parent directory must already be private,
owned by the effective user, and not writable by other users. Startup probes
an existing socket before removing a stale node. Unix clients authenticate
from same-user peer credentials and receive a stable UID-derived principal.

`shutdown_grace_ms` is the configured bound for graceful process shutdown.
The current daemon stops schedulers, cancels loaded foreground work, writes a
consistent checkpoint for every loaded durable session, closes subscribers,
and releases session and data-root locks. Crash recovery classifies any
committed nonterminal work as interrupted rather than silently rerunning an
uncertain side effect.

`max_attachments_per_session` limits all attachments across all clients.
`subscriber_queue_capacity` bounds each live subscriber. Slow subscribers are
closed without blocking the agent; clients recover through durable replay or
a replacement snapshot.

`max_events_per_session` bounds the in-memory durable replay window and is the
`maximum_events` advertised during initialization. `response_artifact_ms` bounds
raw response-artifact lifetime. The legacy `completed_stream_ms` spelling is
accepted only as the default for an omitted `response_artifact_ms`; it does not
retain or enable replay of live deltas. Protocol 1.0 recovers through durable
events and snapshots, not a completed-stream replay API. The maintenance service removes
expired response files with Eio without following symbolic links. Journals are
pruned only behind an installed snapshot. Snapshot retention keeps the current
checkpoint and one validated fallback. Journal pruning uses the older retained
checkpoint's transaction sequence, so a truncated current snapshot can still
recover through the fallback and subsequent committed transactions. Each
checkpoint seals a nonempty journal segment; repeating this for an empty segment
does not create additional segments. These operations run under the session's
serialized writer and do not race journal appends.

Durability modes are:

- `each`: synchronize every accepted transaction before acknowledgement.
- `interval`: group synchronization according to `journal_flush_ms`.
- `unsafe_buffered`: acknowledge buffered writes without a persistence
  guarantee; use only when loss after process or host failure is acceptable.

Snapshots are considered after either `snapshot_every_events` committed events
or `snapshot_every_ms`. Graceful shutdown also requests an actor-serialized
checkpoint so state and its transaction hash cannot be paired across a racing
commit.

Job limits are enforced hierarchically for daemon, principal, prompt,
workspace, session, kind, and nested job depth. Capacity is acquired before a
job is claimed and released for every terminal or failed-start path.

## Workspaces

A physical workspace uses an existing canonical directory and is never
deleted by session cleanup. A temporary workspace is created by the server for
one session:

- `location system_tmp` creates it under the system temporary area unless a
  `managed_root` is configured.
- `location session_dir` creates it inside the durable session directory.
- `cleanup on_session_stop`, `on_session_delete`, or `retain` controls the
  lifecycle of server-managed temporary data.

Workspace access modes are:

- `read_only`: a runtime-level read-only workspace contract.
- `shared_write`: multiple eligible root sessions may use the same conflict
  domain concurrently.
- `exclusive`: one running root session may hold the conflict-domain lease.

`conflict_domain` lets multiple configured workspace entries share one quota
or exclusivity identity. If omitted, the resolver derives a stable identity
from the canonical workspace.

Each prompt/workspace pair may set `max_root_agents` and choose `reject` or
`queue` overflow behavior. Queued starts are fair across sessions and survive
client disconnect because desired lifecycle is durable.

The workspace only determines `${workspace}`. Other ChatMD variables remain:

| Variable | Meaning |
|---|---|
| `${tool_dir}` | Directory where the daemon or embedded host was launched. |
| `${workspace}` | Resolved configured physical or temporary workspace root. |
| `${prompt_dir}` | Directory containing the pinned root ChatMD prompt. |
| `${source_dir}` | Directory of the declaring ChatMD source, including imports. |
| `${session_dir}` | Current durable session data directory. |
| `${cache_dir}` | Current session cache directory. |
| `${home}` | Effective user's home directory supplied to the runtime. |

## Prompts and pinned revisions

A prompt entry names one root ChatMD file, its allowed workspace IDs, and its
permission profile. Disabled prompts remain visible but cannot create new
sessions. `runtime_policy` is an optional stable host policy identifier.

At catalog preparation, Ochat expands and validates the ChatMD source closure,
including imported ChatMD and external ChatML scripts, and stores a content
addressed prompt artifact. A durable session pins that artifact and its
permission profile revision. Restart uses the pinned artifact, not whatever
currently exists at the live source path. Catalog reload affects future
sessions; explicit prompt upgrade or rebuild changes an existing session.

The operator is responsible for exposing only prompt/workspace combinations
whose declarations and permission profiles are safe together. Selecting a
workspace does not grant filesystem, process, network, or tool authority.

## Permission profiles

`tool_default` supports:

- `deny`: reject tool invocations.
- `allow`: trusted automation compatibility mode.
- `ask`: request an authorized client response when one is attached.
- `policy`: use the installed deterministic policy evaluator.

An approval timeout may be disabled with `(approval_timeout none)` or set with
`approval_timeout_ms`. Fallbacks are `deny`, `allow`, `allow_if_policy`,
`(model_reviewer ID)`, or `(external_reviewer ID)`. Missing, throwing,
malformed, or unavailable reviewer implementations fail closed. Reviewer
implementations are host-injected and security-revisioned; the stock daemon
binary does not invent a reviewer from an ID. Every model or external review
is represented by a session-owned, non-redeliverable durable job. The reviewer
receives only a redacted invocation, and its terminal job and permission
resolution are committed before tool execution resumes. The same path handles
unattended decisions and interactive approval timeouts.

Manifest authorization is `deny`, `assume_authorized`, or `require_grant`.
`require_grant` requires an exact persisted session grant or an operator
bootstrap grant matching prompt ID, workspace ID, authenticated principal,
prompt source SHA-256, and canonical shell-manifest SHA-256.

Shell authorization remains enforced by Shell_runtime. The generic permission
gate delegates shell tools so one invocation cannot produce duplicate generic
and shell approval prompts. Permission requests, resolutions, durable grants,
revocations, and redacted security events are actor-owned durable state.

## Operator manifest grants

An optional grant record has this shape:

```lisp
(manifest_grants
 (((id coding-production-v1)
   (prompt coding)
   (workspaces (project))
   (manifest_sha256 HEX_SHA256)
   (source_sha256 HEX_SHA256)
   (principals (pri_EXPLICIT_PRINCIPAL_ID)))))
```

An empty `principals` list matches any authenticated principal that otherwise
has permission to create the session. Grants authorize only the exact compiled
source and manifest hashes. Editing the prompt invalidates the bootstrap match;
it does not silently extend old authority.

## HTTP authentication

HTTP is disabled by default. Authenticated listeners may use one or more of:

- a static hashed bearer-token file;
- an instance-scoped OAuth bearer validator selected by ID; or
- identity asserted by an explicitly trusted reverse proxy.

The stock executable supports static bearer files and trusted reverse-proxy
headers. An `oauth_validator` ID requires an application embedding
`Agent_server.Daemon.start` to supply the matching resolver; startup fails
closed if no implementation is installed.

Unauthenticated loopback mode requires `require_auth false`. Binding an
unauthenticated non-loopback address additionally requires the conspicuous
`unsafe_allow_unauthenticated_remote_http true` override. `server.info` reports
when anonymous HTTP development authentication is active.

### Static token file

The file is one S-expression list. It stores SHA-256 digests, never plaintext
tokens:

```lisp
(((token_sha256 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef)
  (principal_id pri_example_operator)
  (scopes
   (prompt.list
    workspace.list
    session.create
    session.transcript.read
    session.message.send
    session.own
    permission.respond
    security.read
    grant.manage
    audit.read
    session.stop
    session.delete
    configuration.admin
    diagnostics.read))
  (attributes ((team platform)))
  (expires_at none)))
```

Generate a strong token through an operating-system CSPRNG, deliver the raw
token only to the client, and place its lowercase hexadecimal SHA-256 in this
file. The daemon loads token records through Eio, compares digests in constant
time, and keeps authentication state instance-local.

### Trusted reverse proxy

```lisp
(reverse_proxy
 ((trusted_addresses (127.0.0.1 ::1))
  (principal_header x-ochat-principal-id)
  (scopes_header x-ochat-scopes)))
```

The direct TCP peer must exactly match a configured address before asserted
headers are considered. Identity headers from untrusted peers are ignored.
The principal header contains one protocol principal ID; scopes are separated
by commas or ASCII whitespace. Missing halves, empty values, duplicate headers,
unknown scopes, and duplicate scopes fail authentication.


## Defaults and validation

| Field | Default / constraints |
|---|---|
| `version` | Required; 1 |
| `server.data_dir`, `server.unix_socket` | Required paths; socket parent must be private |
| `http.enabled`, `address`, `port` | false, 127.0.0.1, 8787; valid TCP port |
| `http.require_auth` | true |
| `http.static_tokens_file`, `oauth_validator`, `reverse_proxy` | Absent; authenticated HTTP needs an installed authentication path |
| `http.max_connections`, `idle_connection_timeout_ms` | 1024, 300000; positive |
| `shutdown_grace_ms` | 30000 |
| `max_attachments_per_session`, `subscriber_queue_capacity` | 1024, 512; positive |
| `event_retention.completed_stream_ms`, `response_artifact_ms` | 3600000; artifact default follows completed-stream setting |
| `event_retention.max_events_per_session` | 100000 |
| `durability.journal_flush` | interval |
| `durability.journal_flush_ms` | 50 |
| `durability.snapshot_every_events`, `snapshot_every_ms` | 100, 5000 |
| `unsafe_allow_unauthenticated_remote_http` | false |
| `prompt.enabled` | true |
| `workspace.prompt_limits` | Empty; explicitly configure root-agent limits for bounded admission |
| `prompt_limit.overflow` | reject |
| `temporary.cleanup` | on_session_delete |
| `approval_timeout` | none unless configured; do not also set `approval_timeout_ms` |
| `approval_fallback` | deny |
| `job_limits.daemon_total`, `per_kind` | 16 each; positive |
| `job_limits.per_principal`, `per_prompt`, `per_workspace` | 8 each; positive |
| `job_limits.per_session`, `max_nested_depth` | 4 and 8; depth may be zero |

The [validated configuration types](../../lib/agent_server/config.mli) enumerate
all fields. The [validator](../../lib/agent_server/config_validator.ml) defines
accepted syntax, units, ranges, defaults, and diagnostic remediation; a field
being optional in an internal record does not invent a new public config key.

## Reload and limits

All changes to the `server` record require restart, including auth/listener,
storage, durability, retention, queues and job limits. `SIGHUP` may transactionally
reload catalog workspaces/prompts/profiles/operator grants. Existing sessions
retain their pinned revisions; see [operations](operations.md).

The daemon also polls the config file every second and attempts reload when its
mtime increases. Editing the config is therefore a live change, not staging for
a later signal. Prompt/import-only edits require `SIGHUP` (or an explicit config
reload); the watcher does not monitor their timestamps. See the operations guide
for rejected candidates and timestamp limitations.

The stock HTTP binary additionally supplies fixed limits: 16 MiB RPC bodies,
128 batch envelopes, 16 concurrent batch dispatches, and 1024 outgoing entries.
They are not additional S-expression fields. The embedded host has its own
explicit defaults; do not infer them from this daemon table.

A reviewer, deterministic policy or OAuth resolver ID selects a host-injected
implementation; the stock binary does not download or synthesize one. See
[embedding](embedding.md) and [permissions](permissions-and-security.md).
