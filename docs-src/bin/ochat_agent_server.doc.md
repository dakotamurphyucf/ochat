# ochat-agent-server – durable multi-client agent daemon

`ochat-agent-server` hosts configured ChatMD prompts as durable Ochat agent
sessions. Sessions are controlled through the shared typed agent protocol over
a private Unix socket and, when enabled, HTTP RPC plus per-session SSE.

The daemon is the long-running owner of agent state. A detached session keeps
running when every client disconnects. Multiple read/write and read-only
clients may attach to the same session, receive ordered updates, reconnect from
a durable event cursor, and replace their projection from a snapshot when the
cursor is no longer replayable.

Workspace selection sets the logical `${workspace}` root used by ChatMD and
runtime defaults. It is not an authorization boundary. The prompt's tool
declarations, shell manifest, permission profile, and host policy determine
what an agent may actually access.

For complete tutorials and integration contracts, start at the
[agent-server documentation](../agent-server/README.md). The
[configuration reference](../agent-server/configuration.md) is the canonical
operator schema/semantics guide; this page retains its original sections for
existing links and command discovery.

## Synopsis

```console
$ ochat-agent-server -config /etc/ochat/agent-server.sexp
$ ochat-agent-server -config /etc/ochat/agent-server.sexp -validate-only
$ ochat-agent-server -config /etc/ochat/agent-server.sexp -print-config
$ ochat-agent-server -inspect-store /var/lib/ochat
$ ochat-agent-server -migrate-store /var/lib/ochat -dry-run
$ ochat-agent-server -migrate-store /var/lib/ochat
$ ochat-agent-server -config /etc/ochat/agent-server.sexp \
    -import-legacy LEGACY_ID -prompt coding -workspace project
```

Configuration paths must be absolute. Relative paths inside a configuration
file are resolved against the directory containing that file and are
normalized before use.

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

The event retention settings bound in-memory durable replay and raw response-artifact
lifetime. `completed_stream_ms` is a compatibility default for an omitted
`response_artifact_ms`; it does not enable replay of completed live streams.
Protocol 1.0 recovers through durable events and replacement snapshots.
The maintenance service removes
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

## Session lifetime and clients

Daemon sessions may be:

- `Detached`: independent of every client connection.
- `Owner_bound`: controlled by one exclusive owner attachment, a renewable
  lease, and a disconnect grace period.

Read/write attachments may send messages and perform authorized mutations.
Read-only attachments receive snapshots and events but cannot mutate or answer
permissions. Owner read/write attachments are exclusive. A reclaim token is
returned only once, stored by the client, persisted only as a digest, and
rotated after successful reclaim. An authenticated matching principal may also
reclaim according to the lease policy.

Attachment mode and principal scope are separate checks. All session mutations
validate the actor-owned attachment/lease before runtime, queue, or filesystem
preparation. Transcript scope alone omits permissions, grants, jobs, schedules,
and tool payloads according to their respective scopes. Hidden durable events
retain their sequence with an empty payload, so replay remains contiguous.
Tool-content placeholders remain renderable in the TUI and ChatMD exports.
Recoverable streams require security-state scope because provider deltas may
carry tool arguments; transcript-only clients receive finalized history instead.
Export blobs are bound to the creating principal's exact scope projection.

List pagination returns signed, principal/query/data-bound continuation cursors.
Changing the collection or restarting the host invalidates an old cursor with
`invalid_request`; restart the listing. `session.get` and `session.export`
support bounded before/after/tail/cursor history windows. Partial windows report
structural incompleteness and navigation cursors; complete them before using
them as model input. An effective-history request returns the selected window
in `effective_history` and an empty, explicitly incomplete canonical window.

Mid-operation snapshots include bounded active tool/agent start summaries, not
executable continuations. Completion/cancellation removes those summaries.
Static relative local nested prompts, including declarations in imported files,
are captured in the pinned artifact; their child imports/scripts are captured
recursively. Explicit absolute local paths remain external dependencies. MCP
tool discovery caches are separate per connected declaration/authenticated
client and are destroyed with their runtime; the deprecated MCP prompt-serving host is unchanged. Maintained MCP tools are
not deprecated.

Use `chat-tui --connect ...` for an interactive client or
`ochat-agent-stdio --connect ...` for an NDJSON gateway. Closing either client
detaches its attachments. It does not imply that a detached session stopped.

## HTTP routes

The HTTP transport provides:

- bounded JSON RPC POST requests, including batches whose commands may execute
  concurrently after initialization; ordered response collection does not
  serialize their side effects;
- a logical-connection notification SSE queue and a separate replayable per-session SSE subscription;
- session snapshot retrieval for projection replacement;
- authenticated server-owned blob streaming; and
- health reporting.

Logical HTTP connections have a configured idle timeout and bounded outgoing
queue. Active SSE streams keep their logical connection alive. Exceeding queue,
attachment, body, batch, or connection limits closes or rejects the affected
client without blocking session execution.

Remote exports return an opaque session-owned blob reference, never a native
server path. The common client downloads bounded chunks, verifies continuous
cursors, exact length, and SHA-256, writes a temporary sibling through Eio,
synchronizes it, and atomically installs the destination.

## Reload and shutdown

`SIGINT` and `SIGTERM` request graceful shutdown. `SIGHUP` reparses and
transactionally prepares configuration before publishing a catalog reload.
Prompt, workspace, permission-profile, and operator-grant changes may reload.
Listener, storage, durability, and retention changes are restart-required and
are rejected without partially applying the candidate.

During graceful shutdown the daemon enters `Draining`, stops schedulers and
maintenance, checkpoints loaded durable sessions through their actors, closes
runtime and subscriber resources, releases capacity and locks, and closes the
data store. Initialization requests received while draining are rejected with
`server_shutting_down`.

## Store inspection, migration, and legacy import

`-inspect-store DIR` validates the store schema and prints a migration plan
without taking normal daemon ownership. `-migrate-store DIR -dry-run` plans while holding the store lock. Schema 1 is
the only supported on-disk schema; apply rejects unsupported older/newer versions.
Inspection/planning is not a complete journal/artifact integrity scan.

Legacy import reads an existing standalone `Session_store` session through its
current migration reader and creates one stopped durable daemon session:

```console
$ ochat-agent-server -config /etc/ochat/agent-server.sexp \
    -import-legacy old-session -prompt coding -workspace project
```

The target prompt and workspace are explicit. Import maps conversation,
tasks, key/value state, moderator state, and shell state where compatible,
records source provenance under the new session archive, and never modifies
the legacy source.

## Recovery and retention

Startup validates the data-root identity and schema, rebuilds recoverable
indexes when necessary, and reconstructs sessions from the newest valid
snapshot plus verified journal transactions. An incomplete final journal frame
is truncated safely. Middle-journal corruption, hash-chain mismatch, missing
pinned prompt data, changed physical workspace identity, or unsupported schema
fails the affected load closed.

Stopped inactive sessions remain indexed but are unloaded from memory. They
reconstruct on first access. Sessions with running intent, owner grace,
runnable jobs, or schedules load during startup. Prompt-artifact maintenance
retains revisions referenced by the live catalog or any indexed durable
session. Temporary blobs and expired idempotency records are removed by the
Eio maintenance fiber.

## Operational checklist

Before production startup:

1. Validate the config with `-validate-only`.
2. Keep the data directory and Unix socket parent private and owned by the
   daemon user.
3. Review every enabled ChatMD tool declaration and every allowed workspace.
4. Prefer `require_grant` for shell manifests and pin exact source/manifest
   hashes.
5. Grant each HTTP principal only the scopes it requires.
6. Put TLS and client authentication at a trusted proxy when the built-in
   listener is exposed outside a protected host network.
7. Stop the owning daemon before a consistency-domain backup of its entire data root.
8. Test restart and restore procedures before relying on unattended sessions.
9. Monitor health, persistence errors, subscriber disconnects, queue pressure,
   job capacity, and failed session recovery.

All ordinary file access in the new daemon, session, transport-client, and
agent-store stack uses Eio paths and flows. Advisory locking and directory
synchronization are the only Unix descriptor operations, and they run against
Eio-owned descriptors through the system-thread boundary.
