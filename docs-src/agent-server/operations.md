# Operations and recovery

## Start and supervise

Use the [private tutorial](tutorials/unix-daemon.md) first. For a persistent
deployment, choose an absolute private data root and socket parent owned by the
daemon account. Supply provider credentials and host security policy to the
daemon process environment; a connected TUI's environment does not configure it.

```sh
ochat-agent-server -config /absolute/private/server.sexp -validate-only
ochat-agent-server -config /absolute/private/server.sexp -print-config
ochat-agent-server -config /absolute/private/server.sexp
```

These paths are placeholders. Use a service manager to run the same foreground
command under a dedicated account, with an explicit cwd/environment, restart
policy, private writable data directory and bounded shutdown grace. Do not
background it by tying its stdin/lifetime to a stdio client. Unix sockets are
always enabled; optional HTTP should bind loopback unless deployment policy
explicitly protects a broader listener.

Check initialization/`server.health` or authenticated `/v1/health`; inspect typed
failure and diagnostics, not merely existence of a socket file. Keep stderr/log
collection separate from protocol stdout. Redact credentials and local data
before sharing logs. Health is not proof every agent/tool/provider will succeed.

## Reload, shutdown, and credentials

`SIGHUP` validates and transactionally prepares catalog changes before publication.
Prompts, workspaces, permission profiles and operator grants can reload. Any
change to the `server` record is restart-required, including listeners, auth-file
path, durability, retention and limits. A rejected candidate does not partially
replace the live catalog. Existing sessions retain their pinned revisions.

The daemon also polls the config file every second and reloads when its mtime
increases. Save edits only when ready to publish; use a separate staging file
for preparation. Equal/backdated mtimes are not detected, and editing only a
prompt/import/script does not trigger the watcher. Use `SIGHUP` to force re-read.
Rejected changes retain the old catalog and are retried on subsequent polls;
diagnostic health reports the last error. Per-prompt artifact-construction
failures are different: a successfully installed catalog can mark that prompt
unavailable while preserving previous revisions for existing sessions.

Static token records are immutable in the authenticator after loading; replacing
the contents of the same path is not an automatic token reload. Restart to load
new records. Plan client credential distribution before removing old access.
Do not expose raw token values through command arguments or config-print output.

`SIGINT`/`SIGTERM` request graceful shutdown. The daemon enters draining, stops
schedulers/maintenance, terminates loaded work, checkpoints durable actors,
closes runtime/subscriber resources and releases locks. Initialization during
drain is rejected with `server_shutting_down`. Allow the configured shutdown
window before a supervisor escalates. A kill or machine failure follows recovery,
not the same guarantees as an acknowledged graceful shutdown.

## Reset, rebuild and upgrade initializers

Rebuild and prompt upgrade prepare the selected prompt and run its moderator
startup before committing the new session state. Supported initializers must
not depend on a synchronous durable `Model.call`: preparation has no initialized
session actor, so that service returns an error without claiming a job or
executing the model call. Handle that error explicitly, or move the call to a
later event after the session starts. Initializers may construct moderator state
and enqueue detached schedules or asynchronous jobs; those become authoritative
only if the administration commit succeeds.

This is a session-state transaction, not a general initializer sandbox. Source
helpers, tools and custom ChatML initialization can perform filesystem or other
external effects outside private preparation cache/response storage. Such effects
can occur before the final revision check and cannot be rolled back on failure.
Use initializers without irreversible external effects, and review executable
prompt changes before rebuilding/upgrading. A failed preparation preserves the
previous pinned revision, moderator, shell and history; it does not promise an
unchanged external filesystem or remote service.

See [session administration semantics](sessions-and-workspaces.md#prompt-revision-pinning)
for history and generation behavior. Both operations finish stopped.

## Durable state

The data root contains server/schema/ownership identity, indexes, session state,
checksummed transaction journals and snapshots, pinned prompt/source artifacts,
blobs/exports, idempotency records and security/audit state. One process owns it.
Session writers serialize commits/checkpoints so snapshot state and transaction
identity cannot be paired across a racing mutation.

`each` synchronizes accepted transactions before acknowledgement. `interval`
groups synchronization by its configured interval. `unsafe_buffered` explicitly
allows loss after process/host failure. Do not apply the strongest mode's durability
claim to every configuration. Filesystem/device durability also matters.

Recovery loads the newest valid snapshot with verified committed transactions;
an incomplete final frame can be truncated. Middle-journal corruption, invalid
hash chains, missing pinned sources, changed physical workspace identity and
unsupported schemas fail closed. A validated fallback snapshot plus retained
journal allows recovery if the current checkpoint is incomplete. These paths do
not promise automatic repair of arbitrary storage damage.

Nonterminal foreground operations and non-redeliverable work are classified as
interrupted, not blindly rerun. Durable jobs/schedules include their own delivery
state. External side effects may be uncertain; reconcile before retrying them.
See [orchestration](chatml-orchestration.md).

For retained extension invocations, daemon recovery restores missing initial tool
outputs from recorded results and marks unfinished invocations interrupted. It
does not rerun their handlers. Removed calls receive an explicit discarded-output
disposition. Conflicting output evidence fails recovery. See the
[invocation recovery contract](extensibility-foundations.md#invocation-recovery-at-daemon-restart)
for exact history bindings, allocation and worker/reset recovery behavior.

## Backup and restore

1. Stop accepting new work and gracefully shut down the owning daemon.
2. Confirm the process exited and no other process owns the store.
3. Back up the whole data root as one consistency domain with permissions intact.
   Also inventory external absolute/dynamic prompt dependencies, physical
   workspaces, executable pins, operator config and private credentials separately.
4. Restore into a private isolated location, not over a running store. Adjust
   deployment paths deliberately; canonical workspace identity can affect restore.
5. Inspect schema, start with protected listeners, check recovered sessions and
   artifacts, then allow ordinary clients. Preserve the original backup until done.

A live recursive copy is not a documented consistent backup. A ChatMD export
omits important daemon state. Treat backups as sensitive even if the UI is
redacted; encryption-at-rest is an operator responsibility.

## Inspection, migration, and legacy import

The store-container schema and each session's state schema are separate. The
current store container uses schema 1; the current session state uses schema 20.
Normal session loading upgrades supported older state shapes and rejects fields
that could not exist in the declared version. This does not migrate a workflow's
application-specific moderator state.

For an extensibility upgrade, stop the daemon and back up the complete store,
configuration and external resources before starting the new binary. Keep the
matching old binary and backup together for rollback. Inspect recovered sessions,
interrupted work and pending approvals before resuming them; refresh process-bound
output cursors after restart. Existing prompt revisions remain pinned. Use an
explicit stopped-session prompt upgrade when changing a captured definition, and
review its [workflow migration behavior](../guide/chatml-session-lifecycle.md#source-replacement-is-not-workflow-migration).
Downgrading the binary against a newly written store is not a supported rollback;
restore the matching backup instead of editing schema numbers.

```sh
ochat-agent-server -inspect-store /absolute/private/store
ochat-agent-server -migrate-store /absolute/private/store -dry-run
ochat-agent-server -migrate-store /absolute/private/store
```

Inspection reads schema/session-directory information without acquiring daemon
ownership. Migration planning/application takes the lock. Store-container schema 1 is currently
the only supported container schema; applying unsupported older/newer formats fails without
inventing a conversion. Neither operation is a complete artifact/journal integrity
scan. A dry-run is not a repair tool. Preserve evidence on corruption and avoid
manual journal edits.

Legacy import is separate:

```sh
ochat-agent-server -config /absolute/private/server.sexp \
  -import-legacy LEGACY_ID -prompt hello -workspace project
```

Run with no daemon owning the target root. The old `Session_store` source is read
through its compatibility reader; a new stopped durable session is created with
explicit catalog mapping. Conversation, task/key-value/moderator/shell state are
mapped where compatible and source provenance is archived. The source is not
modified. Do not assume old IDs, executable continuations or external assets are
transferred merely because transcript import succeeds.

## Retention and resource monitoring

Maintenance expires raw response artifacts, temporary blobs and idempotency
records; retained event windows determine whether clients can replay. Journal
pruning occurs behind installed snapshots, retaining a validated fallback.
Stopped inactive actors unload while their sessions remain indexed; running
intent, owner grace, runnable jobs and schedules can keep/load actors.

Managed job-result preparations have separate cleanup. Maintenance preserves
referenced artifacts, validates retained session/history/cache/blob consumers, and
removes only private preparations proved unreferenced. It can load an indexed
stopped session for this work. Loaded runtimes and active execution, uploads or
reads defer collection; archived sessions remain preserved. Invalid, incomplete,
linked or oversized roots report an error and retain the affected evidence.
Collection statistics distinguish discarded results, retired preparation markers
and deferred attempts. Library hosts can configure the bounded scan through
`Session_factory.limits.job_result_collection`; this is not a new CLI setting.
See [the implementation contract](extensibility-foundations.md) for lock order,
root coverage and the qualified extensibility host scope.

Monitor persistence errors, recovery failures, loaded actors, descriptors, queue
pressure, permission waits, subscriber disconnects, job/quota saturation, disk
usage and provider latency. Temporary workspace cleanup obeys its policy; physical
workspaces are never removed by that cleanup. Archived sessions/artifacts can still
consume disk. Do not infer unlimited retention or a fixed memory ceiling from a
short load test. See [test evidence](testing.md).

Reset/rebuild/delete are authorized protocol mutations with revision checks.
Review preservation flags and archive/remove choices in [the protocol](protocol.md).
Never delete locks, broad directories or unknown processes as a generic fix for
contention. [Troubleshooting](troubleshooting.md) lists safer checks.
