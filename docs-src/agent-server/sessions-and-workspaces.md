# Sessions and workspaces

## Lifecycle and commands

The actor is the authoritative writer of state, history, event sequence and
mutation preconditions. A client must have the required principal scope and a
valid actor-owned writable attachment before preparing mutations.

| Operation | Meaning |
|---|---|
| Create | Pin prompt/workspace/profile; choose liveness/persistence; optionally start and attach. |
| Start | Set running intent and acquire workspace/quota capacity; may queue. |
| Attach | Obtain a client view and optional subscription; does not restart a stopped session. |
| Detach | Remove this attachment; detached session execution continues. |
| Send message | Commit input and report started/deferred disposition; not a completion promise. |
| Cancel operation | Target a specific active operation; do not reuse a stale operation ID. |
| Stop | Stop session work using graceful/cancel mode, preserving durable session data. |
| Compact | Replace current canonical history with retained instructions/reminders and a new summary, then rebuild the effective projection. |
| Delete history | Revision-checked removal of one canonical occurrence and its matching tool pair, while idle/stopped; distinct from deleting a session. |
| Export | Read a selected revision/history view into an authorized blob, not a native server path. |
| Reset | Revision-checked mutation with explicit history/tasks/cache/workspace/grants/labels preservation flags. |
| Rebuild | Revision-checked reconstruction using pinned or current-catalog prompt choice. |
| Upgrade prompt | Revision-checked target revision with explicit migration permission. |
| Delete | Revision-checked archive/remove policy and confirmation; irreversible removal requires deliberate selection. |

The wire [request reference](protocol.md) specifies the exact fields. TUI's
`--stop-session` uses graceful mode unless `--cancel` is supplied.
`--delete-session` removes unless `--archive` is supplied; prefer explicit archive
when recoverability matters. Do not infer wire defaults from convenience flags.

Desired state is running/stopped. Observed state additionally distinguishes
queued-for-slot, starting, recovering, idle, running-turn, compacting,
waiting-for-permission, stopping and failed. Running intent need not mean a
model request is currently active. An idle ChatML host may still listen for work.

## Ownership and multiple clients

Detached sessions have no connection owner. Owner-bound sessions have one
exclusive owner-read-write attachment, lease generation and expiry, reclaim
identity and disconnect grace. The TUI defaults grace to 30 seconds when creating
owner-bound sessions. Read-only and ordinary read/write attachments are distinct
from the owner and cannot silently acquire its authority.

The reclaim token is returned once and stored server-side only as a digest;
successful reclaim rotates it. Treat it as a secret. An authenticated matching
principal can reclaim according to the lease policy. A dropped TCP connection is
not necessarily detected instantly; idle/heartbeat and lease rules determine when
the server observes loss. Detach/quit does not mean every session stops immediately.

Multiple writers serialize through the actor. New user input during a turn may
be durably deferred, preserving ordering rather than starting concurrent turns
against the same mutable history. Use response disposition and operation events
to distinguish queued work from finished work. A rejected stale attachment or
read-only mutation must not prepare runtime/filesystem changes first.

## Workspaces and paths

Physical workspaces reference existing directories and are not deleted by session
cleanup. Temporary workspaces belong to one session and use `system_tmp` or
`session_dir`, optional managed root, and on-stop/on-delete/retain cleanup.
Cleanup concerns server-managed temporary data, not arbitrary user directories.
System temporary locations remain subject to the OS's own cleanup policy.

`shared_write` permits eligible root agents together; `exclusive` leases a
conflict domain to one running root; `read_only` expresses a runtime workspace
contract. None replaces tool capabilities or OS sandbox enforcement. Alias
workspace entries can share an explicit conflict domain. Per-prompt root-agent
limits choose queue or reject overflow; nested jobs also have independent limits.

| Variable | Native local / daemon meaning |
|---|---|
| `${tool_dir}` | Original host launch cwd, unless explicitly supplied by an embedder. |
| `${workspace}` | Selected physical or allocated temporary workspace root. |
| `${prompt_dir}` | Root prompt's source directory in its runtime source context. |
| `${source_dir}` | Directory of the specific declaring source, including imports. |
| `${session_dir}` | This host's current session data directory. |
| `${cache_dir}` | Host-supplied Ochat cache root for this runtime; daemon sessions use their session cache. |
| `${home}` | Home directory supplied by the host. |

For example, launch a daemon from `/opt/ochat` with a prompt in `/etc/ochat/prompts`
and workspace `/srv/project`. Its `tool_dir` and `workspace` differ. Native-local
and daemon hosts run the prompt from `prompt-artifacts/REVISION/tree/` inside
their data root: `${prompt_dir}` is the materialized root directory, and a tool
imported from `shared/tools.chatmd` has the corresponding materialized `shared/`
`${source_dir}`. Neither variable points back to `/etc/ochat/prompts` at runtime.
Only captured source/script files are copied, not arbitrary neighboring assets.
Use `${workspace}` or an explicit allowed root for project files. Remote clients
cannot change these variables by changing cwd. Default relative `read_file`
roots still follow `${tool_dir}`.

## Prompt revision pinning

Sessions pin expanded prompt/source and permission-profile revisions. Static
relative nested-agent sources and their imports/scripts are captured transitively
with a 256-agent-source/8 MiB captured-source bound. Absolute nested paths and
dynamically computed spawn paths remain external dependencies. No whole-workspace
snapshot or executable continuation is captured. Catalog reload affects future
sessions; changing an existing session uses explicit rebuild/upgrade semantics.

Captured imports, scripts and relative nested-agent sources must stay beneath
the root prompt directory. Relative `..` within that tree is allowed, but a
sibling outside it or an absolute import/script is not a supported captured
dependency. Organize shared declarations under a common root directory. The
standalone compatibility parser can read broader paths; successful
`-validate-only` parsing is not proof that artifact construction will succeed.
Check catalog availability before creating sessions.

New artifact files are owner-read-only; their directories remain owner-managed
for pruning. Load/rebuild and runtime construction (including cached revisions)
verify the materialized file inventory and hashes,
rejecting changed, missing, unexpected or symlinked files. Parsing then uses
verified captured bytes with no filesystem fallback for source edges. Older
artifacts retain their existing modes but receive the same verification.
This is not continuous filesystem monitoring or a sandbox against the daemon
account: keep the store outside tools' writable roots. Absolute/dynamic external
agent dependencies remain outside this pinning guarantee.

Rebuild and prompt upgrade require a stopped, unborrowed session and an exact
expected revision. The server constructs and starts the replacement moderator
against detached candidate state before committing. Failure, including a failed
moderator start or archive write, preserves the previous pinned revision,
moderator, shell and history. Preparation does not reserve history IDs or claim
jobs through the live actor. Its response logs and cache use private storage.
Custom source helpers and ChatML initializers may still invoke external tools
or filesystem effects: session-state atomicity does **not** roll those effects
back. Review executable prompt initializers before administration.

Rebuild advances the generation and replaces canonical history with the selected
prompt's actual initial messages, using fresh IDs. It preserves tasks/key-value
data, labels and workspace, and clears deferred input, permissions, grants,
jobs, schedules and old moderator/shell state before initialization. Upgrade
keeps canonical history and generation, preparing new moderator/shell state for
the target revision. Both finish stopped; neither starts a conversation turn.
Reset advances the generation and honors its explicit preservation flags;
dropping history keeps the original initial-prompt prefix. It clears deferred
input, pending permissions, jobs, schedules and moderator state.

## History and synchronization

Compaction is a history replacement, not just a reversible display overlay.
Retained occurrences keep their IDs; the new reminder gets a fresh ID and the
actor commits `history.replaced` with a new compaction generation. Before that
replacement commits, the host writes a checksummed, independently retained
pre-compaction state archive. Its operation ID, revision and SHA-256 reference
commit atomically with replacement history. Archive failure leaves the old
history intact and terminates compaction as failed when persistence permits.

Reset, rebuild and prompt upgrade also retain a checksummed pre-change state
archive before their state commit. They do not overwrite earlier archives.
Fetch `session.get` to discover `archived_revisions` (newest
first), then call `session.export` with one of those revisions and read the
returned blob. Export applies the current principal's visibility rules and
attachment checks, even for old state. Arbitrary journal revisions are not
exportable. `history.replaced` events do not carry the archive inventory; refresh
the snapshot when you need that inventory.

Administrative `session.updated` events additionally carry a
`replacement_snapshot`, filtered for the receiving principal and bound to that
event's revision and cursor. The common client replaces its complete projection,
including empty history, deferred input, permissions, grants, jobs and schedules.
Clients that ignore this additive field must fetch `session.get` after
administration; replay and all transports use the same projection rules.

These private archive files survive restart and ordinary event/journal/snapshot
pruning. They are not a backup, executable continuation or restore command;
removing session data, including transient-session cleanup, removes archives.
Legacy TUI snapshot-before-compaction is a separate, retention-bound mechanism.
See [archive storage](../lib/agent_session/compaction_archive.doc.md).

`session.delete_history` requires `session.message.send`, a writable attachment,
the expected revision and an idempotency key. It rejects active operations and
borrowed idle-moderator execution. Deleting a function/custom call or output also
deletes the nearest matching opposite occurrence in its tool family; a reused
provider call ID does not delete unrelated turns. The actor updates prompt-prefix
accounting and broadcasts the committed replacement to every subscriber. It
does not execute a tool, undo its effects, or mutate existing retained archives.

Canonical history records durable conversation identity; effective history is the
model-facing view after moderation/compaction. Request the view you need. An
effective-only request deliberately leaves canonical history empty and incomplete.
Before/after/tail/cursor windows can be structurally incomplete; do not feed a
partial window to a model as though it were a complete conversation.

List/history cursors are signed and bound to principal/scopes, query, collection
contents and host. Changed data or restart invalidates them with `invalid_request`;
restart the listing rather than modifying the cursor. Reconnect event cursors
instead address the durable event sequence and are subject to event retention.

Attach/reconnect may return events or a replacement snapshot. Replace the old
projection coherently, retaining local unsent draft separately. Mid-operation
snapshots contain bounded active-call summaries (up to 1024 starts, 4096-byte
invocation summaries), not executable continuations. Terminal events clear them.

## Restart and recovery

Durable records survive daemon restart. Session actors recover from validated
snapshots and committed journal transactions. Nonterminal execution is classified
as interrupted according to its job/operation kind; uncertain external effects
are not blindly rerun. Scheduled intent and durable delivery records support
reconciliation, not continuation serialization. Temporary workspace policy,
pinned sources, leases and quota admission are re-evaluated by the host.

See [operations](operations.md) before restoring state, resetting, rebuilding or
deleting a session. Export is not a full backup.
