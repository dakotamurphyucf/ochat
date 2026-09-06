# Administration remediation implementation notes

Scope: agent server/session/store/protocol/client administration, focused tests,
and agent documentation. Do not use this file as a completed verification claim.
The main coordinator owns Dune builds and the shared audit/checker refresh.

## Transaction boundary

Rebuild and upgrade prepare detached immutable candidate state from a captured
stopped revision. Preparation has no actor reference, history reservation,
permission mutation, job claim, or quota claim. Local history allocation starts
at the captured reservation boundary. Initialization-created grants, schedules,
jobs and moderator state stay in the candidate. Preparation owns a child Eio
switch and private cache/response storage; public path substitutions retain the
real session coordinates. Preparation resources close before commit.

The actor rechecks the attachment, expected revision, stopped lifecycle and idle
moderator borrow immediately before committing. An independently retained
pre-change archive is written before the candidate journal transaction. Failed
preparation, stale compare-and-set, archive failure or journal failure must not
install the candidate. The previous pinned revision, moderator, shell and history
remain authoritative. Successful operations remain stopped; later runtime loading
uses the committed prepared history and moderator snapshot.

Session-state atomicity is not external-effect rollback. Custom source helpers
and ChatML initializers can perform tool/filesystem effects outside private
storage. Model calls routed through the durable job service fail closed while
the candidate has no actor; other executable initializer effects must be reviewed
by the operator. A failed initializer must still leave actor state unchanged.

The preparation child switch is explicitly cancelled and joined after its result
is captured, before deleting private storage. This also closes long-lived tool
discovery listeners that normal switch completion might otherwise wait on.

Rebuild starts a new generation with fresh initial prompt messages and fresh IDs,
preserving tasks/key-value data, labels and workspace. It clears deferred input,
permissions, grants, jobs, schedules and previous moderator/shell state. Upgrade
preserves canonical history and the existing generation, preparing the target
revision with a fresh moderator/shell state. Reset retains its explicit flags.

## Archives and compatibility

Keep the historical Compaction_archive API and conversation field readable; add
a defaulted archive kind rather than rename persisted records. Old references
default to compaction and retain their existing filenames. Administrative
archives use kind-specific names and the same framing, SHA-256 and durable-file
boundary. The existing archived_revisions inventory and authorized export API
include the generalized references. Ordinary snapshot/journal pruning never
removes them.

## Subscriber replacement

Add an optional replacement_snapshot to existing session.updated event payloads
for administrative state replacement, without extending the event kind enum.
Bind its revision/session/sequence to that envelope. Apply current principal
projection to the nested snapshot on every transport and replay path. The common
client installs it as a replacement, clearing absent/empty child collections,
live summaries and terminal-operation cache. Old event payloads and snapshots
remain decodable; old clients ignoring the additive field need an explicit get
after administration.

## Verification plan

- Failed preparation and failed archive/journal commit preserve old state.
- Real prompt message changes appear after rebuild; upgrade retains history.
- Two subscribed clients, including a transcript-only principal, converge after
  reset with cleared history/deferred/permission/grant/job/schedule collections.
- Reset/rebuild archives export before and after restart and ordinary pruning;
  missing/corrupt files fail closed; old archive reference decoding still works.
- Correct D09 wording: Standard receipts expire after a fixed one day; Protected
  receipts are not expired by pruning.

## Implementation and verification checkpoint

Core implementation is in `Administration`, `Session_actor`, `Session_transition`,
the generalized archive reference, detached `Session_factory` preparation,
`Runtime_owner.with_administration`, the event snapshot helper, principal
projection and the common client reducer. The server's rebuild/upgrade handlers
no longer install prompt state before runtime construction. The optional
replacement snapshot does not change the event kind enum or snapshot schema.

Main coordinated the build and confirmed the fresh updated administration inline
runner passed all seven tests with zero failures. The first six cover:

- failed post-start initialization (after a detached timer enqueue) and missing
  read roots, for both rebuild and upgrade, preserve serialized actor state and
  history allocator bounds and remove private preparation storage;
- rebuild installs actual new messages and fresh IDs; upgrade retains history;
- two subscribed clients converge after reset clears all tested collections;
- security-only readers receive no nested grants/jobs/schedules, with event-local
  revision/sequence anchoring;
- real archive IO failure preserves the previous pinned revision, moderator,
  nonempty shell and history after successful preparation;
- old three-field archive references decode with the compaction kind.

A seventh passing test covers synchronous `Model.call` during staged startup:
fail before actor claims/execution, preserve exact state. Operator-facing restrictions and the
external-effects caveat are in `agent-server/operations.md`, not only these notes.
No new global startup capability restriction was introduced.

The E2E administration scenario now checks actual rebuilt messages, upgrade
history retention, reset/rebuild archive export after ordinary snapshots have
provably been pruned, restart survival, and corrupt archive rejection. Main
confirmed `@agent-e2e-admin` passed all 12 cases with these expanded assertions,
and the administration tests also passed in the broad `dune runtest` run.
These are main-reported verification results; this agent ran no Dune builds or
test runners. No full-suite success or separate client/session runner result is
claimed here. Code and tests remain frozen at main's request; only this dedicated
verification note was updated after confirmation.

## Follow-up: executable cross-transport history deletion coverage

Main subsequently authorized a narrow conformance follow-up after the method
inventory correctly rejected missing `session.delete_history` coverage. Added
`conformance.history-deletion`: real snapshot-selected canonical deletion over
Unix, HTTP, stdio-to-Unix and stdio-to-HTTP, with reader denial, stale revision
rejection, unchanged snapshots after rejected requests, idempotent success replay,
exact remaining canonical history and matching replacement events for writer and
reader subscribers. The inventory now points to that executable case; the
operator testing page describes its assertions. Main confirmed the new case and
all eight existing conformance cases passed, followed by successful transport,
security and documentation gates. Administration runtime code remains frozen;
the main coordinator performed these builds and runs.

The main coordinator owns global documentation inventory/checker refresh and
the shared A/D remediation record. This dedicated note is not that shared record.
