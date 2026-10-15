# Developer command storage

Status: durable storage and daemon command lifecycle are implemented. The
hosted source capture, execution policy and client service remain separate
integration work.

A top-level check, fixture run or live run has its own command receipt. Child-job
results and Root phase checkpoints remain separate evidence. Commands live in
the existing developer-run document and use its sole Store writer, complete
before/after transaction images and pending-write ownership.

## Identity and phases

A command key is unique across every epoch of one stable principal/run. Its
immutable identity includes its original owner, resolved capture, command kind
and request fingerprint. The host computes the request fingerprint over the
original route or source selector, requested environment and editor revision,
entrypoint, inputs, fixture selection and chosen execution/output policy. It
excludes resolved source bytes and mutable host policy; the separate capture
fingerprint pins the actual source and contract. The Store compares both exact
identities but does not inspect the request fields. It allocates the command ID
only after proving the key absent; an exact retry returns the persisted ID.
Historical reads retain the original epoch and never authorize new execution.

All kinds begin Reserved. Only Live_run may enter Waiting_permission or
Execution_started. Check and Fixture_run remain inert with respect to host tools.
A live success requires the execution barrier; failure, limits, interruption or
cancellation may finish before it. The barrier records that execution could
begin, not proof that an external effect occurred. Terminal receipts are
immutable: repeating the same terminal is idempotent, and changing it conflicts.

Receipts preserve structured diagnostics, fixture failure categories, logs and
results. Stored source ranges are bounded comparison coordinates; they do not
grant source access or authenticate bytes. Result JSON may contain null, while an
optional diagnostic location is omitted when absent. Explicit null is rejected
for that optional location.

## Bounded recovery

Command limits bound row count, the aggregate encoded size of complete rows and
individual terminal size. The actual document also enforces its existing byte,
depth, field, node and shared work limits. Fixture logs use the aggregate budget;
storage adds no smaller per-log byte ceiling.

Encoding reserves enough space for interruption of all nonterminal commands and
all live inference attempts together, including their combined document revision
increments. Checking those two groups independently would not guarantee that
both could be recovered in a nearly full document. This reserve is a size check,
not a persisted recovery decision. Arbitrarily large successful results still
require their own bounded admission.

The unreleased developer-run layout advances to version two with the command
array. A version-one layout is rejected explicitly rather than interpreted using
new assumptions. This does not introduce historical OCaml runtime type copies.

## Execution and uncertain writes

The command interface distinguishes a newly acknowledged Start from an observed
stored Start. Only the first uninterrupted successful Start commit may issue its
process-local acknowledgement, which is consumed once. Reconciliation, repeated
commit and restart return observations only. Actual Root, actor, source and
provider admission are still required before effects.

An uncertain write retains the exact selected Pending in the existing Store map.
A key miss or cached absence does not release it. Repreparation requires a fresh
complete before-image comparison under the writer and preserves the same selected
row and terminal bytes. A proven absent admission may be retired after
cancellation; frozen terminal selections cannot be discarded that way. Recovery
joins or reconciles owned work before choosing a truthful terminal, and never
replays an effect to discover its result.

On restart, the host may read an existing command key before recapturing source.
An unresolved same-run write makes even a missing-key answer unavailable. For
fresh work, the host first observes the persisted latest epoch, reissues its
same-epoch Store handle from that owner and capture, acquires the physical run
claim, and finishes recovery before reserving a new command. Recovery marks
older and latest nonterminal command rows Interrupted through the same writer;
it preserves completed receipts and retains uncertain writes for reconciliation.
This startup handle does not authorize execution: current actor, source and
target policy must be checked separately before any new effect.

## Daemon lifecycle

The [command lifecycle](../../lib/agent_server/developer_command_lifecycle.mli)
uses one bounded, process-local entry per run. A same-run slot sequences
historical lookup, startup recovery and fresh reservation. Recovery owns the
physical Store run claim until the host has settled authentic abandoned job and
inference inventory, finished its window and released the claim. An uncertain
window or command write retains its exact claim or Pending; it is never treated
as a key miss. A later authorized same-key lookup may reconcile only the
selected terminal without invoking the command callback again.
Passive reads prove disclosure of the existing row before starting recovery.
Only a fresh reservation from the current slot can launch once; an old
Reserved observation cannot start a callback.
If a fresh reservation's slot exits before launch, the lifecycle finishes
that row as Interrupted through the existing Store writer. An uncertain save
retains the exact selected receipt and capacity charge for reconciliation.

Callbacks run under a daemon-owned switch, each with a joined child switch.
The submitting request can end while the callback and its capacity charge
remain. Explicit cancellation checks a separate control policy, cancels and
joins that callback, then settles its receipt. Passive waiting only reads a
result after a fresh disclosure check. A live callback must cross the physical
Root check and consume a fresh one-use execution acknowledgement before
starting effects; neither historical receipts nor recovered Starts can supply
it. The lifecycle does not select source, grant tools or schedule jobs.
