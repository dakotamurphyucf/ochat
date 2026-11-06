# Local cumulative goal accounting

Status: pure aggregate, bounded canonical-row bindings and transition plans.
Actor publication, durable accounting documents, unknown-field preservation and
authenticated descendant composition are separate integrations. These types grant
no execution or attribution authority.

`Agent_session.Goal_accounting` represents one local source-ledger partition for a
stable goal ID and host-qualified session. A newly created scope starts at zero
with fences at the current ledger admissions. Creating that initial value does
not authorize replacing an existing account with zero; the actor's goal catalog
must reject duplicate creation. Objective edits, hold/resume, source generation
replacement and goal closure do not reset the partition.

The partition stores exact admitted host-turn and inference-attempt counts, a
settled six-component aggregate and at most 256 live bindings. A binding references
an actual abstract `Inference_ledger.Handle` and immutable admitted Run provenance.
It does not copy configuration, provider observations, context, diagnostics or
mutable workflow state. Compaction inference is an attempt; it is not an admitted
host Turn operation. A provider terminal cannot create a host-turn admission.

A usage view combines the settled aggregate with fresh contributions from the
exact bound canonical rows. Complete usage revisions replace the old contribution:
7 followed by revision 11 contributes 11, never 18. Duplicate and stale canonical
observations add nothing. Canonical host state supplies the existing missing-usage
classification, shared with the retained ledger summary. Actual zero remains
distinct from absence, explicit null, interruption and definite non-submission.
Input, output, reported total, cached input, cache-write input and reasoning output
are independent components. No subset is added again into its container.

Actual and Estimated sums and contribution counts remain separate. One bounded
estimator method/version and a mixed-estimator marker preserve aggregate provenance.
Every Unknown reason and attributed Untracked coverage remain labelled. All lifetime
counters use checked nonnegative int64 arithmetic. Token overflow is explicit and
never wraps; a downward live revision can restore exactness, while an overflowing
settled sum cannot later be corrected after its source rows retire.

One `plan` checks the before/after canonical ledgers, folds each removed bound
ended row's final retained contribution into the settled aggregate, removes the
binding and optionally attributes one actual host admission. Missing bound rows,
changed handles, invalid retirement and stale admission fences reject. A query
cannot silently treat an already missing row as zero: retirement must have folded
it in the owner transaction. Canonical late observations after retirement cannot
resurrect a binding or change that sealed contribution. Corrections are available
only while the actual canonical row remains retained.

Plans carry ephemeral references to the actual immutable before/after ledgers and
an accounting candidate. They are not durable copied provider state or a second
publication path. The actor must match its actual state/CAS fences, preflight the
integrated document's quota and future-schema compatibility, persist the canonical
ledger and all affected accounting partitions atomically, and acknowledge before
dispatch. A closed old goal's binding can retire during a different goal's admission;
its account must be folded in that same transaction. Protected unknown binding
fields cannot be discarded to make retirement fit. The durable document owner must
resolve this precommit eligibility, not discover it after canonical effects.

Pure checks validate available session, ordinal, current Goal episode and operation
facts. New Attempt and Turn attribution additionally require the current episode
source to equal the adopted Goal source, including installation epoch, and its
generation to equal the after-ledger generation. Source adoption cannot authorize
new work against an old episode merely because its generation is unchanged. Old
bound rows remain available for retained correction and retirement reconciliation.
These checks do not authenticate source/operation attribution or enforce one goal
per Handle across partitions. In particular, an Untracked `Turn_handle` exposes no
ordinal, session accessor or retained operation row, so structural count deltas
cannot prove fresh qualified operation identity. The actual operation owner supplies that fact and fences
retired-operation reuse. Authenticated descendant composition must preserve the
local partition identity and cannot import arbitrary totals as new consumption.

Observed token evidence can gate future admissions according to the selected goal
policy; it is not a hard spend, billing or currency cap. Already admitted work,
uncertain submission and delayed evidence can overshoot. An unlimited token policy
can disclose uncertainty without an automatic usage hold. Host admission ceilings,
permissions and rate limits remain independent.

Joined inference facts use the original process-local graph and canonical
registration. Revoking source authorization or closing its preparation blocks
fresh provider use while the same owner can acknowledge Running, usage,
completion and truthful interruption. This factual path preserves canonical
admission counts and cannot start another operation. A failed release retains
the exact live registration. Successful release retires provider permission and
keeps joined exact-handle membership only while its canonical row remains
retained. A new graph with the same source and generation cannot inherit that
membership. Graph finish clears it after positive factual acknowledgement.
A physical journal error
keeps the existing canonical writer unavailable until recovery, so retained
custody does not authorize replay from an uncertain head.

Runtime resource close joins the resource once and retains its result separately
from graph settlement. If factual graph finishing fails, a later explicit close
can retry that same finishing step without reopening the runtime or repeating
resource close. Primary resource failures retain precedence, and incomplete
settlement never counts as a successful close.
The existing runtime owner retains the same detached resource through a failed
close and retries it on explicit unload or close. It cannot become loaded again,
and constructing another runtime or replacing its source refuses while unborrowed
cleanup is pending. A separate runtime already loaded before that old close failed
keeps its original execution checks and remains usable. Normally borrowed resources
that survive Stop keep their existing lease behavior.

The grouped accounting tests use real canonical ledgers, compact boundary examples
and an independent unbounded latest-fact oracle over small retained windows. The
oracle recomputes small exact totals from explicit test facts, rather than using
production aggregate helpers. It exercises revision replacement, ordering,
interruption, Untracked coverage and final sealing; separate int64 examples cover
overflow and checked counter exhaustion.

## Full-profile local retirement qualification

The real 256-admission boundary and physical Store reopen are exercised by the
[local retirement executable](https://github.com/dakotamurphyucf/ochat/blob/main/test/goal_accounting_retirement/README.md):

```sh
dune build @test/goal_accounting_retirement/goal-accounting-retirement-local
```

This slow qualification is outside normal CI and `runtest`; compact partition,
retirement and preservation laws remain in ordinary tests. It preserves the
durable 256-attempt/256-turn/4 MiB profile and every real Actor/Factory admission.
Only the target Goal executes a provider. Unrelated pressure admissions are
truthfully released before submission, with no fabricated completion or charge.
The executable verifies one fold before retirement, duplicate/stale correction
idempotency, physical reopen and rejection of retired-row resurrection. It uses a
900-second local timeout and reports Eio wall time/phase every 32 admissions.
The separate fixture timeout default remains 30 seconds. A diagnostic run that
only reaches the boundary is not end-to-end qualification; retain the full local
exit result when reporting retirement/reopen coverage.

## Immediate-child usage snapshots

Delegated usage is a factual accounting projection. It grants no child execution,
Goal control, transcript access, source lease or cleanup-completion authority.
`Session_factory.import_child_usage` requires both entries to be the same
Factory's actual registered Actor/Owner/Handle tuples. It checks the original
current transcript-read authority independently for parent and child. A decoded
child ID, immutable reference or usage snapshot cannot substitute for those checks.

Each delegated child maintains its own cumulative partition over its real
canonical inference ledger. The shared retirement fold seals exact ended rows
before they disappear. It also retains admitted Attempt/Turn counters when the
canonical profile cannot retain a tracked row. New generation and recovery do not
reset the cumulative partition. The canonical ledger and its partition change in
one paired leaf; an ordinary ledger-only update cannot leave the partition stale.
Ordered terminal/start edges in one transaction remain ordered through replay and
Created preservation. A failed leaf admits no prefix of that transaction.

A snapshot contains the child's immutable delegation reference, actual acknowledged
session revision, its own usage/counters and an aggregate of its immediate-child
contribution slots. It contains no provider Handle, configuration, tool resources,
conversation or read permission. Export does not discover mutable descendants and
silently change a snapshot under an unchanged revision. An older snapshot may be
imported only while independent current read authority and the same live
Actor/Store association remain valid; its old revision is not an authority proof.

The parent keeps at most one current snapshot in each bounded immediate-child
slot. A newer snapshot replaces that child's whole contribution. Repeated or stale
snapshots do not add another charge; different contents at the same revision
reject. Authoritative retained usage corrections can decrease token totals, while
cumulative admission counters cannot decrease. A grandchild reaches an ancestor
only through its immediate parent's subtree snapshot. A direct grandchild import
into that ancestor rejects rather than adding a second accounting path.

New Factory births start with a complete empty child inventory. The real creation
path acknowledges an immutable Unobserved slot before publishing Linked or
starting the child. Captured occurrence publication includes its slot in the same
reciprocal-link transaction. An unknown slot acknowledgement does not permit start,
refund, removal or invented zero; same-request recovery retains the actual child
identity and bounded slot. A later failure after a known slot ACK leaves that
Unobserved association protected.

Historical absent accounting is explicitly incomplete. A factual first import
may register an actual immediate association but does not convert an old inventory
to complete. Unobserved children, unknown historical inventory and missing old
own-partitions remain visible uncertainty; scanning only the currently visible
children cannot prove that no other historical children existed. Unknown carrier
fields and protected row data participate in admission/preservation checks and
cannot be discarded to make a fold or import fit the configured bound.

These child contributions remain separate from the parent's own canonical
partition. They do not turn a parent count reservation into evidence of a child
Operation or inference admission, and do not install an ancestor Goal grant.
Execution and later messages continue to require their authentic immutable child
admission, current source/generation, original authority and actual control policy.
Observed token totals remain evidence rather than a distributed hard spending cap.

## Scoped descendant evidence

The lower delegation admission can freeze a child's inherited Goal origins.
An origin names the owning session and Goal, the originating Workflow episode,
and the first-child branch through which that Goal reaches the child. Different
branches of one persistent Goal may originate in different episodes. These
immutable facts grant no permission. Historical missing origin remains unknown;
an explicitly unrelated child has a known empty scope vector.

Scoped usage travels in the same immediate-child snapshot as whole-child usage.
Each scope reuses the child's actual own partition and adds only associated
immediate-child subtotals. At an intermediate session, the full inherited scope
must match. At the owning session, disjoint first-child branches are combined by
owner and Goal identity. Positively unrelated subtrees contribute neither usage
nor uncertainty to that Goal, even if their whole-child usage is unobserved.
Missing historical associations remain labelled uncertainty. Scoped fields
require their storage semantic marker; decoding never silently fills an old
origin or changes an older snapshot's logical digest.

The runtime snapshot projection reads the acknowledged session state directly:
its frozen origin selects the scope vector, its canonical ledger supplies the
shared own partition, and its retained child contributions supply each scoped
subtotal. Retained Included heads travel with those subtotals. It does not scan
live descendants or substitute whole-child totals for a scoped subtotal. A
correction to observed token usage can therefore update the same snapshot
without inventing another admission or advancing an Included prefix.

A branch reservation has a monotonically increasing nonce, the next included
prefix, and one exact prospective leaf occurrence. Confirmation replaces that
pending occurrence with included evidence. A proven no-writer closure consumes
the nonce without advancing the prefix, so the next reservation cannot reuse
the closed identity. Replaying an older terminal receipt cannot clear a newer
pending occurrence. The pure branch operations validate these data relations;
the runtime must still establish the real canonical acknowledgement or sealed
no-writer proof. A timeout is neither proof and cannot refund capacity.

The session owns the descendant component and its captured host limits. Creating
an actual new Goal records an empty root alongside that Goal's original-ledger
account bootstrap. This works in an existing, nonempty session: it does not turn
historical child inventory or the session's own missing origin into known zero.
A later episode or source change does not reset an existing root or its branches.

A historical session without a descendant component can register its first new
positively Unrelated child through a separate closed transition. It retains the
session's missing own origin, all canonical history, and existing child inventory
and future fields, while adding no Goal root or inherited head. An old child slot
cannot be relabelled through this transition. The runtime must authenticate the
actual private Unrelated reservation before selecting it; the DATA transition
alone grants no child execution authority.

Changes use a closed `Goal_descendants.Update`: root or branch registration,
reservation, local canonical inclusion, child import, or sealed no-writer closure.
The session validator replays that reason against the actual ordered state and
checks the complete candidate. An import carries its child replacement in the
same update that confirms the corresponding pending reservation. Ordinary child
updates cannot change a positive-origin slot or its scoped observation.

Local inclusion consumes one actual canonical ledger edge. The receipt identifies
the local session's next committed revision; parent reservations and imported
receipts instead retain their remote coordinates. Snapshot encoding, journal
replay and native publication all preserve this distinction. Replacing a state,
editing its carried value, or importing a newer snapshot cannot silently bypass
the ordered checks. Existing-slot growth is reserved within the captured component
and complete session limits, including preserved future fields.

Capacity checks cover both additive growth (bytes, fields and nodes) and maximum
JSON depth. They measure compatible future receipt shapes through the existing
preservation carrier, then include their actual enclosing component/session paths.
A currently shallow pending value is therefore rejected if its required terminal
receipt would exceed the configured depth. This does not increase the configured
limits or reserve another copy of canonical usage.

Ordered replay keeps an unstamped prefix separate from a finalized mutation.
Both undergo complete state, preservation and capacity checks. Finalization
also checks each newly included local occurrence against the actual published
session revision, after optional transaction metadata is applied. Later corrections
or a generation reset in the same transaction cannot erase that obligation.
Only the dedicated accounting operations can supply creation evidence; ordinary
context observations cannot introduce it.

The host-turn transition can carry inherited receipt data to the actual
`Turn_handle` owner. That owner pairs it with the same canonical admission before
final state validation. The input must match exactly one real Turn start;
compaction, unused input, another operation, or repeated consumption rejects.
Supplying receipt data alone does not authenticate a remote reservation or
authorize a worker to start.

Fresh local admissions consult the Goal's committed descendant counts and pending
reservations as well as its own counts. Pending or unknown capacity defers admission
without recording an exhausted-budget Hold; a known committed subtotal at the
ceiling still holds the Goal. Selected observed-token policies combine the own
and accepted scoped descendant subtotals, preserving incomplete coverage. New
activation and resume use that same observed-token policy. Already-admitted work
does not repeat fresh count or token eligibility checks.

A private runtime policy cell separates stable Goal policy from the creator's
episode. Source, budget, objective and owner changes retire the cell; episode
binding or progress alone does not. Explicit Hold and Stop retire it before the
persistence writer, even when the requested state equals the current state. A
failed write cannot revive that cell. Automatic budget Holds can preserve an
already-admitted occurrence, but its final deadline check still applies. This
helper and its retirement hooks do not issue a reservation or authenticate a
route; the runtime integration must retain the same cell across those steps.

The runtime integration remains incomplete. New root sessions start explicitly
Unrelated. Factory child creation now freezes the genuine native origin before
source preparation, or records Unrelated for management creation. It acknowledges
the exact parent association before child initialization and installs the frozen
origin in the child's initial state. Existing lower keys retain their original
origin instead of selecting the current Goal or episode.

Birth refresh separates finite Actor capture and consumption from Store reads and
original authorization outside the mailbox. A failed, cancelled or superseded
refresh cannot restore an older cached successful view. The final check still
requires the exact captured state, controller and original execution owner.

If lower admission succeeded but parent association was interrupted, recovery
uses that original private admission and its frozen origin. A missing Unrelated
slot can be registered; an Associated admission requires existing local roots
and registers all its local branches atomically. Existing Unknown slots cannot
be reclassified. The finite retained-registration operation preserves the
canonical ledger and does not require the current Goal episode or source to
match the original attribution. This recovers accounting facts only: original
source and execution authorization still govern whether the child may run.

Before linking and initializing a new child, the Factory observes its actual
stopped, unloaded state and imports that snapshot through the same accounting
kernel used by operator imports. This acknowledges observed zero usage rather
than inventing a zero snapshot. A retained, already linked child can contribute
nonzero usage on retry; importing it replaces the prior cumulative snapshot.
The factual import retains the original callback's acknowledgement ownership
without requesting another Goal admission or granting transcript access.

Before this integration is complete, every governed Turn/Attempt producer must
connect to the root reservation and confirmation protocol. Those
execution paths must consume genuine canonical acknowledgements or sealed
no-writer outcomes. Retained origin, reservation and Included data cannot replace
execution-owner proofs.

Inherited Turn planning retains the genuine Turn handle and its immediate
before/after ledgers through full transition validation. Those ledgers can differ
from the transaction's starting and final ledgers when it contains other
accounting changes. The retained edge is planning evidence; only the actual
writer can acknowledge that its candidate was persisted and installed. Ordinary and
inherited planning share source settlement and final Run bookkeeping-capacity
validation; retaining the edge does not skip either check.

The native root adapter separates finite Actor capture and consumption from
external Store reads. It retains each reservation attempt before the writer can
run and records a successful acknowledgement at State installation, before
publication. An uncertain attempt cannot invoke the writer again. A fresh attempt
cannot adopt an existing pending reservation merely because its durable occurrence
matches; only the original attempt retains acknowledgement or reconciliation
custody. Fresh and admitted checks retain the same policy cell; factual import and sealed
no-writer cleanup do not request fresh Goal eligibility. Confirmation retains
only the actual import acknowledgements needed by the frozen root vector.
The child execution paths must still use these adapters before the feature is
complete; the adapter APIs alone do not authorize a provider or worker effect.

Factual confirmation can observe usage under the actual child's installed
canonical acknowledgement, or the immediate parent's positive import
acknowledgement when continuing up the ancestry chain. Each step uses that
source session's own delegation reference. It checks the same Factory, Store,
full session identity, private admission and frozen origin, then captures the
current accounting snapshot. It does not require the old execution owner to
remain live or load a runtime. A missing snapshot remains an error, not zero
usage. Reading newer usage does not replace the retained immutable snapshot;
refresh only supplies a current source check. Actual receipts still determine
whether an observation confirms a reservation.

A successful no-writer close is retained on the original native reservation at
State installation. Publication failure or a later branch occurrence cannot
erase that known result. Retrying with the same physical child seal returns the
retained close acknowledgement; absence of a pending row is never treated as
proof that the writer did not run.

The Factory installs an exact source binding before building a runtime, including
an ordinary reload. Closing or replacing that runtime retires fresh admission
through the binding before graph cleanup can wait. The retained binding still
provides factual confirmation and sealed cleanup; it does not grant execution
permission. Root reservation and admitted-view refresh repeat the original
delegation authority check after their external waits, then validate all root
views together without another wait.

Each native child admission retains one private slot for its prospective
occurrence. Another admission cannot reuse that slot. Releasing a sealed slot
requires a positive close acknowledgement for every attempted root reservation;
an uncertain writer retains custody. A canonical acknowledgement installs the
slot's completed state alongside the session state, before publication. Direct
messages, managed sends, Run user inputs, history continuations and inference
Attempts consume this ownership. Idle continuation retains its separate source
integration and validation requirement.

The child canonical-write attempt is tracked separately from a generic storage
write. A local Goal policy may persist a metadata-only Hold instead of the
requested Turn. That Hold does not mark the child occurrence as attempted;
only entering persistence for the exact checked canonical candidate does. This
lets cleanup distinguish a real uncertain child write from a rejected admission
without inferring either outcome from an error message.

Direct message submission has an inherited Turn path that captures the actual
input, operation and adopted authorization, reserves ancestor capacity outside
the mailbox, and installs the child Turn before confirming its usage upstream.
The worker is held behind that confirmation. Its ongoing guard follows the
admitted operation's authorization, including when it adopted an earlier queued
input; it does not require the submitting client's writer attachment to remain
connected. Cancelling the initial submission cannot lose an unreturned admission
slot, and ordinary submission remains cancellable. Idle continuation still
requires its own source integration and validation.

Native managed sends use the same inherited Turn protocol with the actual parsed
history entry and durable managed receipt. The Factory retains the original
borrowed native scope across ancestor waits, refreshes that exact execution's
admitted operation views outside the mailbox, and checks its currentness again
before the child writer. This adds no client attachment, child-creation permission
or fresh count/token eligibility. The original private delegation and management
authorization checks remain mandatory. A busy or deferred send keeps its existing
no-Turn path. Replaying an acknowledged receipt does not submit the input again;
a new idle Turn must still reserve and confirm its own occurrence.

Ordinary and prepared Run user-input admissions retain their actual Run admission,
operation, adopted custody and prospective execution binding through the same Turn
plan. The native canonical acknowledgement installs that exact Run binding with
the candidate state before constructor custody closes. Only after upstream
confirmation can the original suffix launch the worker and return its correlated
Run or managed receipt. Later execution follows the acknowledged operation and
Run binding, rather than a closed preparation or a disconnected writer attachment.
Authored Start retains its separate admission owner; it is not user-input Turn
coverage.

A prepared Run transfers one closing obligation from its caller to that same
inherited admission. Caller cancellation, the handler's finalizer and the outer
Run-start cleanup cannot close custody retained by an uncertain canonical writer.
A genuine sealed no-writer result or the actual canonical acknowledgement settles
that obligation once. Lifecycle retirement can still revoke execution; retained
custody is not a new permission. If canonical acknowledgement is already known,
a later confirmation error cannot turn its retained Run receipt into rejection.
Otherwise a definite prewriter failure remains rejected and an attempted unknown
write remains uncertain. Neither a timeout nor a missing row proves no write.

A physical preparation owns one reservation set. Beginning a root attempt
retains it in that set before replying, so cancellation cannot lose its ownership
and duplicate calls cannot create competing native attempts for the same root.
Retries reuse the retained attempt. Sealed cleanup runs in one bounded
Actor-owned fiber per slot; errors retain the same seal and reservation set for
an explicit later retry. It never seals an attempted child writer or refunds
capacity from a missing row.


A cancelled direct Turn retains its cancellation request even while the canonical
writer is suspended. If that write later succeeds, the actual acknowledgement
remains owned by the Actor until terminal cleanup and upstream confirmation have
both succeeded. These factual steps retain successful partial results for an
explicit retry; cancellation cannot turn a successful write into a no-writer
refund. The cancelled worker cannot start. Runtime shutdown and eviction
drain these retained owners before detaching the runtime. Ordinary unload refuses
an outstanding admission or acknowledgement, even if the public operation is
already idle. Explicit stop/shutdown joins bounded factual cleanup outside the
Actor mailbox and ownership locks. A lifecycle fence cannot hide pending facts.
Factual imports still require the same registered Actor, runtime and Store handle;
closing the registry alone does not revoke that accounting ownership. A failed
cleanup may retry factual writes only through the same retained entry; another
entry or incomplete recovery owner cannot substitute for it. An uncertain
canonical writer still requires storage recovery.

Native tools check the exact dispatched execution's lifetime and inherited
operation guard immediately before their effect, including after approval or
other waits. This check is separate from authority to create a child agent:
ordinary tools in an observation callback do not acquire or require child-agent
creation authority. Agent creation continues to check its original, stronger
authorization.


For historical delegated sessions whose own origin field is missing, fresh
execution reads the actual private delegation through the runtime's retained
Factory binding. A known unrelated origin permits ordinary execution without
rewriting historical state. Missing private origin remains unknown; a scoped
origin without matching local accounting cannot execute. The same immutable
State, Factory owner and Store observation are checked after external reads.
Ordinary root sessions continue to use their explicit nondelegated definition.


The inference allocator now retains the real canonical Attempt handle through
ancestor reservation and confirmation before returning a guarded allocation.
Reserving the Attempt advances ancestor state, so the allocator refreshes the
enclosing admitted Turn's views after reservation and before the canonical
Attempt write. This refresh does not admit the Turn again or consume its budget;
the original execution owner, Attempt reservation and final currentness checks
remain mandatory. Provider submission then refreshes both the original execution
checks and the inherited Goal checks. The unbound graph adapter uses this allocation path too;
it cannot discard the submission guard by returning only a handle. External
ancestor waits remain outside the Actor mailbox and graph-wide cancellation
mask. Factual settlement uses the same retained native graph registration after
original authority retirement; it cannot authorize another provider submission.

Already acknowledged descendant policy views retain a lifecycle ownership witness
from a successful checked Store observation. Ordinary metadata publication keeps
that witness, while actual lifecycle controls, Handle/lock retirement and failed
publication invalidate it. Fresh observations and writers still use the exact
metadata/projection epoch. The controller binds the host's same concrete canonical
writer: an in-flight normal write remains available, while an uncertain journal
or closed writer denies execution. This session-ID check is negative affinity;
matching IDs cannot recreate physical resource ownership. Every admitted effect
still checks the current root association and policy, the same native ACK and
confirmation, and its original source and private permission guards.

History edit-and-continue and standalone Continue use the same inherited Turn
admission and installed operation guard. The exact validated history transition,
archive reference and selected worker are retained while roots are reserved;
the final writer repeats the original writer lease and history revision checks.
A stale writer or changed history cannot publish that prepared edit or launch its
Turn. Save-only edits, unavailable continuations and deletion do not reserve a
Turn or wait for ancestor cleanup. After the canonical acknowledgement, the
installed operation owns execution and ancestor confirmation; client disconnect
cannot undo the accepted history. Before handing off that acknowledged Turn,
the Actor checks the same selected worker. Replacement during ancestor
confirmation returns a conflict without redirecting the old admission to the
replacement worker; the accepted history and Included accounting remain retained.
The worker is selected before its execution fiber is forked.

Held Goal and recurrence user inputs retain their genuine parsed-input scope,
Run association and selected worker through inherited Turn admission. The real
controller stages once against the complete candidate; its prepared native owner
is retained before persistence, and only the actual State acknowledgement
installs it. Unknown writers keep that same input, source and reservation set.
Lexical cleanup may revoke execution without discarding factual ownership.
The worker waits for ancestor confirmation, then uses the original held
first-effect controller and the same inherited admitted guard. Parsing is not
repeated and the original Run receipt remains the caller's admission result.
If Run admission committed before an input write failed, the failure retains
that known admission receipt; it does not make the Run admission uncertain.
Recovery can restore a synced but unacknowledged Turn, including its submitted
input, and import its factual charge once without restarting the abandoned
worker. An unsuccessful resource close remains a failure; factual recovery is
not evidence that the original live resources closed successfully.
An irreversible local no-writer seal retires execution and the staged controller;
the exact Slot, host and prepared factual owner remain until every root close is
acknowledged. Retiring execution does not itself release ancestor reservations.

Explicit Goal Resume uses the same inherited Turn admission after its actual
existing runtime lifecycle has joined. Only a live same-episode continuation
captures an Administrative Turn and its real Run work; the original Run,
episode, account, source and command receipt are retained. The exact selected
worker and source remain pinned across ancestor waits. The existing Goal runtime
and Operation acknowledgements compose with the canonical Included update at
one State installation, and launch follows actual ancestor confirmation on that
same worker. A prepared controller is not an acknowledgement: an uncertain writer
retains the exact prepared owner for factual cleanup and recovery. Waiting,
settlement, controlled and retained outcomes reserve no Turn and keep their
actual metadata results and failure classification. No new input, Run, lifecycle
callback replay or fresh count decision is inferred from a command receipt.

## Aggregate legacy idle completion

The aggregate legacy moderator callback retains one process-local completion
owner from its idle claim through callback exit and native completion. A late
cancellation cannot release a later claim. Checkpoint, Stop, suppression and
compaction metadata use the existing writer. An attempted write without an
installed State acknowledgement retains its actual candidate, diagnostic and
runtime; neither a failed return nor the absence of an inherited reservation
proves that no write occurred.

A Goal Hold acknowledgement belongs to its selected metadata candidate, rather
than the requested Turn. Checkpoint, foreground reconciliation and
compaction-start acknowledgements remain separate facts. An acknowledged
operation retains its actual custody, staged Turn or compaction history, and
physical worker until launch or factual terminal cleanup. Publication failure
cannot erase that acknowledgement or select a replacement worker.

Source close joins the exact completion cleanup before raw runtime close. If
terminal acknowledgement is known but subsequent cleanup fails, both the fact
and its unresolved cleanup diagnostic remain retained; the callback or writer is
not replayed. This ownership bookkeeping does not classify a source or create an
inherited admission.
