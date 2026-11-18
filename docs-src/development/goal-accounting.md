# Cumulative Goal accounting

Each session Actor publishes its bounded Goal accounting alongside canonical
inference changes. The Factory carries authenticated descendant attribution into
child admission, and the owning Goal consumes acknowledged contributions along
the original ancestry. Durable accounting documents preserve coverage and
compatible unknown fields across replay. Fresh work still requires the original
execution authority and current Goal policy.

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

Closing a Goal preserves this correction path for its associated descendants.
For example, a retained child Attempt reporting 7 input tokens can later report
11 after the operator abandons the Goal. Importing the new observation replaces
the contribution with 11; the closed Goal, admitted Turn/Attempt counts and
confirmation prefix remain unchanged. Repeating the import adds nothing, and
physical session recovery retains the corrected contribution without replaying
the provider call. A correction grants no new execution permission.

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

`Runtime_builder` carries one structured closer. `close_reported` returns its
observation; `close` projects the same result through the ordinary exception
interface. A Product records its physical close observation before resolving its
join promise. Its original and rebound bindings therefore share that observation,
including the original diagnostic identities and exception backtrace.

A partially constructed resource Cell can retain its preparation closer before
the delegated driver starts. Its adoption report distinguishes a still-running
constructor, a runtime that was never created, and the actual runtime already
closed by its driver. Cleanup refuses while the original constructor is running.
After it exits, an unadopted Product still needs its own fallback close; an adopted
runtime must match the Cell's actual binding and cannot be closed a second time.
The registered preparation closer owns its graph settlement as well.

When the last resource lease finishes cleanup, its Cell retains the successful
report before releasing the target. Repeated last-release calls return that
same report without repeating cleanup, including when a separate pending-cleanup
call completed a previously failed release. Native resources and their promoted
Product share this receipt; it is never reused for a different resource.

Independent ancestry keeps preparation failures separate from cleanup obligations.
The original caller still receives its construction or admission error, and the
original diagnostic history retains it. Native preparation cleanup omits only
failures explicitly recorded as preparation failures; actual release failures,
unknown scope exits and incomplete custody still prevent successful cleanup.
This classification comes from the original operation, never from whether a
callback happened to run.

Native borrow outcomes also retain evidence of the selected preparation failure
from the original admission, source or lease operation. A partial constructor
can omit that already-returned failure from cleanup only after proving that no
runtime was created and its dependent local work has joined. Actual cleanup
failures remain owed. If the only remaining failure belongs to the exact acquired
native pin, a later explicit partial close can repair that pin after the child
graph has finished. Returned-runtime failures keep their existing behavior.

An expected stop cancellation can remain visible in immutable diagnostic history
without being presented again as a shutdown error. Successful shared cleanup
acknowledges only an original, singleton post-callback cancellation check already
normalized by the same delegated driver. Callback, resource-release, mixed and
unclassified failures remain independently reportable.

The existing runtime owner retains the same detached resource through a failed
close and retries it on explicit unload or close. It cannot become loaded again,
and constructing another runtime or replacing its source refuses while unborrowed
cleanup is pending. A separate runtime already loaded before that old close failed
keeps its original execution checks and remains usable. Normally borrowed resources
that survive Stop keep their existing lease behavior.

Each retired-resource cleanup request retains its original entries before clearing
the temporary queue. Its internal report distinguishes an entry not yet attempted,
a previously closed resource, a resource retained by its actual borrower, blocked
cleanup, and an actual close result. A skipped close is not proof of physical
release. Close results and original backtraces are recorded before subsequent
bookkeeping, so a later bookkeeping exception cannot erase work already performed.
The direct API still returns or raises its original first failure; structured
source reports preserve all actual cleanup failures separately from the primary
callback result. These records support recovery without recreating the old queue.

Each actual lease retains its own cleanup report, and its source extent retains
that same lease association. The report is completed before the lease's join
signal is resolved; observing that signal means cleanup has returned, not that
it succeeded. The cleanup bracket covers admission as well as the callback body,
so a failure after publishing a lease cannot bypass its detach path. Source and
lease reports retain the original exceptions and backtraces separately from the
callback's result, including after a later physical repair.

Permanent shutdown reports resource disposal separately from these canonical
cleanup diagnostics. `Runtime_owner.close_and_wait_reported` reports `Joined`
only after the actual producers and required physical, factual, callback release
and End duties permit disposal. It can retain earlier failures without changing
the original cleanup report, its snapshots or closed admission. An empty queue,
a terminal diagnostic or a positive End reply alone cannot establish this proof.
The enclosing session reports `disposed` only after its own remaining resource
stages finish. Unresolved custody remains `Retained`; an explicit later cleanup
can retry a permitted original duty, without replaying the callback or initializer.

A bound canonical backend can also release process-local custody for one failed
Initial input whose root write entered the actual writer but received no journal
acknowledgement. This requires the same completed permanent retirement, joined
input and Init producers, factual graph and callback release receipts, physical
cleanup, and the exact original failed End. Writer unavailability alone grants
nothing. Unbound backends, live producers and unknown receipts remain retained.

Only reported permanent close initiates this release. Strict close does not. The
Actor then permits State, Shutdown and repeats of the same release request;
checkpoint and execution effects remain denied. No End is replayed, journal write
retried, Run acknowledged or callback rerun. The original End failure and unknown
Run remain unchanged while actual custody can permit final resource disposal.
The Owner retains the exact request before dispatch and observes it before any
close prefix on a later reported call. An interrupted reply retains its original
failure segment, which is forwarded even if that same request later settles.

Disposal diagnostics use stable segments recorded at the actual original cleanup
outcomes and the same retained child-scope associations. A parent can finish after
an earlier prerequisite fails while a selected child still owns its finalizer.
A fresh diagnostic view includes that child's later original cleanup failure;
the earlier view and the parent's public original report remain unchanged.
Repeated views of the same producer share its segment, while different producers
retain distinct failures even when their exceptions and backtraces are identical.
This includes an actual retirement completeness check that refused disposal before
a later reported close settled custody. Each newly executed check owns its own
refusal diagnostic; `Joined` can retain those earlier refusals alongside the
original callback failure. Re-observing the same check adds no duplicate segment.
A copied prior failure is suppressed only when that same producer is also owned
in the observed graph, at its actual original position.

Diagnostic completeness is separate from physical custody. Both are required for
positive disposal. Uncertainty about an original Init handler's completion stays
with that Init observation; a later release cannot silently resolve it. A normal
child that finishes after its parent's original report is freshly observed rather
than making that parent permanently incomplete. These views follow existing
retained scopes; they do not create a history of released owners or select new
cleanup work.

Internally, runtime cleanup scopes begin before delegated or warm preparation
and execution-exclusion checks. They retain each genuine preparation source
before that source can yield. If setup fails before the outer lease is admitted,
the scope still retains the completed original source cleanup report; it does
not manufacture an acquired lease. Existing callers continue to distinguish a
pure preparation deferral from an error or exception after preparation starts.
These retained source associations are factual data. The cleanup-recovery API
observes them and permits only the exact retries described below.

Internal source cleanup records the exact policy-restoration, native-setup and
delegated-load End obligations when those registrations are acquired. If an
installed-runtime cleanup fails first, the skipped End remains unattempted;
clearing the local stage does not turn it into a successful End. The retirement
producer separately retains its original restored-resource and native-loan
cleanup targets before an earlier failure can skip them. A native borrower also
records its actual pin-close outcome before its enclosing lease finishes.
A rejected source request cannot select unrelated pending resources for cleanup.
Administration cleanup retains failures through its final bookkeeping while
preserving the accepted operation's primary result. Local construction records
its own close and End outcomes before closing an uninstalled binding, so a
failed End cannot hide a separate binding-close failure. Warm-load cleanup also
finishes inside its original source extent, before that extent's report is
sealed. A binding already acknowledged as installed remains with its actual
runtime owner. Source and lease recovery observe those retained records without
selecting unrelated work.

Original cleanup evidence also distinguishes the physical release operation from
later Owner bookkeeping. A successful pin close cannot erase a separate error
while clearing its owner. Policy-claim cleanup retains its actual custody check,
Actor drain and loan-release outcomes; an absent current claim is not proof that
its old loan closed.

The natural policy-claim finalizer records its actual loan-release result before
clearing the claim. Retirement that already captured the same claim retains that
result even after the live holder disappears. A successful release satisfies
only the loan obligation; the Actor claim and its enclosing wrapper must still
finish. A later explicit retry can repair a failed release without changing its
original diagnostics or repeating a release already known to have succeeded.

Opaque dependency and policy-resource callbacks record
whether they were required and what their original invocation returned, without
retaining executable callbacks for replay. Pending retirement retains the same
source report even after its temporary source pointer is cleared. These records
preserve concrete failure boundaries instead of matching or subtracting exception
values from a combined error report. Source, lease and administration records
likewise distinguish selection, retirement, detachment and final bookkeeping.
A nested administration unload failure belongs to its own operation; it is not
counted again as a source-selection failure. Its partially selected cleanup queue
still runs, with the original unload error kept primary.

Cleanup batches also retain where their original call occurred. A local binding
keeps its construction receipt, so its cleanup can be associated with the correct
End operation. Source records distinguish stage setup from direct administration
transitions. These associations preserve actual cleanup order, including skipped
source-tail duties; they do not authorize replaying completed operations.

Internal cleanup scopes also seal an immutable observation of their retained
obligations when the original scope finishes. This observation keeps completed
work, live dependencies, permitted exact retries and unrepairable failures
distinct. It reads only the original retained objects; it neither runs cleanup
nor discovers new work from the current Owner. The original report remains
unchanged. The source and lease recovery API exposes these observations through
its original view and progress snapshots.

Each failed physical close is associated with the promise of its actual
producer. Repeated views of that same failure appear once; distinct attempts
remain distinct even when they raise the same exception. A newer live attempt
does not erase an earlier failure or make another retry safe. Cancellation,
physical release and subsequent bookkeeping retain their separate outcomes.

Retirement records both the requested scope and the phases that actually ran.
An ordinary phase cannot stand in for a requested complete-resource phase.
Required but unattempted work remains visible, and an unavailable original
operation is not replaced by a sweep of current sessions or resources.

Reusable resource retirement joins physical cleanup while keeping the session's
Actor, Store handle and metadata available for a later authorized restart.
`Runtime_owner.claim_resource_retirement` elects one producer or returns the same
pending attempt to join. A stronger request can extend an in-progress ordinary
unload under continuous admission exclusion; it cannot strengthen a finished fact
or widen an exact Cell election. Permanent close joins an elected Cell attempt
before replacing foreground work, then selects its own Whole Owner retirement.
A failed narrower attempt requires its existing explicit retry authority. The final
barrier check and permanent admission closure happen under the same Owner lock;
a fresh Cell election cannot enter after that closure. Joining an already
authorized covering Whole attempt does not elect or widen new work.

A failed complete retirement retains its exact cleanup targets and original
result. Explicit retry creates a new attempt for those targets; observers of the
old attempt still receive its original failure. A cancelled waiter does not
acknowledge a failure it never received. Queueing work or finishing only the
ordinary unload phase does not establish complete cleanup.

When Actor retirement begins, it retains the original operation and selected
cleanup objects before revocation, clearing or cancellation can discard them.
Repeated retirement requests keep that same selection. The retained references
identify work; they do not freeze its progress or grant permission to run new
work. Selection covers that retirement's obligations, rather than every past
callback or unrelated background borrower that may survive an ordinary stop.

`Session_actor.retire_runtime_worker_with_cleanup` exposes those selected
obligations through a cleanup holder and the same foreground retirement promise.
Its `Retirement_cleanup.snapshot` operation only observes the latest immutable
snapshot. An external `advance` supplies that exact snapshot and the original
Actor; it rejects wrong-domain, wrong-Actor, stale or concurrent calls before
starting cleanup. Progress distinguishes a still-running original producer
(`Pending`), a permitted exact retry (`Retryable`), an immutable failure
(`Terminal`), and positively completed selected obligations (`Joined`). Errors
and original exception backtraces remain available in the snapshot. Later
observations never change an earlier snapshot.

Each cleanup attempt records its result and whether release had occurred when
that result was produced. A later successful retry cannot change the recorded
boundary of an earlier failure. Observing an older attempt also cannot erase a
later permanent failure. These records describe actual existing attempts; they
do not authorize repeating callbacks, publication or provider calls.

Advancement preserves the existing cleanup order and schedulers. It returns
while an original producer is still running, rather than moving that producer's
join into cleanup. A caller integrating this API must keep the foreground join
in its existing position, then advance again to check the same retained factual
cells after the selected parents finish. Admitted cleanup defers caller cancellation until its actual
operation finishes. An early positive factual observation, a missing current
entry or an empty queue cannot establish `Joined`. Completion applies to this
selection, not to unrelated surviving background work or every Actor resource.

Owner ordinary retirement now follows that sequence: advance the selected
cleanup, join its original foreground operation, and advance again before closing
physical resources. It retains each pass result separately. Pending work may
continue to the foreground join, but a final pending result cannot authorize
physical cleanup. Complete-resource retirement also records its later cleanup
drains separately; those calls do not enlarge the earlier Actor selection.
Physical cleanup reports retain selection or revocation, the actual release, and
subsequent bookkeeping as distinct outcomes. Source and lease recovery uses
these retained records; it does not infer cleanup success from current Owner state.

Callback completion keeps three separate facts: the callback's actual return,
the release of its bookkeeping, and the full result of the cleanup attempt that
performed that release. Control-driven cleanup can release bookkeeping while the
callback is still unwinding, so it is not evidence that the callback returned.
Publication can also fail after bookkeeping was released. The exact releasing
attempt retains that failure and its original backtrace; a later stale callback
call cannot replace it or authorize repeating publication or terminal effects.
These records preserve existing callback results and do not grant execution
authority.

The immediate claim operation runs outside Actor callbacks and Owner locks. It
refuses an active source or administration stage before installing an election,
because that stage's finalizer may itself need ordinary unloading. For callbacks,
`signal_resource_retirement` installs the admission fence without yielding,
locking, cancelling or doing physical cleanup. An external worker then runs the
returned attempt. It joins the actual source or administration extent before
capturing physical resources, including cleanup failures from an extent that
finished before the worker started. Duplicate signals join the same attempt.

Public Stop and permanent close join signalled work and deliver its first failure
before retrying. Only the exact source finalizer can run an ordinary cleanup
prefix without joining the pending attempt that waits for that finalizer. An
explicit retry retains the actual failed physical producer even if an intervening
retry failed to launch; it neither reconstructs resources nor changes old results.
Surviving native consumers must
also be drained through their actual owners before reusable complete retirement;
historical delegation records do not establish a live resource dependency.

A lexical source consumer names one actual registered source stage, even after
that Owner begins a replacement stage. Its nonyielding `Source_consumer.revoke`
closes only the captured stage's gates. `Source_consumer.join` runs externally:
it cancels that stage's actual source lease and waits for the existing report
from all its finalizers. The observer may be cancelled without completing or
changing that report. Repeated joins retain the same actual negative or positive
cleanup evidence; they neither retry failed cleanup nor certify physical Cell or
whole-Owner retirement.

Deferred builders receive this same source consumer with their construction
request. Policy restoration exposes it through `Policy_restoration_source.consumer`.
Both handles come from the active registered source callback, rather than a later
lookup of the Owner's current source. Passing the handle does not itself register
a dependency or associate a Cell target; those operations require their own checks.

Source revocation received during installation still permits the actual install
ACK to be captured into Owner cleanup custody. The closed source gate then denies
continuation before any resumed callback or disclosure. A source-only consumer
does not enter the whole-Owner retirement election or select a current replacement
source. Retained physical native resources require their separate exact cleanup
owners and reports.

A Cell cleanup target identifies one actual elected Build or Native_build unit.
It survives native publication and Product promotion: the native resource and
its Product attachment share the same target and existing reference count.
`Cleanup_target.check` verifies the creating domain and Cell before observing
that exact unit. A held target exposes only the actual currently attached
native-first and Product identities. An ordinary or loaded-first Product has
only a Product facet; absent facets grant no wildcard matching or authority.

A target becomes Released only after the actual full or partial cleanup succeeds,
or after the trusted Build closer acknowledges that same complete cleanup.
Releasing an individual pin or a nonlast Product attachment keeps the target
held. Failed cleanup keeps the same target for factual retry. Released evidence
is sticky for that old unit, including after a replacement is created in the
same Cell; it cannot close or certify the replacement. This identity grants no
current resource use, source permission or whole-Owner retirement authority.
Owner integration must separately prove affinity to the actual retained binding,
loan or construction before revoking any work.

The fixed Factory cleanup descriptor retains its same Cell rather than a second
pair of cleanup callbacks. A read-only `Resource_consumer.capture` checks the
creating Owner domain and that exact Cell, and accepts only an actually Held
target. A later `check` can recognize the old unit's sticky Released state while
a replacement remains held. Capture does not prove a current published Owner
binding and grants no execution, cancellation or close authority. Signalling the
consumer separately elects or joins retirement of that exact target. Repeated
signals retain the actual original attempt; explicit retry preserves its coverage
and failed-producer lineage without changing old results.

Cell coverage permits reuse of the existing Owner retirement body only when all
affected bindings, source custody and borrower bases belong to that same resource.
Mixed or unknown associations refuse before effects. The guard is checked again
after joins and before physical operations; it never retargets a replacement.
Whole Owner coverage remains separate from Ordinary versus All-resources strength.
An already Released old target does not join, cancel or retire an unrelated new
resource, even if that replacement has its own pending election. Its captured
source and lease obligations must still be joined; Released alone is not their
cleanup receipt or proof of complete Owner retirement.

Factory construction binds each active source to the exact target returned by its
resource selection. The association is write-once: repeating the same target is
allowed, while another target, a closed source or a released resource is refused.
Native construction checks this association inside its actual lifetime driver,
so a refusal still has a driver exit and the existing cleanup owner for resources
already acquired. This association does not turn the source and resource into a
single cleanup obligation.

Dependency report storage distinguishes active physical borrowing from retained
cleanup information. Original Native diagnostics stay immutable while the existing
provider recovery tracks physical repair. A consumer cleanup snapshot is usable
only while its captured completion identities, including original child scopes,
remain current; an in-progress retry or a changed completion requires another
observation of that same consumer. Detached records remain completion debt until
external dependency cleanup and report ownership permit their removal.

Factory records each real provider-to-consumer dependency before dependent work
can yield. The borrowed provider runtime and downstream Source or Cell have
different identities; a native ID alone cannot establish the association.
Retirement drains those actual registered consumers before provider cancellation
and physical release. A historical child that holds no current dependency remains
unaffected. Historical delegation rows are not a physical borrow graph; their
existing logical Stop and epoch checks remain separate.

Retirement fixes each dependency's role when it first selects that borrow. If the
borrow is active, the retained report owns its exact external consumer join and
provider cleanup report. Factual repair reobserves that same consumer after
provider repair; it cannot reuse a stale successful snapshot or look up a new
consumer. The role remains fixed after the borrow ends, so detachment cannot
remove consumer obligations already selected by retirement.

If the borrow had already ended before selection, retirement instead takes over
its provider cleanup report without newly retiring the historical consumer. The
original native diagnostics remain available, including any original outer error;
the provider's own current cleanup obligations still determine whether repair has
succeeded. A detached borrow whose finalizer is still running is not complete.
Retirement issues the selected cancellations before awaiting finalizers, then
repairs physical resources and rechecks completion.

A completed scope result owns its provider report before detached admission
metadata is removed. Native cleanup results also retain that report through the
same captured release handle, so an eligible repair of that loan or its enclosing
lease can complete the handoff without another parent unload. Unfinished or unavailable cleanup evidence keeps
the metadata retained. Reading session state or asking whether a source lane is
idle never performs this handoff or retries cleanup.

A consumer cleanup failure that still prevents custody release remains the
selected failure while its provider loan is held and the native receipt is
unsealed. Forwarding that failure does not seal the receipt or permit provider
cancellation or release; positive consumer results still require the original
native provenance checks.

Joined consumer custody can permit provider cleanup while canonical failure
diagnostics remain reportable. An actual later physical or control failure keeps
its result precedence. Complete publication and removal of retained metadata
require available canonical evidence, joined custody and genuinely sealed original
reports, including for a Cell request with no registered leases. A positive raw
return or an empty list alone cannot satisfy that proof.

An explicit retry of the current failed retirement may repair its selected
consumers through their existing retained-target checks. An initial close or
repeated signal only observes the retained original attempt. The new retirement
records its own first consumer invocation as original evidence even when that
invocation executes an authorized child repair; earlier attempts stay immutable.

Original reports and earlier snapshots remain immutable during these retries.
If a failed consumer join prevented a selected lease cancellation from ever
running, a later permitted repair can execute that same still-unattempted action
after the consumer joins. A detached or already finished lease takes its actual
no-op branch. Previously attempted cancellation is not replayed. The original
unattempted slot stays unchanged; the new cancellation and later join-control
observations belong only to current evidence. Getters neither execute these
operations nor add diagnostics to the original view.

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
into that ancestor is rejected. A Job or Run receipt records the outcome of child
work. Descendant token totals use the child-owned canonical usage snapshot.

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
fields and protected row data participate in admission and preservation checks.
They remain subject to the configured fold and import bounds.

Stopping a child leaves its last acknowledged contribution or unobserved slot
in the parent. The same evidence is retained if removal is permitted after the
child's delegation obligations are discharged. Further imports require the
same current parent and child owners and fresh disclosure authority.

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

A recurring definition for a fresh child can record the controller session, its
Goal ID, the Workflow admission that created the association, and the Goal
revision observed at capture. The controller checks the original Run receipt,
its Goal link and its installed source. On later occurrences, the stored episode
identifies the origin even after that Run ends. The controller still checks the
current Goal and source before admitting fresh work. A historical definition
without this association keeps its original unbound encoding and digest.

The definition and its enclosing session storage carry the required semantic
marker `recurrence.controller-goal.v1` when this association is present. A reader
that cannot enforce the association must reject that data. Editing a schedule
preserves its original Goal binding.

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

A State encoding prepares one immutable export for each present child-contribution
and descendant component. Each export contains the actual preserved document and
its existing future-capacity proof; the same encoding reuses these values at the
later host-capacity check. Preparation stays at the component's original field
position, and host depth checks still use the cumulative remaining limits. This
reuse lasts for one encoding call. It does not change validation of supplied raw
documents, preserve results across edited values, increase limits, or grant any
execution authority. Authored re-admission still checks final capacity against
the original caller State, even when decoding rehydrates native fields.

Ordered replay keeps an unstamped prefix separate from a finalized mutation.
Both retain complete native, preservation and capacity checks. The prefix uses
only its checked ordered cursor's expected publication revision for combined
Goal-policy context; native counter headroom still uses the original unstamped
counters. Captured Created images remain fully validated before their compatible
unknown fields are merged. After optional transaction metadata is applied, ordinary
full State validation runs and each newly included local occurrence must match
the actual publication revision exactly. A higher publication stamp cannot hide
a mismatched occurrence. Later corrections or a generation reset in the same
transaction cannot erase that obligation.
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

If the inherited deadline expires after the child's canonical Attempt and root
confirmation, those facts remain counted, but the provider submission is denied.
The foreground operation preserves the original typed rejection, including its
non-retryable deadline error, instead of reporting a retryable internal failure.
This does not refund the attempt or authorize another submission.

If the child commits its admission but the root's confirmation write fails,
the live root keeps that reservation pending. The child retains its actual
admission receipt; failed cleanup does not turn it into unused capacity. On a
process restart, a complete confirmation transaction in the root journal is
replayed once, preserving the same child receipt and count. Importing that same
child snapshot again does not add another charge or replay provider work.
A process crash is not a power-loss simulation: even a failure before the sync
call can leave a complete readable journal frame.

New root sessions start with an explicit Unrelated origin. Factory child creation
freezes the genuine native origin before source preparation, or records Unrelated
for management creation. It acknowledges the exact parent association before
child initialization and installs the frozen
origin in the child's initial state. Existing lower keys retain their original
origin instead of selecting the current Goal or episode. A creation failure can revoke an
unfinished reservation. Once the child is linked, a denied retry preserves
the original association and its acknowledged accounting.

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

Governed Turn and Attempt producers use the root reservation and confirmation
protocol. Confirmation consumes the genuine canonical acknowledgement. Releasing
a reservation without an admission requires the sealed no-writer outcome. Fresh
execution also requires proof from the original execution owner.

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
only the actual import acknowledgements needed by the frozen root vector. The
child's execution path checks that vector and its current policy before the
effect boundary.

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
messages, managed sends, Run user inputs, history continuations, idle notification
and event follow-up Turns, and inference Attempts consume this ownership.
Goal-owned notification work retains its separate runtime source and admission
checks.

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
slot, and ordinary submission remains cancellable.

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
remain mandatory. A captured child can share its Goal root with the recurrence
controller. After a reservation advances that controller's store revision, the
allocator refreshes the original child startup authorization before previewing
or committing its Attempt. Provider submission then refreshes both the original execution
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
Once a recurrence Run holds its actual startup custody, validation checks that
same live prepared source and held Run together with the registered target owner.
Its own transient registry reservation does not invalidate the admitted target.
A distinct controller, fresh admission and retained-receipt reconciliation keep
their separate entry checks; a saved receipt cannot reconstruct live custody.
If Run admission committed before an input write failed, the failure retains
that known admission receipt; it does not make the Run admission uncertain.
Recovery can restore a synced but unacknowledged Turn, including its submitted
input, and import its factual charge once without restarting the abandoned
worker. An unsuccessful resource close remains a failure; factual recovery is
not evidence that the original live resources closed successfully.
An irreversible local no-writer seal retires execution and the staged controller;
the exact Slot, host and prepared factual owner remain until every root close is
acknowledged. Retiring execution does not itself release ancestor reservations.

An uncertain first authored lifecycle write leaves the prior lifecycle and Run
visible. If final settlement fails after the authored lifecycle joins, the last
confirmed Run and its acknowledged callback and child results remain visible.
Reported permanent close can release the process-local writer custody in these
two cases once the exact startup producer and retirement stages have joined and
the original construction End failure is recorded. A live or unobserved join
retains custody. Release preserves the original write and End errors, the
confirmed state and journal bytes. Strict close still reports the original
failure.

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

Goal start and resume distinguish fresh selection from ongoing authorization.
Fresh selection rejects a lifecycle-reserved entry. Once selected, ongoing
authorization rechecks the actual registered Actor, runtime owner and Store
handle before and after host policy; a temporary maintenance reservation alone
does not revoke that association. Closing, failed cleanup, owner replacement
and expired original authority still reject execution, and the Actor retains
its source, preparation and canonical-state checks.

A Goal-owned subscription wake retains its actual waiting Run, joined lifecycle
callback, original authority and exact correlated subscription occurrence. In an
associated child, the selected Turn composes both native Goal acknowledgements
with inherited ancestor receipts at the same canonical State installation. The
real Goal controller and the inherited admitted guard then govern the same worker;
launch follows ancestor confirmation without another local or ancestor quota
admission. A prepared or uncertain writer retains factual custody without
claiming handoff or authorizing effects. Positive native handoff remains distinct
from source cleanup, including when its reply is lost. Quiet or metadata-only
notification results reserve no Turn. Unrelated pending input is not adopted by
this Goal consumer, and Job and timer wakes keep their distinct queued paths.

## Idle work in an inherited Goal scope

A prospective governed admission reserves the child’s current state while it
waits for ancestor acknowledgements. New idle moderator work, including a
session-start callback, waits until that actual reservation is released.
Already-running callback completion, native acknowledgements and factual cleanup
remain available; they must be able to finish the work that owns the reservation.

An idle notification wake or a saved event/observation follow-up can start a new
Turn in a child session. OChat selects the actual work once, retains its original
history and input identities, and uses the same inherited admission path as an
explicit submission. The child worker starts only after the selected Turn has
been recorded locally and confirmed by the governing ancestors. Each subsequent
provider Attempt still has its own admission.

Versioned moderator callbacks retain the actual producer's original admission for
newly emitted queue entries. The checkpoint records each entry's full source and
structural occurrence; its process-local continuation is installed only with the
same State acknowledgement. Claiming that exact entry transfers its retained
admission into the running callback, whose completed native observation can then
retain its own continuation. Queue payloads and persisted attribution do not
grant permission. Historical entries without original custody remain unavailable,
and a known expired continuation cannot start new callback work. A separately
validated resumed Run keeps its existing admission path.

Manager-backed tool callbacks capture the actual checkpoint before handler
admission, under the existing handoff. When no checkpoint is persisted yet, its
source index and queue metadata are saved with the result. Failed capture admits
no callback. Captured DATA supplies neither authorization nor Manager
installation. The older trusted DATA-only path preserves raw untracked snapshots
with unavailable queue provenance.

External Job, timer and ingress deliveries compare the Manager's complete known
snapshot with the saved checkpoint, then append their exact retained frame while
preserving the full queue carrier and unfamiliar fields. The delivery record and
queue occurrence share one State acknowledgement. Existing genuine Job loans and
resumed Run admissions keep their own authority; payloads, creator identities and
external producer principals cannot supply a missing original admission.
A Job created with only a captured source can persist its completed delivery
without acquiring a physical Manager or a queue continuation. A retained Manager
must still match the exact original resource. For queued callbacks, Run completion
planning validates the completed callback together with the checkpoint that consumed
its queue entry.

Selected timers and ingress registrations created by an owned callback retain
independent process-local loans from that callback's original admission. The
actual selected receipt chain and its final DATA install with those loans at one
State acknowledgement. A later timer-to-subscription link preserves the same
original, and repeated ingress deliveries transfer children without renewing it.
A duplicate receipt transfers no child. Failed speculative work, source/runtime
retirement, cancellation, revocation and expiry cannot supply a replacement
original; reopening retained DATA restores no live loan. A resumed Run continues
to use its separately validated Run admission.

An ingress registration can retain immutable source and inference provenance.
Historical absence stays unavailable; an explicitly captured null origin stays
distinct from an Unbound or Run origin. A queued ingress delivery correlates its
exact retained receipt and frame with that original source, including the
installation epoch. These are durable DATA facts, not a creator admission or a
new permission. The separate `runtime.ingress-provenance.v1` reader fence prevents
an older queue-aware reader from ignoring this correlation.

A custom worker can omit `Summary.moderator_snapshot` when completing a Turn.
Omission preserves the last acknowledged checkpoint and its source installation.
Supplying a snapshot still requires the normal checkpoint validation; explicit
checkpoint changes or removal go through `Capabilities.commit_moderator` while
the worker is running.

Runtime retirement retains the actual result of a worker’s terminal checkpoint,
even when the worker finishes before shutdown begins. Cancellation retires its
execution authority while preserving this completion result. A failed checkpoint
remains a failure: joining retirement does not retry the save or imply that the
operation or runtime resources closed successfully.

Metadata-only notification delivery and callback completion do not reserve an
ancestor Turn. Pending user input or a saved follow-up may still require a Turn,
even when the notification itself is quiet. Eligible notification wakes may share
one operation while retaining their separate notification history entries. Repeated
idle polling does not create a second operation for an accepted wake. A deferred
user message adopted by a follow-up keeps its original history identity and
starts a User_submit Turn, preserving the existing automatic follow-up budget
exemption and reset behavior.

The producer priorities remain distinct. Event/observation follow-up adopts
eligible pending user input for checkpoint or Turn work; its explicit compaction
action keeps its own path. Aggregate legacy completion checks pending user input
before choosing its compaction or automatic Turn action. Suppression, Stop and
metadata-only work keep their existing native completion paths.

If earlier admission cleanup must finish first, OChat retains the same selection
through one cleanup attempt. It does not replay a callback or select different
work afterward. Changed source, worker or session basis returns the existing
stale result. Work selected for metadata only does not wait on this admission
cleanup.

Selection does not grant permission to use a replacement worker or a different
source. If either changes during ancestor admission, factual accounting remains
and the original operation is retired; the replacement is not launched. An
acknowledged operation uses its admitted guard rather than charging the same
Turn again through a fresh budget check.

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

For a genuinely governed aggregate callback, capture retains the actual selected
Turn and its completion owner. Finish drives ancestor admission after the finite
callback block has exited. Concurrent finish calls join one driver and observe
the same result or exception; cancelling a joining caller does not cancel the
first driver. That result remains available after the completion owner is
released. Ordinary completion keeps its cached-result behavior.

Source retirement joins the linked admission or acknowledged-operation cleanup
before releasing that same callback owner. Unknown writer outcomes retain the
owner and its actual partial acknowledgements. Cleanup does not rerun the
callback, reselect its output, or release another callback's claim.


Canonical policy attribution can retain a bounded causal DATA component beside
the physical inference ledger. Each selected canonical Attempt records its exact
source work and immutable Goal coverage choices. A later Attempt from the same
source can have different coverage; it cannot relabel earlier work. Subset folds
reuse the canonical handle observations and retirement rules rather than creating
another physical usage ledger. A retained stream head keeps the cause and work
witnesses needed to interpret it after the live work row is removed.

Native coverage belongs to the full session owner and stable Goal ID. If the
Goal starts a later episode, an Attempt already counted by its native account
must not also count as a causal contribution simply because the incoming cause
retains the earlier episode. Admission still validates the actual current episode
and source, the exact native account increment, and the same canonical handle.
Unbound work still contributes through its causal route.

These descriptors, subsets and historical facts grant no execution permission.
Missing historical accounting remains unavailable for causal selection while
ordinary unrelated writes continue to preserve its absence or explicit null.
Current readers use native State schema28, universal State13 and Delta9, with
`session.policy-canonical-usage.v1` on positive carriers and mutations. Older labels
cannot carry the new positive DATA. Compatible unknown children remain bounded
and preserved; their presence can protect a canonical ordinal from retirement.
Live caller admission, cross-session Goal books and provider authorization are
separate consumers of this DATA boundary.

An actual delegated policy claim now requires the same child Caller governance
binding throughout Factory selection, Owner preparation and the parent writer.
The parent source installation precedes the new receipt in the same candidate.
A registered caller, including one with no governing Goal targets, installs its
validated Cause and source-work seed with that receipt. A genuinely unrelated
controller-free caller instead retains a new parent Event descriptor using the
parent's own source and inference origin. Missing source or governance evidence
cannot select this unrelated path. An identical completed replay creates no
second seed or original loan.

The existing claim attempt retains the receipt, original and classification
before persistence. Its State acknowledgement activates them together before
publication; an uncertain write retains the same attempt. These seed facts do
not authorize provider work or replace the separate per-Attempt Goal reservation
and confirmation driver.

A subsequent Caller from this actual standalone Event or its executing native
Invocation consumes the descriptor retained by that same live owner. The private
source resolver validates its exact subject, origin and full current installation;
it does not treat absent accounting as unrelatedness. Without that positive
owner boundary the public strict source query is unchanged. This supplies only
the incoming ancestry association: actual local Goal targets, inherited scopes,
controller identity and original custody still pass their independent checks.

## Pure causal Goal contribution books

`Goal_policy_contributions` stores remote contribution receipts under the full
session owner and stable Goal ID. A book survives source and episode changes.
The component permits at most 16 books and 64 lifetime streams, including
self-targeted streams derived from the canonical policy component. Settling a
receipt does not free a stream slot or reset its nonce.

A remote reservation records the exact next canonical occurrence and its original
cause. Confirmation must match that reservation. Later observations at the same
admitted prefix replace the complete cumulative usage, including downward token
corrections. Stale or duplicate observations preserve a newer pending reservation.
Closing a reservation before a writer requires its exact current receipt; the
pure data operation does not itself prove that no writer can still run.

Self-targeted usage is derived from the canonical policy records rather than
copied into remote slots. `Goal_policy_totals` combines local, physical-descendant
and causal usage, and deduplicates pending occurrences by their canonical identity.
Contradictory descriptions of the same pending occurrence are rejected.

An older session without a contribution book has unknown causal history, even
when its retained canonical records supply a known subtotal. That subtotal still
counts. Ordinary admission can preserve historical compatibility; new causal
admission with a finite limit requires known coverage. Only a new Goal's actual
creation/accounting transition may establish known-empty history. Decoding a book
or constructing a pure update grants no execution permission.

Ordered session validation retains the actual preceding count view for each
canonical edge. Later budget changes cannot excuse an earlier exhausted
admission, and a new reservation seals an unassociated edge. Actual Goal creation
initializes known history in the same catalog/accounting transaction. Existing
Goals are never initialized merely by restoring or reading them. Goal metadata
keeps physical local accounting separate from its composed governance totals and
labelled coverage.

Full state documents use version 14, delta documents version 10, and native state
schema 29. Positive books require `session.goal-policy-contributions.v1`.
Missing and null historical fields retain their original encoding. Exact archive
state validation is unchanged; uncommitted cursor coordinates do not authorize a
standalone archive. Live causal callers must still supply the original authority,
canonical acknowledgement and actual no-writer seal through their runtime owner.


The shared inference Plan keeps physical descendant and causal-policy admission
as closed alternatives, including a combined occurrence. It selects the actual
native executing Event, Invocation or Model Job, authenticates every target owner
before filtering covered and inline choices, and freezes the same canonical
selection before any causal root reservation. Covered native or physical counts
still require the current target policy; they do not create an overlay charge.
Inline policies use the actual local Goal cell. Remote reservations retain their
own exact root cell, pending prefix and writer kernel.

One canonical State ACK installs the native ledger/accounts, physical Included
receipts, policy update, closed acknowledgement and prebuilt factual graph
registration. Provider submission waits for the full initial confirmation vector
and refreshes the original execution, source and admitted policy after waits.
Entered or unknown writes retain the same Plan and root wrappers; factual cleanup
cannot reopen provider execution. The existing graph facts duty independently
completes initial confirmations before usage corrections and retains exact
partial progress. Source retirement can run this factual duty without renewing
execution permission or replaying an H.

## Retained physical runtimes and fresh execution bindings

A stopped session can retain its physical runtime through an actual resource loan.
When an eligible delegated load or existing-Goal resume attaches a new worker to
that same source, the Factory reuses the original Manager, native services and
inference resource. The worker binding is new; retaining resources does not retain
execution permission. An execution or job loan still excludes this handoff.
Ordinary roots use a separate warm-only preflight before lease admission, explicit loading,
idle drain or locked-runtime admission. Its actual resource-bearing source stage
pins the selected Product before preparation can yield. Only initial absence with
no expected Product permits the existing cold path. Once a Product is selected,
source, original, cancellation or installation failure cannot fall back to a cold
constructor. The Factory verifies the retained authored artifact without compiling
it, binds a new worker against the same Manager and complete checkpoint, and keeps
the original source/generation/Stop basis until the Actor's actual install. A Stop
committed before Owner unload therefore rejects a stale attachment. A successful
install ACK remains owned even if retirement wins before its Owner handoff.
Administration retains its reset/unload path and background-borrower rejection;
it never uses this same-source warm adapter.

Source preparation keeps its callback outcome separate from actual cleanup results.
It records intermediate disposal failures before returning them to the callback,
then retains those failures through lease release and final stage cleanup. The
original report becomes final after its acquired finalizers return and its stage
is cleared. It never changes: a later successful retry does not rewrite the
original failure. A callback error or cancellation alone does not establish that
cleanup failed.

Observed Owner APIs carry the selected result together with its original
diagnostic groups. `with_delegated_load_source_observed` and
`load_delegated_runtime_observed` preserve the actual returned value, including a
session summary. Observed lease APIs expose their result through
`Lease_outcome.observation`; the separate cleanup handle continues to own recovery.
The raw APIs project the same implementation, so observation does not run a
callback or finalizer again.
The Factory's delegated loader uses these observed ports through the same source
selection checks. Validation failures and parent cleanup keep their own original
diagnostics; forwarding an existing failure does not create another occurrence.

A source failure keeps the same diagnostic identity when it reaches outer lease
admission. Independent cleanup failures remain visible even when a callback error
is the result returned to the caller. Selection follows the existing scope's
error rules, including an uncaught finalizer exception that replaces an earlier
callback result. Original exceptions and backtraces remain available. These
observations are diagnostic data, not permission to execute or retry work.

Native resource borrows also expose an observation of their result through
`Native_borrow_outcome.observation`. Their separate
`Native_borrow_outcome.Cleanup.observation` contains only preparation cleanup,
pin release and enclosing lease cleanup, in that order. A handled callback error
does not turn successful cleanup into a failure. A source finalizer exception
remains a cleanup failure even if it prevented the legacy preparation report
from being returned. Retrying an eligible pin cannot repair that separate failure.

Concurrent `Prepared_native.close_reported` callers share the observation of one
actual common release attempt. A later explicit failed retry has its own
diagnostic identity; successful cache writes are not repeated. The native driver
retains these observations on its existing promises. Failed local cleanup leaves
the driver available for an explicit retry. Driver exit alone does not certify
that ancestor resources have been released.

A native child rejects an already-cancelled or finished parent switch before
starting. Its completed switch retains one stable diagnostic group containing
the selected failure and supporting release-hook failures, including cancellation
that the raw aggregate may omit. These parts preserve evidence; they are not a
count of distinct failures. Repeated close retains the same group and never
reruns the hooks. Requested cancellation becomes successful close only after a
complete release report with no hook failures.

A retained cleanup handle separately exposes the latest completed cleanup
snapshot. `original` and `snapshot` return `None` while the original scope is
running. During a retry, readers continue to see the previous immutable snapshot.
An explicit legacy close can also publish a joined snapshot after it completes
those exact resources. Any remaining End or control duty keeps the source fenced.
The read-only `Cleanup_recovery.original_view` exposes the original diagnostics
without consuming their delivery or changing older observations. A fresh view may
include the later completion of a child that was already selected. Its
`diagnostics_complete` flag means those diagnostic facts are known; it does not
prove that cleanup has finished or permit a retry. Use the cleanup snapshot for
current progress.

`Cleanup_recovery.Snapshot.observation` reads the result captured with that
specific snapshot. It does not run cleanup, allocate a new failure identity, or
consult a newer snapshot. A new failure produced by a retry has its own diagnostic
identity; a retained failure keeps its existing identity. Earlier snapshots
retain their original result and backtrace even after recovery succeeds. The
current result follows the producer’s explicitly selected failure, even when
older diagnostics precede it in the retained history. Selecting that failure
does not reorder the diagnostic history or change earlier reports.

The accessor returns an outer error if the snapshot lacks the producer evidence
needed for this projection. That refusal is not a newly observed cleanup failure.
A returned observation can contain a protocol error or raised exception; its
selected diagnostic identifies that actual failure. A successful observation has
no selected failure, but can retain other diagnostic groups. Check
`Snapshot.progress` separately for cleanup completion and use the existing
handle's retry operation to request recovery.

For Independent execution, the graph bracket also retains its original cleanup
report separately from the latest retry report. An explicit retry only revisits
failed graph operations on the same graphs. It never reruns the agent callback,
release hooks or graph operations that already succeeded. A failed release hook
remains a failure after the graph is repaired. The full Switch release report
and original exception traces remain available alongside the diagnostic groups;
an empty diagnostic list alone does not establish that release finished. Retry
requests made while the original bracket or another retry is running are refused
before doing work.
`Independent_cleanup.Report.observation` projects a stored report without
running a retry or consuming the original failure. The native lifetime reports
that original failure before attempting later factual recovery.

The snapshot reports the next available action:

| Progress | Meaning |
| --- | --- |
| `Joined` | Every duty owned by this scope has completed or legitimately transferred its custody. |
| `Retryable` | At least one retained duty permits another factual attempt. Other duties may still be terminal. |
| `Blocked` | No duty is currently retryable, and an original producer or dependency is unfinished. |
| `Terminal` | Remaining failures have no permitted retry in this scope. |

These states describe the captured scope, not whether the entire session or Actor
is idle. Every actual negative remains available in duty order. An unfinished
scope with no negative is still unfinished; an empty failure list does not prove
successful cleanup.

A caller retries with the same handle, its exact latest snapshot, and its owning
runtime owner. Stale snapshots, another owner, another domain, and overlapping
retries are refused before work starts. A retry operates only on retained
resources: it does not rebuild a runtime, rerun a callback, repeat initialization,
or select replacement resources. Completed duties are not repeated. A new
snapshot is published after admitted cleanup, while earlier snapshots and the
original report remain unchanged.

A dependency consumer retains its actual source or selected resource, together
with the owning runtime. Source consumers validate that ownership before a
borrow; a session identifier is not a substitute for the retained owner or
cleanup handle. Deferred preparation issues its consumer from the already
reserved source stage, after checking pending cancellation.

Retirement reports distinguish completed physical cleanup from a diagnostic that
must still be reported. The final producer preserves an actual physical failure;
otherwise it selects the current cleanup report before publishing its outcome
once. An unavailable report causes a completion refusal, not a successful
physical-cleanup receipt. Repeated reads preserve the same outcome. A completed
handle whose cleanup custody has settled can retain its diagnostic after leaving
the owner's pending slot; that old handle cannot authorize another retry.

Permanent joined close also joins an actual All-resources retirement accepted
while its final cleanup yields. It captures that exact attempt in the same close
report, waits outside the owner mutex, and checks the current retirement fence
again under the mutex before recording disposal. It neither elects replacement
work nor retries a failed late attempt, and it does not replay the already
completed physical cleanup stages.

A native provider can already hold a dependency on a particular resource when its
owner begins permanent joined close. If the provider reaches that resource before
the owner selects its whole-runtime cleanup, the close waits for that exact
resource first. If both observe a physical failure, each keeps the original
diagnostic. An authorized retry can repair the individual resource. When the
provider originally joined a whole-runtime cleanup, only the target owner can
elect its replacement. The provider can observe that replacement even after it
finishes and the owner starts unrelated cleanup.

Cancellation is deferred while the admitted factual cleanup finishes; this is
not a timeout guarantee for the underlying resource close. An original producer
that is still running is reported as blocked instead of being joined by the
retry. Cancellation of an observer does not become a resource-cleanup failure.

Failed source cleanup retains its original handle before its source association
is cleared. New source preparation and administration remain excluded until that
same scope has a positively joined cleanup result. Existing acknowledged runtime
custody and already admitted callbacks retain their own lifecycle rules. Cleanup
custody permits disposing of the original resources, including after permanent
owner close; it never grants permission to start a new session operation.

Session display name and labels do not change retained runtime identity. Their
accepted metadata updates remain in the complete stored State. The policy resource
key, ordinary attachment basis and completed-policy access witness share a pure
Spec projection that excludes only these two display fields. Every other Spec
field, including the typed inference selection, remains exact. Physical resource,
source, permission, lifecycle and Stop checks still apply independently; metadata
comparison grants no execution permission or new original admission.

Starting an Independent child reads the committed delegation chain before loading
the child's runtime. A loaded ancestor can supply that metadata during snapshot
retention. OChat checks the same registered Actor, runtime and storage handle
before and after reading its state. Exclusive lifecycle work returns a retryable
conflict. An evicted ancestor goes through the usual registry loading path.
The subsequent source, policy and Goal checks still apply at admission.

Independent children admitted as plain generated definitions may retain a stopped
parent without loading its execution runtime. Their source preview verifies the
exact immutable generated
artifact, private locator, durable pins and single-file moderator identity as data;
it constructs no native resources and supplies no executable Program. After actual
Actor admission, the existing independent ancestry scope constructs resources once,
then full generated restoration resolves current capabilities against that genuine
registry. The restored artifact, effective pins and moderator identity must match
the selected data before a runtime or initializer is exposed. Owned children still
require a live parent, a current private relationship and the exact selected
source. Borrowing the parent's native resources does not itself load its ordinary
execution runtime or run a lifecycle callback.

`Generated_definition.Metadata_source` separately validates a Linked, unrevoked
plain-generated publication with lawful Owned or Independent lifetime, including an
Owned ancestor. Invocation-owned records require an authored-tool origin in the
durable store and remain in that separate source family.
It reuses the same full artifact, materialized-tree, bounded parser and moderator
validation without compiling a Program or constructing resources. Its abstract
DATA type cannot be used as the existing Independent-only `Source` selection;
`Source.load` retains its original lifetime restriction. Neither metadata result
proves current private authority, policy admission, Invocation ownership or physical
Source custody. Authored and Captured families retain their distinct authentic
selection and program-role validation. This metadata operation does not itself
enable moderated Independent execution or replace the separate authority, birth
and readiness Stop-epoch guards.

When source preparation selects a retained runtime, it pins that runtime before
yielding. If its last external
borrower leaves while preparation is suspended, the pin keeps that exact resource
alive until the replacement has its own lease or preparation is abandoned. No
cold rebuild substitutes for a failed warm selection.

A retained native loan follows the same current-use checks as a fresh pin.
An unfinished or failed ordinary attachment, or pending constructor cleanup,
blocks new use through either path. Existing loans still retain their original
resources and can be released; retaining cleanup custody grants no permission
to start more work.

Recovery must distinguish the latest lifecycle attempt from an earlier successful
startup. A saved checkpoint or an old successful receipt alone cannot establish
that the current script installation completed initialization. A later failed or
interrupted resume must remain visible; replacing the source, reinstalling the
same code or resetting the session must not reuse evidence from the former
installation. Historical records without sufficient evidence remain unknown.
These recovery facts do not grant permission to run a callback, reconstruct a
Manager or launch a worker. Current permissions, Goal limits and runtime ownership
remain separate checks.

Cold policy restoration uses a separate construction admission. The Factory
verifies the current source, permissions, caller and Goal governance, then restores
one physical extension Manager, native services and inference resource from the
captured checkpoint. The session may be stopped or running with its runtime
unloaded. Restoration does not install an ordinary worker, run startup or resume
callbacks, deliver queued work, execute host Tasks or call a model provider.
Module and initial-state expressions are evaluated as part of normal Manager
construction before saved state is restored; their local computation, mutation,
printing and failures are not suppressed.

The runtime owner retains a baseline resource lease after the construction stage
ends. Each policy callback obtains its own lease and fresh caller admission; the
published resource association never reuses the construction caller's authority.
Ordinary resume remains pending until explicitly admitted and must attach to the
same restored resource before releasing the baseline lease. Failed or cancelled
publication records which owner holds the actual lease, and cleanup preserves the
original failure while retaining any unfinished resource teardown. A failed
selected resource never silently falls back to a replacement constructor.

Source preparation can retain its immutable classification before a child session
is reserved. `Runtime_source_preparation.Prepared_revision` captures the selected
moderator and restored programs inside the original preparation callback. Later
binding checks the actual child's recorded selection, preparation role, complete
artifact publication and source/program contents. It does not compile the code
again or run its initializers. An ordinary authored specialist uses the delegated
program contract while retaining its child's UI or headless session invocation.
An absent program bundle does not establish that no moderator was declared; the
actual declarations distinguish no moderator, legacy and extensibility scripts.
These retained facts provide no execution permission or Goal admission.

Authored declaration metadata can also be prepared without constructing its
private tools. The same bounded source-plan phase used before authored native
setup captures the exact defining declaration, nested source graph and delegated
program role. Selection checks both the recorded name and complete source
fingerprint; equivalent repeated entries retain their deterministic representative,
while inconsistent matches refuse. Publication validation compares the expected
artifact at its actual revision and creation time. The prepared revision exposes
only its already-selected moderator as DATA. Neither the plan nor that observation
retains Startup callbacks, capabilities or current consumer authority, and neither
satisfies the full Source preparation's separate wrapper/private-resource checks.
Factory eligibility and execution still require their actual current associations
and resource scopes.

Native preparation and policy-runtime construction can also happen in that order.
A child may first need its parent's native tools and captured scripts, while the
parent has no loaded worker or Manager. The per-session resource owner retains
that prepared unit, its original source lifetime and its ancestor dependencies.
Preparation checks the actual private relationship, source, profile and generation;
it supplies no tool caller, Goal permission or lifecycle admission.
These identity checks must not load the parent's ordinary runtime. For a saved
moderator checkpoint, the native path validates the selected extensibility source
or the current installation's lifecycle evidence. It does not infer modern
moderator support from checkpoint bytes alone. Historical checkpoints lacking
that evidence remain unavailable to this native path; their presence cannot
silently trigger a parent load or substitute for execution permission.

An existing native loan checks its typed authority basis across profile and
private-store reads. It preserves the original source, runtime configuration,
installation state, owner epoch and lifetime-specific Stop and invocation checks.
Timestamp, transaction-counter and factual accounting updates do not by themselves
invalidate that resource loan. The final comparison and resource pin do not yield;
private authorization and later policy, Goal and execution checks still apply.
New native construction retains its separate, exact source-preparation fence.

`Delegation_authority.Resource_path` records a bounded, exact private ancestry
path. Its metadata observation validates the real records and current lifetime
boundary without claiming that live capabilities have been selected. Native
preparation uses the same validator: lower frames check metadata, the exact
ancestor authority selects its original capabilities, and all frames are checked
again after ancestor lookups. A lower Independent edge can relax execution
liveness for resource preparation; invocation-owned lifetimes still require their
original active invocation. The ancestor's ordinary execution authority stays
unchanged. A path captures the actual leaf's saved parent stop epoch so lawful
resumes do not reuse the admission's birth epoch. The ancestor authority keeps
its own saved epoch; the leaf cannot override stale ancestor authority. Changing
the leaf's saved epoch requires a fresh path. The resource authority basis retains
the verified endpoint and lifetime boundary, without retaining the first
consumer's permission or callbacks.

`Native_resource_origin` carries that exact ingress route through lexical
preparation. Its current validator returns an opaque observation of the same
frozen path and prefix, tied to the original source owner. An observation from
another path cannot substitute merely because both paths reach the same ancestor.
The admission checks its actual target and current consumer before and after
validation; it does not cache a successful authorization. The resource authority
basis keeps only the verified endpoint and lifetime facts, not the consumer path.
`Native_resource_custody` separately checks the actual provider's
loan, native identity, source and permission profile. It does not retain the
first consumer's authorization callback. Permanent closure of the provider
revokes current use even before its loan finishes releasing. An invalid use
check never prevents factual cleanup or retry of that same loan; later consumers
still need their own admission and current authorization.

Native compilation carries an explicit preparation purpose. Ordinary construction
keeps its original execution checks; resource setup validates the exact admitted
ancestry and original source family at its construction checkpoints. This purpose
is lexical: it is not stored in the prepared runtime, registered functions or
execution context. The ancestor retains its original execution authority. MCP
construction uses the preparation check separately from the retained context
check and each admitted caller's current authority.

Factory selection uses the actual caller's Source lease separately from the
physical Resource loan between ancestors. Each borrow retains its own release
report. Persistent capability lookups retain only the same loan's custody and
resources, so closing the first caller does not become another caller's permanent
permission check. A fresh caller must pass its own complete admission. Ordinary
Independent runtime construction uses this same shared-parent path. A separate
eligibility check verifies each moderated ancestor's current `extensibility-v1`
source, using the exact retained source or the captured source and acknowledged
lifecycle evidence when cold. The original owner admits policy execution under
the current caller and governing Goals.

A later, independently admitted policy restoration or ordinary load attaches a
Manager and execution binding to the same prepared unit. It does not recompile
the retained scripts or replace the native registry. Ordinary loading still runs
initialization when it is genuinely pending; only its acknowledged completion can
advance the installed source identity. An existing completed installation is not
reset merely because native preparation happened first.

Unloading the parent leaves shared resources alive while a child still holds its
resource loan. Reattaching an ordinary worker preserves the retained Manager,
native registry and source. Final release joins attachment cleanup and the shared
native lifetime. If preparation, attachment or cleanup fails, the owner retains
the exact partial resources and unfinished cleanup. An explicit cleanup retry
operates on those same resources, preserving completed cleanup steps instead of
rebuilding the source or replaying initialization.

Each native borrower retains a cleanup handle for its exact resource loan.
Releasing that loan does not prove the entire shared resource has closed.
Preparation cleanup, loan release and the enclosing runtime lease cleanup
remain separate, immutable outcomes. Once the dependent runtime and inference
graph have joined, a later explicit close can retry the same failed loan when
preparation and enclosing lease cleanup succeeded. If the loan instead joined
and the enclosing lease failed, repair uses that lease's retained cleanup
operation and its original provider Owner. It preserves completed physical
steps and requires a completed, sealed recovery result before reporting success.
The same released loan then completes its existing report handoff without
repeating physical release; the caller rechecks recovery completion after that
handoff. An in-progress retry or unavailable evidence cannot stand in for
completion.

The original failure and its diagnostics stay unchanged after repair; the
current close result records the actual repair outcome. A failed preparation
or an opaque outer escape without a retained recovery operation remains a
failure. The delegated runtime checks these actual outcomes even when its
driver reports a successful close. Cleanup never grants permission to use the
resources again or repeat initialization.

Preserving a queued frame does not renew its execution permission. Subsequent
ordinary draining still applies timer cancellation, source and Goal checks.
Graceful stop preserves eligible pending timer work; Cancel can retire it without
running its callback.

Inherited resources also retain their original ancestor capability bindings.
Construction, native-resource adoption and later worker bindings preserve that
same view. A child reads its immediate parent's held registry and uses the
retained lookup for higher ancestors; it cannot substitute a newly looked-up
registry when that view is missing. Each lookup rechecks its actual resource loan
before and after disclosure. These resource checks supplement the existing
private-delegation, source, lifetime and execution-authority checks.

The same native lifetime also carries a scoped lookup of each held ancestor's
actual selected immutable Source. Each node retains its provider's physical
custody and Actor/Owner/Handle association, without the first dependent's
execution check. Ordinary construction, native preparation and Product adoption
preserve that same lookup. Expired or replaced providers fail rather than loading
another resource. A failed callback or original exception remains primary if its
physical view expires during use.

A scoped delegated-policy resource can compare that Source with the Source of
its already pinned Product by physical identity. This factual comparison may wait
for the Owner mutex; it grants no policy claim or execution permission. The
transport supplies the complete request-bound policy-vector preparation path.
It classifies every actual Source before restoration, pins all required policy
Products root-to-leaf, then evaluates actual receipts leaf-to-root with ordered
rewrites. An explicitly unmoderated Source continues through the same pure call
validation without inventing an approval receipt. Missing, foreign or legacy
Sources cannot become absence.

Restoration uses a genuine preflight Caller binding. Once all resources are held,
the request captures and immediately binds that SAME Caller again, with no await
between capture and binding. Both bindings must still match the same frozen
source, work and Goal data at every policy effect boundary. The existing seed
consumes stored preflight data; the final guard proves that those immutable facts
are unchanged. Actual nested cleanup reports retain their original errors and
raw traces. Stateful Independent construction separately checks ancestry
eligibility at birth and recovery, and later access requires fresh authority.
A terminal creator Run leaves the child's recorded Goal associations in place.
Later operations must obtain fresh admission against the current governing Goals.
When Stop holds a governing Goal, its owner's Hold denies new governed work even
if an Independent edge still permits the child to remain running.

This single-resource mechanism is distinct from preparing an Independent child's
complete ancestor resource and policy-claim vector. Its availability does not
remove the ancestry, native dependency, source or Goal checks on child creation,
later input or cold recovery.

A completed lifecycle may be reused only through a process-local witness tied to
the actual Product, Manager, selected source, worker and live binding. Stored
checkpoints and equal revisions cannot manufacture that witness. Existing-Goal
resume may use it without replaying initialization; initial and delegated-load
admission retain their own checks. A pending legacy resume instead executes the
actual callback and acknowledges its checkpoint through the existing shared gate.

The Factory owns partial construction before any resource can escape. Cleanup
retains the original failure and exact unfinished closer. Maintenance and inactive
close remain excluded until factual cleanup succeeds. A retry closes that same
resource and graph; it does not rebuild the runtime or replay a constructor.
Physical resource release runs once, while unfinished graph settlement can be
retried explicitly.


Callback cleanup belongs to the invocation that acquired it. A Product-backed
worker retains its original operation receipt on that Product, including across
retained runtime bindings. Idle, resume and queued-event callbacks without a
foreground operation belong to the individual runtime lease that admitted them.
Reusing an idle port does not reuse a previous caller's cleanup owner.

A user callback failure, a cleanup failure and a terminal bookkeeping failure
are separate outcomes. A handled user failure can leave cleanup complete. In
contrast, releasing a resource or successfully persisting a later operation does
not erase an earlier cleanup failure. Completed operations are removed from the
live cleanup collection only after their actual callback cleanup and terminal
bookkeeping are positive; this collection is not conversation history.

Cleanup observations preserve the original operation and callback identities.
Previously returned observations remain unchanged. Resource retirement selects
its finite set of callback obligations before physical close; later observation
refreshes those same obligations rather than attaching future Product work to
an old cleanup result. Observation cannot run callbacks or grant permission to
reuse a retired runtime. Custom worker and Job hosts remain responsible for
retaining their actual callback receipts through the documented capture hooks.

A retained legacy resume that still needs its constructor callback uses a fresh
inference graph issued by that callback's actual initialization scope. Its lexical
model view carries the graph identity, allocator binding and observation/completion
callbacks together; changing identity alone would leave usage attached to the old
graph. The physical Manager, native tools and moderator history allocator remain
shared. The existing callback gate creates this graph only for the callback that
actually runs. Reusing a completed acknowledgement does not create another graph.

The resource reserves cleanup ownership before graph creation can yield. Once the
callback exits, the same graph is sealed and finished. Failed factual cleanup stays
visible even while other resource loans keep the physical runtime alive. Explicit
cleanup can retry that graph without replaying the callback or closing healthy
shared services. Graph settlement and a failed callback's terminal receipt remain
separate cleanup obligations; completing one does not imply the other completed.


Synchronous constructor Model calls retain their actual producer before queued
Job admission. Acknowledged admission and a running claim are separate facts: if
the constructor exits between them, cleanup cancels the actual unclaimed Job
without inventing an execution. Once execution returns, the producer records its
first result before joining cancellation or awaiting another Actor reply. It also
retains the first completion clock pair; a failed clock retains the result and
its diagnostic rather than substituting an interruption or sampling again.

Each admission, claim, terminal and explicit cancellation writer keeps its exact
candidate and observed writer progress. Only a proved pre-writer failure permits factual preparation of
a replacement attempt. An entered or unobserved failed writer retains its original
outcome and excludes later canonical writers against that uncertain constructor
basis, including new Model admission. This exclusion grants no execution rights
and does not prevent pure observation or returning an existing resource loan.
Pending cleanup still retains the runtime. An already installed terminal Job
remains the winner, including cancellation; cleanup does
not overwrite it with a later provider result or revise an existing Run proof.

Explicit joined cleanup settles the retained Job through the existing atomic
Job/Run completion writer before retiring its constructor callback. The same
acknowledgement advances only that callback's frozen constructor projection,
including any Goal catalog settlement derived by the same Job/Run transaction.
Its original terminal outcome, completion time and closed Run scope are retained.
Graph settlement and resource release remain separate obligations. Finite unload
rejects while a constructor Job still needs this cleanup. Finite close requests
cancellation and retains pending resources; it cannot treat that request as a
producer join. Returning the last resource loan also cannot close a retired runtime
while its constructor Job remains unresolved. Joined unload and strong close wait
for the actual producer before performing the factual passes outside the Owner lock. Cleanup never submits a
provider again. An unexpected Actor publication exception retains the existing
fatal-owner contract: durable recovery, rather than further RPCs to that Actor,
accounts for any acknowledged Job state.


A real host holding the current Event or Invocation Model admission capability
may select `Safe_retry` when first creating a Model Job. Its attempt limit must
be positive and its backoff nonnegative, within the existing machine-integer
bounds. `Never` remains supported; this port does not admit `Idempotent` or change
ChatML Model.call/Model.spawn defaults or syntax. The retry policy is part of
the Job's immutable creation identity. The normal retry acknowledgement retains
the same original loan and the next claim advances the attempt; an admitted DTO
must not be rewritten to acquire retry custody. Failed pre-writer commits retain
the prior phase, while an uncertain attempted write cannot authorize replay.
Each actual retry inference still requires its own canonical Attempt and the
corresponding governing Goal accounting.
Retry timing uses checked millisecond addition to the observed completion time.
If that time cannot be represented, the failed attempt becomes terminal just as
it does when retries are exhausted; its original failure and stored completion
remain intact. Background recovery preserves an artifact's original completion
time, while retry scheduling uses the current completion observation. Neither
path queues a wrapped timestamp or replays the completed provider.

### Forwarded policy provenance

A policy cause keeps the actual immediate caller and Event even when earlier
sessions also govern the work. Pure forwarding DATA carries the original Goal
descriptors in a flat sequence of source-qualified frames and introduction
indices. It does not nest whole causes or manufacture inherited scopes. Every
leg, captured depth, source endpoint and normalized target is validated. With a
captured ancestor bound D, each introduction has at most D+1 targets and the
complete vector has at most (D+1)^2; overflow rejects the whole transition.

A caller source is explicit: either an installed moderator `Run_source`, or an
unmoderated artifact identity containing generation, prompt revision,
configuration revision and the verified complete artifact manifest digest. A plain
ChatMD caller can retain every authenticated inherited Goal scope without gaining
a moderator or installation epoch. Missing source evidence is not an unmoderated
identity. Direct Local targets require the actual installed caller source; every
policy callee remains installed, and the next hop must match that exact callee.
These are validated DATA distinctions, not runtime installation or permission
proof. Runtime issuers authenticate the actual prepared source and worker.

The runtime builder records verified no-moderator selection with the complete
artifact and final physical worker, including each new retained-product binding.
Actor installation binds that factual classification to its accepted generation
and metadata-neutral runtime identity. Manager or checkpoint absence alone cannot
produce it. Ordinary manual runtimes without this fact keep their existing
services and cannot acquire a governed source identity from missing metadata.

Every State ACK either preserves that same installation basis or clears it. A
validated live configuration update prepares its replacement before persistence
and installs it at the same positive State ACK, before publication. Rejected or
unknown writes do not advance the basis; a publication error after ACK does not
undo it. Fresh callers can capture the new revision, while earlier callers and
bound preparations retain their exact original binding and reject after its
replacement. Stop, original-custody, ancestry and effect checks remain separate.

A stable stream records only its route and target. Event, Invocation, Model Job
attempt and source-work hashes belong to the occurrence witness, so a new genuine
event on the same route does not allocate another lifetime stream. Each frame
has a validated digest of its complete known body, including work and attempt.
The existing keyed carrier preserves an unchanged earlier frame's unknown fields
while an unprotected sibling occurrence changes. Replacing a frame that owns
protected unknown fields rejects instead of transferring those fields to new work.

Exports copy the complete child from the current canonical carrier and revalidate
the exact registered ready work. Currentness compares the entire regenerated
known and raw export, including every earlier frame and original target. Earlier
frames are historical DATA; the export does not refresh old permissions or grant
live execution. Direct Local targets retain their original caller/source law;
a forwarded Local target additionally requires its exact introduction route and
Run-origin witness.

The pending Goal receipt derives the exact forwarding witness digest from that
same cause. Import checks its presence and correspondence along with the existing
canonical occurrence and prefix. A pending forwarded import reserves the existing
complete projection budget, including concrete enclosing depth, until confirmation
or proved no-writer closure. Reservations subtract the actual preserved projection
already counted and can reject the whole reserve; no component budget increases.
Both source alternatives pass through the same complete child validation and
projection reservations. The installed envelope adds one structural level, which
is charged by the actual encoded Cause, Stream and frame. A changed source changes
the immutable stream/frame identity; it cannot reuse another source's protected
extensions. Future forwarded occurrences still reserve the entire existing
projection profile, including its depth beneath the Book. No byte, field, node or
depth ceiling increases, and insufficient headroom rejects the whole transition.

Native domain encoders remain known-only. Complete copied raw children and wire
updates pass through the existing validated decode/adopt carriers, preserving
compatible extensions and rejecting protected conflicts. Runtime caller and
private ancestry authentication remain separate from these pure DATA operations.


The private causal writer helpers distinguish the policy check before reservation
from the count-free activation check while that exact reservation is held. After
the canonical admission ACK, the same retained policy cell uses its admitted
check; it does not reserve capacity again. An observed pre-writer failure can
seal only its original retained attempt, State, candidate and owning plan. The
seal cannot erase attempted or unknown writer facts, and factual cleanup does
not require the current State to remain equal to that original State.


Finite causal route observation retains the same occupied native preparation and
frozen per-H selection. It resolves every original forwarding-frame path, current
cause path and genuine callee ancestry suffix through the actual shared Store
outside the Actor mailbox. Each finite consume repeats the original Actor and
lifecycle basis. Scope anchors match actual full delegation records; they never
construct references, and their branch generation must match the frozen source.
Covered and inline targets undergo the same route validation before reservation
filtering. The opaque owner list contains locators only. Each actual root repeats
the shared-Store and full-record checks before issuing a remote overlay route.
These association observations grant no provider permission or quota reservation;
the remaining per-H reserve, canonical ACK and confirmation driver is separate.

The causal receiver retains each finite root reservation in the same Plan-owned
Set before replying. Its actual Actor link accompanies the existing native
attempt; it does not duplicate request ordering or phase ownership. The root
uses the existing strict Goal policy cell, checks prospective count availability
before reservation, and refreshes an exact owned pending reservation without
counting it a second time.

Reserve and automatic Hold writes use the existing canonical commit kernel. A
single retained root fence rejects unrelated writes while an outcome is unknown.
Only the exact observed no-writer failure or the actual State-ACK suffix can
settle that fence. The ACK suffix installs prebuilt custody before fallible
publication, so a publication failure preserves the factual reservation or Hold
ACK. A known non-reservation outcome does not imply that no storage write took
place. These finite receiver operations grant no provider permission; canonical
admission and every required root import confirmation remain separate steps.


The factual terminal receiver observes complete preserved canonical stream
projections at the actual callee. It checks the same Factory, Store and full
session identity without renewing the old execution original, policy source,
worker or live work row. An old stream may remain factual after a later State
generation; absence never establishes an empty stream.

Initial import consumes only its exact pending prefix through the actual root
State ACK. Confirmation checks the complete ordered import vector against that
same canonical Attempt and its exact original head. Later usage corrections have
a separate opaque ACK and cannot confirm an admission. They replace usage at the
current confirmed prefix without changing counts, pending reservations or the
current head. A projection awaiting another attempt's initial confirmation
returns a distinct waiting outcome; it neither consumes that pending reservation
nor reports a provider failure. Complete unchanged duplicates and superseded
observations need no writer; a duplicate that carries preserved raw-data changes
uses the same factual writer.

Each correction operation is retained before its prepare reply and shares the
existing root commit fence. Lost replies reconcile that exact immutable operation
receipt; a later correction cannot rewind an earlier ACK into the current phase.
The actual reservation attempt also retains its initial ACK in one narrow
receipt installed before publication, so later corrections cannot erase initial
confirmation or its lost-reply reconciliation.
Only an actual failed no-writer outcome can restore the prior settlement. Unknown
writes remain retained, and the actual State-ACK suffix installs prebuilt custody
before publication. The composed per-attempt Plan consumes the initial
acknowledgements before provider submission. The graph's factual delivery uses
the retained receivers to complete confirmation and apply usage corrections.


Lifecycle retirement closes admission before joining a runtime. During that join,
retained canonical usage and terminal import, close, correction and reconciliation
may still complete through the same Actor, controller, Store handle and writer.
Fresh route capture, reservations and admitted-policy refresh remain denied.
Factual capture and consume still check exact current State. A successfully
consumed complete usage observation remains an immutable historical fact after
later metadata or usage ACKs: later import checks the same actual source Actor,
Factory, controller, full session and installed canonical ACK, plus retained writer
availability. The root still checks its exact current candidate before writing;
a closed handle or uncertain write cannot be treated as success.

Graph registrations retain two distinct duties: confirming the initial import
vector and settling later usage. An outside-mailbox sweep progresses initial
confirmations before corrections, so a correction waiting on another initial
import cannot block that import's driver. Fresh allocation requires settled facts.
Cleanup failures remain owned for retry, including a session whose initialization
failed before registry publication. The registry retains that actual entry and
its cleanup failure rather than discarding the resources or replacing the primary
initialization error.

Policy and ordinary source admission first settle pending inference-usage facts
through this bounded sweep, outside the runtime owner mutex. Source election then
rechecks pending facts and the existing ownership, source and permission guards.
A failed or cancelled sweep preserves its original outcome and acquires no source
stage. Administrative source changes retain their separate admission path.

Ordinary idle polling also performs the finite factual sweep before checking for
pending moderator work. This lets terminal usage settle even when no callback or
notification remains. Factual progress grants no execution authority and does not
change a no-work result into a successful wake; subsequent work selection keeps
its existing source, ownership and permission checks.
When the sweep reports deferred initial confirmation, the unfinished duty stays
owned and idle polling continues its existing work-selection path. Actual failures
still propagate. Admission and teardown use the strict drain, which requires
settlement and reports deferred confirmation as a retryable conflict.

A native graph finish and its enclosing script's cleanup are separate steps.
The native acknowledgement retains the exact original scope in the Actor's
existing graph owner. If usage settlement fails after that acknowledgement, a
later cleanup pass uses the retained scope to remove the graph from both the
Actor and the enclosing script. It does not repeat the native write or infer
that the script has finished from an empty routing table. Successful cleanup
drops the retained scope reference; an exact repeat of the acknowledged finish
remains harmless.

The same per-attempt custody retains the initial observation and successfully
imported ordered prefix across failed or lost replies. Later correction retains
its exact observation and prepared operation until actual settlement, then drops
that operation's payload. This transport has no separate election or runtime
owner; the graph registration's factual duty owns progress. It never turns a
missing source, zero remote targets or a complete vector into provider permission.
All actual target owners, including inline and physically covered targets, must
be validated before the remote reservation vector is completed.

Direct inference from a running Async Job retains `Async_job_attempt` work with
that Job's exact claimed attempt. Model Jobs retain their separate work kind.
Both use the same canonical Handle, current native row checks and causal
selection; an Async ancestor is never relabelled as a Model Job. Retries keep
the stable Job member while each allocated Handle names its actual attempt.
The work descriptor is immutable data and grants no execution or Goal quota.

### Per-call Goal account serialization

A Goal account child can prepare an immutable `Goal_accounting_document.Export`
containing its complete merged document and retirement-capacity result. A single
account catalog encoding reuses each child's preparation for its document and the
later catalog growth check. Unknown envelope and payload fields, original member
order, protected retirement conflicts and capacity limits retain the same checks.
These preparations are local to one serializer call, not caches or accounting
authority. Growth-only operations retain their empty-binding fast path and do not
add intrinsic capacity admission. Raw decode/adopt checks and independent State
validation remain separate.

During one canonical policy-usage validation, repeated checks for the same
immutable Cause and typed Target reuse their first complete stream derivation.
The original raw stream constructor runs at the first actual use; validation
order, short-circuiting, source children and exact subset checks remain intact.
Only visited successful derivations are retained, and only for that validation
call. Public stream queries and independent raw codec checks do not share this
local storage or gain accounting authority.


During one policy carrier export, capacity checks reuse its exact complete current
document and one future projection. Preserved raw children and projection capacity
are checked before growth; work reservations, supplied-raw capacity and late depth
checks keep their original order. Public growth and depth queries retain their
separate contracts. Independent decode and adoption checks still validate their
own complete raw input; preparations belong only to their enclosing serializer
call and grant no authority.

Within one State capacity check, the selected original policy carrier similarly
prepares one complete current/future pair. Its future depth is checked with both
State ancestors against the cumulative host limits before requiring the supplied
raw child. That actual raw child is then validated, followed by reserved growth
and the remaining host budget, in the same order. Native and encoded State paths
retain their original capacity basis. Within one full or ordered-prefix State
serializer, the policy field export retains its complete document and current/future
capacity pair for that same encoding's later host check. The export is first forced
at the original field position; the host still checks its actual supplied raw child
in the order above. Authored readmission creates a separate encoding, and Native
State validation, raw admission and adoption checks still prepare independently.
No export is retained on the State or shared across operations, revisions or domains.

A Goal contribution book export reuses its first complete current document after
checking original raw children. Its private prepared capacity holds the existing
full growth/depth calculation, including forwarded profiles and future work.
State prepares that result once at its original depth-check point, then preserves
the two-ancestor host check, actual supplied-raw validation and cumulative growth
order. Independent raw admission and public queries still prepare separately.
The public Book type intentionally aliases its private carrier, as canonical
policy usage does; the capacity receiver remains private and grants no authority.
