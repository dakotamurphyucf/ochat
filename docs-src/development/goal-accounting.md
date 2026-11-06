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

The current integration is a DATA foundation. The actual public Create command
registers its local root, while delegated and recurrence session creation still
leave Goal origin unknown. Those creation paths do not yet capture an active
Goal's inherited scopes or enforce descendant turn/attempt ceilings. Runtime
integration must supply the actual frozen origin, reserve the parent frontier
before child effects, and consume genuine canonical acknowledgements or sealed
no-writer outcomes. Retained origin, reservation and Included DATA cannot replace
those execution-owner proofs.
