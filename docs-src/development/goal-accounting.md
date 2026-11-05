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
