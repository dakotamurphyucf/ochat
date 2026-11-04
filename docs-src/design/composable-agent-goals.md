# Persistent goals and owned execution

Status: accepted engineering direction with a persistent actor-owned metadata
catalog, internal authenticated operator commands, immutable episode-association
planning and actual Run terminal convergence. Authenticated controller execution
admission, full authored goal workflows and cumulative goal accounting remain
separate integration work. Existing Run lifecycle, actor receipts, bounded inference accounting
and continuation scheduling are the foundations. This document does not claim a
passing authored goal workflow or full context-window rollover support.

## One objective owner

A goal is a session-local objective with progress, admission control, budgets and
accounting. `Session_actor` owns its sole durable record in a validated named-field
child of `Session_state`. Operator controls, selected native tools and ChatML
helpers use this same owner. A moderator checkpoint may retain a goal reference
or helper-local data; it must not retain another writable objective status.

A goal's objective disposition is `Open` or explicitly `Closed` with validated
achievement or abandonment evidence. An accepted achievement request keeps the
objective open until the actual Run completion settles. Its admission control is enabled or held with a typed
reason: operator pause, required intervention, budget evidence, unsafe accounting
uncertainty or source review. These decisions describe the objective and policy;
they do not assert that external work has ended.

Actual execution remains owned by
[`Agent_protocol.Run`](../../lib/agent_protocol/run.mli). Its
Admitted/Active/Waiting/Terminal lifecycle, owned work and terminal evidence are
authoritative. A goal links sequential immutable Run episodes. The execution view
is derived from the current Run and actor custody; it is not another writable
Goal Running/Waiting/Interrupted enum. A terminal Run cannot reopen. A goal can
remain open after a failed, limited or interrupted execution episode.

At most one goal is enabled for foreground execution in a session, consistent
with the existing single unfinished Workflow owner per source. A Run binds at
most one goal, and a goal binds at most one nonterminal Run. Other objectives may
remain held or closed. This adds no goal scheduler or fairness policy.

The domain uses abstract validated values for goal identity, bounded objective
and progress, control, closure evidence, budgets and accounting. Revisions advance
with checked arithmetic. Decoding validates the same invariants as construction;
storage preserves historical absence and forward fields. Closed goals cannot be
enabled or resumed. A new objective identity is distinct from editing an existing
objective, whose accounting scope remains intact.

## Commands, authority and composition

The shared command owner admits creation, objective revision, progress updates,
hold, resume, closure and permitted budget changes. Each command has an expected
revision and retained idempotency receipt. An identical retry returns the original
receipt; a changed request under the same key conflicts. A historical receipt
never grants current source or execution authority.

The host captures the authenticated principal, actual session generation, source
installation and execution owner. A model-supplied goal ID, imported instruction,
helper module or stored digest grants no additional authority. Native goal tools
must be explicitly selected in ChatMD and remain subject to current tool policy.
Credentials remain with the runtime host.

Objective and progress updates from a moderator callback stage under its actual
borrowed execution. The actor seals preparation and atomically commits the goal
change, moderator checkpoint, actual Run decision and execution receipt. Failure
before commit leaves all of them unchanged. Scopes expire on rollback, commit,
cancellation or callback exit. Ordinary model/native and operator commands use
their actual invocation or command owner; they do not borrow a fictitious callback.

Helpers reuse [`Run_action`](../../lib/agent_protocol/run_action.mli)
Continue/Wait/Finish. A pure helper recommendation does not authorize execution.
The existing action-combination contract rejects incompatible surviving decisions
before persistence. Wait requires an exact host-issued timer delivery, job attempt
or subscription occurrence; human intervention is a held goal policy, not a fake
wake occurrence. No new language syntax is required.

Goal activation uses the existing checked Run preparation and admission path.
Current public Run starts require an actual writable attachment, and
`Authored_start` requires genuine unconsumed startup. Headless automation and goal
reactivation therefore need one shared typed actor admission extension. It must
capture current host authority from the actual controller occurrence or authorized
command, recheck it at final commit, and retain the existing preparation and
persistence-before-worker handoff. It must not fabricate an attachment or replay
consumed startup.

## Continuation and control

[`Run_intent`](../../lib/agent_session/run_intent.mli) and the existing
[follow-up planner](../../lib/agent_session/observation_follow_up.mli) remain the
durable continuation owners. Returned runtime requests are already retained by
the actor; a caller consumes their intent at the actual scheduling boundary and
does not independently schedule them again. Compaction precedes its exact
dependent turn. Applied scheduling intent proves admission, not completion.

Goal control and budgets are checked at each actual owned admission, including
idle follow-ups, foreground provider continuations and attributed descendants.
Checking only idle follow-ups would miss existing foreground and administrative
entry points. The actor captures attribution from the real execution owner,
never by guessing from its current operation or accepting an arbitrary ID.

Pause fences new goal-owned work at the serialized admission boundary. Cancellation
of already admitted work is a separate operation using current owned cancellation
and receipt contracts. The view must show outstanding work until its real owner
settles. Holding a goal retires only its owned future continuation, with a retained
reason, and leaves unrelated session jobs and capability intent alone. Resume
issues a new admitted decision; it does not rearm a retired receipt.

A model cannot clear an operator hold or raise an operator-imposed budget. An
ordinary user message neither clears a hold nor resets cumulative goal accounting.
Explicit operator resume rechecks control, source and outstanding effects. A still
valid nonterminal Run can receive a new admitted decision; an interrupted or
terminal Run requires a new actual episode through the checked admission path.

Achievement is explicit and has two boundaries. A selected completion tool stages
an achievement request with the checkpoint and the existing Run Finish action.
The committed request retains its exact action-receipt association and bounded
proposed evidence. Its reply means requested, not achieved. Existing Run Finish
may remain pending until the actual owned root operation and other required work
settle; there is no universal human-confirmation requirement.

The goal becomes achieved only in the actor transaction that validates and commits
the actual matching `Run.Terminal.Completed` evidence and terminal receipt. If
Finish can validly settle immediately, request and achievement may share that
transaction; otherwise the goal remains open through the pending interval. The
actor joins goal closure to the existing
[`Run_transition.settle_operation`](../../lib/agent_session/run_transition.mli)
boundary. A failed, cancelled, limited or interrupted winner leaves the objective
unachieved and held for explicit review. Repeated settlement cannot close it again.
No independent operator-only achievement shortcut is selected.

Pending achievement is a receipt association, not another writable Run lifecycle.
Whether the request is pending, superseded by adverse outcome or successfully
settled is derived from its exact Run/action/terminal evidence. A later episode
cannot satisfy an earlier episode's request. Successful completion does not stop
the session or cancel unrelated work. A model turn ending, exhausted budget, lost
connection or stopped session cannot imply achievement. Exhaustion holds an
unachieved objective; an actual Run may end Limited without converting the goal
to successful completion.

### Proposed domain receipt fields

The pure goal domain distinguishes requested achievement from validated closure
without storing a second execution status:

```ocaml
module Achievement_request : sig
  type t
  val run_id : t -> Agent_protocol.Id.Run.t
  val action_receipt : t -> Run_receipt_reference.t
  val proposed_evidence : t -> Evidence.t
end

module Achievement : sig
  type t
  val request : t -> Achievement_request.t
  val terminal_receipt : t -> Run_receipt_reference.t
end

module Disposition : sig
  type t = Open | Closed of Closure.t
  (* Closure is validated Achieved of Achievement.t or Abandoned of Reason.t. *)
end
```

`Run_receipt_reference` captures the existing receipt's immutable Run, source,
principal, key, kind and revision identity, with no mutable lifecycle field. A
request reference must identify an admitted Finish action for the bound episode;
an achievement reference must identify its actual matching Completed terminal
receipt. A pure constructor validates structural consistency; the actor proves
these references against its authoritative Run index at commit. A request or
terminal receipt supplied as arbitrary JSON is not sufficient proof.

An open goal may retain a latest achievement-request reference. Retrying it
returns the existing request receipt. A new episode cannot overwrite an unresolved
request, and a stale request cannot close a newer episode. The public view derives
request disposition from retained Run evidence. The actor retains or retires old
associations according to the same bounded evidence policy when explicitly moving
to another episode; it does not manufacture another Pending/Active/Failed status.
Accounting can continue to receive valid late evidence independently of either
request or closure.

### Implemented pure evidence and capacity boundary

The pure protocol/domain, universal carrier and actor-owned metadata catalog are
implemented. Internal original-operator commands cover create/read, objective and
progress edits, hold, permitted budget changes, explicit source adoption and
abandonment. They never enable or resume execution. A minimal scoped `Goal.finish`
primitive stages completion beside the actual Run Action receipt and checkpoint;
only actual matching terminal evidence closes the objective. Its pure admission
association planner still requires an authenticated controller consumer. Actual
controller-admitted authored execution and worker-join qualification, configurable
native/tool wrappers, continuation and cumulative accounting remain integration
work. The [episode and settlement contract](../lib/agent_session/goal_runs.doc.md)
details these boundaries. The
[metadata command contract](../lib/agent_session/goal_catalog.doc.md) specifies the
16-goal/256-receipt finite retention policy, source/reset/recovery holds and complete
host capacity reservation. Internal holds do not consume user receipt slots;
actual terminal settlement preserves that separation.
`Goal_episode.Settlement` retains an immutable episode and exact Terminal receipt,
not a copied terminal classification. Live constructors validate the actual Run and
canonical receipt digest. Achievement additionally hashes canonical
`Run.Terminal.Completed(requested_result)` from the exact retained Finish request
and compares it to that Terminal receipt. Decoding an achieved marker alone cannot
produce achievement. The Goal wire payload retains the request and settlement once;
closed state references that pair instead of embedding another copy. Other retained
Run history remains owned by the existing Run index under its retention policy.

The foundation uses a 128 KiB owned projection and a 256 KiB complete carrier, each
at depth 64. These are finite engineering bounds. Before accepting a request, the
pure domain reserves its exact immutable request plus conservative maximum bounded
mutable metadata and current/predecessor episode/receipt slots. The carrier charges
all retained unknown JSON and envelope overhead plus positive reserved owned growth,
including escaped bytes, fields and nodes. The actor must preflight that complete
carrier before staging the associated Finish, in the same owner transaction; it
must not first discover a goal capacity failure after the Run settles. Completion,
adverse retirement and next-episode binding then fit that reserved quota. Existing
unknown variant/container conflicts remain explicit guards. The actor must
preflight future-schema compatibility before effects as well as reserved quota;
reservation never permits discarding protected future data. The shared Run
terminal-capacity owner also reserves known work proofs, terminal receipts and
pending Finish result copies, additively with Goal growth in the complete host.
The persistence owner preflights the actual transaction and Journal payload
profiles, while candidate admission protects shared terminal counter headroom.
This guarantees known bounded disposition and an honest recovery record under
the fixed admitted profiles, not arbitrary future worker history or diagnostics.
An oversized worker commit leaves durable evidence unchanged; recovery can
truthfully interrupt the Run and keep the objective Open. No receipt, imported
record or capacity reservation reconstructs host authority.

## Recovery and source changes

Client detachment is not objective completion. Existing host lifetime and cancellation
rules decide whether execution continues; the goal record remains durable.

[`Run_retirement.recover`](../../lib/agent_session/run_retirement.mli)
currently interrupts every nonterminal Run, including Waiting, and preserves
unconfirmed custody rather than replaying effects. Goal recovery retains objective,
progress and accounting, reconciles inference/job/Run evidence, and holds the open
objective for explicit resume. It does not reinterpret an interrupted callback as
an unconsumed instruction. Process-local scopes are never reconstructed from stored
receipts.

Actual source replacement, removal and reset rotate the existing installation
identity and retire old Run custody. Goal evidence and charges survive; open goals
are held for explicit source adoption. A matching runtime restore does not grant
new authority. Reset does not implicitly delete objective evidence or reset its
spent budget. Source adoption must recheck current authority and cannot make an
old receipt executable under the replacement source.

The goal owner lives outside transcript projection. Existing summary/history edits
and compaction cannot erase its record. Full future context-window transitions
must preserve this owner and expose only an authorized goal view to the new
context. Current moderator phases do not provide a full context-transition hook;
this design does not claim that future machinery is implemented. Existing exact
compaction/continuation receipt behavior remains authoritative.

## Accounting and limits

Goal accounting is cumulative across Run episodes, user submissions, pause/resume,
recovery and source adoption. It records exact admitted goal-owned host turns and
inference attempts, plus separately labelled observed usage. A host turn is counted
with its actual durable Operation_started admission, not each provider response.
An attempt is counted before dispatch after persistence acknowledgment, including
attempts that later fail or become interrupted. Attributed compaction inference
counts as an attempt; compaction itself is not a host turn.

These counters differ from
[`Automatic_turn_budget`](../../lib/agent_session/automatic_turn_budget.mli),
whose qualified follow-up count resets on a newly admitted user turn. Lexical
ChatML execution budgets, context capacity and host rate/permission ceilings also
remain independent. Raising or disabling a goal limit cannot relax host ceilings.

Usage retains [`Inference.Observation`](../../lib/inference/observation.mli)
semantics: Actual, Estimated and each Unknown reason stay distinct. Input, output,
reported total, cached input, cache-write input and reasoning output are independent
components. Cache/reasoning subsets are not added again to their containers, and
reported total is not silently replaced with input plus output. Arithmetic exposes
overflow instead of wrapping. A token policy selects a documented component.

An observed-token stop prevents further admission when the selected evidence
reaches its threshold. It is not a guaranteed hard token, billing or currency cap:
already dispatched work, uncertain submission and delayed usage can overshoot.
Unknown usage holds automatic continuation only when a selected token policy
cannot safely decide. A policy with no token threshold reports uncertainty without
an unnecessary hold, while independent admission and host ceilings still apply.
No missing value becomes zero or successful completion.

An optional absolute deadline can gate admission and includes pause and downtime.
This does not claim exact cumulative active elapsed time, tool execution time or
non-overlapping descendant duration. Pricing and exact billing are outside this
contract.

### Canonical rows and bounded accumulation

The durable [`Goal_accounts`](../../lib/agent_session/goal_accounts.mli) child
preserves bounded partitions through the existing session delta and snapshot
owner. Stored claims resolve only to exact retained canonical handles; historical
absence does not invent zero charges. See the [storage contract](../lib/agent_session/goal_accounting.doc.md)
for quotas, unknown-field custody, retirement eligibility and additive host
reservations. This storage foundation does not yet implement actual runtime
attribution or inference admission gates.

The existing
[`Inference_ledger`](../../lib/agent_session/inference_ledger.mli) is a bounded
retained window, not lifetime totals. It retains at most 256 attempts and 256 host
turns within a 4 MiB profile, reports tracking loss, and ignores observations for
retired rows. Subtracting session summaries cannot implement cumulative goals.

The goal owner keeps a bounded private attribution binding from an actual ledger
handle/ordinal to goal, Run and host provenance. The ledger remains the sole owner
of per-attempt usage, state, configuration and diagnostics. Goal state does not copy
raw inference rows or maintain another provider observation stream.

At each boundary the same actor transaction does the following:

1. Actual turn admission checks the goal and increments its admitted-turn counter
   with the real operation. Failure to persist prevents worker launch.
2. Actual inference admission plans its canonical ledger row and retirements.
   Before a bound terminal row retires, fold its final contribution into the goal's
   settled aggregate and remove that binding. Add the new actual binding and
   attempt counter, or explicit attributed Untracked uncertainty. Commit all
   accounting changes before returning the dispatch handle.
3. Added or replaced canonical usage changes the live contribution from that exact
   retained row. Duplicate or stale observations add nothing. A newer authoritative
   revision replaces the old contribution; same-revision conflicts reject.
4. Terminal/release/recovery evidence changes canonical interruption/submission
   classification without recounting admission or changing objective disposition.

The goal view combines settled aggregates with current bound-row contributions.
For example, actual total 7 at revision 0 replaced by 11 at revision 1 contributes
11, not 18. Another interrupted, possibly submitted attempt contributes explicit
unknown usage. A later user message does not erase either contribution.

Retirement seals the final retained contribution. An observation after retirement
cannot resurrect a row or add a new charge. Coverage explicitly states that later
post-retirement corrections are unavailable. Indefinitely revisable exact lifetime
totals would require a different retention contract. Untracked admissions record
uncertainty; they do not fabricate zero or silently lose an attributable occurrence.
Storage/counter exhaustion gates goal work explicitly without changing unrelated
canonical work's existing telemetry policy.

Goal closure preserves charge bindings. Authorized late revisions may still update
the accounting view of a closed objective while canonical rows remain retained;
they cannot reopen the goal or clear an operator hold. Goal creation begins a new
scope and does not retroactively attribute unrelated historical session attempts.

### Stored invocation inference provenance

Invocation records retain original inference attribution outside the authored
context ABI. The optional claim is either positively unbound or the exact
originating Run ID. The host captures it when the genuine operation, registered
Job, event, or parent invocation admits the call. Supplied positive claims are
rejected at native admission. A retained model-call intent compares its original
identity, body, and classification rather than acquiring a later operation's
origin.

A queued callback can be drained by an unrelated foreground operation. When an
exact acknowledged wake retains the moderator execution in a Run, that causal
Run takes precedence over the drainer for callback actions and invocation
provenance. The operation still controls the physical worker lifetime. Selection
finds the causal owner before checking live source and work eligibility; a
terminal or stale causal Run never falls back to the unrelated drainer. Retained
terminal evidence still names that original Run for historical validation and
grants no fresh execution permission.

Retained invocation records preserve the claim through outcomes, observation,
publication, recovery, and source replacement. In particular, an idle
`Tool_observed` callback inherits the original invocation's classification after
its foreground operation or Run has finished.
A terminal Run remains named. Final carrier validation checks the Run's session
and source generation, known observer/parent evidence, and any actual retained
Invocation work. This evidence adds no live Run-work obligation and grants no
execution or inference permission.

Historical absence remains unavailable. Native work under a genuine admitted
parent with unavailable classification can preserve that absence; it does not
become positively unbound. This includes native background child preparation
and its selected-child acknowledgement. Inference classification and retained
work ownership are separate: an actual retained Run parent still requires its
current authority, source and exact owned occurrence, and the child joins that
Run atomically while retaining unavailable inference classification. Missing,
stale or mismatched parents never fall back to an unrelated draining operation.
Model
attribution requires positive evidence and the separate current physical owner,
authentication, source, and accounting gates. Unknown invocation and nested origin
fields remain protected by the universal carrier, including journal replay and
snapshot restoration; replacements cannot erase or change a retained row’s
optional claim. Reset can remove current invocation rows under the existing
lifecycle policy. Any retained archive preserves the original classification
without restoring its live authority.

### Stored background inference provenance

A background Job can retain a versioned inference origin: positively unbound work,
or the immutable ID of its originating Run. Historical records that omit this
field have unavailable provenance. Absence is never converted into unbound work,
zero usage, or a choice of the current Goal. Existing records cannot acquire a
classification retroactively or change it during retry, result delivery, source
replacement, or recovery.

The complete Session carrier checks a Run origin against actual retained work for
the same Job ID and generation in that exact Run. It also checks the Run session
and source generation. An explicit unbound claim contradicting retained Run work
rejects. This check runs after the complete ordered transaction, so a new Job and
its matching Run occurrence can be published atomically. Unknown Job fields and
unknown fields inside its origin remain under the universal document carrier.

The relationship is lifetime evidence. A queued Job cancelled before its first
claim can retain a cancellation proof for its planned first attempt while the
Job attempt counter remains zero. A queued retry similarly preserves earlier
terminal history and the cancelled planned successor. Neither case changes the
origin or invents an execution attempt.

Stored provenance grants no execution permission. The allocating owner must still
prove the exact current Job attempt, its physical callback lifetime, current
original authority, and current Goal policy before provider submission.

### Live inference admission and accounting

The session actor supplies separate ports for selecting a model Job and admitting
an inference attempt. Each port retains its actual operation, Job attempt, native
invocation, or moderator callback. Runtime construction does not provide an ambient
fallback for an expired callback. The runtime binds model recipes to those ports
under the existing moderator execution lock and restores the prior binding when
the callback exits.

Requests through model recipes use the inference identity allocated to their
actual child Job. When a recipe is rebound, it uses the current request context;
it does not restore
identity or submission-guard overrides captured by an earlier callback. Identity
metadata itself grants no permission to allocate or submit a request.

A new model Job records its original classification at admission. If it belongs
to a Run, the Job and its exact work occurrence commit together. An idle observation
of a live Run can introduce real Job occurrences without fabricating a parent work
item. An observation of a terminal Run keeps that historical origin but cannot
start new attributed model work. Historical missing provenance remains unavailable;
ordinary native work can retain that absence, but model admission cannot treat it
as positively unbound work.

A completed native result can retain its original admission for its observation
callback in the existing bounded invocation inventory. The result acknowledgement
installs this continuation. The process-local inventory has one combined slot per
exact retained Invocation or Moderator_execution owner. Persistent hosts bound
those owner inventories through their existing complete-state and journal limits.
Observation and follow-up components share that slot; adding a follow-up does not
impose a second count quota on an already executed native result. A successful
observation-claim acknowledgement transfers the observation continuation. A failed
claim leaves the original continuation available for retry.
The transfer preserves the exact result, source and original permission context,
including when an unrelated foreground turn drains the observation. Claiming the
observation does not consume a separate handler follow-up on the same result.

Missing admission differs from an actual admission with no agent credentials.
Known expiry remains a negative fact. Already-completed results can settle;
missing or expired custody cannot authorize new Model or authenticated agent
actions. Ordinary declared Tool calls retain their existing capability and
host-policy checks. Follow-up turns and compactions check every required original
admission again at their final write boundary; adopting another user's
queued input cannot replace an observation's original permission. Saved receipts
and historical results do not reconstruct these process-local permissions.

The actual resource graph owns each inference allocation. The actor acknowledges
the canonical ledger row and its Goal charge in the same persistence transaction
before handing back the allocation. The allocation also carries a final submission
guard: current physical ownership, original authority, source, Goal policy, and
deadline are checked again after yielding preparation or authentication work and
before provider submission. A rejected allocation never becomes an unattributed
request. A model preparation failure after a durable Job claim follows the same
terminal completion path as execution failure; it does not leave a Running Job
behind or invent a provider attempt.

Host turns are accounted at the real Operation_started transaction; compaction
is not a host turn. Ledger changes reconcile existing Goal partitions against the
same before/after canonical rows. Revised usage replaces the retained contribution,
and retirement preserves protected future accounting data rather than dropping it
to make space. Missing historical accounting is an explicit admission failure,
including for an unlimited budget.

These owner and storage contracts do not by themselves qualify every authored
workflow. Legacy queued callback service binding, cross-session attribution, and
the composed Goal/recurrence example retain the integration requirements below.

### Durable moderator queue provenance

Legacy queued values and one-shot Schedules can retain optional original source
and inference facts in the existing state carrier. Historical absence remains
unavailable; it cannot become a later Goal or a new unbound claim. Complete
ordered transactions preserve exact occurrence identity and retained producer
evidence. These records grant no live continuation or Model authority. See
[Durable moderator queue provenance](durable-moderator-queue.md) for the bounded
frontier, old-reader fence, preservation rules and temporary source/reset boundary.

### Authenticated descendants

Same-session inference descendants inherit attribution only through actual
host-owned invocation/job/Run lineage. Separately owned generated-session
inference also belongs in the implementation scope and requires an authenticated
host-issued delegation recorded before child dispatch. Its receipt binds the
parent goal, actual child identity/admission and attribution policy; stored IDs
alone grant no authority.

The child remains authoritative for its canonical usage. Parent aggregation must
consume acknowledged contribution revisions once, preserve uncertainty and remain
bounded through retry/restart/retirement. A parent job outcome and the child's
inference contribution are provenance and charge respectively; counting both as
tokens would double charge. Explicit sharing and current visibility checks apply.
No global session store or unlimited lifetime deduplication table is introduced.

For goals plus recurrence, the controller retains the objective/control while an
occurrence may launch a child actual Run. This avoids a second unfinished Workflow
on the controller source. The final authored fixture must exercise the shared
headless admission and authenticated attribution paths; retaining a child job ID
alone is insufficient evidence.

## Required validation

Implementation acceptance needs bounded transition/reference-model tests and real
actor/durable-host evidence. The essential cases are failed save before admission,
interruption after acknowledgment, reordered usage revisions, fold-before-retire,
untracked/unknown/overflow accounting, pause between decision and scheduling,
selective intent retirement, achievement requested before root-operation join,
adverse terminal outcomes before achievement settlement, explicit achievement
with outstanding work, source
replacement and restart without effect replay. Generated-descendant tests must
cover delayed/repeated delivery, child failure/restart and unauthorized attribution.

A real ChatMD definition must combine selected goal tools, builtin instructions,
checked helpers and recurrence without another writable goal status or continuation
scheduler. Provider fixtures must be controlled and require no live credentials.
Existing Run, moderator lifecycle and inference-ledger tests establish foundations;
they do not by themselves prove the goal capability is delivered.
