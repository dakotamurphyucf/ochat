# Goal episodes and actual Run settlement

Goals retain objective/control metadata; the existing Run lifecycle owns execution.
`Run_state.goal_links` permanently binds each associated Workflow to one Goal and
its original Admission receipt. Rotating the Goal's bounded current/predecessor
episode references never frees an old Run for reassignment. Missing historical
link fields remain absent; admitted link envelopes preserve compatible unknowns.
The existing Run index's finite bounds cover retained associations. There is no
new store or lifecycle executor.

`Goal_run_service.start_episode` is the authenticated host entry point for a new
Goal episode. It accepts the actual Factory entry, original operator proof and an
immutable `Goal_activation_request`: a headless Workflow request plus the original
Goal/catalog CAS intent. An exact retained association returns its Admission as
data before runtime borrowing. A same-key request cannot add, remove or replace
that association. An ordinary Run-start retry cannot omit a Goal's original link.

For fresh work, the Factory selects the actual compiled source without running
its initializer. `Goal_episode_admission.prepare_held` consumes the real inert
`Run_admission.Held_plan`, validates session/principal/source and both original CAS
frontiers, and enables/binds the Goal with a single Goal revision advance. The
actor commits Goal, Run Admission, episode and immutable link together. It installs
inert ownership and publishes the acknowledged canonical state before issuing
opaque live activation to the existing runtime owner. No input parsing, Model,
native tool or startup callback precedes the association. The lower pure
`prepare` planner remains available for a real `Run_admission.t`; it requires an
already Enabled Goal and never supplies runtime authority itself.

A new episode requires either no predecessor or its exact actual terminal Run
receipt. It does not resume a live paused episode, clear unresolved achievement
or reset cumulative accounting. Ordinary Run start has no optional Goal field;
there is no metadata command to attach an existing Run. Decoded transitions also
reject late additions, link removal, reassignment and orphan associations. The
whole ordered Batch is checked after its intermediate deltas have been applied.

The service preserves domain errors through the Factory's joined cleanup. An
uncertain admission write remains uncertain. A known Admission whose activation
fails is returned with that exact receipt and error; it is not a claim that all
effects were rolled back or that replay is safe. Retained receipt data never
recreates process-local activation custody.

The existing-episode source and command path uses a separate immutable
`Goal_resume_request`. Its original Goal, catalog and Run revisions and complete
source identity remain fixed while the source is selected. The actor checks both
the new operator proof and the existing Run's original authentication, and keeps
the same physical Run binding. The pure `Goal_resume_admission` planner changes
only the held Goal and its command receipt. It neither creates another Admission
nor resets the episode, account or passive Wait.

`Session_factory.with_goal_resume_source_reported` stages that command through the
existing exclusive runtime source owner. It joins target and generated-parent
source loans before closing the actual actor preparation. Its result separates
the callback result, known command receipt and cleanup outcome. An unknown first
write yields no acknowledged continuation; an acknowledged command retains its
receipt even when later publication or cleanup fails. A returned staging value
is data after its source scope closes: it does not prove that a new Operation
started or that runtime initialization succeeded. Full existing-episode execution
uses the runtime continuation consumer rather than interpreting that staging
value as an execution result.

## Resuming an existing episode

`Goal_run_service.resume_episode` is the host entry point for continuing a held,
nonterminal Workflow. It authenticates the current registered session and checks
for the command's retained receipt before acquiring the runtime. Retrying the same
command returns historical receipt data, including when the runtime is busy; it
does not repeat initialization or submit another turn.

The runtime owner selects one of these paths:

| Existing state | Resume behavior |
| --- | --- |
| Run waiting for an owned occurrence | Preserve the exact wake; do not load a runtime or create a turn. |
| Runtime already loaded | Reuse that runtime without replaying `Session_start` or `Session_resume`. |
| Runtime unloaded after a same-process stop | Build from the selected captured source and run the genuine `Session_resume` lifecycle before continuing. |

The cold path uses the shared constructor and existing worker. The original Run
identity remains the identity used for native effects; the resuming operator's
authorization is checked separately. Both proofs are required for fresh work.
The new command cannot replace the original Run principal or recreate an expired
process-local binding from stored IDs.

A completed resume reports the actual outcome: `Started` identifies the admitted
Administrative turn, `Waiting` retains the callback's chosen wake, and `Settled`
identifies the original episode's terminal settlement. `Controlled` retains the
exact acknowledged Hold or Abandon from this actual lifecycle callback. It does
not report execution or delivery adoption. A cold callback that waits, finishes
or controls its Goal receives no extra continuation turn. All paths retain the same Run, Admission, episode and cumulative account.
Any new admitted turn or inference is charged through that existing account.
An owned timer created by the resume callback records that Run's source and
origin alongside its actual callback ownership. These are retained provenance;
they do not authorize a later callback without the current runtime checks.

The command acknowledgement and execution outcome are separate. `Uncertain`
means the first command acknowledgement is unknown. `Resume_failed` carries a
known command receipt and a later construction, execution-admission or cleanup
error. Neither result authorizes blindly retrying effects. Source loans join
before the service classifies its result; cancellation and unexpected exceptions
propagate after cleanup. A retained receipt alone never grants execution.

Callers should distinguish these results when presenting a resume command:

| Result | What the caller knows |
| --- | --- |
| `Rejected` | The command was rejected before its first write. Correct the reported condition before issuing a new command. |
| `Uncertain` | The command's acknowledgement is unknown. Reconcile durable state; do not infer that no work happened. |
| `Resume_failed` | The command was acknowledged, but a later step failed. Keep its receipt and show the error; completed effects are not rolled back. |
| `Resumed` | The actual continuation started, waited, settled, or acknowledged control as reported. `Controlled` grants no execution permission; `Started` does not mean the Goal has been achieved. |
| `Retained` | This exact command already has a receipt. The reply is historical data and does not report a new continuation. |

An exact retry keeps the original key, source and revision intent. Changing the
payload while retaining its key can conflict with the stored command; issuing a
new key does not resolve uncertainty about the first command. Inspect the current
Goal and Run to show subsequent progress, rather than interpreting a retained
receipt as current execution status. If both the callback
and cleanup fail, the callback failure remains primary; cleanup failure replaces
only a successful callback result.

This same-process resume is distinct from restarting after process recovery.
Recovery does not restore live authentication or runtime ownership from storage.
An interrupted episode requires its actual terminal evidence before a new episode
can be admitted.

## Finishing and reconciling episodes

`Goal.finish` stages an untrusted bounded completion intent in the existing
`Run_action_service`. The actor's actual queued callback borrow and current
`Run_scope` authority remain necessary. The actor joins the sealed intent to the
actual prepared Run Action receipt, then commits Goal request, Run decision and
moderator checkpoint atomically. Local receipt IDs are lexical custody, not
execution or completion evidence. Catch rollback and callback cancellation
invalidate the staged selection through the existing Run action owner.

`Goal_run_evidence` authenticates stored associations against the host's retained
Run index: exact Admission, Action intent and terminal receipts must be present.
Private Goal invariants already identify a closed achievement's embedded request,
settlement and current episode; the host separately proves membership in the real
retained index. Data constructors and decoded records alone do not confer host
authority.

`Goal_run_resolution` plans internal terminal and review dispositions together.
Matching actual Completed closes a selected request as Achieved. Actual adverse
terminal outcomes retire its request and hold Enabled objectives; adverse current
episodes without a request also hold Enabled objectives. Existing holds remain
exact. Completed without a request leaves Open disposition unchanged. A Completed
result different from the requested result is impossible under valid same-request
staging: if encountered, the actual Run still commits, the unresolved Open request
remains, and Enabled control becomes held for review. No adverse receipt or
successful achievement is invented. Explicitly abandoned objectives remain closed.

Actual worker settlement, source replacement, reset and recovery pass their
prospective Run index to this planner. Each changed Goal advances once and all
selected existing Goal updates share one catalog revision. Internal settlement
never requires a fresh user-command receipt slot. Failed persistence installs
neither Goal nor Run state; uncertain acknowledgement requires exact durable
recovery, never replay of inference or external effects. Reconciliation without
new facts is a no-op.

Every Enabled Goal reserves one revision for an automatic review hold; an
unresolved request reserves one additional revision for its settlement. A held
pending Goal reserves only the settlement revision. The catalog reserves one
revision per unresolved Open request plus one if any Goal is Enabled. Metadata
commands cannot consume this headroom. Abandonment and terminal settlement release
the corresponding promise. Objective changes reject while a request is unresolved,
including through decoded updates, so old evidence cannot close a revised objective.

The universal Goal carrier probes future hold and achievement shapes with its
existing preservation codec. These private JSON probes are never decoded as Goal
values or admitted as evidence. Every Enabled carrier preflights its eventual
review hold; unresolved request carriers additionally preflight achievement.
Complete retained unknown data plus positive future byte/field/node growth is
charged against child, catalog and whole session limits on decoding, update,
adoption and encoding. Protected future fields cannot make a promised automatic
disposition uncommittable after effects have begun.

The shared [Run terminal-capacity owner](run_terminal_capacity.doc.md) additionally
reserves known work proofs, the actual terminal receipt and pending Finish result
copy. Run/job and Goal positive growth are summed against the complete preserved
host. Actual transaction/journal profiles and shared counter frontiers are checked
before effects. An unbounded new worker body can still fail ordinary admission;
the bounded truthful recovery disposition remains possible without claiming
Completed or achieving the goal from missing evidence.

Qualification includes the pure association and settlement model, actual recovery
save failure and journal/snapshot restoration, and native compiler/Catch staging.
The authenticated host fixtures now exercise actual catalog creation, loaded input
and cold resume with one canonical attempt/host-turn charge, retained association
retries, uncertain admission and physical reopen. An actual `Goal.finish` remains
Open while its root worker is pending; matching completion achieves it, whereas
cancellation leaves it unachieved. Explicit Hold retires pending activation even
when its save fails; an automatic budget hold preserves the same already-admitted
inference while denying fresh work.

## Selective control transitions

`Goal_control_transition` prepares retirement of future executable work in the
same transaction as a fresh Hold command. It retires the selected Goal's pending
Continue intents and pending Turn/Compact follow-ups using their retained Run
association. It preserves passive Waits, Finish intents, admitted work, accounting
and immutable outcomes. Unrelated work remains unchanged. An End_session request
keeps its existing precedence over Turn/Compact; Hold does not suppress that stop
request. A retained command returns its existing receipt without applying another
retirement transition.

Session Stop first saves a safety disposition for goals and recurrences. Enabled
Goals become Held with `Operator_pause`, and existing hold reasons remain intact.
This update uses no Goal command-receipt slot, so a full receipt collection does
not prevent stopping. A later cancellation failure does not undo the saved hold
or imply that cancellation completed.

`Goal_execution_selection` supplies snapshot-only `Eligible` or `Suspended_goal`
classification for an exact retained Run. Stale Run/episode/source evidence and
closed Goals are rejected. Eligibility grants no authority: actual schedulers and
execution owners must repeat their occurrence, original-permission and final
admission checks. Wiring suspension through all wake consumers and configurable
Goal helpers remains integration work. These components add no second Workflow
scheduler and do not complete end-to-end goal automation by themselves.

## Native command custody

A selected native Goal adapter can stage objective/progress edits and a bounded
same-episode Resume intent in its actual callback transaction. The lexical native
receipt is speculative until the parent checkpoint acknowledges it. Parent
rollback, a caught-and-rolled-back Tool.call, failed native completion and invalid
multiple-Resume selection retire the speculative loans. A durable Accepted
Invocation records the original request; it does not say that the Goal command
has executed.

The Resume intent freezes generation, full source, Goal/catalog/Run revisions,
principal, key and request digest. The host must not refresh those fields to make
a stale command pass. Callback commit can itself change an owned Goal Run's
revision, so a Resume staged by that same Goal's callback may truthfully become
stale. An unrelated ordinary callback can target a passive Goal without that
self-change. Resume uses the existing episode, Run binding and accounting owner;
it neither creates a replacement episode nor enables metadata as a substitute
for authenticated continuation.

The existing schedule host selects an accepted command only after the callback,
Operation and runtime loans have joined. Its finite dispatcher reserves the same
exclusive source stage and checks transferred command custody, current host
policy, the original Run authority and exact source before admission. Native
admission itself is borrowed and expires with the callback; retaining that token
is not a post-callback authorizer. A successful dispatch returns an acknowledged
Started, Waiting or Settled fact without waiting for arbitrary Workflow
completion. There is no detached dispatcher or additional command queue.

Dispatch requires the configured execution-enabled schedule host. Client
detachment does not stop that host. An absent or disabled host can leave an
Accepted command pending; it must not be represented as Started. Host shutdown
retires the process-local command loan. Reopening preserves durable Invocation
and Goal receipts as historical DATA but never reconstructs the loan or replays
an accepted request. A fresh authenticated status reader distinguishes
Interrupted_before_dispatch, Dispatching/Unknown and an actual Goal command ACK.
Only the same actual preparation can supply a live handoff detail; after restart
that detail may be Unavailable even when the durable Goal receipt is known.

Known Goal ACK and attempted/uncertain writer outcomes prevent a later negative
host detail from being persisted as a definitely pre-admission Rejected command.
No status projection writes a post-Started Invocation revision that could
invalidate the first execution prefix. Status IDs and decoded receipts grant no
execution permission. New Invocation children require the understood native
Resume semantics marker in both session snapshots and delta documents;
historical absence retains its previous wire representation.

## Native creation and safe controls in the callback checkpoint

A trusted selected native adapter may stage a new Goal, a Needs_input Hold, or abandonment in its actual callback transaction. It captures the genuine native Admission for its exact Create/Hold/Abandon effect and passes the immutable typed request to the Actor. The Actor derives the real principal and session and repeats current original, native binding, source and host-policy checks. A tool name, Goal ID, stored receipt or projected state grants no authority.

Create freezes generation, full installed source, catalog revision, Goal ID, objective, budget and retry key. Generation zero is valid. It has no existing Goal revision or supplied creator. Acceptance creates one held Source_review Goal with its empty stable account in the same canonical transaction. The budget adds Goal ceilings without relaxing runtime, provider or host limits; creation does not enable work or reset another account. First episode activation remains a separate genuine admission.

Safe native controls freeze generation, Goal/catalog revisions, key and a validated reason. Hold exposes only Needs_input; it cannot impersonate an operator pause or an automatic accounting/source gate. Abandon changes the objective disposition without claiming the Run or its admitted work is terminal. The current original and host write policy still authorize each command. Receipts are immutable metadata DATA, and exact retained replay does not repeat retirement.

The existing private callback Plan applies selected Goal edits and selective Run-index retirement in order. A following Progress or Goal.finish action therefore sees the same projected Goal/catalog state, while actual original/source/owner checks continue against the real Actor. A fresh same-Goal Hold/Abandon combined with Continue or a new turn request rejects the entire callback checkpoint. Passive Wait, an explicit Run-owned Finish, unrelated owners and already-admitted work retain their actual contracts. Achievement still uses the checked Goal.finish Run action and real aggregate evidence.

Fresh explicit Hold retirement occurs only after the actual final checks, immediately before the yielding writer. Same-valued Hold is still a new explicit control. A failed save cannot revive the prior pending activation; known ACK also invalidates registered fresh-allocation views before publication. Automatic accounting holds remain distinct and preserve the exact already-admitted last allocation. Writer ACK installation records only factual completion after the accepted control revokes fresh custody.

An intentional Hold or Abandon can end the same constructor's permission to
continue. The actual joined Init, initial authored lifecycle or existing-Goal
lifecycle retains one opaque `Goal_callback_control` only after the callback's
writer acknowledges the fresh command. It binds the exact owner, source, Run,
episode and final committed candidate. Hold followed by Progress retains the Hold
receipt and validates the later final Goal projection. A retained receipt replay,
Create alone, unrelated metadata control or failed/unknown writer cannot supply
this completion fact.

The shared runtime constructor returns `Built` with its real value or
`Callback_completed` with closed `Controlled` or `Settled` result data. Goal activation reports
`Controlled { admission; control }` using its actual earlier Admission. Existing
episode Resume keeps its actual Resume receipt with a `Controlled` continuation.
The fact exposes its accepted disposition and an optional exact passive Wait as
data; it does not authorize another effect. Validated actual terminal settlement
takes precedence over control for initial activation, Resume and wake completion.
Initial activation reports `Settled { admission; settlement }` with its real
Admission and independently validated terminal episode receipt. It does not claim
runtime installation or startup handoff. An accepted Finish alone is insufficient:
the actual aggregate writer must acknowledge completion of all admitted work.
Abandon
alone never claims that the Run or its admitted work is terminal.

For control during pre-install Init, construction stops before effect flush,
initialization completion or runtime installation. A legacy constructor whose
initialization was Pending keeps that Pending state; a Scoped Init over existing
Ready history keeps that history. Its real partial runtime and Init owner join
cleanup before a successful result escapes. An installed v1 lifecycle can instead
acknowledge control after its genuine Ready commit. The runtime then retires while
that committed Ready history remains truthful. Control fabricates neither an
installation nor a rollback of completed initialization.

A legacy Pending constructor over retained history restores its actual moderator
checkpoint and runs `Session_resume`; a first Goal episode may supply a user input
that control then prevents from being submitted. The installed v1 authored path
instead joins its genuine `Session_start` after physical preparation has committed
Ready. These are distinct actual lifecycle boundaries.

Goal metadata requires an actually installed moderator source. A fresh legacy
Pending session cannot attach its first Goal before its genuine callback has
installed that source. The remaining fresh startup integration must join native
Create inside that callback to native Start after its release. Source installation
already belongs to the actual callback claim; a saved or published source
description cannot replace it.

While durable desired state remains Stopped before initial input handoff, native
Goal metadata is permitted only for the actual callback of the admitted initial
Goal constructor. The Actor checks its exact event borrow, current constructor
scope, joined callback ownership and held Run after the existing original, source
and effect checks. The actual initial Run retains its issued family's association:
a Goal constructor validates its immutable Goal link, episode and exact Admission;
a recurrence constructor validates its reciprocal occurrence origin. The callback
and its native children retain that genuine Run attribution for accounting and
completion. Ordinary, unbound and delegated stopped callbacks remain denied.
An explicit Stop still revokes the actual callback, including a Stop that leaves
the durable desired state unchanged.

Callback, publication and cancellation exceptions remain observable; cleanup
failure replaces only a successful callback outcome. Factual currentness checks
inspect the actual source, Store observation and candidate without re-enabling a Held
Goal or renewing closed execution custody.

A pending wake's `Controlled` result is separate from `Handed_off`: the old wake
is not delivered and no queue, Delivery or turn acknowledgement is fabricated.
An already claimed Job keeps its actual claim and completes through the existing
failed-to-obtain-runtime path, reporting non-execution failure rather than success,
requeue or a refund. Actual admitted independent work and cumulative accounting
remain owned by their existing completion paths. Known control closes the exact
local execution owner without the generic no-handoff Run interruption.

Ordinary delegated loading still consumes the same constructor. Its real Scoped
Init or deferred Installed_resume route does not acquire a Goal lifecycle owner
from stored source or Goal metadata. The existing source-stage cleanup and
administration cancellation remain authoritative. The public checked-helper and
Start/Resume packaging remain separate integration work; no new executor or
metadata permission issuer is added.
