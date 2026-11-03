# Goal episodes and actual Run settlement

Goals retain objective/control metadata; the existing Run lifecycle owns execution.
`Run_state.goal_links` permanently binds each associated Workflow to one Goal and
its original Admission receipt. Rotating the Goal's bounded current/predecessor
episode references never frees an old Run for reassignment. Missing historical
link fields remain absent; admitted link envelopes preserve compatible unknowns.
The existing Run index's finite bounds cover retained associations. There is no
new store or lifecycle executor.

`Goal_episode_admission.prepare` is a pure candidate planner for a real
`Run_admission.t`. It requires an Enabled goal, matching session/principal/current
source, both CAS frontiers and an exact new revision-zero Admitted Workflow. It
produces reciprocal Run and Goal updates. The actual controller owner must mint
and revalidate its reservation and commit those updates inside genuine Run
admission. No public metadata attach-existing-Run command or optional goal field
on ordinary Run start is supplied. Decoded transitions independently reject
late additions, link removal, reassignment and final orphan associations. The
whole ordered Batch is checked after its intermediate deltas have been applied.

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

The pure association and settlement model, actual recovery save failure and
journal/snapshot restoration, and native compiler/Catch staging have focused
fixtures. Authenticated controller enable/resume admission and a full authored
Goal.finish through a genuine worker join remain the combined controller/settlement
integration gate. Accounting, continuation policy and configurable Goal helpers
remain separate integration work. This foundation adds no second Workflow
scheduler and claims no completed end-to-end goal automation.
