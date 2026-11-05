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

Same-live-episode resume, selective continuation controls and configurable Goal
helpers remain separate integration work. This component adds no second Workflow
scheduler and does not complete end-to-end goal automation by itself.
