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
identifies the original episode's terminal settlement. In particular, a cold
callback that waits or finishes its Goal does not receive an extra continuation
turn. All paths retain the same Run, Admission, episode and cumulative account.
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
| `Resumed` | The continuation actually started, waited, or settled as reported. `Started` does not mean the Goal has been achieved. |
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
