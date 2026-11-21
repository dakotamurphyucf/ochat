# Local context rollover

OChat can retire an active inference window and start another within the same
durable session. The session actor owns the transition. It archives the retired
window before replacing the model-facing projection, then records the new
window, the transition receipt and the continuation obligation together.
[Local context windows and recoverable history](local-context-recovery.md)
defines window and occurrence identity, archive membership, note revisions,
visibility and explicit sharing.

The first automatic policy is
opt-in for a qualified durable host and selected adapter/profile/model. Existing
sessions retain their current summarizing compaction and moderator controls.
The provider receives ordinary inference requests. Session state, archive,
notes, recovery hints, idempotency and continuation all belong to OChat.

## Current seams

| Existing contract | Rollover integration |
| --- | --- |
| `Inference.Request.Target` fixes the admitted adapter/profile revision, identity, model and settings. The stream constructs model input after the turn-start overlay, guidance, tools and assets. The OpenAI adapter builds the private finalized payload, including captured/reconstructed items and effective parameters. [Target](../../lib/inference/request.mli#L86), [stream](../../lib/chat_response/in_memory_stream.ml#L2194-L2299), [OpenAI payload](../../lib/openai/inference_input.ml#L355-L415) | Measure the finalized candidate under its actual target. A provider/model string, canonical transcript and early turn snapshot cannot substitute for that preparation. |
| `Context.prepare` checks input and target binding and returns a `Prepared.t`. `Inference_client` prepares before `before_dispatch`, identity allocation and `Prepared.start`. The stream also invokes a separate earlier `before_model_call` path that can authorize moderator or wake acknowledgement. [Prepared and Context](../../lib/inference_runtime/inference_runtime.mli#L258-L355), [client](../../lib/inference_client/inference_client.ml#L58-L93), [worker](../../lib/agent_session/operation_worker.mli#L207-L245) | Evaluate before attempt allocation and first-request wake consumption. Keep early borrow/currentness checks, but delay a wake acknowledgement that represents request admission to the qualified admission path. Preparing, warning, waiting and committing a local reset never record an inference attempt. |
| The stream awaits its parallel tool calls, persists paired outcomes before their callbacks, then adopts deferred entries and consults turn-end policy. The actor's existing adoption is durable for the current operation. [Tool completion](../../lib/chat_response/in_memory_stream.ml#L2064-L2167), [turn-loop boundary](../../lib/chat_response/in_memory_stream.ml#L2333-L2408), [deferred adoption](../../lib/agent_session/session_actor.ml#L25181-L25206), [safe-point contract](../chatml-safe-point-and-effective-history.md#exact-safe-point-boundaries-already-in-code) | Cut only at a complete turn boundary, or idle, and fence the worker's continuation history by the committed window. A tool calling rollover queues intent. Its own result and all sibling dispositions belong to the original cut. |
| Existing compaction estimates history text against a 20,000-token threshold and accounts separately for reserved guidance. The manual actor admission is idle/stopped and the terminal transition persists a compaction archive and a summarized history. [Estimate](../../lib/context_compaction/config.mli#L3-L14), [compactor](../../lib/context_compaction/compactor.ml#L91-L118), [manual admission](../../lib/agent_session/session_actor.ml#L26261-L26286), [compaction terminal](../../lib/agent_session/session_actor.ml#L25865-L25917) | Keep the existing command, summary, threshold and `Request_compaction` semantics. An automatic retry from the same preflight cannot invoke both rollover and summarizing compaction. Ordinary compaction still uses its own idle admission and archive semantics in a rollover-enabled session. |
| The store validates journal continuity and the archive writer syncs its checked document before publication. An uncertain writer requires recovery. [Archive contract](../../lib/agent_session/compaction_archive.mli), [commit writer](../../lib/agent_store/commit_writer.mli), [recovery](../../lib/agent_store/recovery.ml#L61-L95) | Reuse archive-before-reference and the actor's validated transaction path. Preserve the existing attempt, job, notification and operation ledgers through a new window. |

## Capacity policy and evidence

A qualified host registers a versioned capacity policy for the exact effective
target and input encoder. Runtime modes are `Off`, `Manual` and `Automatic`.
The default is `Off`. Host policy and actual target support cap session policy.
An authorized session can opt into `Manual` on a durable host with local
recovery support, or `Automatic` when the host has qualified the capacity
declaration, estimator, checkpoint recipe and fallback for its selected target.
No match by model-name substring or remote account eligibility. A profile,
settings, encoder, routing, or target change invalidates existing capacity
decisions and any incompatible transport reuse. Status reports the selected
mode and whether the requested automatic policy is currently available.
The initial adapter is OpenAI. Numerical limits, margins and reserves come
from the specific host qualification. A missing numeric policy leaves
automatic pressure handling inactive and preserves only the admitted manual
actions for that host.

The stock daemon loads the optional `server.recovery_capacity` policy at
startup and uses the session's durable recovery-window opt-in. It qualifies
the OpenAI Responses encoder against targets captured by the ordinary prompt
and profile resolver. A stock request for automatic handling reports manual
because stock startup supplies no qualified checkpoint and fallback recipe.
The stock policy syntax and bounds are described in the agent server
configuration guide.

Use `Context_estimate` for pre-dispatch observations. Its opaque host
preparation ID labels the exact candidate, and its public projection never
contains the payload, instructions, tool names, credentials or a private
fingerprint. Extend the capacity assessment with typed unknown causes
such as `No_declared_limit`, `Unresolved_output_bound`,
`Uncounted_component` and `Stale_preparation`. An estimate records the
estimator version and coverage. It accounts for the entire encoding,
including required instructions and overlays, host guidance, schemas and
tool choice, ordered history and tool outputs, item framing and qualified
image/document or captured costs. A material uncounted component yields
`Unknown`. Byte length, a policy with no declared bound, and an adapter
without a qualified cost model do not produce numeric remaining capacity.

Provider-reported input usage remains an actual observation of the original
dispatched attempt. It can appear as `Measured` capacity evidence for that
attempt only when the qualified adapter attests that its count covers the
same effective input basis. An exact preflight counter can likewise supply
measured evidence if independently qualified, using a distinct projection
from `Context_estimate`. A new prepared candidate needs fresh evidence.
Cached/charged tokens, goal usage, operation accounting and billing stay in
their own projections. Store bounded provenance on the existing safe
observation path. [Count and context estimate](../../lib/inference/observation.mli#L61-L155),
[effective configuration](../../lib/inference/observation.mli#L165-L245),
[OpenAI usage decoding](../../lib/openai/inference_output.ml#L320-L325).

The prepared assessment is accessible through
`Inference_runtime.Prepared.assess_capacity` immediately before dispatch.
Pass the trusted policy for that exact target and encoder. Validate a
previously captured result with `capacity_assessment_is_current` before
using it on the active path. The check uses private preparation and policy
identities, so having the same displayed preparation ID cannot authorize
reuse. With no policy the accessor returns `Policy_missing` and does no
counting. Assessment neither changes dispatch nor acknowledges a wake.

Startup configuration in `Daemon.options.context_capacity_host` supplies one
numeric policy per exact target. The host selects the mode separately for each
session, and the default is disabled. The session's
actual canonical writer identity supplies the host-qualified session reference.
The actor's capacity port reads the latest finalized root preparation for
one running operation. A changed target, source revision, active window, host
policy or provider binding makes that preparation stale. A retained assessment
describes the request that just finished in that operation. Each subsequent
request needs its own assessment. The port grants no authority to start a window
transition. [Host and status contract](../../lib/agent_session/session_context_capacity.mli).

The OpenAI Responses adapter offers a versioned O200k estimate over compact
JSON of the finished request. It includes the finalized text, instructions,
guidance, tool schemas and framing. The 1 MiB bound is checked on the
immutable JSON before scanning content coverage or encoding the request
for the tokenizer. A preparation retains its bounded numeric reading or
closed coverage result across explicit assessments. Policy and threshold
decisions are recalculated separately.
Argument isolation still permits measuring that request's cache-omitted
wire. Requests carrying live protected tool results have no capacity count
source, so their private content does not affect the reported estimate.
Images, documents, opaque reasoning, unknown retained item shapes and
requests beyond the bound return a typed unknown. A host must qualify
the estimator and choose its safety margin for the actual target. Real
provider input usage remains separate. No automatic pressure policy ships
with this assessment. [Assessment contract](../../lib/inference_runtime/context_capacity.mli),
[prepared API](../../lib/inference_runtime/inference_runtime.mli),
[OpenAI implementation](../../lib/openai/inference_capacity.ml).

For a bounded target, use checked nonnegative token arithmetic:

| Quantity | Definition |
| --- | --- |
| `total` | The minimum of the qualified true total capacity and any tighter host/session allowance. An explicit `Unbounded` policy or an unknown model limit leaves automatic rollover unavailable. |
| `output` | The effective output reservation of this actual preparation, bounded by the model's output limit when declared. A provable effective adapter default is acceptable. An omitted or null setting alone is unknown. |
| `hard_input` | `min(provider_input_cap, total - output)`, omitting the separate input-cap term only when the profile declares no additional input constraint. |
| `soft_input` | `hard_input - checkpoint_reserve`. The profile declares a positive reserve for the qualified checkpoint path, including its added prompt, tool schema or result overhead and any output allowance above the ordinary request's reservation. Each actual checkpoint preparation independently fits its own hard input allowance. |
| `warn_input` | `soft_input - warning_lead`. The profile declares a positive lead and a bounded estimator safety margin. The conservative upper estimate including that margin is the value compared to the thresholds. An exact counter needs no estimator margin. |

Reject malformed/overflowing declarations and policies with nonpositive
usable ranges. No reserve can increase a true provider or host limit. Any
reported remainder pairs available input tokens with the observation basis
and model, flags when it is stale, and may represent an exceeded bound.
Values at threshold are deterministic. Warn at `warn_input`, gate
ordinary traffic at `soft_input` in automatic mode, and withhold a candidate
known to exceed `hard_input` whenever local context mode is active. Exact
equality at the hard limit can still admit a qualified checkpoint if its
own prepared upper bound fits. Unknown status makes no automatic numeric
decision. Ordinary authorized dispatch can use the existing path when
preflight is unknown and there is no already-pending capacity block.
An exact, qualified context-limit rejection may trigger the bounded recovery
path at a safe boundary.

| Trigger | Policy |
| --- | --- |
| Near limit | One durable reminder disposition per window and policy revision, with separate stages for claimed and accepted by the next actual attempt. An interrupted claim is reconciled by the actor. The reminder is advisory and low authority. Prepare again with it included and withhold it if it cannot fit. |
| Soft limit or model-requested transition | Require a valid committed checkpoint for that cut. Use an existing eligible checkpoint or offer the qualified script one bounded opportunity, including at most one optional model checkpoint attempt per old window. Withhold any checkpoint inference that cannot fit. Explicit model intent with an absent/failed checkpoint settles as `Checkpoint_required`. An authorized caller can request again after a checkpoint or submit an operator force. At soft pressure with no valid checkpoint or fitting opportunity, proceed by the forced policy and record why. |
| Hard limit or typed context-limit rejection | A forced capacity transition can use a valid checkpoint already present. Missing, failed, expired or revoked checkpoints produce an explicit archive-only outcome. Do not submit the already oversized candidate or a speculative checkpoint call. A provider-triggered recovery needs a closed adapter classification correlated to the exact attempt and proven safe to continue after its terminal outcome. Arbitrary text, a generic 4xx and an ambiguous attempt are insufficient. |
| Operator request | Require exact expected window and session revision. The ordinary request requires a valid checkpoint. A separately authorized force is allowed with an honestly classified absent checkpoint. Unknown capacity does not prohibit an authorized local force. |
| Smaller model, target or settings change | Reacquire the new immutable target and reprepare before any further dispatch. Use ordinary same-window admission if it fits. Otherwise consider the local policy and qualify the required fresh projection with the new target. A former observation never authorizes the new request. The actor never silently selects a larger model, another account, or a different output setting. |

A checkpoint is a receipt for a selected set of already committed local
note revisions, with current authority, owning session, old window and
qualified conversation coverage. Confirm these at commit. Its coverage
records the canonical cut and effective provenance as defined by the recovery
contract. The actor can validate durable selection and coverage, while the
script owns assessment of the notes' semantic completeness. The actor may
accept a checkpoint made at an earlier boundary only
if the covered context still matches. A checkpoint tool's acknowledgement
and a same-batch rollover admission may form the validated terminal cut
suffix. Any later substantive user/history change invalidates coverage.
The qualifier must distinguish this suffix from real new context. Unknown
outcomes of checkpoint writes require receipt readback. A bare
`new_context` intent, a generated text message, or an uncommitted note
offer does not satisfy this condition.

The authored instruction asset describes how to save a per-session
checkpoint, read its committed receipt, request a rollover at a safe point,
and distinguish pending admission from a committed successor. The reminder
uses that same workflow and refers only to installed operation names. The
archive-only outcome gives the successor authorized local recovery
references and an explicit missing-checkpoint status. It never claims that
a summary or note was saved. Tool specs, built-in imports and ChatML
helpers use the same host-restricted scopes throughout the workflow.

Initial recovery budgets are per old window and per committed
continuation. An unchanged failed automatic basis waits for an explicit
relevant change, such as a new model, a newly committed checkpoint, an
authorized operator command or a qualifying source edit. A failed
fresh-window continuation gets only one permitted smaller *projection*
attempt with optional hints removed if they materially changed that request.
It cannot recursively create fresh windows.

## Actor transitions

The following types name the required seams. Use the existing typed IDs,
source receipts and capability checks behind private validated constructors.

```ocaml
type capacity_evidence =
  | Unknown of Preparation_identity.t * Capacity_unknown_reason.t
  | Estimated of Inference.Observation.Context_estimate.t
  | Measured of Measured_capacity.t

type pressure =
  | Below_warning
  | Near_limit
  | Checkpoint_due
  | Hard_limit_exceeded
  | Pressure_unknown

type boundary =
  | Idle of Idle_boundary.t
  | Before_dispatch of Dispatch_boundary.t
  | Settled_batch of Batch_boundary.t

type trigger =
  | Operator of Operator_force.t
  | Model of Tool_invocation.t
  | Capacity of Capacity_pressure.t

type checkpoint_quality =
  | Checkpointed of Checkpoint_receipt.t
  | Archive_only of Checkpoint_absence.t

type continuation =
  | Continue
  | Wait of Wait_reason.t
  | Finish

type status =
  | Pending of Rollover_id.t
  | Committed of Rollover_receipt.t
  | Superseded of Supersession_reason.t
  | Rejected of Rollover_rejection.t
  | Interrupted of Recovery_disposition.t
```

The capacity evaluator takes the immutable preparation and exact policy.
Its result binds the capacity evidence and decision to the input/output
bounds and preparation identity. A `Measured_capacity.t` constructor
checks scope and counter coverage. A capacity-based dispatch decision
uses evidence from the final preparation. The actor validates the local
transition independently of that numerical decision.

**Native operations.** Expose separate typed operations for context status,
checkpoint receipt selection and rollover admission through the selected
ChatMD native tools. Status returns safe capacity evidence and authorized
receipts. Checkpoint selection refers to an exact committed note revision
and current coverage. Rollover request returns the admission status. The
catalog binds operation identity, schemas and scopes. Every invocation uses
current host and moderator admission with the installed authority. Authored
guidance can suggest or hide optional capabilities in a workflow, while
actual capability selection and visibility rules determine what runs.
Operator requests use the same actor service with separately verified
operator authority. [Native dispatch](../../lib/agent_session/native_tool_dispatch.mli).

ChatMD selects `context_remaining`, `context_checkpoint` and `context_new_window` individually with
`<tool name="…"/>`. The former requires the original transcript scope and
reports the current window, canonical revision and prepared capacity status.
The latter requires the original send scope and takes `expected_window`. Its
own committed dispatch acknowledgement pins the canonical revision and the
window from the exact foreground preparation. Admission rechecks preparation
identity and current authority at the journal writer. Model and operator
requests require an enabled local mode. An operator supplies the full expected
scope and revision and uses the current owner writer. Force uses a second
trusted authorization check. A `pending` receipt records durable admission.
Only the archive and successor transaction can publish `committed`.

**Admission.** The model tool, operator or internal capacity controller
submits a cause, expected old window, revision and private scoped
idempotency identity through the current host authority. A tool returns
`Pending` only after durable intent admission. The same key plus
normalized arguments returns the same operation and current status. Reusing
the key with different arguments yields a conflict. A separate intent against
a superseded window can observe the authorized prior receipt or conflict,
never create a second successor. Internal retries retain their identity.
Replay checks live caller authorization. No physical replacement occurs
inside a model tool, pre-tool hook or pending provider attempt.

**Reserve and prepare.** At a safe point the actor takes an exclusive
transition claim for the owning runtime generation. Its expected basis
contains window and active-projection identity, session revision, canonical
cut and high-water occurrences, active operation or idle lease, source and
moderator revision, selected target and policy, and authorized checkpoint and
continuation intent. `Before_dispatch` requires no live attempt and no
unsettled prior tool batch. `Settled_batch` requires a durable terminal model
receipt and a durable outcome or cancellation disposition for every call in
the batch. Calls and results are sealed in their paired order. An active
moderator borrow or background state-writing lease must join or release its
ownership before the claim advances. Storage/lease conflicts expose pending
or typed rejection via existing error paths.

The worker's existing safe-point queue remains the authority for pending
inputs and notifications. At rollover, the actor makes the cut before the
next deferred adoption. Already adopted entries remain exact old-window
occurrences and can be selected into the new projection by existing
authorized identity. Pending entries keep their application admission IDs
until adoption through the actor. Use a read-only immutable preview to
preflight any selected continuation input, never an extra dequeue.
Concurrently admitted entries outside the captured basis remain queued. If
an admission changes the expected session revision, resample and validate
the same intent against the refreshed revision and cutoff before publication,
or expose a retriable conflict. No previously executed hook effect is
repeated just to refresh a revision.

The canonical seal uses a distinct `rollover-<operation>.frame` archive.
Existing summary locations continue to resolve to their original frames. Live
rows from the closing window keep their original ordinals in the rollover
frame. A carried row gets an ordinal in the successor and must match the prior
visible row, including its payload and revision. [Canonical seal](../../lib/agent_session/recovery_window_seal.mli)

**Archive and publish.** Build a validated, bounded candidate using the
pre-commit hook and current local authorization. Preflight the minimum
first request when there is a saved follow-up, including required current
instructions, tools/settings and user intent. Remove optional hints once
when needed in preflight. If the required projection still cannot fit, mark
`Required_context_too_large` and keep the previous window authoritative.
Recheck the candidate after any hook or read that released actor ownership.
Write and verify the archive and reachability needed by its manifest,
including durable occurrence/capture revisions and assets, before committing
a reference.

In one journal transaction under the current owner, validate expected old
window/revision, fence, source, target, checkpoint status and archive receipt.
Append the retired membership, allocate the single successor, install
the new active baseline, bind the immutable transition receipt and record
pending post-commit hook/continuation obligations. A committed receipt binds
old and new IDs, cut/revision, archive identity, trigger and checkpoint
quality. The actor publishes the new window after this transaction and
serves authorized status from the journal. The runtime continuation receives
the new window's versioned projection or returns to the actor scheduler.
Capture the exact active window with the foreground launch and its history.
The actor checks this ownership when the worker publishes history or returns
a terminal result, then checks it again before persistence. An empty successor
or a retained common prefix can satisfy a history prefix check, so the window
identity remains part of the fence. Cancellation closes the worker's history
publication while its terminal acknowledgement settles the same operation.
Advance the logical window ID independently of session copy, generation and
existing summary-compaction counters.

An idle actor can settle an intent through its owning writer at an exact
window and revision. The writer can record an explicit checkpoint recipe
with the history it covers and the local note revisions it selects. The
checkpoint remains eligible while that history and those revisions match.
Missing or stale checkpoint evidence supersedes an ordinary
intent. An explicitly forced intent can publish an archive-only result
with a recorded reason and current force authority. The journal encodes
the successor, exact archive digest, checkpoint quality and a pending
continuation obligation in a single rollover delta. The archive uses the
complete previous persisted document, and the writer verifies its framed
bytes after sync before publishing the delta. While an intent is pending,
each canonical write also verifies that the next journal transaction and
snapshot can still represent a no-archive supersession.

A live settlement compares the complete selected response's ordered call
positions with the actor-acknowledged result occurrences before the worker
asks for deferred input. The actor checks the original foreground launch,
its actual acknowledged root attempt, the stored terminal receipt, and each
canonical call/result relation. A cancelled tool whose outcome was published
is included as that outcome. Cancellation before publication leaves the cut
unavailable. The archive transaction retires the old active slot and records
the predecessor operation in the pending continuation. Queued entries and
wake dispositions keep their original identities. The successor scheduler
claims that continuation under current authority before starting a new
foreground operation.

The authored `context_checkpoint` registration shares the durable checkpoint
ledger with the owner-writer path. It records the exact foreground model
invocation and submitted root attempt, note-catalog revision, immutable
selection, and digest and length of the canonical prefix at the writer.
The selected tool returns only receipt status. The key is scoped to the
original principal and window. Readback can confirm the registration after
restart, and an exact retry leaves the original coverage cut in place.
At settlement both the actor and journal reducer require the original
relation-bound acknowledgment and the exact covered prefix, with eligible
control occurrences supplied by the verified settled batch. A changed
configuration, prompt, moderator source or note catalog invalidates coverage.

A native intent requires a recorded eligible checkpoint. A previous proof
can cover the canonical prefix when the only additional occurrences are the
exact, acknowledged Context control calls and results of this batch. A
changed row or selected note invalidates that evidence. Explicit operator
force may archive an uncovered batch if its original owner authorization
is still current at the writer. Retrying an existing intent or reopening the
session cannot attach live authority to that original foreground cut. An
authorized owner can explicitly settle the pending intent at a later,
quiescent cut.

**Reconcile and resume.** Cancellation, stale configuration or history,
revoked authority and storage failure before publication leave the old
window authoritative. An unreferenced staged archive is eligible for
ordinary safe orphan cleanup after reconciliation. An uncertain journal
write requires reopen/readback before any retry. A committed rollover
resumes from its saved window and obligations even if in-memory publication
or the reply was lost. Recovery rebuilds from the local archive and journal.
A checkout of an older source or a new session generation cannot claim old
live callbacks as current. Old tool results match only the original operation,
occurrence and execution custody. A duplicate yields the recorded
disposition and a late invalid one cannot append to the new window. Queue
delivery, attempt admission and versioned continuation ownership are
reconciled independently using their existing identities and idempotency.
If an automated attempt was admitted but its outcome is unknown, use the
existing interruption semantics before any new scheduling.

Rollover has no reset authority over working files, filesystem capabilities,
current provider/account/credentials, goal identity/progress and usage,
other automatic-turn accounting, jobs, schedules or deliveries. It reuses
the original semantics of edits, idle compaction, cancellation and reset.
Disabling the feature stops new admissions while existing committed windows
and recovery records remain readable under current authority.

## ChatML hooks and continuation

Add proposed `Before_context_rollover` and `After_context_rollover` events
to the real installed moderator only. They follow the current runtime's
bounded execution, typed source provenance and lifecycle borrow rules. The
pre-commit hook sees the proposed expected basis, classified pressure and
checkpoint capabilities. It can select an admitted checkpoint reference,
offer bounded hint references, select a continuation or veto. It cannot
assert a note write that did not commit, change the target, broaden
authority, alter the queued cut or publish a window. An explicit veto,
authority failure or loss of source currentness stops every trigger,
including force. A fault exposes pre-commit failure, and may use archive-only
force only when no mandatory hook guard failed. A forced transition can
proceed without an optional checkpoint helper.

The post-commit hook sees the committed receipt and can refine the
continuation using the same bounded, current borrow. For each phase the
actor durably claims hook execution and commits typed results through the
existing effect discipline or records failure/interruption. An interruption after
a claim does not automatically run arbitrary effects again. Post-commit
failure keeps the new window and holds follow-up pending explicit recovery
or the configured wait policy. Rebuild and reconnect can read that
disposition without manufacturing another lifecycle event. Hook callbacks
receive no additional model, tool or filesystem authority. Staged effects
are verified against their real originating borrow before commit.
[Current event phases](../../lib/chat_response/moderation.mli#L131-L145),
[runtime requests](../../lib/chat_response/moderation.mli#L343-L359),
[existing Continue/Wait/Finish composition](../../lib/agent_session/run_action_service.mli#L24-L33).

| Script outcome | Actor effect |
| --- | --- |
| `Continue` | Make only the existing eligible follow-up or explicitly requested script turn ready through the scheduler. Real pending input and genuine tool follow-up keep their admission identities. Script-initiated turns respect the current turn, rate and pause policy. Persist selection with the rollover continuation and claim it at actual admission. |
| `Wait` | Suspend that automatic follow-up. An already authorized delivery, timer, job or operator action can release a matching wait through the existing scheduler. With no such wake, hold with visible status. Inputs remain durably pending under ordinary admission rules. |
| `Finish` | Finish the rollover-induced continuation and release its lease. Preserve the session and the normal admission of future user work. Only the existing explicit session-end request ends the session under its normal policy. |

When no hook selects continuation, carry an actual pending follow-up under
the current scheduler policy or finish when no follow-up exists. Existing
session termination wins. A conflicting script turn request is a typed
validation failure. A hook cannot loosen the scheduler's bounds or rely on
a live closure surviving restart. [Scheduler policy](../../lib/chat_response/runtime_semantics.mli),
[ChatML orchestration](../agent-server/chatml-orchestration.md).

The first prepared request in the successor rebuilds the effective host and
moderator instructions, mandatory safety and developer configuration,
applicable tool policies, current world-state references and the actual
admitted user intent. Add bounded hints from committed, currently authorized
note or history references. The recovery contract sets visibility and payload
limits. Reauthorize protected hints during final assembly and at submission
using the origin's delivery guard. Do not persist copied hint bytes into a
reusable transport prefix. Hints have explicit untrusted provenance and
grant no access. Tool visibility still follows real host and moderator
authority. A fresh transport uses local state, current instructions and
eligible local references. A stored source digest or a former transport
prefix cannot restore revoked material.

The exact dispatch preparation runs again after the final hook and any
intervening edits or input. Try one fresh projection with optional hints
omitted if that can change a size failure. Retrying a dispatched failure
also requires a settled rejection with no model-output/tool effects and
the same continuation custody. If the required input is still
too large, interruption is ambiguous, or the qualified limit remains
unavailable after a known capacity failure, hold with typed status. A
pre-commit failure keeps the old window. A post-commit dispatch block keeps
the new window, its old-window archive and any unadmitted input. Both are
bounded and recoverable through an authorized input or target change.

### Settled foreground successor

The successor retains the original admitted user occurrence, local initial
prompt and any already committed notification input with an unresolved wake.
A managed submission keeps its original receipt and input in a suspended
assignment. The first successor operation stages eligible FIFO input and
wakes without adopting or acknowledging them. The selected worker can
materialize late deliveries through the normal authorized initial notification
consumer. Withheld preparation leaves the resulting wake pending. An exact
dispatch transaction
adopts them and attaches the managed receipt to the same new operation using
its acknowledged root attempt. Replay checks the exact eligible queue and
wake selection. A historical committed batch keeps its original carry
semantics and may be explicitly finished.

The session selects a fresh root binding at the first preparation of the new
window, even when the selected target is unchanged. The actual finalized
prepared request is checked for the exact required input and qualified
capacity before it can allocate an attempt. A known oversize request or
unavailable qualified capacity holds at the successor. An authorized owner
can continue with an exact current window revision after changing the target.
The provider's final write guard records admission after validating current
source, operation and credential checks. Once the admission is durable,
the same obligation cannot launch again after a lost reply or uncertain
submission.

The standard turn worker exposes a body-free post-commit selection to the
authored lifecycle integration. Its `Continue`, `Wait` and `Finish` outcomes
apply only to the exact retired foreground owner and committed successor.
A current inherited Goal root requires a fresh inherited admission before
any successor dispatch. An authorized parent can repeat its native managed
send with the original key and input. The child selects the retained user
occurrence and stages the new Turn through the existing parent reservation,
child acknowledgement and parent confirmation. The selected operation starts
only after that handoff. Its newly prepared provider attempt must also pass
the inherited admission protocol before accepting the continuation. The
managed receipt stays suspended until this exact attempt commits. Interrupted
admission preserves the retained input and reconciles its acknowledgements
through the existing descendant lifecycle. An authorized controller can use
`agent_wait` with the retained receipt ID to observe its suspended state.
Recovery never reconstructs a retired process credential. An archive-only
cut waits for current authority.

The optional successor hint port borrows its authorized source during
preparation and request execution. The hint is request-only User guidance,
composed with installed guidance and source checks. Submission with a hint
uses isolated provider transport. The host holds grouped source custody across
admission and the first write. A same-session source allows only the exact
pending history suffix and its derived locator extension while all existing
source coordinates remain unchanged. A capacity rejection before attempt
allocation permits one preparation with the optional hint omitted.

## Codex comparison

The September 8 investigation pinned Codex
`44ab72674e0f0d8fc5f87a01389912d617793206`. It described a token-budget mode
using reminder/fallback prompt assets and a requested new window. The
November 20 public client at `51f4b8af01db1fb04e0cc6235444573ecceda716`
still queues its new-window tool for a later turn boundary, selects emergency
reset using its budget policy, invokes hooks around replacement, and reinstalls
the current world state with a new window identity. Manual compact selects
reset in that mode. Its separate summarizing or provider-integrated compaction
dispatch follows current provider capability. The current bundled catalog
and eligibility flow differ from September. Public source cannot establish
service eligibility, history reachability, retention or checkpoint commit.
[September source](https://github.com/openai/codex/commit/44ab72674e0f0d8fc5f87a01389912d617793206),
[November turn boundary](https://github.com/openai/codex/blob/51f4b8af01db1fb04e0cc6235444573ecceda716/codex-rs/core/src/session/turn.rs#L807-L839),
[reset and hooks](https://github.com/openai/codex/blob/51f4b8af01db1fb04e0cc6235444573ecceda716/codex-rs/core/src/compact_token_budget.rs#L42-L75),
[ordinary compaction dispatch](https://github.com/openai/codex/blob/51f4b8af01db1fb04e0cc6235444573ecceda716/codex-rs/core/src/session/turn.rs#L1916-L1984).

OChat takes the later safe-point handoff, bounded recovery hints, fresh
instruction assembly and observable hooks. Its local archive transaction and
revocable per-session note references supply recovery. Budget thresholds come
from qualified OChat prepared requests. Existing summarizing compaction
keeps its command and idle gate in every mode. Hosted history, notes, hint
generation, remote compaction, hidden reasoning and provider account
eligibility are outside this transition's contract.

## Qualification and implementation boundaries

| Boundary | Required evidence |
| --- | --- |
| Final preparation and accounting | Expect tests vary model/profile, settings, overlays, tool schemas, images/documents, captured input, omitted output, unknown and exact boundary values. Show that target changes invalidate preparation and WebSocket prefix reuse. Show a withheld candidate creates no attempt or provider usage and keeps dependent wake intent pending. Check measured, estimated and unknown status plus separation from goal usage. |
| Checkpoint and policy | Exercise one warning across retries/restart, all checkpoint quality outcomes, veto on force, a bounded checkpoint attempt near hard pressure, safe specific provider rejection versus ambiguous failure, failed fit of required instructions, and a smaller-model switch before the next dispatch. Keep an explicit ordinary-compaction regression. |
| Actor and store | Fault before archive write, after archive sync, during journal publication, and after commit with lost response. Reopen and verify exact recovered membership, current window, causality and one receipt. Repeat both identical and conflicting keys and race two requests from the same old window. Verify orphan handling and exact missing/corrupt archive error paths. |
| Queue, tools and continuation | Parallel calls settle with results/cancellation in durable order. Queue input before/after the cut, wake acknowledgement and late/duplicate results survive restart exactly once. Test stale configuration, source edit or authority at both commit and dispatch. Exercise pre/post hook fault, loss after hook claim, each continuation outcome, no self-trigger loop, no duplicate scheduling, and an unqualified fallback path. |
| Authored recovery workflow | Under one composed moderator, write a per-session note, commit a transition, recover exact history and selected note revisions, then demonstrate explicit sharing and revocation. Verify a smaller request after omitting hints, safe status for clients at each stage, and continuity of goals, filesystem state, provider selection and jobs. |

Qualify the durable host with offline scripted adapters and isolated store fault
tests. Use an authenticated provider profile when actual transport observations
or provider errors need qualification. Record the host, adapter, artifact and
lifetime for each result. Embedded, daemon and headless execution need evidence
from their actual admission and storage paths. A remote client observes the
lifetime of its runtime host.

The actor owns the durable transition and restart receipts. The turn driver
owns the tool-batch and input boundary where it can consume a committed result.
The shared clients and authored ChatML helpers observe that same result. [Inference transition tests](../../test/agent_session/inference_transition_tests.ml),
[compaction tests](../../test/agent_session/compaction_tests.ml),
[moderator lifecycle tests](../../test/agent_session/moderator_lifecycle_tests.ml).
