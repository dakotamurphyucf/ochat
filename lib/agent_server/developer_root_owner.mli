open! Core

(** One physical host owner for an admitted developer run epoch. The actor holds the
    original authenticated principal, exact immutable capture, pinned live
    capability registry, an exclusive per-run physical claim on the shared
    daemon-owned Store, and the daemon-wide capacity ledger. It owns root action
    and physical worker lifetime. The host supplies source/selection admission
    and user-facing run receipts. *)

type t
type run = t

module Limits : sig
  type t

  (** Positive finite bounds for one active root callback, provisional jobs,
      active PLUS completed/retained native terminal scopes, and
      worker/quarantine scopes, active inference attempts and physically
      admitted call bindings. One root terminal intent occupies a separate
      fixed slot. The active-attempt bound is positive and at most 256; an
      attempt reserves its slot before Prepared/effect and retains it through
      exact terminal bookkeeping, including uncertain quarantine. The call
      binding bound is positive and at most 16384 per run; the actual selected
      inference Context further bounds each parent Registration by its
      candidate limit. A full map rejects the strict producer hook before tool
      execution; it never evicts a live delayed-call parent. Durable historical
      inference rows have the Store's separate configured limit. The Store's
      one-pending-per-run barrier, document/selection limits and shared
      Job_capacity limits remain binding. *)
  val create
    :  max_active_runs:int
    -> max_staged_jobs:int
    -> max_native_occupancy:int
    -> max_active_workers:int
    -> max_quarantined_workers:int
    -> max_active_inference_attempts:int
    -> max_admitted_call_bindings:int
    -> t Or_error.t
end

module Registry : sig
  type t

  (** Trusted host check bound to one physical Candidate. The preparation may
      yield and must prove current actor/source/selection/pin under the fresh
      attempt checkpoint. It returns one bounded NONYIELDING check for that
      exact proof. Neither callback grants a Root, Store writer or capability.
      Root stores the same function in its existing provisional admission; a
      recovery resume cannot replace it with a different capture candidate. *)
  type final_admission =
    actor:Operator_authorization.t
    -> capture:Chatml_developer_capture.t
    -> checkpoint:(unit -> unit)
    -> (unit -> (unit, Agent_protocol.Error.t) result, Agent_protocol.Error.t) result

  (** Comparison only: the scheduler and this physical Root registry must use
      the same capacity ledger. This grants no reservation or Root admission. *)
  val uses_capacity : t -> Job_capacity.t -> bool

  (** Configured bound on active Worker callbacks per Root. This bounds offer
      fibers; actual claims still use the Worker and shared capacity ledger. *)
  val max_active_workers : t -> int

  (** Snapshot at most the configured active-run bound of physically owned,
      same-domain Live roots. Excludes provisional startup recovery, Closing,
      Quarantined and Closed slots; returning a root grants no job Claim.
      Closed/foreign-domain registry use fails rather than pretending the
      scheduler saw an empty host. No Store I/O occurs under registry lookup. *)
  val active_for_scheduler : t -> (run list, Agent_protocol.Error.t) result

  (** One daemon-local owner registry over the ACTUAL shared Store and
      Job_capacity. No separate scheduler, document writer or capacity ledger
      is made. The daemon retains this value through all run owners and closes
      it before closing the Store. [sw] owns all action/worker scopes. *)
  val create
    :  sw:Eio.Switch.t
    -> store:Agent_store.Developer_run_store.t
    -> capacity:Job_capacity.t
    -> now:(unit -> Agent_protocol.Timestamp.t)
    -> limits:Limits.t
    -> t

  (** Reserve this run ID in the bounded in-memory registry, then acquire the
      Store's non-yielding physical Run_claim BEFORE a yielding admission read.
      Duplicate same-run admission rejects even across independent Registry
      values because the claim belongs to the exact Store. Failure, exception
      and cancellation release only this provisional slot/claim. The original
      authenticated actor and admitted capture are compared with the actual
      handle, and Store latest-Active state is read before returning. [registry]
      is the pinned host registration; [selected] must resolve within it.
      [job_policy] is the actual admitted host resource ceiling for saved
      Background_request values, not a script-selected fallback.
      [selected_model], when present, was issued by the actual provider port
      for this exact original actor token and is pinned once. Admission
      rechecks its current binding after yielding Store/worker preparation.
      None makes Model.spawn unavailable; there is no ambient fallback or
      in-place replacement.
      The host validates capture origin and workspace membership before calling
      this trusted host API; its pure capture value alone grants neither.
      When [final_admission] is supplied, [checkpoint] is required before a
      provisional slot is reserved. After all Store/tracking/worker waits,
      Root invokes its yielding full check, repeats its own physical
      [verify_admission], then invokes the returned nonyielding check immediately
      before publishing Owned Live. No yield separates that check and the table
      swap; a stale or Busy final check never exposes a Root. Existing callers
      without [final_admission] retain their prior admission checks. *)
  val admit
    :  ?final_admission:final_admission
    -> ?checkpoint:(unit -> unit)
    -> t
    -> actor:Operator_authorization.t
    -> capture:Chatml_developer_capture.t
    -> registry:Chat_response.Tool_capability.t
    -> selected:Chat_response.Tool_capability.t
    -> selected_model:Provider_operator_port.Model_selection.t option
    -> job_policy:Chat_response.One_off_request.policy
    -> handle:Agent_store.Developer_run_store.handle
    -> workspace_conflict_domain:string
    -> (run, Agent_protocol.Error.t) result

  (** Adopt the exact already-finished Startup claim from a currently entered
      Live command callback. The caller established this same physical
      Store/handle/claim/window before ANY current-epoch Reserve and consumed
      its durable Start before effectful native preparation. Registry first
      installs its bounded Admitting slot. It then invokes the trusted
      NONYIELDING [transfer_finished_claim] callback, whose [accept] verifies
      Store.issued_by plus exact Recovery.issued_finished and installs both
      tokens before Lifecycle relinquishes custody. A rejection leaves
      Lifecycle owner; an accepted transfer remains in Admitting on any later
      failure, exception or cancellation. Publication reuses the ordinary
      Tracking/Worker/final-admission kernel. It never opens Recovery again or
      interrupts concurrent same-run Check/Fixture callbacks. *)
  val admit_recovered
    :  ?final_admission:final_admission
    -> ?checkpoint:(unit -> unit)
    -> t
    -> actor:Operator_authorization.t
    -> capture:Chatml_developer_capture.t
    -> registry:Chat_response.Tool_capability.t
    -> selected:Chat_response.Tool_capability.t
    -> selected_model:Provider_operator_port.Model_selection.t option
    -> job_policy:Chat_response.One_off_request.policy
    -> handle:Agent_store.Developer_run_store.handle
    -> workspace_conflict_domain:string
    -> transfer_finished_claim:
         (accept:
            (claim:Agent_store.Developer_run_store.Run_claim.t
             -> window:Agent_store.Developer_run_store.Recovery.t
             -> (unit, Agent_protocol.Error.t) result)
          -> (unit, Agent_protocol.Error.t) result)
    -> (run, Agent_protocol.Error.t) result

  (** Trusted daemon cleanup after Host has authorized retirement and joined
      ALL same-run command callbacks. The exact physical handle selects one
      retained Admitting entry; no principal/ID-only lookup can release it.
      This cleanup remains retryable after the original actor expires, since
      retirement was already authorized before cancellation began. It closes
      provisional Worker/Tracking and releases the exact claim through the
      existing cleanup kernel. An unresolved write or cleanup error retains
      Admitting; an Owned Root must use its ordinary close path. A missing
      entry is harmless only when no transfer occurred. No Recovery reruns. *)
  val retire_admission
    :  t
    -> handle:Agent_store.Developer_run_store.handle
    -> (unit, Agent_protocol.Error.t) result

  (** The startup form of [admit]. After acquiring the same physical Run_claim
      and authenticating the original actor/capture, open the one-shot Store
      recovery window and call [recover] BEFORE creating or exposing a worker.
      The callback selects only terminal bookkeeping through that window and
      must finish it; returned [Ok ()] without a finished window rejects.
      Caller exceptions/cancellation keep their identity. A clean preselection
      failure can abort the empty window and release admission. An unresolved
      Pending retains the provisional slot and Run_claim, excluding any new
      same-run owner or effect until [resume_recovery] settles it. A supplied
      [final_admission] is retained with this same provisional slot and physical
      claim. Its full check runs after the final Store/worker wait for THIS
      attempt and returns a fresh nonyielding check; a failed check follows
      existing joined cleanup without discarding unknown Store custody. *)
  val admit_with_recovery
    :  ?final_admission:final_admission
    -> ?checkpoint:(unit -> unit)
    -> t
    -> actor:Operator_authorization.t
    -> capture:Chatml_developer_capture.t
    -> registry:Chat_response.Tool_capability.t
    -> selected:Chat_response.Tool_capability.t
    -> selected_model:Provider_operator_port.Model_selection.t option
    -> job_policy:Chat_response.One_off_request.policy
    -> handle:Agent_store.Developer_run_store.handle
    -> workspace_conflict_domain:string
    -> recover:
         (Agent_store.Developer_run_store.Recovery.t
          -> (unit, Agent_protocol.Error.t) result)
    -> (run, Agent_protocol.Error.t) result

  (** Re-enter only this Registry's retained provisional recovery for the
      original current actor object and exact physical handle. The same window
      and any retained terminal preparation are reused; no worker or new
      Run_claim is created. A revoked actor cannot select a different terminal
      outcome or receive a worker. Retained custody is preserved until the
      original actor becomes current again, or a separately authorized future
      cleanup policy reconciles the exact frozen preparation. There is no
      fresh-actor rebind in this API.
      Missing, foreign, closed, concurrent or already-owned slots reject. If
      this provisional slot has [final_admission], a FRESH [checkpoint] is
      required for resumed full verification of the SAME captured Candidate;
      the earlier attempt's checked receipt cannot be reused. *)
  val resume_recovery
    :  ?checkpoint:(unit -> unit)
    -> t
    -> actor:Operator_authorization.t
    -> handle:Agent_store.Developer_run_store.handle
    -> recover:
         (Agent_store.Developer_run_store.Recovery.t
          -> (unit, Agent_protocol.Error.t) result)
    -> (run, Agent_protocol.Error.t) result

  (** Exclude new admissions, revoke and join each exact run owner, and leave
      shared Store closure to the daemon. Unknown per-run quarantine retains
      its claim and causes an Error, never permits a second owner. Other runs
      may still complete cleanup. Self-close from one of its own callbacks
      rejects before revoking anything. *)
  val close_and_wait : t -> (unit, Agent_protocol.Error.t) result
end

module Current : sig
  type t

  (** The exact immutable selection this witness observed. A later narrowing
      invalidates the witness even if it happens to preserve a particular tool. *)
  val selected : t -> Chat_response.Tool_capability.t
end

module Claim_preflight : sig
  type t

  (** Selected comparison data for scheduling; unlike [Current.t], this
      value cannot be passed to effect-currentness checks. *)
  val selected : t -> Chat_response.Tool_capability.t
end

(** Optional nonsecret model selection pinned at physical admission. The
    selection remains issued by its original provider port; this projection
    does not turn a copied target into authority. *)
val selected_model : t -> Provider_operator_port.Model_selection.t option

module Selection : sig
  type t

  (** Exact ordered jobs sealed from one root action. This projection remains
      data; only the issuing owner may commit it under its live action scope. *)
  val jobs : t -> Agent_protocol.Developer_job.t list
end

module Action : sig
  type t

  type cleanup =
    | Aborted
    | Physically_owned
  [@@deriving sexp_of]

  (** Construct and reserve a real typed background job through
      Developer_root_job_admission and the daemon-wide Job_capacity. The
      request must have been produced under the run's authentic selected host
      surface; a changed principal/epoch/selection after a yielding source
      preparation rejects before reservation. The returned ID is only a handle
      within this physical action, not execution authority. *)
  val stage_background
    :  t
    -> request:Chat_response.Background_request.t
    -> checkpoint:(unit -> unit)
    -> (Agent_protocol.Id.Job.t, Agent_protocol.Error.t) result

  (** Admit an external Model.spawn at THIS call boundary. The request is the
      shared recipe decoded from the script; the host re-admits its original
      JSON under the real whole-envelope and job/document bounds before any
      durable or capacity admission. This physical Action derives its
      original owner/capture, a fresh Job ID, Run_root ancestry, the provider
      port's pinned nonsecret Target and the actual project/workspace capacity
      bucket. The caller cannot supply those coordinates. The existing Store
      Prepared/Handoff and shared capacity ledger publish one Queued Model_call
      before returning its ID. This job is independent of [seal]'s final
      Job.start selection and survives a later callback failure/Task.catch.
      No model secret is acquired or model execution performed here.
      An uncertain write retains this root's exact physical reservation and
      pending transaction; the language callback is never replayed. The root
      callback Switch and call cancellation remain live through the final
      nonyielding Store admission, including protected cleanup paths. *)
  val admit_external_model
    :  t
    -> request:Agent_protocol.Model_spawn_request.t
    -> checkpoint:(unit -> unit)
    -> (Agent_protocol.Id.Job.t, Agent_protocol.Error.t) result

  (** Abort only this action's own unsealed provisional reservation. The
      issuing physical scope and fiber must still own it, or the action must
      have conclusively aborted it during bracket cleanup. A sealed/queued,
      published or uncertain selected job is never released by lexical
      rollback; its exact Store handoff retains custody. Repetition after a
      proven abort is harmless. *)
  val rollback_staged
    :  t
    -> Agent_protocol.Id.Job.t
    -> (unit, Agent_protocol.Error.t) result

  (** Cleanup for a lexical Job scope that outlives this action's callback.
      Check the exact issuing action, domain and staged ID. While the action is
      live and unsealed, perform [rollback_staged]; after its bracket, report
      an already proven abort or leave a sealed/queued/uncertain reservation
      under this root's physical handoff. This cannot release selected work,
      grant an ID authority, or undo a published job. Ordinary Task.catch
      continues to use strict [rollback_staged] while the callback is live. *)
  val cleanup_staged
    :  t
    -> Agent_protocol.Id.Job.t
    -> (cleanup, Agent_protocol.Error.t) result

  (** One-shot seal of a distinct subset of this action's own staged IDs.
      Unknown/duplicate/foreign IDs reject. Sealing excludes later staging and
      fixes exact immutable job records; the caller returns this opaque value
      with its outcome. Sealing requires the live action on its issuing Eio
      domain. Unselected reservations remain owned until the atomic
      handoff either publishes selected jobs or aborts them. *)
  val seal
    :  t
    -> selected:Agent_protocol.Id.Job.t list
    -> (Selection.t, Agent_protocol.Error.t) result
end

(** Bare Script-root action borrowed from this run's already entered Worker
    Claim. Unlike [Action], it does not reserve a Root terminal slot or commit
    a separate root-selected transaction. The Worker alone commits its parent
    state together with this action's selected children. *)
module Worker_action : sig
  type t

  module Selection : sig
    type t = Developer_job_worker.Children.Selection.t
  end

  (** Use this root's original actor, capture, current selected registry and
      resource policy with the existing background admission constructor.
      Derive Run_root plus the exact parent ID/attempt and incremented depth
      from the physical Worker Parent; never invent a native invocation.
      Recheck actor/selection/parent after yielding preparation and immediately
      before the Worker's shared capacity reservation. Each check takes a fresh
      nonyielding physical currentness witness while requiring the original selected
      registry/revision: this Worker may perform a native Tool.call that advances
      the Root effect generation between two legitimate Job.start operations,
      but cannot inherit a changed selection. The returned ID is
      lexical comparison data only. *)
  val stage_background
    :  t
    -> request:Chat_response.Background_request.t
    -> checkpoint:(unit -> unit)
    -> (Agent_protocol.Id.Job.t, Agent_protocol.Error.t) result

  val rollback_staged
    :  t
    -> Agent_protocol.Id.Job.t
    -> (unit, Agent_protocol.Error.t) result

  val cleanup_staged
    :  t
    -> Agent_protocol.Id.Job.t
    -> (Action.cleanup, Agent_protocol.Error.t) result

  (** Seal exactly the staged IDs returned from this Worker-root lexical
      callback. Physical Worker scope and Claim identity are retained in the
      opaque selection and rechecked after child join. This performs no Store
      operation. *)
  val seal
    :  t
    -> selected:Agent_protocol.Id.Job.t list
    -> (Selection.t, Agent_protocol.Error.t) result
end

module Native_terminal : sig
  type t

  (** Physical scope issuer and immutable original invocation. Developer
      lexical Job admission compares these before borrowing the native
      selected ceiling; copied owner coordinates are insufficient. *)
  val issued_by_root : t -> root:run -> bool

  val invocation : t -> Agent_protocol.Host_invocation.t

  module Selection : sig
    type t

    (** Exact immutable jobs sealed by one physically registered native
        terminal scope. IDs alone are data, not publication authority. *)
    val jobs : t -> Agent_protocol.Developer_job.t list
  end

  (** Exact physically registered Dispatching bridge for the shared developer
      invocation kernel. Both operations verify this scope's invocation and
      owner; the issuer retains the one Store Prepared/Handoff pair on [t].
      Neither method grants native effect authority. A failed [save_dispatch]
      preserves its original error or exception. Before releasing the Root
      mutation gate it makes one protected reconciliation of an attempted
      write: exact Installed/Absent evidence updates local custody, while an
      unknown outcome retains the same pending pair for later recovery. *)
  val read_dispatch
    :  t
    -> Agent_protocol.Id.Invocation.t
    -> ((int * Agent_protocol.Host_invocation.t) option, Agent_protocol.Error.t) result

  val save_dispatch
    :  t
    -> expected_revision:int option
    -> dispatched:Agent_protocol.Host_invocation.t
    -> (int, Agent_protocol.Error.t) result

  type dispatch_reconciliation =
    | Installed of Agent_protocol.Host_invocation.t * int
    | Absent
    | Uncertain

  (** Only after the native registration bracket exits. Installed is evidence
      for truthful Fail/Cancelled bookkeeping, never permission to start a
      callback after an uncertain save. Absent proves no dispatch; Unknown
      retains the scope, run claim and one Store pending barrier. *)
  val reconcile_dispatch : t -> (dispatch_reconciliation, Agent_protocol.Error.t) result

  (** Whether the exited bracket still owns this exact physical registration.
      A false result means ordinary bracket cleanup already retired an
      unentered, conclusively absent dispatch. This does not prove absence;
      callers must first use [reconcile_dispatch]. *)
  val retained : t -> (bool, Agent_protocol.Error.t) result

  val callback_complete : t -> bool

  (** Nonyielding liveness of this exact physical callback, including its
      registered cancellation context and actual child Switch. False while
      its children join after cancellation/failure, or after bracket exit.
      The selected invocation issuer retains this check on its active parent;
      a protected nested child cannot infer liveness from its own context. *)
  val effect_parent_live_now : t -> bool

  (** Nonyielding admission to seal the result of this already-entered,
      durably dispatched native callback. Requires the exact live registered
      action/fiber, original selected object and selection revision, current
      actor and Root; it does not grant another effect or staged job. An
      earlier terminal may already be queued, so this check deliberately does
      not require the queue's new-effect witness. A native dispatch increments
      the Root effect revision itself, and another admitted dispatch may do so
      while this callback runs; that revision is not completion identity.
      The lexical caller must separately rebind its selected subset and check
      its physical parent Claim/attempt before sealing. *)
  val check_completion_now : t -> (unit, Agent_protocol.Error.t) result

  (** Return the already frozen canonical pair, if any. A late exception may
      not replace it with Fail/None. *)
  val sealed : t -> (Agent_protocol.Invocation.outcome * Selection.t) option

  (** Attach one immutable pre-entry Fail/Cancelled terminal to this physical
      scope before an uncertain dispatch can outlive the caller's stack. *)
  val remember_failure
    :  t
    -> terminal:Agent_protocol.Host_invocation.t
    -> (unit, Agent_protocol.Error.t) result

  (** Resolve that exact retained terminal after the bracket exits and
      Dispatching is known durable through [save_dispatch] or reconciliation.
      Ordinary completed callbacks also attach their canonical terminal before
      the first yielding submission, so same-owner recovery never invents a
      replacement outcome or selection. *)
  val resolve_remembered : t -> (int, Agent_protocol.Error.t) result

  (** This scope is terminal custody, not native-effect permission. The shared
      invocation policy must durably Dispatch and independently prove its
      selected binding, permission and live parent before a handler runs. A
      job-descended scope derives launch parent_job and depth from its exact
      registered physical Claim ancestor; root-origin staging uses None/0.
      A yielding preparation repeats the ancestor's nonyielding liveness check
      immediately before reserving capacity. [check_parent_now] is the trusted
      selected callback's nonyielding direct-invocation/deadline check at that
      same boundary; it cannot choose a result or selection. *)
  val stage_background
    :  t
    -> request:Chat_response.Background_request.t
    -> checkpoint:(unit -> unit)
    -> check_parent_now:(unit -> (unit, Agent_protocol.Error.t) result)
    -> (Agent_protocol.Id.Job.t, Agent_protocol.Error.t) result

  (** The native call-boundary counterpart of Action.admit_external_model.
      Its decoded request is re-admitted under the real whole-envelope and
      job/document bounds. This exact registered scope must own a durable Dispatching invocation.
      Its root derives Invocation ancestry and any nested Job ID, attempt and
      depth from the live physical Claim already attached to this scope. After
      yielding preparation it rechecks that Claim and the selected callback's
      direct-parent/deadline witness before reservation and again at the final
      nonyielding Store admission point. [check_parent_now] is a trusted,
      nonyielding effect-entry check; it cannot choose a provider, outcome,
      capacity bucket or job selection. A copied invocation
      or result DTO cannot admit a model call. The durable job remains even if
      the native callback later fails; unknown write custody stays with the
      root's single bounded mutation/recovery slot. *)
  val admit_external_model
    :  t
    -> request:Agent_protocol.Model_spawn_request.t
    -> checkpoint:(unit -> unit)
    -> check_parent_now:(unit -> (unit, Agent_protocol.Error.t) result)
    -> (Agent_protocol.Id.Job.t, Agent_protocol.Error.t) result

  (** The same exact provisional rollback as Action.rollback_staged, bound to
      this physical native terminal's internal action. It cannot remove a job
      from a frozen canonical outcome/selection or an uncertain handoff. *)
  val rollback_staged
    :  t
    -> Agent_protocol.Id.Job.t
    -> (unit, Agent_protocol.Error.t) result

  (** The native counterpart of Action.cleanup_staged, bound to the same
      registered terminal and its physical staged ledger. After a sealed or
      uncertain terminal, lexical cleanup must retain capacity with the root
      rather than reinterpret its ID as an abort grant. *)
  val cleanup_staged
    :  t
    -> Agent_protocol.Id.Job.t
    -> (Action.cleanup, Agent_protocol.Error.t) result

  (** One-shot seal of this scope's own staged IDs after the callback's
      canonical outcome is known. [submit] compares this exact outcome with
      the resolved terminal record. Nonempty selection stays paired with that
      outcome, including Complete and Pending, through settlement/recovery. *)
  val seal
    :  t
    -> outcome:Agent_protocol.Invocation.outcome
    -> selected:Agent_protocol.Id.Job.t list
    -> (Selection.t, Agent_protocol.Error.t) result

  (** Submit the exact resolved terminal of this scope's durable Dispatching
      invocation with its physically sealed selection into the ONE root/native
      terminal queue. [None] is for a callback that never returned a sealed
      success; provisional work is aborted and no child job is published.
      This may return typed persistence uncertainty while retaining the exact
      immutable intent/reservations. Even an empty native terminal is retained
      across a proven absent write and reprepared from the same frozen outcome;
      only an empty root checkpoint may retire as absent. Repeating the caller
      action cannot replay its callback. Successful return is the durable
      invocation revision. *)
  val submit
    :  t
    -> terminal:Agent_protocol.Host_invocation.t
    -> selected:Selection.t option
    -> (int, Agent_protocol.Error.t) result

  (** Trusted terminal-only recovery for the exact physically retained scope
      after its registration bracket has exited without submission. Reread its
      durable Dispatching record and validate the supplied canonical Resolved
      transition against that exact context; no callback or effect is entered.
      If [seal] already froze an outcome/selection, recovery must preserve that
      pair exactly, including nonempty Complete or Pending selections. Without
      a seal, only a caller-produced Fail or Cancelled terminal may be saved;
      Complete and Pending would invent a successful result or orphan work.
      An uncertain write retains the same frozen intent for later bounded
      reconciliation. This does not authorize arbitrary status rewriting: the
      shared native-policy actor owns the truthful terminal reason and invokes
      this method under its protected failure bookkeeping. Terminal-only native
      bookkeeping with no selected jobs may finish after principal/selection
      revocation under the exact Store owner, epoch and Dispatching proof;
      nonempty work publication still needs current authorization. *)
  val resolve_retained
    :  t
    -> terminal:Agent_protocol.Host_invocation.t
    -> (int, Agent_protocol.Error.t) result

  (** Release an exact retained registration only after proving that dispatch
      never happened. The bracket must have exited before callback entry or
      sealing, and this physical scope must have no staged reservations.
      Reobservation accepts only an absent record or its original Admitted
      record; Dispatching, Resolved and uncertain reads retain the slot/claim.
      No await occurs after this proof and before retiring the exact queue
      slot. The shared native adapter must join dispatch admission inside the
      bracket, so a legitimate dispatch cannot start after it exits. *)
  val abandon_retained_without_dispatch : t -> (unit, Agent_protocol.Error.t) result
end

val owner : t -> Agent_protocol.Execution_owner.developer
val capture : t -> Chatml_developer_capture.t

(** Host-only admission facts retained by this physical owner. The captured
    actor and policy are not reconstructed from request coordinates. *)
val original_actor : t -> Operator_authorization.t

val job_policy : t -> Chat_response.One_off_request.policy

(** Physical Store-issuer comparison for composing result disclosure with
    this run. Equal host/run IDs or paths do not establish this binding. *)
val issued_by_store : t -> Agent_store.Developer_run_store.t -> bool

(** Borrow result-publication custody from this root's actual retained Store
    handle. The Store revalidates its live issuer and persisted run identity;
    equal host/run coordinates or a caller-supplied Result_root cannot replace
    it. This does not grant result reads, worker entry, current-epoch effects or
    Blob authority. The caller must bind the returned root to this same Store
    and its shared Blob coordinator before selecting a result. *)
val result_root
  :  t
  -> (Agent_store.Developer_run_store.Result_root.t, Agent_protocol.Error.t) result

(** Immutable limits of this owner's actual Store handle. This is comparison
    configuration for decoding a saved Model_call; it grants no Job Claim or
    source/effect authority. The Store remains the final validator. *)
val document_limits : t -> (Document_schema.Limits.t, Agent_protocol.Error.t) result

module Job_result : sig
  type t

  (** Bind one result-publication owner to this physical run before installing
      any Tool/Script or Model runner. [blobs] must be the shared coordinator
      bound to the same Store; Publisher construction checks that physical
      binding and the upload/whole-result limits. The Root's owning switch
      outlives all joined Workers and retained selections. A successful bind
      occupies the Root's sole publisher slot; a second bind conflicts rather
      than creating an independent pending table or renewing [max_pending]. A
      failure before installation leaves the slot empty for a corrected retry.
      Closing the Root joins effects and reconciles exact publisher selections
      through the existing Worker/Store path before releasing its run claim.
      This is publication custody, not result-read or Blob disclosure. *)
  val bind
    :  run
    -> blobs:Agent_store.Blob_store.t
    -> limits:Agent_store.Developer_result_publisher.Limits.t
    -> storage:Agent_store.Developer_result_publisher.Storage.t
    -> (t, Agent_protocol.Error.t) result

  (** Exact physical Root comparison only; matching owner/run coordinates or
      another Result_root cannot attach this owner to a different run actor. *)
  val issued_by_root : t -> run -> bool

  (** Shared Tool/Script/Model selection policy over ONE Publisher instance and
      [max_pending] table. The caller supplies the Worker's original physical
      Claim and normalized proposal; this operation does not claim a job or
      write a terminal state. [None] is valid only when the proposal has no
      result. An Inline terminal is frozen once, potentially as Artifact under
      the bound storage policy, and wrapped in the same Worker.Selected token.
      An already-Artifact caller proposal rejects. Unknown intent/Blob/terminal
      persistence retains the exact publisher token and capacity until Worker
      reconciliation; no handler, selection or effect is replayed. A closing
      Root may still finish exact already-entered Worker bookkeeping after its
      callbacks join; this method never grants a fresh Claim or effect. *)
  val select
    :  t
    -> claim:Agent_store.Developer_run_store.Claim.t
    -> proposed:Agent_protocol.Developer_job.State.t
    -> (Developer_job_worker.Selected.t option, Agent_protocol.Error.t) result
end

(** Read-only authentic data retained by the physical run owner. *)
val native_ceiling : t -> Chat_response.Tool_capability.t

val native_selected : t -> Chat_response.Tool_capability.t
val native_now : t -> Agent_protocol.Timestamp.t

(** Derive a physically owned job ancestor from this root's actual Worker
    issuer and an entered callback token. This never accepts a caller-supplied
    worker issuer, durable Running row or copied job coordinates. *)
val job_parent
  :  t
  -> Agent_session.Developer_job_scope.active
  -> (Agent_session.Developer_job_scope.Parent.t, Agent_protocol.Error.t) result

module Job_candidate : sig
  type t

  (** Read only the genuine latest-Active Store candidates for this live root.
      Recheck its original actor, physical run claim and lifecycle after the
      yielding read and after per-candidate host projection checkpoints. Each
      result retains the exact Store observation/lookup
      and this issuing run; a job DTO or matching owner coordinates cannot
      construct or rebind it. A failed read exposes no partial candidates. *)
  val read_current
    :  run
    -> checkpoint:(unit -> unit)
    -> (t list, Agent_protocol.Error.t) result

  val job : t -> Agent_protocol.Developer_job.t
  val host_job : t -> Agent_protocol.Host_job.t

  (** Validate that [candidate] was issued by this physical run, and delegate
      its exact Observation/Job_lookup to the existing [with_claimed_job].
      That worker alone transfers shared capacity, persists Running before
      [f], owns the one-shot callback/child Switch and selects terminal state.
      Saturated/not-due/stale candidates do not call [f]. The callback is the
      existing physical worker continuation, never an authorization Boolean
      or a second scheduler. A queued job whose executor is not installed
      must remain queued; callers must not use this operation to fail it. *)
  val with_claimed
    :  run
    -> t
    -> ?on_capacity_decision:(Developer_job_worker.Capacity_decision.t -> unit)
    -> ?select:
         (claim:Agent_store.Developer_run_store.Claim.t
          -> proposed:Agent_protocol.Developer_job.State.t
          -> (Developer_job_worker.Selected.t option, Agent_protocol.Error.t) result)
    -> (active:Agent_session.Developer_job_scope.active
        -> sw:Eio.Switch.t
        -> (Agent_protocol.Developer_job.State.t, Agent_protocol.Error.t) result)
    -> (Agent_protocol.Developer_job.t option, Agent_protocol.Error.t) result

  (** The claimed-worker counterpart for a bare Script root. [action] is
      minted only after this candidate's exact Claim entered; its selection
      must come from this same live callback and physical parent. The Worker
      commits the parent and selected children through one Prepared, and
      returns the Store-proven selected-set disposition. A copied Job or
      matching run ID cannot substitute for [candidate]. [active] is that
      same Worker's live parent proof for selected native calls; it cannot be
      constructed from the action or a job DTO. *)
  val with_claimed_selected
    :  run
    -> t
    -> ?on_capacity_decision:(Developer_job_worker.Capacity_decision.t -> unit)
    -> ?selection_witness:Claim_preflight.t
    -> ?select:
         (claim:Agent_store.Developer_run_store.Claim.t
          -> proposed:Agent_protocol.Developer_job.State.t
          -> (Developer_job_worker.Selected.t option, Agent_protocol.Error.t) result)
    -> (active:Agent_session.Developer_job_scope.active
        -> action:Worker_action.t
        -> sw:Eio.Switch.t
        -> ( Agent_protocol.Developer_job.State.t * Worker_action.Selection.t
             , Agent_protocol.Error.t )
             result)
    -> ( (Agent_protocol.Developer_job.t * Developer_job_worker.Children.Settlement.t)
           option
         , Agent_protocol.Error.t )
         result
end

(** Enter an exact queued attempt through this root's owned physical Worker,
    under its registered child Switch and the shared daemon capacity ledger.
    The callback receives only the entered active Claim; it may call the same
    selected native executor as a direct job child. The Worker durably claims
    Running before entry, joins children before finalizing the proposed state,
    and never replays the callback after uncertain persistence. This method
    does not expose the Worker issuer or add a scheduler. Root currentness and
    lifecycle are checked before delegation; every effect still performs its
    own final root/Claim/permission checks. [select] retains the Worker's exact
    already-frozen result custody when the higher result actor supplies it. *)
val with_claimed_job
  :  t
  -> expected:Agent_store.Developer_run_store.Observation.t
  -> lookup:Agent_store.Developer_run_store.Job_lookup.t
  -> ?on_capacity_decision:(Developer_job_worker.Capacity_decision.t -> unit)
  -> ?select:
       (claim:Agent_store.Developer_run_store.Claim.t
        -> proposed:Agent_protocol.Developer_job.State.t
        -> (Developer_job_worker.Selected.t option, Agent_protocol.Error.t) result)
  -> (active:Agent_session.Developer_job_scope.active
      -> sw:Eio.Switch.t
      -> (Agent_protocol.Developer_job.State.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Developer_job.t option, Agent_protocol.Error.t) result

module Job_control : sig
  type snapshot

  type cancellation =
    | Already_terminal
    | Durable_cancelled
    | Claimed_cancel_requested

  (** A snapshot returns immutable comparison data only. Its opaque issuer and
      exact Store-read job bind this root, owner, capture and attempt without
      retaining the whole observed run document; neither the job DTO nor its ID
      alone grants cancellation authority. *)
  val job : snapshot -> Agent_protocol.Developer_job.t

  (** Read this run owner's current Active epoch through its physical Store
      handle and exact admitted capture, then recheck the original actor and
      local lifetime after the read. Historical or foreign jobs are not
      returned. *)
  val find_current
    :  run
    -> Agent_protocol.Id.Job.t
    -> (snapshot option, Agent_protocol.Error.t) result

  (** Cancel the exact observed attempt under its issuing physical run owner.
      A foreign Store/root, changed attempt, new epoch or immutable mismatch
      rejects; same-attempt terminal completion is idempotent. A prewrite
      conflict may reread only this snapshot's immutable job and attempt,
      never retarget a newly claimed attempt. Queued and Waiting_completion
      use a fresh Store CAS; a queued retry or Waiting_completion first joins
      any physical worker handoff and then rereads the exact ended state. A claimed Running or
      Waiting_permission job targets only the actual registered worker. An
      already terminal job remains unchanged. [Durable_cancelled] requires a
      reconciled Store installation; [Claimed_cancel_requested] only confirms
      the physical cancellation request, even if its callback has joined;
      it never implies terminal persistence or result disclosure. A
      self-cancel signals without waiting for its own callback/children.
      Unknown I/O retains the exact Prepared in this owner's single bounded
      control-recovery slot, the Store pending barrier and capacity;
      no second writer, scheduler, effect replay or blind retry is introduced.
      The original actor, admitted capture, selected run and latest epoch are
      rechecked after yielding reads and immediately before a new write or
      signal. *)
  val cancel_current : run -> snapshot -> (cancellation, Agent_protocol.Error.t) result
end

(** Read actual latest Active epoch through the Store, then recheck original
    actor, same-domain physical lifetime and selected registry without yielding.
    This can yield. No request-supplied ID, digest, or copied generation creates
    a witness. *)
val current : t -> (Current.t, Agent_protocol.Error.t) result

(** Nonyielding local scheduling snapshot of the same physical owner, actor,
    selected object and revision. This is comparison data only: it does not
    establish latest-Active Store state, authorize an effect or issue a Claim.
    The Worker reserves capacity first, then [with_claimed_job] or
    [Job_candidate.with_claimed_selected] performs a fresh yielding [current]
    and compares this selection before preparing the Store Claim. *)
val claim_preflight : t -> (Claim_preflight.t, Agent_protocol.Error.t) result

(** Final nonyielding check after another operation's yielding parent/permission
    read and immediately before a real host effect. It compares the physical
    witness to this owner, actor currentness, epoch/selection revision and open
    lifetime. A queued/draining/retained terminal intent excludes new effects;
    after known settlement a fresh witness can be issued to a still-live
    parent. No Eio operation, Store read or host callback occurs here. The
    effect caller must perform no await between this check and effect entry.
    Native callers combine it with exact parent and deadline checks. *)
val check_effect_now : t -> Current.t -> (unit, Agent_protocol.Error.t) result

(** Explicit host-owned narrowing of this run's already selected live registry.
    It resolves every name against the CURRENT selection, so it cannot widen or
    rebind by name. It invalidates all existing Current witnesses before the new
    selection is visible. An in-flight action must recheck after yielding and
    cannot publish jobs captured under an obsolete selection. *)
val narrow_selection : t -> names:string list -> (unit, Agent_protocol.Error.t) result

(** Run one bounded, registered root callback under its own Eio child switch.
    It may stage jobs but none is eligible before durable selection. The callback
    returns its typed value and an opaque Selection minted by this exact Action;
    the action switch and children join before preparation. An Error, unexpected
    exception or cancellation aborts provisional capacity and never replays the
    callback. On success, persist [phase] plus exact selected jobs in one
    Developer_run_store.prepare_root_selected transaction and settle under one
    Store handoff: Installed publishes selected and aborts unselected; a
    nonempty Absent retains the exact intent/reservations for bounded
    reprepare, while an empty Absent aborts provisional work;
    Diverged/unknown retains finite quarantined ownership. A
    settlement callback may repeat idempotently, but the action/effects do not.
    Root and native terminals share one bounded FIFO/drain slot; no terminal
    lock is held across the callback, so it may synchronously invoke a native
    child. An Interrupted/Closed phase rejects while any native callback or
    retained native intent exists, before changing the epoch beneath it.
    A nonempty selection requires [phase = Active]; closing or interrupting an
    epoch cannot admit fresh queued work. This writes a checkpoint phase, not a
    user-visible outcome/receipt. The run service owns action idempotency and
    terminal result persistence. *)
val with_root_action
  :  t
  -> phase:Agent_store.Developer_run_store.Checkpoint.phase
  -> (action:Action.t
      -> sw:Eio.Switch.t
      -> ('a * Selection.t, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Reserve one native occupancy slot BEFORE the first yielding dispatch read.
    This scope only owns bounded callback/terminal custody; the shared native
    policy and physical live-parent checks remain necessary before effects.
    Nested callbacks do not hold the terminal drain lock. A returned uncertain
    callback keeps its occupancy, preventing replacement calls from growing
    the retained queue. If initial admission fails before any scope is built
    or exposed, its private slot is abandoned directly because this bracket
    could not dispatch. An exposed scope is abandoned only after proving it
    never dispatched; an unsubmitted durable or uncertain Dispatching record keeps
    its occupancy and run claim. The native policy integration must resolve
    success, failure, cancellation and unexpected exception into a canonical
    terminal and call [Native_terminal.submit] under protected bookkeeping
    before this bracket exits. If that resolution itself fails or is cancelled,
    the trusted actor may use [Native_terminal.resolve_retained] with the exact
    physical scope after the bracket exits. No terminal outcome is invented
    from a partial callback. *)
val with_native_registration
  :  t
  -> ?job_parent:Agent_session.Developer_job_scope.Parent.t
  -> invocation:Agent_protocol.Host_invocation.t
  -> (scope:Native_terminal.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Enter this registered callback only after the shared invocation adapter
    durably saved its exact Dispatching record. The child switch joins before
    the resulting outcome/selection can be submitted. No policy admission or
    terminal CAS is performed by this bracket itself. *)
val with_native_callback
  :  Native_terminal.t
  -> dispatched:Agent_protocol.Host_invocation.t
  -> (sw:Eio.Switch.t -> ('a, Agent_protocol.Error.t) result)
  -> ('a, Agent_protocol.Error.t) result

(** Physical inference bookkeeping for one entered Model_call Worker callback.
    This is a lexical Root/Store/Claim owner, not a second inference algorithm,
    provider resolver, capacity ledger or document writer. The shared
    Inference_client remains responsible for preparation, actual attempt and
    completion callback order. The selected provider and actor guard are
    composed by the higher model runner. *)
module Model_inference : sig
  type t

  (** Compare a saved Agent.run source with this exact physical Root before
      preparing child MCP/shell/native resources. This factors the existing
      [with_claimed_agent] admission: current saved source and Worker, exact
      registered Host invocation object in this Root's native_calls, entered
      native callback key, live Native_terminal and current Root. It creates
      no inference scope or Store tracker. A copied owner/artifact/ID cannot
      substitute for the entered callback. Expected denial is typed; original
      cancellation and unexpected exception identity are preserved. *)
  val check_saved_agent_source
    :  run
    -> selected:Agent_session.Captured_agent_runner.Claimed_developer.Selected.t
    -> (unit, Agent_protocol.Error.t) result

  (** Nonyielding recheck after a child selected borrow shadows the saved
      parent's direct fiber binding. The exact physical selected-ancestor
      proof, registered invocation/entered native callback, terminal and Root
      local currentness all remain required. Call immediately before child
      native/managed effect admission, with no intervening yield. *)
  val check_saved_agent_source_descendant_now
    :  run
    -> selected:Agent_session.Captured_agent_runner.Claimed_developer.Selected.t
    -> child:Agent_session.Native_tool_invocation.selected_borrowed
    -> (unit, Agent_protocol.Error.t) result

  (** Borrow this root's actual entered, durably Running Model_call Claim. The
      scope checks exact Worker issuer, saved job/attempt/capture and current
      original actor, epoch and pinned model target. It borrows the exact
      bridge-issued Context from this Root's physical Model_selection; its
      immutable adapter limits bound candidates per admitted parent Registration. The
      Root also enforces [Limits.max_admitted_call_bindings] across all its live
      brackets, at most 16384. This callback owns a child Switch beneath the
      Worker switch. Children join before call bindings retire and before this
      borrow expires, including on error/cancellation. No second Claim or
      capacity reservation is made. The strict admitted-call hook runs during
      its owning inference attempt; after that attempt settles, only the
      bounded committed-call binding remains until this scope's child Switch
      joins. Each physical Attempt reserves a separate
      Root active-attempt slot BEFORE its Prepared/effect path; a full bound
      rejects with a typed resource error without waiting under the mutation
      gate. That slot remains owned until exact terminal/interruption storage
      settles, even if the lexical callback unwinds or Root quarantines. An
      escaped port cannot begin new work.
      Original exception/backtrace survives joined cleanup and exact retained
      Store bookkeeping. *)
  val with_claimed
    :  run
    -> active:Agent_session.Developer_job_scope.active
    -> f:(t -> sw:Eio.Switch.t -> 'a)
    -> ('a, Agent_protocol.Error.t) result

  (** A distinct entry to this SAME joined inference scope and Store Tracking
      manager for an already entered saved Agent.run v2 Tool callback. Require
      [selected] from the exact registered runtime source and physical selected
      native borrow. Root resolves its OWN durably Dispatching Native_terminal
      by that proof's exact Host invocation, then checks the same Worker Claim,
      selected implementation and entered native callback. No caller-supplied
      terminal or wire ID can select a different native owner. The immutable
      Async_tool v2 payload is classification, not admission. The scope retains
      both physical proofs and rechecks them with
      original/current actor, Root epoch, Worker Parent and selected provider
      after yields and nonyieldingly before Tracking admission/dispatch. It
      owns the same bounded attempts, committed-call bindings, child Switch,
      submission guards, pending reconciliation and cancellation behavior as
      [with_claimed]. An unrelated/v1 Async_tool or an expired/foreign copied
      artifact cannot enter. [with_claimed] remains strictly Model_call-only.

      An external Model.spawn issued from this Agent context uses the existing
      Native_model parent for that terminal's actual Dispatching invocation and
      entered Async_tool job/attempt/next depth. It never recasts that job as
      a Claimed_model Model_call parent. No native callback or provider effect
      is replayed after uncertain storage. *)
  val with_claimed_agent
    :  run
    -> selected:Agent_session.Captured_agent_runner.Claimed_developer.Selected.t
    -> f:(t -> sw:Eio.Switch.t -> 'a)
    -> ('a, Agent_protocol.Error.t) result

  (** The original bridge-issued selected Context borrowed for this lexical
      scope. It is input data for the shared Inference_client, not a renewed
      actor, credential or effect lease. Use the strict ports and composed
      submission guard for every dispatch. *)
  val context : t -> Inference_runtime.Context.t

  (** Strict source/host-service admission while this exact callback is live.
      Yielding refresh verifies the original actor, entered Worker parent,
      latest Root epoch and selected provider; a final nonyielding scope check
      follows it. Invoke before source fetch and again after a yielding fetch or
      approval wait. A typed denial raises this module's strict tracking
      exception for the Worker to settle as failure; unexpected exceptions
      and cancellation keep their original identity and backtrace. This is
      not an enduring credential or transport lease: every
      Prepared still uses [before_dispatch] and each HTTP/WS application write
      uses the composed [submission_guard]. *)
  val check_current : t -> unit

  (** Admit an external Model_call child at this exact claimed Model_call's
      application call boundary. The child is durably Queued under the current
      parent job/attempt at the next nested depth. This reuses the one external
      model transaction, mutation gate and shared capacity reservation; it
      rechecks the entered Worker Claim, current selected Target/actor/provider,
      callback Switch and parent after each yield and immediately before the
      first rename attempt. A copied job, request or context cannot authorize
      the child. Unknown persistence retains exact Prepared/Handoff and capacity
      beyond lexical unwind for Root recovery; no callback or effect is replayed.
      Bare One_off worker scopes do not gain this operation. *)
  val admit_external_model
    :  t
    -> request:Agent_protocol.Model_spawn_request.t
    -> checkpoint:(unit -> unit)
    -> (Agent_protocol.Id.Job.t, Agent_protocol.Error.t) result

  (** The one shared Inference_client allocation port. Its [with_attempt]
      admits Root or Nested only while this physical scope is live and the
      exact Worker Claim remains current. Nested also requires this scope's
      retained committed-call binding for the full parent Scope/entry/alias;
      copied Scope values and a valid row from another Root never suffice.
      An acknowledged Prepared row is durable before the attempt callback,
      credential lookup or network submission. A parent row may already be
      Terminal while a delayed admitted child begins, provided this Worker,
      actor and selected provider still authorize the new effect. *)
  val identity : t -> Inference_client.Identity.t

  (** Run after yielding request preparation. Check the Prepared target against
      the original selected model and recheck Root/Worker currentness. The full
      prepared configuration and accounting identity are checked at [on_attempt]
      against the acknowledged registration. This is a host pre-dispatch check,
      not a provider credential lease. *)
  val before_dispatch : t -> Inference_runtime.Prepared.t -> unit

  (** Strict producer callback from the real In_memory_stream call commit.
      Compare the token's exact physical Attempt to the current Started
      registration acknowledged under this scope, before the parent attempt
      reaches Terminal. Retain only the bounded validated Scope/History_entry
      ID/alias binding after the attempt settles. The run-wide and per-parent
      candidate caps reject before tool
      execution; no live delayed-call binding is evicted. A foreign real stream
      with copied IDs and a different Attempt object rejects. This token is
      provenance, not permission for a later effect. *)
  val on_admitted_call : t -> Chat_response.In_memory_stream.Admitted_call.t -> unit

  (** Strict callbacks for the shared Inference_client. All preparation and
      settlement uses this Root's existing mutation gate and the Store's sole
      writer/pending barrier. The gate is never held across user code,
      credential/network I/O or child joins. [on_attempt] durably acknowledges
      Started before a provider effect. Observations use complete snapshots;
      terminal/interruption outcomes remain immutable truth. If a Started or
      Observed write is uncertain, retain its exact Tracking.Pending plus a
      bounded immutable deferred outcome per physically issued Registration,
      rather than replaying a callback or
      replacing the primary cancellation/exception. Late observations and
      completion may settle an issued historical registration after new-effect
      eligibility ends. *)
  val on_attempt : t -> Inference_runtime.Attempt.t -> unit

  val on_observation : t -> Inference.Observation.t -> unit
  val on_completion : t -> Inference_client.Completion.t -> unit

  (** Root/entered-Claim component only. The selected provider/actor guard is
      composed by the higher runner. Each refresh may yield; its returned
      attempt-local final check is nonyielding at HTTP/WS submit admission. *)
  val submission_guard : t -> Inference_runtime.Submission_guard.t
end

(** One bounded pass over exact inference Tracking.Pending and immutable
    deferred observation/completion, then the physically retained root/native
    terminal head
    and later immutable intents, any exact inactive-cancel Prepared in this
    owner's finite control-recovery slot, and one exact externally admitted
    Model_call Prepared/Handoff. It reconciles each owned Store write
    before allowing a new mutation; Installed retires only the exact terminal
    job's unclaimed reservation, proven Absent may retry the same pinned
    attempt, and unresolved/diverged state retains the run claim and capacity.
    A known model installation publishes its original shared-capacity
    reservation once; a proven absent write aborts it; a proven prewrite CAS
    conflict may reprepare only the same immutable job within the finite
    retry budget. An attempted unknown write retains its exact Handoff,
    reservation and mutation exclusion. No inference application callback or
    transport reruns. Unknown/divergent state
    keeps the run quarantined and
    blocks new actions/mutations until trusted recovery resolves it. This
    requires the issuing Eio domain and no active root/native/model callback or
    terminal drainer, including
    when there is currently no pending handoff. Only one public reconciliation
    may be in flight for this owner: a second returns a retryable Conflict
    before waiting for Store I/O, so a retained old result cannot clear a newer
    root/native pending transaction. Recovery excludes new effects and root or
    native actions. The same bounded pass also reconciles exited, physically
    retained native Dispatching scopes: a proven absent unentered scope is
    retired, while an installed dispatch uses its attached exact terminal and
    sealed selection without rerunning the handler. Unknown custody stays
    quarantined. Close uses this operation after joining callbacks and workers;
    close during an active recovery returns an error and retains physical
    claim/capacity. *)
val reconcile_pending : t -> (unit, Agent_protocol.Error.t) result

(** Exclude admission, cancel/join root, native and model callbacks and worker
    children BEFORE draining inference Tracking, the pending handoff and bounded
    terminal queue, so newly published queued jobs cannot start through this
    owner. Finish this run's exact Tracking manager only after Pending/deferred
    state settles and every issued inference row is terminal/interrupted; a
    failed finish retains the Run_claim and capacity. Then retire only this
    run's unclaimed capacity, then release
    Store.Run_claim. The daemon-owned Store remains open for unrelated runs. A
    self-close rejects before revocation. If quarantine is unresolved, remain
    closed with the claim and permits retained; a same-run replacement cannot
    silently clear them while another run remains usable. Once released,
    The run service may admit the next epoch/capture on this same Store and construct a
    new owner using the SAME capacity ledger. Durable queued rows can later use
    its unreserved recovery claim path; Running/Dispatching effects are never
    replayed by this owner. *)
val close_and_wait : t -> (unit, Agent_protocol.Error.t) result
