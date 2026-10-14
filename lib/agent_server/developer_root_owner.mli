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
      worker/quarantine scopes. One root terminal intent occupies a separate
      fixed slot. The Store's one-pending-per-run barrier, document/selection
      limits and shared Job_capacity limits remain binding. *)
  val create
    :  max_active_runs:int
    -> max_staged_jobs:int
    -> max_native_occupancy:int
    -> max_active_workers:int
    -> max_quarantined_workers:int
    -> t Or_error.t
end

module Registry : sig
  type t

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
      this trusted host API; its pure capture value alone grants neither. *)
  val admit
    :  t
    -> actor:Operator_authorization.t
    -> capture:Chatml_developer_capture.t
    -> registry:Chat_response.Tool_capability.t
    -> selected:Chat_response.Tool_capability.t
    -> selected_model:Provider_operator_port.Model_selection.t option
    -> job_policy:Chat_response.One_off_request.policy
    -> handle:Agent_store.Developer_run_store.handle
    -> workspace_conflict_domain:string
    -> (run, Agent_protocol.Error.t) result

  (** The startup form of [admit]. After acquiring the same physical Run_claim
      and authenticating the original actor/capture, open the one-shot Store
      recovery window and call [recover] BEFORE creating or exposing a worker.
      The callback selects only terminal bookkeeping through that window and
      must finish it; returned [Ok ()] without a finished window rejects.
      Caller exceptions/cancellation keep their identity. A clean preselection
      failure can abort the empty window and release admission. An unresolved
      Pending retains the provisional slot and Run_claim, excluding any new
      same-run owner or effect until [resume_recovery] settles it. *)
  val admit_with_recovery
    :  t
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
      Missing, foreign, closed, concurrent or already-owned slots reject. *)
  val resume_recovery
    :  t
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
      Neither method grants native effect authority. *)
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

(** One bounded pass over the physically retained root/native terminal head
    and later immutable intents, plus any exact inactive-cancel Prepared in
    this owner's finite control-recovery slot. It reconciles that Store write
    before allowing a new mutation; Installed retires only the exact terminal
    job's unclaimed reservation, proven Absent may retry the same pinned
    attempt, and unresolved/diverged state retains the run claim and capacity.
    No callback reruns. Unknown/divergent state
    keeps the run quarantined and
    blocks new actions/mutations until trusted recovery resolves it. This
    requires the issuing Eio domain and no active root/native callback or
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

(** Exclude admission, cancel/join root and native callbacks and worker
    children BEFORE draining the pending handoff and bounded terminal queue, so
    newly published queued jobs cannot start through this owner. Then retire
    only this run's unclaimed capacity, then release
    Store.Run_claim. The daemon-owned Store remains open for unrelated runs. A
    self-close rejects before revocation. If quarantine is unresolved, remain
    closed with the claim and permits retained; a same-run replacement cannot
    silently clear them while another run remains usable. Once released,
    The run service may admit the next epoch/capture on this same Store and construct a
    new owner using the SAME capacity ledger. Durable queued rows can later use
    its unreserved recovery claim path; Running/Dispatching effects are never
    replayed by this owner. *)
val close_and_wait : t -> (unit, Agent_protocol.Error.t) result
