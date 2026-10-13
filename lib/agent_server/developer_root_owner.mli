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
      The host validates capture origin and workspace membership before calling
      this trusted host API; its pure capture value alone grants neither. *)
  val admit
    :  t
    -> actor:Operator_authorization.t
    -> capture:Chatml_developer_capture.t
    -> registry:Chat_response.Tool_capability.t
    -> selected:Chat_response.Tool_capability.t
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

module Selection : sig
  type t

  (** Exact ordered jobs sealed from one root action. This projection remains
      data; only the issuing owner may commit it under its live action scope. *)
  val jobs : t -> Agent_protocol.Developer_job.t list
end

module Action : sig
  type t

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

  module Selection : sig
    type t

    (** Exact immutable jobs sealed by one physically registered native
        terminal scope. IDs alone are data, not publication authority. *)
    val jobs : t -> Agent_protocol.Developer_job.t list
  end

  (** This scope is terminal custody, not native-effect permission. The later
      shared invocation policy must durably Dispatch and independently prove
      its selected binding, permission and live parent before a handler runs.
      This actor owns the original principal/capture and shared capacity.
      Until that policy adapter is wired, a staged job may name only this exact
      invocation as launch parent and no claimed parent job. *)
  val stage_background
    :  t
    -> request:Chat_response.Background_request.t
    -> checkpoint:(unit -> unit)
    -> (Agent_protocol.Id.Job.t, Agent_protocol.Error.t) result

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
    and later immutable intents. No callback reruns. Unknown/divergent state
    keeps the run quarantined and
    blocks new actions/mutations until trusted recovery resolves it. This
    requires the issuing Eio domain and no active root/native callback or
    terminal drainer, including
    when there is currently no pending handoff. Only one public reconciliation
    may be in flight for this owner: a second returns a retryable Conflict
    before waiting for Store I/O, so a retained old result cannot clear a newer
    root/native pending transaction. Recovery excludes new effects and root or
    native actions. Close during recovery returns an error and retains the physical
    claim/capacity while it still cancels and joins workers; the caller may
    retry close after recovery finishes. *)
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
