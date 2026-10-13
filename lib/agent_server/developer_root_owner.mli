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
      and worker/quarantine scopes. The Store's one-pending-per-run barrier,
      document/selection limits and shared Job_capacity limits remain binding. *)
  val create
    :  max_active_runs:int
    -> max_staged_jobs:int
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
    lifetime. No Eio operation, Store read or host callback occurs here. The
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
    Store handoff: Installed publishes selected and aborts unselected; Absent
    aborts all; Diverged/unknown retains finite quarantined ownership. A
    settlement callback may repeat idempotently, but the action/effects do not.
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

(** One bounded exact reconciliation of physically retained handoffs. No root
    callback reruns. Unknown/divergent state keeps the run quarantined and
    blocks new actions/mutations until trusted recovery resolves it. This
    requires the issuing Eio domain and no active root callback, including
    when there is currently no pending handoff. Only one public reconciliation
    may be in flight for this owner: a second returns a retryable Conflict
    before waiting for Store I/O, so a retained old result cannot clear a newer
    root action's pending transaction. Recovery excludes new effects and root
    actions. Close during recovery returns an error and retains the physical
    claim/capacity while it still cancels and joins workers; the caller may
    retry close after recovery finishes. *)
val reconcile_pending : t -> (unit, Agent_protocol.Error.t) result

(** Exclude admission, cancel/join root actions and worker children, reconcile
    pending handoffs, retire only this run's unclaimed capacity, then release
    Store.Run_claim. The daemon-owned Store remains open for unrelated runs. A
    self-close rejects before revocation. If quarantine is unresolved, remain
    closed with the claim and permits retained; a same-run replacement cannot
    silently clear them while another run remains usable. Once released,
    The run service may admit the next epoch/capture on this same Store and construct a
    new owner using the SAME capacity ledger. Durable queued rows can later use
    its unreserved recovery claim path; Running/Dispatching effects are never
    replayed by this owner. *)
val close_and_wait : t -> (unit, Agent_protocol.Error.t) result
