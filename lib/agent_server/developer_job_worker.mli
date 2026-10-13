(** Existing-capacity adapter for one developer run's physical workers. This
    module does not poll queues, dispatch native tools or disclose results. *)
open! Core

type t

(** Bind the actual daemon-wide capacity ledger and store to one stable
    host/run/principal. Trusted host wiring selects [project_source] and the
    workspace conflict domain from its admitted capture/workspace; neither
    coordinate is accepted from a scheduler request. Each job's capacity key
    is derived internally from those bound values and the exact observed
    owner/kind/depth. [now] must be a non-yielding trusted clock sample.
    [max_quarantined] is at least positive [max_active], so every admitted
    worker has a cleanup slot. *)
val create
  :  sw:Eio.Switch.t
  -> store:Agent_store.Developer_run_store.t
  -> initial:Agent_store.Developer_run_store.handle
  -> capacity:Job_capacity.t
  -> project_source:Chatml_source.Id.t
  -> workspace_conflict_domain:string
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> max_active:int
  -> max_quarantined:int
  -> (t, Agent_protocol.Error.t) result

(** The physical issuer whose active child-proof is accepted by native
    admission. Current principal/capture/selection and effect policy are
    separately checked by the host at each effect boundary. *)
val issuer : t -> Agent_session.Developer_job_scope.t

(** Project exact [lookup] through Host_job.of_developer, then acquire from
    Job_capacity.try_acquire_host_job in the SAME shared capacity ledger as
    conversation jobs. Recheck the exact job/attempt after lease acquisition;
    durably change Queued to Running before entering [f]. Saturated/not-due
    returns [Ok None] with no callback. Unknown claim persistence never enters
    [f]; retain/quarantine the lease pending bounded exact reconciliation.
    Claim entry is one-shot even if commit/reconcile returns the same token.

    [f] runs inside a registered Eio cancellation scope with its own child
    Switch. It returns the proposed end-of-worker state: terminal,
    Waiting_completion or Queued retry. Waiting_permission is an interim state
    in the same live attempt and cannot be returned as an ended worker; its
    resumption does not increment attempt or acquire another lease. The switch
    joins before this module persists the exact state from a fresh Observation
    and releases capacity. A later due Queued retry alone increments attempt.
    An older epoch can record only terminal bookkeeping. Error becomes Failed;
    unexpected exception and actual Eio cancellation attempt protected
    Failed/Cancelled bookkeeping, then preserve original identity.

    On a fresh CAS conflict, the owner rereads and retries a finite number of
    times only while the same physical claim/attempt/immutable launch remains.
    It never reruns [f]. Uncertain persistence uses exact Prepared
    reconciliation, not a blind retry; unresolved state quarantines the lease.
    A returned artifact descriptor must already have separate result-custody
    proof at the higher actor boundary. *)
val with_claimed_attempt
  :  t
  -> expected:Agent_store.Developer_run_store.Observation.t
  -> lookup:Agent_store.Developer_run_store.Job_lookup.t
  -> (active:Agent_session.Developer_job_scope.active
      -> sw:Eio.Switch.t
      -> (Agent_protocol.Developer_job.State.t, Agent_protocol.Error.t) result)
  -> (Agent_protocol.Developer_job.t option, Agent_protocol.Error.t) result

(** Actual next Store handle for this stable run identity. Revoke old active
    effects and retire only unclaimed old-epoch reservations; old claimed
    leases stay charged until physical cleanup. Foreign/future/equal epochs
    reject without changing registry or capacity. *)
val advance_epoch
  :  t
  -> new_current:Agent_store.Developer_run_store.handle
  -> (unit, Agent_protocol.Error.t) result

(** Finite active/quarantine registries reject saturation before lease/entry;
    an existing worker's cleanup is never dropped. One bounded reconciliation
    pass cannot replay a callback. Unknown I/O remains quarantined and blocks
    admission for this run until trusted recovery or daemon shutdown. *)
val reconcile_quarantined : t -> (unit, Agent_protocol.Error.t) result

(** Exclude entries, cancel/join actual callbacks and children, attempt bounded
    reconciliation, then permit Store.close_and_wait. Self-close rejects before
    revocation. If an unknown quarantine remains, keep the owner closed and
    lease retained; caller owns retry/shutdown ordering. Never clear unrelated
    conversation/developer capacity from the shared ledger. *)
val close_and_wait : t -> (unit, Agent_protocol.Error.t) result
