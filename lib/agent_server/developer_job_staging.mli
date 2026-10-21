(** Private reservation/selection bookkeeping shared by Root/Native Action
    and claimed Worker children. This is the existing Job_capacity ledger; it
    owns no actor, physical Claim, source grant, Store writer, callback or
    scheduler. Physical wrappers must check their own scope and current
    selected policy before calling it. *)
open! Core

type t

module Selection : sig
  type t

  (** A one-shot seal tied to the exact staging ledger that issued it. It is
      comparison evidence for the physical Root/Worker owner, not a Store
      receipt or permission to publish children. *)
  val jobs : t -> Agent_protocol.Developer_job.t list

  val ids : t -> Agent_protocol.Id.Job.t list
end

(** Reject nonpositive bounds before creating a staging ledger. *)
val create : max_staged_jobs:int -> (t, Agent_protocol.Error.t) result

val count : t -> int
val is_empty : t -> bool
val contains : t -> Agent_protocol.Id.Job.t -> bool

type status =
  | Provisional
  | Aborted
  | Published

(** Inspect only this ledger's capacity phase for exact cleanup reporting. *)
val status : t -> Agent_protocol.Id.Job.t -> status option

(** The caller has already admitted [prepared] under its real Root policy,
    selected registry and launch parent. Check this staging ledger's bound and
    duplicate ID, then reserve the exact prepared Host_job under [key] in the
    shared physical [capacity]. Saturation rejects without recording an ID.
    This is provisional only; no Store publication occurs. *)
val reserve
  :  t
  -> capacity:Job_capacity.t
  -> key:Job_capacity.Key.t
  -> prepared:Agent_session.Developer_root_job_admission.Prepared.t
  -> (Agent_protocol.Id.Job.t, Agent_protocol.Error.t) result

(** [rollback] is for a still-live physical wrapper and one exact unsealed
    provisional ID. Repetition on an already aborted ID is harmless. It
    rejects once a Store handoff may have begun. *)
val rollback : t -> Agent_protocol.Id.Job.t -> (unit, Agent_protocol.Error.t) result

(** Conclusive pre-handoff cleanup. It aborts all unpublished reservations,
    including a sealed subset, only while no Store write can have begun. Once
    [mark_handoff] succeeds this operation rejects, even if the caller later
    sees an exception, cancellation or unknown save result. Repeated cleanup
    before handoff is harmless. *)
val abort_pre_handoff : t -> (unit, Agent_protocol.Error.t) result

(** Freeze exactly one distinct subset of this ledger's provisional IDs.
    Duplicate/foreign/aborted IDs and repeated seals reject. Unselected
    provisional reservations remain charged until conclusive settlement. *)
val seal
  :  t
  -> selected:Agent_protocol.Id.Job.t list
  -> (Selection.t, Agent_protocol.Error.t) result

(** Move the exact sealed ledger to a retained handoff phase before the first
    potentially publishing result or Store operation, including a yielding
    result-publisher call. A foreign or superseded selection rejects.
    Subsequent cancellation retains capacity until exact Store reconciliation;
    [abort_pre_handoff] cannot release it. This phase does not claim that a
    filesystem write is atomic with the transition. *)
val mark_handoff : t -> Selection.t -> (unit, Agent_protocol.Error.t) result

type settlement =
  | Selected_installed
  | Selected_omitted
  | Absent

(** Called only after handoff and conclusive exact Store evidence. Installed
    publishes precisely selected reservations and aborts the rest;
    omitted/absent abort all proven unpublished reservations. [Absent] is only
    for final conclusive abandonment: a prewrite conflict that will retry the
    same selection must retain handoff. Do not call
    [settle] on an attempted or uncertain result; retained reservations then
    remain charged for the physical owner to reconcile. The exact issuing
    ledger and selection must match. Repeating the same settlement is harmless;
    a foreign selection or contradictory repeat rejects. The helper cannot
    turn job status or an unproved acknowledgement into Store evidence. *)
val settle : t -> Selection.t -> settlement -> (unit, Agent_protocol.Error.t) result
