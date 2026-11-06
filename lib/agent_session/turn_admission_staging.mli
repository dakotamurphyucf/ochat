open! Core

(** Pure actual Turn candidate ownership/phase mechanics, not an executor.
    The module never invokes its opaque plan payload. Actual Actor callers use
    closed suffix DATA, not callbacks or a generic request executor. They consume
    these phases directly within their SAME mailbox handler. *)
type 'plan t

module Acknowledgement : sig
  type 'plan t
end

module Closed_before_writer : sig
  type t
end

(** Capture only after the actual domain planner and original authorization.
    Validates one actual next-revision Turn candidate and exact operation.
    [commit_attempted] is the SAME existing writer-attempt ref when present. *)
val capture
  :  owner:Preparation_tracking.Owner.t
  -> before:Session_state.t
  -> candidate:Session_transition.t
  -> operation:Agent_protocol.Operation.t
  -> remaining_audit:Document_schema.Document.t option
  -> commit_attempted:bool ref
  -> plan:'plan
  -> ('plan t, Agent_protocol.Error.t) result

val candidate : 'plan t -> Session_transition.t
val remaining_audit : 'plan t -> Document_schema.Document.t option
val commit_attempted : 'plan t -> bool ref

(** Callback-free exact immutable before projection and issuing Actor owner.
    Does not refresh policy or add clock sampling. *)
val check_before_writer
  :  'plan t
  -> owner:Preparation_tracking.Owner.t
  -> state:Session_state.t
  -> (unit, Agent_protocol.Error.t) result

(** AFTER the actual persisted candidate was canonically installed. Capture
    acknowledgement before cancellation; no synthetic ACK from a missing row.
    Repeated projections share the SAME consumed phase; finishing remains once. *)
val acknowledge
  :  'plan t
  -> owner:Preparation_tracking.Owner.t
  -> state:Session_state.t
  -> ('plan Acknowledgement.t, Agent_protocol.Error.t) result

(** Consume the SAME installed ACK once and return its concrete suffix DATA.
    No new post-ACK policy/clock callback; the caller performs original suffix. *)
val finish
  :  'plan Acknowledgement.t
  -> owner:Preparation_tracking.Owner.t
  -> state:Session_state.t
  -> ('plan, Agent_protocol.Error.t) result

(** Same live actual unattempted preparation. Negative data only, no decoder,
    refund, retry permission, new key or inferred post-crash absence. *)
val seal_before_writer
  :  'plan t
  -> owner:Preparation_tracking.Owner.t
  -> (Closed_before_writer.t, Agent_protocol.Error.t) result
