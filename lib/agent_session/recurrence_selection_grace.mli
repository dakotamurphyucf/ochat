open! Core

(** Bounded process-local selected-interval grace anchor. No IO, permission,
    surviving monotonic origin or serialization. The actual due owner captures
    exact lateness before commit and installs the anchor only after ACK. *)
type t

val create
  :  occurrence:Recurrence_identity.Occurrence.t
  -> snapshot:Recurrence_phase.Snapshot.t
  -> lateness_ns:int64
  -> (t, Agent_protocol.Error.t) result

val occurrence : t -> Recurrence_identity.Occurrence.t

(** Checked nonnegative elapsed advance. A backward snapshot or overflow rejects
    admission; callers must never substitute zero or wall time for a failed live
    anchor. Calendar and process-recovery wall reconciliation are separate. *)
val lateness_ns
  :  t
  -> snapshot:Recurrence_phase.Snapshot.t
  -> (int64, Agent_protocol.Error.t) result
