(** Rebuildable host catalog/scheduling projection for one session. *)
type t =
  { session : Agent_protocol.Session.t
  ; runnable_job_count : int
  ; deliverable_job_count : int
  ; earliest_schedule_due : Agent_protocol.Timestamp.t option
  ; owner_grace_deadline : Agent_protocol.Timestamp.t option
  ; pending_initial_start : bool
  ; archived : bool
  ; lifecycle_revision : Session_archive_record.Revision.t
  ; admission : Session_archive_record.Admission.t
  }
[@@deriving sexp_of]

(** Typed equality of the complete scheduling projection, including summary. *)
val equal : t -> t -> bool

(** Reject incoherent archive/admission/revision and negative scheduling hints. *)
val validate : t -> (unit, Store_error.t) result

(** Project this session's admitted lifecycle head; preserves exact summary and
    all pending-work hints. Removed is excluded, never encoded as active. *)
val with_lifecycle : t -> Session_archive_record.t -> (t, Store_error.t) result
