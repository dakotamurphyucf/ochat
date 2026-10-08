(** Rebuildable host catalog/scheduling projection for one session. *)
type t =
  { session : Agent_protocol.Session.t
  ; runnable_job_count : int
  ; deliverable_job_count : int
  ; earliest_schedule_due : Agent_protocol.Timestamp.t option
  ; owner_grace_deadline : Agent_protocol.Timestamp.t option
  ; pending_initial_start : bool [@sexp.default false]
  ; archived : bool
  }
[@@deriving sexp]

(** Typed equality of the complete scheduling projection, including summary. *)
val equal : t -> t -> bool
