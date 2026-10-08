open! Core

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

let equal a b =
  Jsonaf.exactly_equal
    (Agent_protocol.Session.to_json a.session)
    (Agent_protocol.Session.to_json b.session)
  && Int.equal a.runnable_job_count b.runnable_job_count
  && Int.equal a.deliverable_job_count b.deliverable_job_count
  && Option.equal
       Agent_protocol.Timestamp.equal
       a.earliest_schedule_due
       b.earliest_schedule_due
  && Option.equal
       Agent_protocol.Timestamp.equal
       a.owner_grace_deadline
       b.owner_grace_deadline
  && Bool.equal a.pending_initial_start b.pending_initial_start
  && Bool.equal a.archived b.archived
;;
