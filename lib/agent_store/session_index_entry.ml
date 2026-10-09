open! Core

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
  && Session_archive_record.Revision.equal a.lifecycle_revision b.lifecycle_revision
  && Session_archive_record.Admission.equal a.admission b.admission
;;

let validate t =
  let module R = Session_archive_record in
  if
    t.runnable_job_count < 0
    || t.deliverable_job_count < 0
    || (t.archived
        && (R.Revision.equal t.lifecycle_revision R.Revision.zero
            || not (R.Admission.equal t.admission Explicit_resume_required)))
    || (R.Revision.equal t.lifecycle_revision R.Revision.zero
        && not (R.Admission.equal t.admission Automatic))
  then
    Error (Store_error.Corrupt "session index lifecycle/hint projection is inconsistent")
  else Ok ()
;;

let with_lifecycle t lifecycle =
  let module R = Session_archive_record in
  if
    (not (Agent_protocol.Id.Session.equal t.session.id (R.session_id lifecycle)))
    || R.Status.equal (R.status lifecycle) Removed
  then Error (Store_error.Corrupt "removed or mismatched lifecycle cannot be indexed")
  else (
    let entry =
      { t with
        archived = R.Status.equal (R.status lifecycle) Archived
      ; lifecycle_revision = R.revision lifecycle
      ; admission = R.admission lifecycle
      }
    in
    Result.map (validate entry) ~f:(fun () -> entry))
;;
