open! Core
module P = Agent_protocol
module L = P.Session_lifecycle

type t =
  { archived : bool
  ; restored_requires_resume : bool
  ; restored_attach_rejected : bool
  ; resumed_without_start : bool
  ; original_restore_replayed : bool
  ; original_resume_replayed : bool
  ; old_archive_did_not_regate : bool
  ; stale_resume_conflicts : bool
  }
[@@deriving equal, sexp_of]

let checked result =
  Result.map_error result ~f:(fun error -> Error.create_s [%sexp (error : P.Error.t)])
  |> Or_error.ok_exn
;;

let run ~request ~created ~key_prefix =
  let key suffix = P.Idempotency_key.of_string (key_prefix ^ ":" ^ suffix) |> checked in
  let attachment = Option.value_exn created.P.Public.Result.Create.attachment in
  let inspect () =
    match
      request (P.Command.Session_get { session_id = created.session.id; history = None })
      |> checked
    with
    | P.Public.Result.Session_get snapshot -> P.Public.Snapshot.fields snapshot
    | _ -> failwith "retained lifecycle get returned the wrong result"
  in
  let non_history command =
    match request command |> checked with
    | P.Public.Result.Non_history result -> P.Public.Result.Non_history.value result
    | _ -> failwith "retained lifecycle mutation returned a history result"
  in
  let observed snapshot = Option.value_exn snapshot.P.Public.Snapshot.Fields.lifecycle in
  let archive_command =
    P.Command.Session_delete
      { session_id = created.session.id
      ; attachment_id = attachment.attachment.id
      ; expected_revision = (inspect ()).revision
      ; policy = Archive
      ; confirmation = P.Id.Session.to_string created.session.id
      ; idempotency_key = key "archive"
      }
  in
  ignore (non_history archive_command : P.Method_result.t);
  let archived_snapshot = inspect () in
  let archived =
    L.Result.Status.equal (L.Observation.status (observed archived_snapshot)) Archived
  in
  let restore =
    P.Command.Session_restore
      (L.Request.create
         ~expected:(L.Observation.expected (observed archived_snapshot))
         ~idempotency_key:(key "restore"))
  in
  let restored_result =
    match non_history restore with
    | P.Method_result.Session_restore result -> result
    | _ -> failwith "restore returned the wrong result"
  in
  let restored = inspect () in
  let restored_requires_resume =
    L.Result.Status.equal (L.Observation.status (observed restored)) Active
    && L.Result.Admission.equal
         (L.Observation.admission (observed restored))
         Explicit_resume_required
  in
  let restored_attach_rejected =
    match
      request
        (P.Command.Session_attach
           { session_id = created.session.id
           ; requested_mode = Read_write
           ; subscribe = false
           ; after_sequence = None
           ; reclaim_token = None
           ; idempotency_key = key "gated-attach"
           })
    with
    | Error error -> P.Error.equal_code error.code Invalid_state
    | Ok _ -> false
  in
  let resume_request =
    L.Request.create
      ~expected:(L.Observation.expected (observed restored))
      ~idempotency_key:(key "resume")
  in
  let resume = P.Command.Session_resume resume_request in
  let resumed_result =
    match non_history resume with
    | P.Method_result.Session_resume result -> result
    | _ -> failwith "resume returned the wrong result"
  in
  let resumed = inspect () in
  let resumed_without_start =
    L.Result.Admission.equal (L.Observation.admission (observed resumed)) Automatic
    && P.Session.equal_desired_state resumed.session.desired_state Stopped
  in
  let original_restore_replayed =
    match non_history restore with
    | P.Method_result.Session_restore replay ->
      Jsonaf.exactly_equal (L.Result.to_json restored_result) (L.Result.to_json replay)
    | _ -> false
  in
  let original_resume_replayed =
    match non_history resume with
    | P.Method_result.Session_resume replay ->
      Jsonaf.exactly_equal (L.Result.to_json resumed_result) (L.Result.to_json replay)
    | _ -> false
  in
  ignore (non_history archive_command : P.Method_result.t);
  let old_archive_did_not_regate =
    L.Result.Admission.equal (L.Observation.admission (observed (inspect ()))) Automatic
  in
  let stale_resume_conflicts =
    match
      request
        (P.Command.Session_resume
           (L.Request.create
              ~expected:(L.Request.expected resume_request)
              ~idempotency_key:(key "stale-resume")))
    with
    | Error error -> P.Error.equal_code error.code Conflict
    | Ok _ -> false
  in
  { archived
  ; restored_requires_resume
  ; restored_attach_rejected
  ; resumed_without_start
  ; original_restore_replayed
  ; original_resume_replayed
  ; old_archive_did_not_regate
  ; stale_resume_conflicts
  }
;;

let require_complete t =
  if
    not
      (t.archived
       && t.restored_requires_resume
       && t.restored_attach_rejected
       && t.resumed_without_start
       && t.original_restore_replayed
       && t.original_resume_replayed
       && t.old_archive_did_not_regate
       && t.stale_resume_conflicts)
  then raise_s [%sexp "retained lifecycle invariant failed", (t : t)]
;;
