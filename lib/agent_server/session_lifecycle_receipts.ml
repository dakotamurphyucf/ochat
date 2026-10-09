open! Core
module P = Agent_protocol
module R = Agent_store.Session_archive_record
module I = Agent_store.Idempotency_store

type t =
  { store : I.t
  ; server_id : P.Id.Server.t
  }

let create ~store ~server_id = { store; server_id }
let corrupt message = Error (Agent_store.Store_error.Corrupt message)

let result t ~key (outcome : R.Outcome.t) =
  let open Result.Let_syntax in
  if
    (not (Option.equal P.Id.Session.equal key.I.Key.session_id (Some outcome.session_id)))
    || not (R.Outcome.accepts_method outcome.action ~method_name:key.method_name)
  then corrupt "lifecycle result differs from original receipt scope"
  else if String.equal key.method_name "session.delete"
  then
    Ok
      (P.Method_result.Session_delete
         { session_id = outcome.session_id
         ; deleted_at = outcome.completed_at
         ; archive = None
         })
  else (
    let action =
      match outcome.action with
      | R.Outcome.Archive -> P.Session_lifecycle.Result.Action.Archive
      | Restore -> Restore
      | Resume -> Resume
      | Remove -> Remove
    in
    let disposition =
      match outcome.disposition with
      | R.Outcome.Applied -> P.Session_lifecycle.Result.Disposition.Applied
      | Already_current -> Already_current
    in
    let%bind value =
      P.Session_lifecycle.Result.create
        ~reference:
          (P.Session_ref.create ~server_id:t.server_id ~session_id:outcome.session_id)
        ~generation:outcome.anchor.generation
        ~session_revision:outcome.anchor.session_revision
        ~latest_event_sequence:outcome.anchor.latest_event_sequence
        ~lifecycle_revision:outcome.lifecycle_revision
        ~status:outcome.status
        ~admission:outcome.admission
        ~action
        ~disposition
        ~completed_at:outcome.completed_at
      |> Result.map_error ~f:(fun failure ->
        Agent_store.Store_error.Corrupt failure.P.Error.message)
    in
    match outcome.action with
    | Restore -> Ok (P.Method_result.Session_restore value)
    | Resume -> Ok (P.Method_result.Session_resume value)
    | Archive | Remove -> corrupt "unsupported lifecycle result method")
;;

let complete t ~key ~request_digest outcome =
  let open Result.Let_syntax in
  let%bind value = result t ~key outcome in
  let expected = P.Method_result.to_json value in
  let%bind original =
    match I.lookup t.store ~key ~request_digest with
    | I.Missing -> corrupt "original protected lifecycle command receipt is missing"
    | Conflict _ -> corrupt "original lifecycle command receipt digest differs"
    | Replay original -> Ok original
  in
  if not (I.equal_retention original.retention Protected)
  then corrupt "lifecycle command receipt has no protected retention"
  else (
    let%bind completed =
      I.complete
        t.store
        ~key
        ~request_digest
        ~accepted_transaction_sequence:original.accepted_transaction_sequence
        ~outcome:(I.Success expected)
    in
    match completed.outcome with
    | I.Success actual when Jsonaf.exactly_equal actual expected -> Ok ()
    | Success _ -> corrupt "generic lifecycle completion differs from retained proof"
    | Pending | Failure _ -> corrupt "generic lifecycle completion is not successful")
;;

let complete_receipt t (receipt : R.Receipt.t) =
  complete t ~key:receipt.key ~request_digest:receipt.request_digest receipt.outcome
;;

let reject t ~key ~request_digest error =
  let open Result.Let_syntax in
  let%bind original =
    match I.lookup t.store ~key ~request_digest with
    | I.Missing -> corrupt "original protected lifecycle rejection receipt is missing"
    | Conflict _ -> corrupt "original lifecycle rejection receipt digest differs"
    | Replay original -> Ok original
  in
  if not (I.equal_retention original.retention Protected)
  then corrupt "lifecycle rejection receipt has no protected retention"
  else (
    let%bind completed =
      I.complete
        t.store
        ~key
        ~request_digest
        ~accepted_transaction_sequence:original.accepted_transaction_sequence
        ~outcome:(I.Failure error)
    in
    match completed.outcome with
    | I.Failure actual
      when Jsonaf.exactly_equal (P.Error.to_json actual) (P.Error.to_json error) -> Ok ()
    | Failure _ -> corrupt "generic lifecycle rejection differs from primary error"
    | Pending | Success _ -> corrupt "generic lifecycle rejection is not a failure")
;;
