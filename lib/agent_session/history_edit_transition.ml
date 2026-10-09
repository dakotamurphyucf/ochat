open! Core
module P = Agent_protocol

type continuation =
  | Save_only
  | Unavailable of P.History_edit.Continuation.unavailable
  | Start of P.Operation.t

type t =
  { delta : Session_delta.t
  ; payloads : P.Event.Durable.Payload.t list
  ; admission : Turn_admission.t option
  ; continuation : P.History_edit.Continuation.t
  ; edited_entry : P.History.entry
  ; archived_revision : int64
  }

let create state ~plan ~edit ~archive ~continuation =
  let open Result.Let_syntax in
  let%bind () = History_edit.validate_basis plan state in
  let requested =
    P.History_edit.Mode.equal (P.History_edit.mode edit) Edit_and_continue
  in
  let%bind () =
    match requested, continuation with
    | false, Save_only | true, (Unavailable _ | Start _) -> Ok ()
    | _ ->
      Error (P.Error.invalid_request "history continuation does not match edit intent")
  in
  let%bind () =
    if
      requested
      && not (List.is_empty state.Session_state.conversation.deferred_user_entries)
    then
      Error
        (P.Error.create
           Pending_input_conflict
           ~message:"edit-and-continue requires an empty pending-input queue"
           ~retryable:false
           ())
    else Ok ()
  in
  let edit_delta = Session_delta.History_edited (edit, archive) in
  let%bind candidate = Session_delta.apply state edit_delta in
  let%bind () =
    if
      List.equal
        P.History.equal_entry
        candidate.conversation.canonical_history
        (History_edit.canonical_history plan)
    then Ok ()
    else Error (P.Error.invalid_request "history edit intent differs from validated plan")
  in
  let%map recovery_deltas, admission, continuation =
    match continuation with
    | Save_only -> Ok ([], None, P.History_edit.Continuation.Not_requested)
    | Unavailable reason -> Ok ([], None, P.History_edit.Continuation.Not_started reason)
    | Start operation ->
      let%bind recovery = History_continuation.prepare candidate ~retiring_history:true in
      let%map admission =
        Turn_admission.create
          candidate
          ~operation
          ~notification_wakes:[]
          ~adopt_deferred:false
      in
      ( History_continuation.deltas recovery
      , Some admission
      , P.History_edit.Continuation.Started operation.id )
  in
  let delta =
    Session_delta.Batch
      ([ edit_delta ]
       @ recovery_deltas
       @ Option.value_map admission ~default:[] ~f:Turn_admission.deltas)
  in
  let payloads =
    [ P.Event.Durable.Payload.History_replaced
        (Session_state.history_window (History_edit.canonical_history plan))
    ]
    @ Option.value_map admission ~default:[] ~f:Turn_admission.payloads
  in
  { delta
  ; payloads
  ; admission
  ; continuation
  ; edited_entry = History_edit.edited_entry plan
  ; archived_revision = archive.revision
  }
;;

let delta t = t.delta
let payloads t = t.payloads
let admission t = t.admission
let continuation t = t.continuation
let edited_entry t = t.edited_entry
let archived_revision t = t.archived_revision
