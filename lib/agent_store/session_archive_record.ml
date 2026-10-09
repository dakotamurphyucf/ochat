open! Core
module P = Agent_protocol

let invalid message = Error (P.Error.invalid_request message)
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

module Revision = P.Session_lifecycle.Revision
module Admission = P.Session_lifecycle.Result.Admission
module Status = P.Session_lifecycle.Result.Status

let valid_state status admission =
  match status, admission with
  | Status.Active, (Admission.Automatic | Explicit_resume_required)
  | (Archived | Removed), Explicit_resume_required -> true
  | (Archived | Removed), Automatic -> false
;;

module Anchor = struct
  type t =
    { generation : int
    ; session_revision : int64
    ; latest_event_sequence : int64
    }
  [@@deriving equal, sexp_of]

  let create ~generation ~session_revision ~latest_event_sequence =
    if generation < 0 || Int64.(session_revision < zero || latest_event_sequence < zero)
    then invalid "negative lifecycle canonical anchor"
    else Ok { generation; session_revision; latest_event_sequence }
  ;;
end

module Outcome = struct
  type action =
    | Archive
    | Restore
    | Resume
    | Remove
  [@@deriving equal, sexp]

  type disposition =
    | Applied
    | Already_current
  [@@deriving equal, sexp]

  type t =
    { session_id : P.Id.Session.t
    ; anchor : Anchor.t
    ; lifecycle_revision : Revision.t
    ; status : Status.t
    ; admission : Admission.t
    ; action : action
    ; disposition : disposition
    ; completed_at : P.Timestamp.t
    }

  let method_name = function
    | Archive -> "session.archive"
    | Restore -> "session.restore"
    | Resume -> "session.resume"
    | Remove -> "session.remove"
  ;;

  let accepts_method action ~method_name:original =
    String.equal original (method_name action)
    || (String.equal original "session.delete"
        &&
        match action with
        | Archive | Remove -> true
        | Restore | Resume -> false)
  ;;

  let create
        ~session_id
        ~anchor
        ~lifecycle_revision
        ~status
        ~admission
        ~action
        ~disposition
        ~completed_at
    =
    let coherent =
      match action, status, admission, disposition with
      | Archive, Status.Archived, Admission.Explicit_resume_required, _
      | Restore, Active, Explicit_resume_required, _
      | Restore, Active, Automatic, Already_current
      | Resume, Active, Automatic, _
      | Remove, Removed, Explicit_resume_required, _ -> true
      | _ -> false
    in
    if not (valid_state status admission && coherent)
    then invalid "lifecycle outcome status/admission differs from action"
    else if
      Revision.equal lifecycle_revision Revision.zero
      && equal_disposition disposition Applied
    then invalid "applied lifecycle outcome has initial revision"
    else
      Ok
        { session_id
        ; anchor
        ; lifecycle_revision
        ; status
        ; admission
        ; action
        ; disposition
        ; completed_at
        }
  ;;

  let equal left right =
    P.Id.Session.equal left.session_id right.session_id
    && Anchor.equal left.anchor right.anchor
    && Revision.equal left.lifecycle_revision right.lifecycle_revision
    && Status.equal left.status right.status
    && Admission.equal left.admission right.admission
    && equal_action left.action right.action
    && equal_disposition left.disposition right.disposition
    && P.Timestamp.equal left.completed_at right.completed_at
  ;;
end

let validate_request ~session_id ~action (key : Idempotency_store.Key.t) request_digest =
  if
    (not (Option.equal P.Id.Session.equal key.session_id (Some session_id)))
    || (not (Outcome.accepts_method action ~method_name:key.method_name))
    || String.length request_digest <> 64
    || not
         (String.for_all request_digest ~f:(function
            | '0' .. '9' | 'a' .. 'f' -> true
            | _ -> false))
  then invalid "lifecycle receipt key or request digest differs"
  else Ok ()
;;

let max_receipts = 32
let receipt_retention_ms = 86_400_000

module Receipt = struct
  type t =
    { key : Idempotency_store.Key.t
    ; request_digest : string
    ; outcome : Outcome.t
    ; created_at : P.Timestamp.t
    ; expires_at : P.Timestamp.t
    ; completion_acknowledged : bool
    }

  let create
        ~key
        ~request_digest
        ~outcome
        ~created_at
        ~expires_at
        ~completion_acknowledged
    =
    let open Result.Let_syntax in
    let%bind () =
      validate_request
        ~session_id:outcome.Outcome.session_id
        ~action:outcome.action
        key
        request_digest
    in
    let%bind expected_expiry = P.Timestamp.add_ms created_at receipt_retention_ms in
    if
      (not (P.Timestamp.equal expires_at expected_expiry))
      || P.Timestamp.compare created_at outcome.completed_at > 0
      || P.Timestamp.compare outcome.completed_at expires_at >= 0
    then invalid "lifecycle receipt timestamps are unordered"
    else
      Ok { key; request_digest; outcome; created_at; expires_at; completion_acknowledged }
  ;;

  let same_proof left right =
    Idempotency_store.Key.compare left.key right.key = 0
    && String.equal left.request_digest right.request_digest
    && Outcome.equal left.outcome right.outcome
    && P.Timestamp.equal left.created_at right.created_at
    && P.Timestamp.equal left.expires_at right.expires_at
  ;;

  let equal left right =
    same_proof left right
    && Bool.equal left.completion_acknowledged right.completion_acknowledged
  ;;
end

type t =
  { session_id : P.Id.Session.t
  ; status : Status.t
  ; admission : Admission.t
  ; revision : Revision.t
  ; receipts :
      (Idempotency_store.Key.t, Receipt.t, Idempotency_store.Key.comparator_witness) Map.t
  }

let initial ~session_id =
  { session_id
  ; status = Active
  ; admission = Automatic
  ; revision = Revision.zero
  ; receipts = Map.empty (module Idempotency_store.Key)
  }
;;

let of_original_archive ~session_id =
  { (initial ~session_id) with
    status = Archived
  ; admission = Explicit_resume_required
  ; revision = Revision.one
  }
;;

let terminal_proof ~status ~revision (receipt : Receipt.t) =
  Status.equal status Removed
  && Outcome.equal_action receipt.outcome.action Remove
  && Outcome.equal_disposition receipt.outcome.disposition Applied
  && Revision.equal receipt.outcome.lifecycle_revision revision
;;

let restore ~session_id ~status ~admission ~revision ~receipts =
  let open Result.Let_syntax in
  if
    (not (valid_state status admission))
    || List.length receipts > max_receipts
    || (Revision.equal revision Revision.zero
        && not (Status.equal status Active && Admission.equal admission Automatic))
    || (Status.equal status Removed
        && List.count receipts ~f:(terminal_proof ~status ~revision) <> 1)
  then invalid "invalid lifecycle head, terminal removal proof or receipt capacity"
  else (
    let%map receipts =
      List.fold_result
        receipts
        ~init:(Map.empty (module Idempotency_store.Key))
        ~f:(fun map (receipt : Receipt.t) ->
          if
            (not (P.Id.Session.equal session_id receipt.outcome.session_id))
            || Revision.compare receipt.outcome.lifecycle_revision revision > 0
            || (Revision.equal receipt.outcome.lifecycle_revision revision
                && not
                     (Status.equal receipt.outcome.status status
                      && Admission.equal receipt.outcome.admission admission))
            || Map.mem map receipt.key
          then invalid "lifecycle receipt owner/revision or duplicate key differs"
          else Ok (Map.set map ~key:receipt.key ~data:receipt))
    in
    { session_id; status; admission; revision; receipts })
;;

let session_id t = t.session_id
let status t = t.status
let admission t = t.admission
let revision t = t.revision
let receipts t = Map.data t.receipts

let equal left right =
  P.Id.Session.equal left.session_id right.session_id
  && Status.equal left.status right.status
  && Admission.equal left.admission right.admission
  && Revision.equal left.revision right.revision
  && Map.equal Receipt.equal left.receipts right.receipts
;;

let expired_acknowledged t (receipt : Receipt.t) now =
  (not (terminal_proof ~status:t.status ~revision:t.revision receipt))
  && receipt.completion_acknowledged
  && P.Timestamp.compare now receipt.expires_at >= 0
;;

let lookup t ~key ~request_digest ~now =
  match Map.find t.receipts key with
  | Some receipt when expired_acknowledged t receipt now -> Ok None
  | Some receipt when String.equal request_digest receipt.request_digest ->
    Ok (Some receipt)
  | Some _ ->
    Error
      (P.Error.create
         Idempotency_conflict
         ~message:"lifecycle key was used for another request"
         ~retryable:false
         ())
  | None -> Ok None
;;

type record = t

module Prepared = struct
  type t =
    { previous : record
    ; next : record
    ; outcome : Outcome.t
    }

  let previous t = t.previous
  let next t = t.next
  let outcome t = t.outcome
end

let target t action =
  match t.status, action with
  | Status.Removed, (Outcome.Archive | Restore | Resume) ->
    invalid "removed session cannot be restored or activated"
  | Removed, Remove -> Ok (t.status, t.admission)
  | (Active | Archived), Archive ->
    Ok (Status.Archived, Admission.Explicit_resume_required)
  | Active, Restore -> Ok (t.status, t.admission)
  | Archived, Restore -> Ok (Active, Explicit_resume_required)
  | Active, Resume -> Ok (Active, Automatic)
  | Archived, Resume -> invalid "archived session requires restoration before resume"
  | (Active | Archived), Remove -> Ok (Removed, Explicit_resume_required)
;;

let prepare t ~expected ~anchor ~action ~key ~request_digest ~now =
  let open Result.Let_syntax in
  let%bind () = validate_request ~session_id:t.session_id ~action key request_digest in
  let%bind existing = lookup t ~key ~request_digest ~now in
  match existing with
  | Some receipt -> Ok Prepared.{ previous = t; next = t; outcome = receipt.outcome }
  | None ->
    let%bind () =
      if Revision.equal expected t.revision
      then Ok ()
      else conflict "lifecycle revision does not match"
    in
    let%bind status, admission = target t action in
    let changed =
      not (Status.equal status t.status && Admission.equal admission t.admission)
    in
    let%bind revision = if changed then Revision.succ t.revision else Ok t.revision in
    let%bind outcome =
      Outcome.create
        ~session_id:t.session_id
        ~anchor
        ~lifecycle_revision:revision
        ~status
        ~admission
        ~action
        ~disposition:(if changed then Applied else Already_current)
        ~completed_at:now
    in
    let%bind expires_at = P.Timestamp.add_ms now receipt_retention_ms in
    let%bind receipt =
      Receipt.create
        ~key
        ~request_digest
        ~outcome
        ~created_at:now
        ~expires_at
        ~completion_acknowledged:false
    in
    let retained =
      Map.filter t.receipts ~f:(fun r -> not (expired_acknowledged t r now))
    in
    if Map.length retained >= max_receipts
    then invalid "lifecycle receipt capacity reached"
    else (
      let next =
        { t with
          status
        ; admission
        ; revision
        ; receipts = Map.set retained ~key ~data:receipt
        }
      in
      Ok { Prepared.previous = t; next; outcome })
;;

let acknowledge t ~key ~request_digest =
  match Map.find t.receipts key with
  | None -> invalid "lifecycle completion has no authority receipt"
  | Some receipt when not (String.equal request_digest receipt.request_digest) ->
    conflict "lifecycle completion digest differs"
  | Some receipt ->
    Ok
      { t with
        receipts =
          Map.set t.receipts ~key ~data:{ receipt with completion_acknowledged = true }
      }
;;

let validate_successor t ~next ~now =
  let open Result.Let_syntax in
  let%bind () =
    if not (P.Id.Session.equal t.session_id next.session_id)
    then invalid "lifecycle successor owner differs"
    else if Revision.compare next.revision t.revision < 0
    then invalid "lifecycle successor revision decreases"
    else if Status.equal t.status Removed && not (Status.equal next.status Removed)
    then invalid "lifecycle successor resurrects removed session"
    else Ok ()
  in
  let fresh =
    Map.data next.receipts
    |> List.filter ~f:(fun current ->
      match Map.find t.receipts current.Receipt.key with
      | Some previous -> not (Receipt.same_proof previous current)
      | None -> true)
  in
  let%bind () =
    match fresh with
    | [] ->
      if
        Revision.equal t.revision next.revision
        && Status.equal t.status next.status
        && Admission.equal t.admission next.admission
      then Ok ()
      else invalid "lifecycle successor changes head without accepted outcome proof"
    | [ receipt ] ->
      let%bind status, admission = target t receipt.outcome.action in
      let changed =
        not (Status.equal status t.status && Admission.equal admission t.admission)
      in
      let%bind expected_revision =
        if changed then Revision.succ t.revision else Ok t.revision
      in
      if
        Revision.equal expected_revision next.revision
        && Revision.equal receipt.outcome.lifecycle_revision next.revision
        && Status.equal status next.status
        && Admission.equal admission next.admission
        && Outcome.equal_disposition
             receipt.outcome.disposition
             (if changed then Applied else Already_current)
        && P.Timestamp.equal receipt.created_at now
        && P.Timestamp.equal receipt.outcome.completed_at now
        && not receipt.completion_acknowledged
      then Ok ()
      else invalid "lifecycle successor differs from one fresh admitted transition"
    | _ -> invalid "lifecycle successor accepts multiple new outcomes"
  in
  Map.fold t.receipts ~init:(Ok ()) ~f:(fun ~key ~data:previous checked ->
    let%bind () = checked in
    match Map.find next.receipts key with
    | None when expired_acknowledged t previous now -> Ok ()
    | Some current when Receipt.equal previous current -> Ok ()
    | Some current
      when Receipt.equal { previous with completion_acknowledged = true } current -> Ok ()
    | (Some _ | None) when expired_acknowledged t previous now -> Ok ()
    | Some _ | None ->
      invalid "lifecycle successor alters retained immutable outcome proof")
;;
