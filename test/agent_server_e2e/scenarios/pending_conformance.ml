open! Core
module P = Agent_protocol
module F = Crash_recovery_fixture

let observe
      ~(request : P.Command.t -> (P.Public.Result.t, P.Error.t) result)
      ~session_id
      ~key_prefix
  =
  let call command = request command |> F.protocol_ok in
  let non_history command = call command |> Support.Public_view.non_history in
  let key suffix = F.key (key_prefix ^ suffix) in
  let attach mode suffix =
    match
      call
        (Session_attach
           { session_id
           ; requested_mode = mode
           ; subscribe = false
           ; after_sequence = None
           ; reclaim_token = None
           ; idempotency_key = key suffix
           })
    with
    | Session_attach result -> result.attachment
    | _ -> F.fail "pending conformance attachment variant"
  in
  let writer = attach Read_write ":pending-writer" in
  let reader = attach Read_only ":pending-reader" in
  let list () =
    match
      non_history
        (Session_pending_inputs
           (P.Pending_query.Request.create
              ~session_id
              ~page:(P.Page.Request.create ~limit:1 () |> F.protocol_ok)
            |> F.protocol_ok))
    with
    | Session_pending_inputs view -> view
    | _ -> F.fail "pending conformance list variant"
  in
  let initial = list () in
  F.require
    (Int.equal (List.length initial.page.items) 1
     && Option.is_none initial.page.next_cursor)
    "pending conformance seed has the wrong queue";
  let item = List.hd_exn initial.page.items in
  let target attachment revision content suffix =
    P.Pending_control.Cancel_request.create
      ~session_id
      ~attachment_id:attachment.P.Session.Attachment.id
      ~expected_generation:item.generation
      ~expected_pending_revision:revision
      ~history_id:item.history.id
      ~expected_content_revision:content
      ~idempotency_key:(key suffix)
    |> F.protocol_ok
  in
  let lookup () =
    match
      non_history (Session_pending_input { session_id; history_id = item.history.id })
    with
    | Session_pending_input outcome -> outcome
    | _ -> F.fail "pending conformance lookup variant"
  in
  F.require
    (match lookup () with
     | Pending current -> P.History.Id.equal current.history.id item.history.id
     | Adopted _ | Cancelled _ | Retired _ | Unavailable _ -> false)
    "lookup lost queued occurrence";
  let denied =
    P.Command.Session_cancel_pending_input
      (target reader initial.pending_revision item.history.content_revision ":read-only")
  in
  F.require
    (match request denied with
     | Error error -> P.Error.equal_code error.code Permission_denied
     | Ok _ -> false)
    "read-only attachment controlled pending input";
  let replace =
    P.Command.Session_replace_pending_input
      (P.Pending_control.Replace_request.create
         ~target:
           (target
              writer
              initial.pending_revision
              item.history.content_revision
              ":replace")
         ~text:"replaced over the selected transport"
       |> F.protocol_ok)
  in
  let replaced =
    match non_history replace with
    | Session_replace_pending_input result -> result
    | _ -> F.fail "pending conformance replace variant"
  in
  F.require
    (match replaced.outcome with
     | Pending current ->
       P.History.Id.equal current.history.id item.history.id
       && Int64.equal
            (P.History.Content_revision.to_int64 current.history.content_revision)
            1L
     | Adopted _ | Cancelled _ | Retired _ | Unavailable _ -> false)
    "replacement changed identity or content revision";
  let retry =
    match non_history replace with
    | Session_replace_pending_input result -> result
    | _ -> F.fail "pending conformance replace retry variant"
  in
  F.require
    (Jsonaf.exactly_equal
       (P.Pending_control.Result.to_json replaced)
       (P.Pending_control.Result.to_json retry))
    "replace retry performed a second mutation";
  let receipt =
    non_history
      (Command_receipt
         { method_name = P.Command.method_name replace
         ; original_params = P.Command.params replace
         })
  in
  F.require
    (match receipt with
     | Command_receipt (Committed (Session_mutation { session_id = known; _ })) ->
       P.Id.Session.equal known session_id
     | _ -> false)
    "pending replacement lost original receipt";
  let stale =
    P.Command.Session_cancel_pending_input
      (target writer initial.pending_revision item.history.content_revision ":stale")
  in
  F.require
    (match request stale with
     | Error error -> P.Error.equal_code error.code Conflict
     | Ok _ -> false)
    "stale pending control did not reject";
  let current =
    match replaced.outcome with
    | Pending current -> current
    | Adopted _ | Cancelled _ | Retired _ | Unavailable _ ->
      F.fail "replacement not pending"
  in
  let cancel =
    P.Command.Session_cancel_pending_input
      (target writer replaced.pending_revision current.history.content_revision ":cancel")
  in
  let cancelled =
    match non_history cancel with
    | Session_cancel_pending_input result -> result
    | _ -> F.fail "pending conformance cancel variant"
  in
  F.require
    (match cancelled.outcome with
     | Cancelled id -> P.History.Id.equal id item.history.id
     | Pending _ | Adopted _ | Retired _ | Unavailable _ -> false)
    "cancel lost truthful disposition";
  F.require (List.is_empty (list ()).page.items) "cancel did not remove queued occurrence";
  F.require
    (match lookup () with
     | Cancelled id -> P.History.Id.equal id item.history.id
     | Pending _ | Adopted _ | Retired _ | Unavailable _ -> false)
    "lookup lost retained cancellation";
  let repeated =
    match non_history cancel with
    | Session_cancel_pending_input result -> result
    | _ -> F.fail "pending conformance cancel retry variant"
  in
  F.require
    (Jsonaf.exactly_equal
       (P.Pending_control.Result.to_json cancelled)
       (P.Pending_control.Result.to_json repeated))
    "cancel retry performed a second mutation"
;;
