open Core
module P = Agent_protocol
module E = P.Moderator_execution
module S = Session.Moderator_state.Identity_snapshot

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let checkpoint snapshot =
  S.sexp_of_t snapshot
  |> Sexp.to_string_mach
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
;;

let same_value a b =
  Sexp.equal (Session.Snapshot.sexp_of_t a) (Session.Snapshot.sexp_of_t b)
;;

let installed ~state ~(snapshot : S.t) =
  match state.Session_state.moderator with
  | Some saved
    when Jsonaf.exactly_equal saved (Runtime_builder.encode_moderator_snapshot snapshot)
    -> Ok ()
  | _ -> conflict "queued event requires the exact installed moderator checkpoint"
;;

let encoded_event event =
  `Object
    [ "snapshot_sexp", `String (Sexp.to_string_mach (Session.Snapshot.sexp_of_t event)) ]
;;

let claim ~state ~id ~(snapshot : S.t) ~now =
  let open Result.Let_syntax in
  let%bind () = installed ~state ~snapshot in
  let%bind event =
    match snapshot.halted, snapshot.queued_internal_events with
    | false, event :: _ -> Ok event
    | _ -> conflict "moderator has no runnable queued event"
  in
  let%bind () =
    let checked =
      let%bind value = Session.Snapshot.to_value event in
      match value with
      | Chatml.Chatml_lang.VVariant ("Internal_event", [ payload ]) ->
        Chat_response.Moderator_invocation.internal_event payload |> Result.map ~f:ignore
      | _ -> Error "queued event requires an extensibility-v1 Internal_event envelope"
    in
    Result.map_error checked ~f:P.Error.invalid_request
  in
  let checkpoint_sha256 = checkpoint snapshot in
  let%bind () =
    match
      List.exists state.moderator_executions ~f:(fun receipt ->
        receipt.context.generation = state.identity.generation
        && E.equal_phase receipt.context.phase Internal_event
        && String.equal receipt.context.source.script_id snapshot.script_id
        && String.equal receipt.context.source.source_sha256 snapshot.script_source_hash
        && Option.is_none receipt.retirement
        &&
        match receipt.status with
        | Running | Failed _ | Interrupted _ -> true
        | Completed _ -> false)
    with
    | true ->
      conflict
        "moderator has an unsettled queued event claim; automatic replay is forbidden"
    | false -> Ok ()
  in
  E.create
    { id
    ; session_id = state.identity.session_id
    ; generation = state.identity.generation
    ; source =
        { script_id = snapshot.script_id; source_sha256 = snapshot.script_source_hash }
    ; operation_id = None
    ; phase = Internal_event
    ; event = encoded_event event
    ; checkpoint_sha256
    ; created_at = now
    }
  |> Result.map ~f:(fun receipt -> receipt, event)
;;

let complete ~claimed ~before ~(snapshot : S.t) ~requests =
  let open Result.Let_syntax in
  let%bind () =
    match
      String.equal snapshot.script_id before.S.script_id
      && String.equal snapshot.script_source_hash before.script_source_hash
    with
    | true -> Ok ()
    | false -> conflict "queued event cannot replace its moderator source"
  in
  let%bind tail =
    match before.queued_internal_events with
    | _ :: tail -> Ok tail
    | [] -> conflict "queued event completion requires a claimed queue head"
  in
  let%bind () =
    match
      List.is_prefix snapshot.queued_internal_events ~prefix:tail ~equal:same_value
    with
    | true -> Ok ()
    | false -> conflict "queued event checkpoint must preserve the unconsumed queue tail"
  in
  E.complete claimed ~checkpoint_sha256:(checkpoint snapshot) ~requests
;;

let claim_retirement ~state ~id ~(snapshot : S.t) =
  let open Result.Let_syntax in
  let%bind () = installed ~state ~snapshot in
  let%bind receipt =
    match
      List.find state.moderator_executions ~f:(fun receipt ->
        P.Id.Moderator_execution.equal id receipt.context.id)
    with
    | Some receipt -> Ok receipt
    | None -> conflict "event receipt is not retained"
  in
  let%bind () =
    match receipt.status, receipt.retirement, receipt.context.phase with
    | (Failed _ | Interrupted _), None, Internal_event
      when receipt.context.generation = state.identity.generation
           && String.equal receipt.context.checkpoint_sha256 (checkpoint snapshot)
           && String.equal receipt.context.source.script_id snapshot.script_id
           && String.equal
                receipt.context.source.source_sha256
                snapshot.script_source_hash -> Ok ()
    | _ -> conflict "retirement requires an unsettled failure at its original checkpoint"
  in
  match snapshot.queued_internal_events with
  | event :: _ when Jsonaf.exactly_equal receipt.context.event (encoded_event event) ->
    Ok (receipt, event)
  | _ -> conflict "failed receipt does not identify the current queue head"
;;

let retire ~claimed ~(before : S.t) ~(snapshot : S.t) ~reason =
  let open Result.Let_syntax in
  let%bind tail =
    match before.queued_internal_events with
    | _ :: tail -> Ok tail
    | [] -> conflict "retirement requires a queue head"
  in
  let expected = { before with queued_internal_events = tail } in
  let%bind () =
    match Sexp.equal (S.sexp_of_t expected) (S.sexp_of_t snapshot) with
    | true -> Ok ()
    | false -> conflict "retirement must remove only the failed queue head"
  in
  E.retire claimed ~checkpoint_sha256:(checkpoint snapshot) ~reason
;;
