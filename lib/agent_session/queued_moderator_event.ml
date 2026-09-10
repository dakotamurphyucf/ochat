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

let captured_timer event =
  Result.bind (Session.Snapshot.to_value event) ~f:Chat_response.Schedule_delivery.decode
  |> Result.map_error ~f:P.Error.invalid_request
;;

let receipt_snapshot (receipt : E.t) =
  let open Result.Let_syntax in
  match receipt.context.phase, receipt.context.event with
  | Internal_event, `Object fields ->
    (match List.Assoc.find fields ~equal:String.equal "snapshot_sexp" with
     | Some (`String sexp) ->
       let%bind event =
         Result.try_with (fun () -> Session.Snapshot.t_of_sexp (Sexp.of_string sexp))
         |> Result.map_error ~f:(fun _ ->
           P.Error.invalid_request "invalid captured queued event snapshot")
       in
       Ok (Some event)
     | _ -> Ok None)
  | _ -> Ok None
;;

let receipt_timer receipt =
  let open Result.Let_syntax in
  let%bind snapshot = receipt_snapshot receipt in
  match snapshot with
  | None -> Ok None
  | Some event -> captured_timer event
;;

let captured_ingress event =
  Result.bind (Session.Snapshot.to_value event) ~f:Chat_response.Ingress_delivery.decode
  |> Result.map_error ~f:P.Error.invalid_request
;;

let ingress_matches_receipt registration frame =
  let open Result.Let_syntax in
  match
    List.find registration.External_ingress.receipts ~f:(fun receipt ->
      P.Id.Ingress_event.equal receipt.id frame.Chat_response.Ingress_delivery.event_id)
  with
  | None -> Ok false
  | Some receipt ->
    let%map expected = External_ingress.delivery_frame registration receipt in
    Chat_response.Ingress_delivery.equal frame expected
;;

let claimed_ingress_ids ~state =
  let open Result.Let_syntax in
  List.map state.Session_state.moderator_executions ~f:(fun receipt ->
    let%bind event = receipt_snapshot receipt in
    match event with
    | None -> Ok None
    | Some event ->
      let%bind captured = captured_ingress event in
      (match captured with
       | None -> Ok None
       | Some frame ->
         (match
            List.find state.ingress_registrations ~f:(fun registration ->
              P.Id.Capability.equal registration.context.id frame.registration_id)
          with
          | None -> Ok None
          | Some registration ->
            let%map matches = ingress_matches_receipt registration frame in
            (* A retired forged frame must not consume the identity of a valid
               event that is still waiting later in the queue. *)
            Option.some_if matches frame.event_id)))
  |> Result.all
  |> Result.map ~f:List.filter_opt
;;

let ingress_retirement_reason ~state ~observer ~event ~subscription_expired =
  let open Result.Let_syntax in
  let%bind frame = captured_ingress event in
  match frame with
  | None -> Ok None
  | Some frame ->
    let retained =
      List.find state.Session_state.ingress_registrations ~f:(fun value ->
        P.Id.Capability.equal value.External_ingress.context.id frame.registration_id)
    in
    (match retained with
     | None -> Ok (Some "ingress.unknown_registration")
     | Some registration ->
       let c = registration.context in
       let%bind matches = ingress_matches_receipt registration frame in
       (match
          matches
          && P.Id.Session.equal c.session_id state.identity.session_id
          && c.generation = state.identity.generation
          && P.Invocation.equal_observer c.source observer
        with
        | false -> Ok (Some "ingress.stale_or_forged_delivery")
        | true ->
          let%bind claimed = claimed_ingress_ids ~state in
          (match List.mem claimed frame.event_id ~equal:P.Id.Ingress_event.equal with
           | true -> Ok (Some "ingress.duplicate_delivery")
           | false ->
             let active =
               List.find state.subscriptions ~f:(fun subscription ->
                 P.Id.Subscription.equal subscription.context.id c.subscription_id
                 && subscription.epoch = c.epoch
                 && Option.is_none subscription.result)
             in
             (match active with
              | None -> Ok (Some "ingress.stale_subscription")
              | Some subscription ->
                let%map expired = subscription_expired subscription in
                Option.some_if expired "ingress.expired_subscription"))))
;;

let claimed_timer_ids ~state =
  List.map state.Session_state.moderator_executions ~f:receipt_timer
  |> Result.all
  |> Result.map
       ~f:(List.filter_map ~f:(Option.map ~f:(fun (timer : P.Schedule.t) -> timer.id)))
;;

let timer_retirement_reason
      ~state
      ~(observer : P.Invocation.observer)
      ~event
      ~subscription_expired
  =
  let open Result.Let_syntax in
  let%bind timer = captured_timer event in
  match timer with
  | None -> Ok None
  | Some timer ->
    let%bind ownership =
      Result.of_option
        timer.ownership
        ~error:(P.Error.invalid_request "queued timer requires captured ownership")
    in
    let retained =
      List.find state.Session_state.schedules ~f:(fun retained ->
        P.Id.Schedule.equal retained.id timer.id)
    in
    let matches_delivery =
      match retained with
      | Some
          ({ status = Delivered; delivery_count = 1; last_delivery_at = Some _; _ } as
           retained) ->
        let claimed =
          { retained with
            status = Delivering
          ; delivery_count = 0
          ; last_delivery_at = None
          }
        in
        Jsonaf.exactly_equal (P.Schedule.to_json claimed) (P.Schedule.to_json timer)
      | _ -> false
    in
    (match
       P.Id.Session.equal state.identity.session_id timer.session_id
       && state.identity.generation = timer.generation
       && P.Invocation.equal_observer observer ownership.source
       && matches_delivery
     with
     | false -> Ok (Some "timer.stale_delivery")
     | true ->
       let%bind previous = claimed_timer_ids ~state in
       (match List.mem previous timer.id ~equal:P.Id.Schedule.equal with
        | true -> Ok (Some "timer.duplicate_delivery")
        | false ->
          (match ownership.subscription with
           | None -> Ok None
           | Some (id, epoch) ->
             let active =
               List.find state.subscriptions ~f:(fun subscription ->
                 P.Id.Subscription.equal subscription.context.id id
                 && P.Id.Session.equal subscription.context.session_id timer.session_id
                 && subscription.context.generation = timer.generation
                 && Option.equal
                      P.Invocation.equal_observer
                      subscription.context.source
                      (Some ownership.source)
                 && subscription.epoch = epoch
                 && Option.equal P.Id.Schedule.equal subscription.timer_id (Some timer.id)
                 && Option.is_none subscription.result)
             in
             (match active with
              | Some subscription ->
                let%map expired = subscription_expired subscription in
                Option.some_if expired "timer.stale_subscription"
              | None -> Ok (Some "timer.stale_subscription")))))
;;

let delivery_retirement_reason ~state ~observer ~event ~subscription_expired =
  let open Result.Let_syntax in
  let%bind timer =
    timer_retirement_reason ~state ~observer ~event ~subscription_expired
  in
  match timer with
  | Some _ -> Ok timer
  | None -> ingress_retirement_reason ~state ~observer ~event ~subscription_expired
;;

let has_unsettled_claim ~state ~(observer : P.Invocation.observer) =
  List.exists state.Session_state.moderator_executions ~f:(fun receipt ->
    receipt.context.generation = state.identity.generation
    && E.equal_phase receipt.context.phase Internal_event
    && P.Invocation.equal_observer receipt.context.source observer
    && Option.is_none receipt.retirement
    &&
    match receipt.status with
    | Running | Failed _ | Interrupted _ -> true
    | Completed _ -> false)
;;

let claim_in_context ~operation_id ~state ~id ~(snapshot : S.t) ~now =
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
      let%bind value = Chat_response.Schedule_delivery.script_event value in
      let%bind value = Chat_response.Ingress_delivery.script_event value in
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
      has_unsettled_claim
        ~state
        ~observer:
          { script_id = snapshot.script_id; source_sha256 = snapshot.script_source_hash }
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
    ; operation_id
    ; job = None
    ; phase = Internal_event
    ; event = encoded_event event
    ; checkpoint_sha256
    ; created_at = now
    }
  |> Result.map ~f:(fun receipt -> receipt, event)
;;

let claim = claim_in_context ~operation_id:None
let claim_foreground ~operation_id = claim_in_context ~operation_id:(Some operation_id)

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

let ordinary_phase = function
  | Chat_response.Moderation.Event.Session_start -> Ok E.Session_start
  | Session_resume -> Ok E.Session_resume
  | Turn_start -> Ok E.Turn_start
  | Item_appended _ -> Ok E.Message_appended
  | Pre_tool_call _ -> Ok E.Pre_tool_call
  | Post_tool_response _ -> Ok E.Post_tool_response
  | Turn_end -> Ok E.Turn_end
  | Internal_event _ -> conflict "internal events require the queued event handoff"
;;

let lifecycle_phase = function
  | E.Session_start | Session_resume -> true
  | Turn_start
  | Message_appended
  | Pre_tool_call
  | Post_tool_response
  | Turn_end
  | Internal_event -> false
;;

let claim_ordinary_in_context ~job ~state ~id ~(snapshot : S.t) ~operation_id ~event ~now =
  let open Result.Let_syntax in
  let%bind () = installed ~state ~snapshot in
  let%bind phase = ordinary_phase event in
  let%bind event =
    Chat_response.Moderation.Event.to_value event
    |> Session.Snapshot.of_value
    |> Result.map_error ~f:P.Error.invalid_request
  in
  let captured = encoded_event event in
  let checkpoint_sha256 = checkpoint snapshot in
  let source : P.Invocation.observer =
    { script_id = snapshot.script_id; source_sha256 = snapshot.script_source_hash }
  in
  let%bind () =
    match
      List.exists state.moderator_executions ~f:(fun receipt ->
        receipt.context.generation = state.identity.generation
        && P.Invocation.equal_observer receipt.context.source source
        && ((lifecycle_phase phase && lifecycle_phase receipt.context.phase)
            || (E.equal_phase receipt.context.phase phase
                && Option.equal
                     P.Id.Operation.equal
                     receipt.context.operation_id
                     operation_id
                && Option.equal E.equal_job_attempt receipt.context.job job
                && String.equal receipt.context.checkpoint_sha256 checkpoint_sha256
                && Jsonaf.exactly_equal receipt.context.event captured))
        &&
        match receipt.status with
        | Running | Failed _ | Interrupted _ -> true
        | Completed _ -> false)
    with
    | true ->
      conflict
        "ordinary moderator event was already claimed; automatic replay is forbidden"
    | false -> Ok ()
  in
  E.create
    { id
    ; session_id = state.identity.session_id
    ; generation = state.identity.generation
    ; source
    ; operation_id
    ; job
    ; phase
    ; event = captured
    ; checkpoint_sha256
    ; created_at = now
    }
  |> Result.map ~f:(fun receipt -> receipt, event)
;;

let claim_ordinary = claim_ordinary_in_context ~job:None
let claim_job ~job = claim_ordinary_in_context ~job:(Some job) ~operation_id:None

let complete_ordinary ~claimed ~(before : S.t) ~(snapshot : S.t) ~requests =
  let open Result.Let_syntax in
  let%bind () =
    match
      String.equal snapshot.script_id before.script_id
      && String.equal snapshot.script_source_hash before.script_source_hash
      && List.is_prefix
           snapshot.queued_internal_events
           ~prefix:before.queued_internal_events
           ~equal:same_value
    with
    | true -> Ok ()
    | false -> conflict "ordinary event must preserve its source and existing queue"
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
