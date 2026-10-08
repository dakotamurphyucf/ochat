open! Core

type t =
  { state : Session_state.t
  ; delta : Session_delta.t
  ; events : Agent_protocol.Event.Durable.t list
  }

let increment name value =
  if Int64.equal value Int64.max_value
  then
    Error
      (Agent_protocol.Error.create
         Invalid_state
         ~message:(name ^ " overflow")
         ~retryable:false
         ())
  else Ok Int64.(value + 1L)
;;

let assign_events state ~revision ~first_sequence ~now payloads =
  List.mapi payloads ~f:(fun index payload ->
    let sequence = Int64.(first_sequence + of_int index) in
    Agent_protocol.Event.Durable.of_payload
      ~session_id:state.Session_state.identity.session_id
      ~sequence
      ~revision
      ~timestamp:now
      payload)
;;

let final_event_sequence current payloads =
  List.fold payloads ~init:(Ok current) ~f:(fun result _ ->
    Result.bind result ~f:(increment "event sequence"))
;;

let projected_payloads ~previous state payloads =
  let previous_projection = Session_state.moderator_projection previous in
  let projection = Session_state.moderator_projection state in
  if String.equal (Jsonaf.to_string previous_projection) (Jsonaf.to_string projection)
  then payloads
  else
    payloads
    @ [ Agent_protocol.Event.Durable.Payload.Moderator_overlay_changed projection ]
;;

let replacement_events state delta events =
  match delta with
  | Session_delta.Created _ ->
    let snapshot =
      Session_state.snapshot ~now:state.Session_state.identity.updated_at state
    in
    List.map events ~f:(fun event ->
      Agent_protocol.Event.Durable.with_replacement_snapshot event snapshot)
  | _ -> events
;;

let validate_session_updates state payloads =
  List.fold_result payloads ~init:() ~f:(fun () -> function
    | Agent_protocol.Event.Durable.Payload.Session_updated summary ->
      let open Result.Let_syntax in
      let%bind admitted =
        Agent_protocol.Session.of_json (Agent_protocol.Session.to_json summary)
      in
      if
        Agent_protocol.Id.Session.equal
          admitted.id
          state.Session_state.identity.session_id
      then Ok ()
      else
        Error
          (Agent_protocol.Error.invalid_request
             "session update belongs to another session")
    | _ -> Ok ())
;;

let track_host_turns ~previous state delta payloads =
  let open Result.Let_syntax in
  let ledger_error _ =
    Agent_protocol.Error.create
      Invalid_state
      ~message:"host turn tracking admission failed"
      ~retryable:false
      ()
  in
  let actual_operation operation active =
    Option.exists active ~f:(fun active ->
      Agent_protocol.Id.Operation.equal
        active.Agent_protocol.Operation.id
        operation.Agent_protocol.Operation.id
      && Int.equal active.generation operation.generation
      && Agent_protocol.Operation.equal_kind active.kind operation.kind)
  in
  let admit_operation operation =
    Agent_protocol.Operation.of_json (Agent_protocol.Operation.to_json operation)
    |> Result.map ~f:ignore
  in
  let invalid message = Error (Agent_protocol.Error.invalid_request message) in
  let started ledger operation =
    let%bind () = admit_operation operation in
    let%bind () =
      match operation.Agent_protocol.Operation.state with
      | Starting | Running -> Ok ()
      | Cancelling | Completed | Failed _ | Cancelled | Interrupted _ ->
        invalid "operation start has a non-started state"
    in
    match operation.kind with
    | Compaction -> Ok ledger
    | Turn _ ->
      let exact_active =
        Option.exists state.Session_state.active_operation ~f:(fun active ->
          String.equal
            (Jsonaf.to_string (Agent_protocol.Operation.to_json active))
            (Jsonaf.to_string (Agent_protocol.Operation.to_json operation)))
      in
      if
        exact_active
        && not (actual_operation operation previous.Session_state.active_operation)
      then
        Inference_ledger.admit_turn ledger operation
        |> Result.map ~f:(fun (ledger, _, _) -> ledger)
        |> Result.map_error ~f:ledger_error
      else invalid "host turn start lacks actual operation admission"
  in
  let finished ledger payload operation =
    let%bind () = admit_operation operation in
    let consistent =
      match payload, operation.Agent_protocol.Operation.state with
      | Agent_protocol.Event.Durable.Payload.Operation_completed _, Completed
      | Operation_failed _, Failed _
      | Operation_cancelled _, Cancelled
      | Operation_interrupted _, Interrupted _ -> true
      | _ -> false
    in
    let%bind () =
      if consistent
      then Ok ()
      else invalid "operation terminal payload differs from its actual state"
    in
    match operation.kind with
    | Compaction -> Ok ledger
    | Turn _ ->
      if actual_operation operation state.active_operation
      then invalid "host turn terminal did not retire the active operation"
      else if not (actual_operation operation previous.active_operation)
      then Ok ledger
      else (
        let%bind () =
          if
            Option.exists previous.active_operation ~f:(fun active ->
              Agent_protocol.Timestamp.compare active.started_at operation.started_at = 0
              && Agent_protocol.Timestamp.compare operation.updated_at active.updated_at
                 >= 0)
          then Ok ()
          else invalid "host turn terminal changed operation occurrence"
        in
        match
          Inference_ledger.find_turn_handle
            ledger
            ~operation_id:operation.id
            ~generation:operation.generation
        with
        | None -> Ok ledger
        | Some handle ->
          Inference_ledger.finish_turn ledger handle operation
          |> Result.map_error ~f:ledger_error)
  in
  let%map inference_ledger =
    List.fold_result
      payloads
      ~init:state.Session_state.inference_ledger
      ~f:(fun ledger payload ->
        match payload with
        | Agent_protocol.Event.Durable.Payload.Operation_started operation ->
          started ledger operation
        | Operation_completed operation
        | Operation_failed operation
        | Operation_cancelled operation
        | Operation_interrupted operation -> finished ledger payload operation
        | _ -> Ok ledger)
  in
  if
    Int64.equal
      (Inference_ledger.revision inference_ledger)
      (Inference_ledger.revision state.inference_ledger)
  then state, delta
  else (
    let state = { state with inference_ledger } in
    let delta =
      match delta with
      | Session_delta.Created _ -> Session_delta.Created state
      | _ -> Session_delta.Batch [ delta; Inference_ledger_changed inference_ledger ]
    in
    state, delta)
;;

let apply ~now state ~delta ~payloads =
  let open Result.Let_syntax in
  let previous = state in
  let%bind () = validate_session_updates state payloads in
  let%bind delta = Session_delta.capture_new_model_jobs state delta in
  let%bind state = Session_delta.apply state delta in
  let%bind state, delta = track_host_turns ~previous state delta payloads in
  let%bind state, delta =
    Managed_submission_tracking.apply ~previous ~state ~delta ~payloads ~now
  in
  let%bind state, delta =
    match delta with
    | Session_delta.Created _ ->
      let%map managed_stops =
        List.fold_result
          previous.managed_stops
          ~init:state.managed_stops
          ~f:(fun receipts receipt ->
            match List.find receipts ~f:(Managed_stop.same_key receipt) with
            | Some retained when Managed_stop.equal receipt retained -> Ok receipts
            | Some _ ->
              Error
                (Agent_protocol.Error.invalid_request
                   "replacement changed an immutable stop receipt")
            | None -> Ok (receipts @ [ receipt ]))
      in
      let state = { state with managed_stops } in
      state, Session_delta.Created state
    | _ -> Ok (state, delta)
  in
  let%bind state, delta =
    match previous.lifecycle.desired, state.lifecycle.desired with
    | Running, Stopped ->
      let%map stop_epoch = increment "stop epoch" previous.stop_epoch in
      let state = { state with stop_epoch } in
      let delta =
        match delta with
        | Session_delta.Created _ -> Session_delta.Created state
        | _ -> Session_delta.Batch [ delta; Stop_epoch_changed stop_epoch ]
      in
      state, delta
    | _ -> Ok (state, delta)
  in
  let payloads = projected_payloads ~previous state payloads in
  let statuses = Session_state.extension_status state in
  let statuses_changed =
    not
      (List.equal
         Agent_protocol.Extension_status.equal
         (Session_state.extension_status previous)
         statuses)
  in
  let inference_changed =
    not
      (Int64.equal
         (Inference_ledger.revision previous.inference_ledger)
         (Inference_ledger.revision state.inference_ledger))
  in
  let payloads =
    if
      (statuses_changed || inference_changed)
      && not
           (List.exists payloads ~f:(function
              | Agent_protocol.Event.Durable.Payload.Session_updated _ -> true
              | _ -> false))
    then
      payloads
      @ [ Agent_protocol.Event.Durable.Payload.Session_updated
            (Session_state.summary state)
        ]
    else payloads
  in
  let%bind revision = increment "session revision" state.counters.revision in
  let%bind transaction_sequence =
    increment "transaction sequence" state.counters.transaction_sequence
  in
  let%bind event_sequence = final_event_sequence state.counters.event_sequence payloads in
  let first_sequence = Int64.(state.counters.event_sequence + 1L) in
  let identity = { state.identity with updated_at = now } in
  let counters = { state.counters with revision; event_sequence; transaction_sequence } in
  let state = { state with identity; counters } in
  let%map () = Session_state.validate state in
  let payloads =
    List.map payloads ~f:(function
      | Agent_protocol.Event.Durable.Payload.Session_updated _ ->
        Agent_protocol.Event.Durable.Payload.Session_updated (Session_state.summary state)
      | payload -> payload)
  in
  let events = assign_events state ~revision ~first_sequence ~now payloads in
  let events = replacement_events state delta events in
  let events =
    if statuses_changed
    then
      List.map events ~f:(fun event ->
        Agent_protocol.Event.Durable.with_extension_status event statuses)
    else events
  in
  { state; delta; events }
;;

let lifecycle ~now state ~desired ~observed =
  let lifecycle = Session_state.Lifecycle.{ desired; observed } in
  apply
    ~now
    state
    ~delta:(Session_delta.Lifecycle_changed lifecycle)
    ~payloads:
      [ Agent_protocol.Event.Durable.Payload.Session_state_changed
          { desired_state = desired; observed_state = observed }
      ]
;;
