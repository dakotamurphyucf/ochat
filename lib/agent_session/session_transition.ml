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

let apply ~now state ~delta ~payloads =
  let open Result.Let_syntax in
  let previous = state in
  let%bind state = Session_delta.apply state delta in
  let payloads = projected_payloads ~previous state payloads in
  let statuses = Session_state.extension_status state in
  let statuses_changed =
    not (Poly.equal (Session_state.extension_status previous) statuses)
  in
  let payloads =
    if
      statuses_changed
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
  let events = assign_events state ~revision ~first_sequence ~now payloads in
  let identity = { state.identity with updated_at = now } in
  let counters = { state.counters with revision; event_sequence; transaction_sequence } in
  let state = { state with identity; counters } in
  let%map () = Session_state.validate state in
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
