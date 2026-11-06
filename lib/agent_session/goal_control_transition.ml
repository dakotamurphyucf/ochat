open! Core
module P = Agent_protocol
module G = P.Goal
module I = P.Invocation
module E = P.Moderator_execution

module Goal_id = struct
  include P.Id.Goal
  include Comparator.Make (P.Id.Goal)
end

module Run_id = struct
  include P.Id.Run
  include Comparator.Make (P.Id.Run)
end

let invalid message = Error (P.Error.invalid_request message)

(* Historical negative reconciliation uses the immutable admission link. It
   must not require the Goal's current episode/Enabled state or reinterpret an
   absent inference classification as positively Unbound. *)
let selected_run (state : Session_state.t) selected (run : P.Run.t) =
  match state.run_state with
  | None -> invalid "Goal control retained Run index absent"
  | Some index ->
    (match Run_state.goal_link index run.id with
     | None -> Ok false
     | Some link ->
       if P.Goal_episode.matches_run (Run_goal_link.episode link) run
       then Ok (Set.mem selected (Run_goal_link.goal_id link))
       else invalid "Goal control retained episode does not match its Run")
;;

let run_of_origin (state : Session_state.t) = function
  | P.Inference_origin.Unbound -> Ok None
  | Run id ->
    (match Option.bind state.run_state ~f:(fun index -> Run_state.find index id) with
     | Some run -> Ok (Some run)
     | None -> invalid "Goal control attributed Run absent")
;;

let selected_optional_run state selected = function
  | None -> Ok false
  | Some run -> selected_run state selected run
;;

let selected_invocation state selected (invocation : I.t) =
  let open Result.Let_syntax in
  let%bind run =
    match invocation.inference_origin with
    | Some origin -> run_of_origin state origin
    | None ->
      Result.map
        (Goal_work_attribution.Retained_parent.of_invocation state invocation)
        ~f:
          (Option.map ~f:(fun parent -> parent.Goal_work_attribution.Retained_parent.run))
  in
  selected_optional_run state selected run
;;

let selected_event (state : Session_state.t) selected event =
  let open Result.Let_syntax in
  let%bind origin =
    Invocation_inference_provenance.of_event ~runs:state.run_state ~jobs:state.jobs event
  in
  let%bind run =
    match origin with
    | Some origin -> run_of_origin state origin
    | None ->
      Result.map
        (Goal_work_attribution.Retained_parent.of_event state event)
        ~f:
          (Option.map ~f:(fun parent -> parent.Goal_work_attribution.Retained_parent.run))
  in
  selected_optional_run state selected run
;;

let executable (requests : I.follow_up) =
  (* Observation_follow_up.plan selects Stop before Compact/Turn. Preserve its
     stop-dominant carrier intact rather than silently suppressing a real Stop. *)
  Option.is_none requests.end_session
  && (requests.request_turn || requests.request_compaction)
;;

let pending_executable = function
  | I.Pending_follow_up requests -> executable requests
  | Compaction_accepted_follow_up requests ->
    executable { requests with request_compaction = false }
  | Applied_follow_up _ | Discarded_follow_up _ -> false
;;

let invocation_parts (invocation : I.t) =
  let observation =
    match invocation.observation with
    | Some { status = Observed; follow_up = Some follow_up; _ } ->
      pending_executable follow_up
    | Some { status = Awaiting | Observing | Observation_failed _; _ }
    | Some { status = Observed; follow_up = None; _ }
    | None -> false
  in
  let handler =
    Option.exists invocation.handler_intent ~f:(fun intent ->
      pending_executable intent.follow_up)
  in
  observation, handler
;;

let invocation_delta state selected invocation ~reason =
  let open Result.Let_syntax in
  match invocation_parts invocation with
  | false, false -> Ok None
  | observation, handler ->
    let%bind owns = selected_invocation state selected invocation in
    if not owns
    then Ok None
    else (
      let%bind invocation =
        if observation
        then I.discard_observation_follow_up invocation ~reason
        else Ok invocation
      in
      let%map invocation =
        if handler then I.discard_handler_intent invocation ~reason else Ok invocation
      in
      Some (Observation_follow_up.delta invocation))
;;

let event_delta state selected (event : E.t) ~reason =
  let open Result.Let_syntax in
  let requests =
    match event.status, event.intent, event.requests with
    | Completed _, Some Pending, Some requests -> Some requests
    | Completed _, Some (Waiting_compaction _), Some requests ->
      Some { requests with request_compaction = false }
    | (Running | Failed _ | Interrupted _), _, _
    | Completed _, (Some (Applied | Discarded _) | None), _
    | Completed _, Some (Pending | Waiting_compaction _), None -> None
  in
  if not (Option.exists requests ~f:executable)
  then Ok None
  else (
    let%bind owns = selected_event state selected event in
    if not owns
    then Ok None
    else (
      let%map event = E.discard_intent event ~reason in
      Some (Observation_follow_up.event_delta event)))
;;

let continue_delta (state : Session_state.t) selected =
  let open Result.Let_syntax in
  match state.run_state with
  | None -> Ok None
  | Some index ->
    let%bind grouped =
      List.fold
        (Run_state.intents index)
        ~init:(Ok (Map.empty (module Run_id)))
        ~f:(fun grouped (intent : Run_intent.t) ->
          let%bind grouped = grouped in
          match intent.disposition, intent.action with
          | Pending, Continue ->
            let%bind run =
              Result.of_option
                (Run_state.find index intent.receipt.run_id)
                ~error:(P.Error.invalid_request "Goal control Continue Run absent")
            in
            let%map owns = selected_run state selected run in
            if owns
            then Map.add_multi grouped ~key:run.id ~data:(Run_intent.retire intent)
            else grouped
          | (Consumed _ | Retired), (Continue | Wait _ | Finish _)
          | Pending, (Wait _ | Finish _) -> Ok grouped)
    in
    if Map.is_empty grouped
    then Ok None
    else (
      let%map next =
        Map.fold grouped ~init:(Ok index) ~f:(fun ~key ~data result ->
          let%bind next = result in
          let%bind run =
            Result.of_option
              (Run_state.find next key)
              ~error:(P.Error.invalid_request "Goal control Continue Run absent")
          in
          Run_state.advance next ~run ~intents:data)
      in
      Some (Session_delta.Run_state_changed next))
;;

let retirement_changes (state : Session_state.t) selected ~reason =
  let open Result.Let_syntax in
  if Set.is_empty selected
  then Ok []
  else (
    let%bind run = continue_delta state selected in
    let%bind invocations =
      List.map state.invocations ~f:(fun invocation ->
        invocation_delta state selected invocation ~reason)
      |> Result.all
    in
    let%map events =
      List.map state.moderator_executions ~f:(fun event ->
        event_delta state selected event ~reason)
      |> Result.all
    in
    Option.to_list run @ List.filter_opt invocations @ List.filter_opt events)
;;

type control =
  | Hold
  | Abandon

let prepare_control (state : Session_state.t) ~goal_id ~delta ~control =
  let open Result.Let_syntax in
  let%bind previous =
    Result.of_option
      (Option.bind state.goal_catalog ~f:(fun catalog ->
         Goal_catalog.find catalog goal_id))
      ~error:(P.Error.invalid_request "Goal control Hold Goal absent")
  in
  let%bind candidate = Session_delta.apply state delta in
  let%bind incoming =
    Result.of_option
      (Option.bind candidate.goal_catalog ~f:(fun catalog ->
         Goal_catalog.find catalog goal_id))
      ~error:(P.Error.invalid_request "Goal control Hold candidate Goal absent")
  in
  let%bind () = G.validate_update previous ~incoming in
  let%bind () =
    match control, G.state incoming with
    | Hold, Open (Held _) | Abandon, Closed (Abandoned _) -> Ok ()
    | Hold, (Open Enabled | Closed _) | Abandon, (Open _ | Closed (Achieved _)) ->
      invalid "Goal control requires its exact fresh metadata candidate"
  in
  let%map changes =
    retirement_changes
      candidate
      (Set.singleton (module Goal_id) goal_id)
      ~reason:
        (match control with
         | Hold -> "Goal held"
         | Abandon -> "Goal abandoned")
  in
  match changes with
  | [] -> delta
  | changes -> Session_delta.Batch (delta :: changes)
;;

let prepare_hold state ~goal_id ~delta =
  prepare_control state ~goal_id ~delta ~control:Hold
;;

let prepare_abandon state ~goal_id ~delta =
  prepare_control state ~goal_id ~delta ~control:Abandon
;;

let prepare_stop (state : Session_state.t) =
  let open Result.Let_syntax in
  match state.goal_catalog with
  | None -> Ok None
  | Some catalog ->
    let%bind selected, documents =
      List.fold
        (Goal_catalog.goals catalog)
        ~init:(Ok (Set.empty (module Goal_id), []))
        ~f:(fun result goal ->
          let%bind selected, documents = result in
          match G.state goal with
          | Closed _ -> Ok (selected, documents)
          | Open (Held _) -> Ok (Set.add selected (G.id goal), documents)
          | Open Enabled ->
            let%bind document =
              Result.of_option
                (Goal_catalog.document catalog (G.id goal))
                ~error:(P.Error.invalid_request "Goal control document absent")
            in
            let%bind goal = G.hold goal Operator_pause in
            let%map document =
              Goal_document.update document goal
              |> Result.map_error ~f:(fun error ->
                P.Error.invalid_request
                  (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
            in
            Set.add selected (G.id goal), document :: documents)
    in
    let%bind catalog =
      Goal_catalog.advance_existing_many catalog ~goals:(List.rev documents)
    in
    let%map changes = retirement_changes state selected ~reason:"session stopping" in
    let changes =
      Option.to_list
        (Option.map catalog ~f:(fun catalog -> Session_delta.Goal_catalog_changed catalog))
      @ changes
    in
    (match changes with
     | [] -> None
     | changes -> Some (Session_delta.Batch changes))
;;
