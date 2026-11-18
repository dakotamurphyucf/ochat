open! Core

module Request = struct
  type t =
    | Idle
    | Timer of Agent_protocol.Schedule.t
    | Job_delivery of Run_job_delivery.t
    | Job_completion of Agent_protocol.Job.t
end

module Selection = struct
  type t =
    | Ordinary
    | No_work
    | Suspended_goal of Goal_execution_selection.Suspension.t
    | Wake of Agent_protocol.Run_wake.t
end

type job_lookup =
  Agent_protocol.Id.Job.t -> (Agent_protocol.Job.t, Agent_protocol.Error.t) result

type schedule_lookup =
  Agent_protocol.Id.Schedule.t
  -> (Agent_protocol.Schedule.t, Agent_protocol.Error.t) result

let error code message = Agent_protocol.Error.create code ~message ~retryable:false ()

let pending_run (state : Session_state.t) (wake : Agent_protocol.Run_wake.t) =
  let open Result.Let_syntax in
  let%bind () = Agent_protocol.Run_wake.validate wake in
  match state.run_state with
  | None -> Ok None
  | Some index ->
    (match Run_state.find index wake.run_id with
     | None -> Ok None
     | Some run ->
       (match run.lifecycle with
        | Waiting retained when Agent_protocol.Run_wake.equal retained wake ->
          if
            List.exists (Run_state.intents index) ~f:(fun intent ->
              match intent.Run_intent.disposition, intent.action with
              | Pending, Wait pending -> Agent_protocol.Run_wake.equal pending wake
              | _ -> false)
          then Ok (Some run)
          else Error (error Conflict "Goal wake lost its actual Pending Wait")
        | Admitted | Active | Waiting _ | Terminal _ -> Ok None))
;;

let queue_head (state : Session_state.t) ~now ~subscription_expired run wake =
  let open Result.Let_syntax in
  let%bind snapshot = Moderator_checkpoint.decode state.moderator in
  match snapshot with
  | None -> Ok false
  | Some snapshot ->
    (match snapshot.queued_internal_events with
     | [] -> Ok false
     | _ :: _ ->
       (* This is the existing pure candidate validation, not a persisted claim.
          Its fresh ID and returned receipt are never exposed as authority. *)
       let%bind executing, event =
         Queued_moderator_event.claim
           ~state
           ~id:(Agent_protocol.Id.Moderator_execution.create ())
           ~snapshot
           ~now:(now ())
       in
       let%bind reason =
         Queued_moderator_event.delivery_retirement_reason
           ~state
           ~observer:executing.context.source
           ~event
           ~subscription_expired
       in
       (match reason with
        | Some _ -> Ok false
        | None -> Run_wake_owner.claim_matches state ~run ~wake ~executing))
;;

let producer_ready
      (state : Session_state.t)
      ~now
      ~subscription_expired
      ~(find_job : job_lookup)
      ~(find_schedule : schedule_lookup)
      run
      (wake : Agent_protocol.Run_wake.t)
  =
  let open Result.Let_syntax in
  let%bind () =
    match wake.occurrence with
    | Job_completion _ ->
      (match
         Option.bind state.run_state ~f:(fun index ->
           Option.bind
             (Run_job_delivery.Key.of_wake wake)
             ~f:(Run_state.find_job_delivery index))
       with
       | Some delivery ->
         if
           Agent_protocol.Id.Run.equal
             (Run_job_delivery.run_id delivery)
             run.Agent_protocol.Run.id
           && Agent_protocol.Run_source.equal
                (Run_job_delivery.source delivery)
                wake.source
         then Ok ()
         else Error (error Conflict "retained Goal Job delivery owner changed")
       | None -> Run_wake_owner.validate state ~run ~wake)
    | Delivered_timer _ | Subscription_delivery _ ->
      Run_wake_owner.validate state ~run ~wake
  in
  match wake.occurrence with
  | Delivered_timer { schedule_id; _ } ->
    let%bind schedule = find_schedule schedule_id in
    (match schedule.status with
     | Scheduled | Delivering ->
       Ok (Agent_protocol.Timestamp.compare schedule.next_due_at (now ()) <= 0)
     | Delivered -> queue_head state ~now ~subscription_expired run wake
     | Cancelled | Failed _ -> Ok false)
  | Job_completion { job_id; attempt } ->
    let retained =
      Option.bind state.run_state ~f:(fun index ->
        Option.bind
          (Run_job_delivery.Key.of_wake wake)
          ~f:(Run_state.find_job_delivery index))
    in
    (match retained with
     | Some delivery ->
       (match Run_job_delivery.disposition delivery with
        | Pending -> Ok true
        | Enqueued _ -> queue_head state ~now ~subscription_expired run wake
        | Claimed _ | Retired _ -> Ok false)
     | None ->
       let%bind job = find_job job_id in
       if not (Int.equal attempt job.attempt)
       then
         Error (error Conflict "Goal wake Job attempt changed without retained delivery")
       else (
         match job.status, job.delivery with
         | (Succeeded | Failed _ | Cancelled | Interrupted _), Pending -> Ok true
         | (Succeeded | Failed _ | Cancelled | Interrupted _), Delivered _ ->
           queue_head state ~now ~subscription_expired run wake
         | _ -> Ok false))
  | Subscription_delivery { delivery_id; _ } ->
    let%bind delivery =
      List.find state.deliveries ~f:(fun delivery ->
        Agent_protocol.Id.Delivery.equal delivery.context.id delivery_id)
      |> Result.of_option ~error:(error Conflict "Goal wake delivery is absent")
    in
    Ok
      (match delivery.status, delivery.wake_disposition with
       | Committed _, Some Pending_wake -> true
       | _ -> false)
;;

let classify (state : Session_state.t) ~producer_ready wake
  : (Selection.t, Agent_protocol.Error.t) result
  =
  let open Result.Let_syntax in
  let%bind run = pending_run state wake in
  match run with
  | None -> Ok Selection.No_work
  | Some run ->
    let%bind attribution = Goal_work_attribution.of_run state run in
    (match attribution with
     | None -> Ok Selection.Ordinary
     | Some _ ->
       let%bind ready = producer_ready run wake in
       if not ready
       then Ok Selection.No_work
       else (
         let%map selection = Goal_execution_selection.of_run state run in
         match selection with
         | Eligible -> Selection.Wake wake
         | Suspended_goal suspension -> Suspended_goal suspension))
;;

let validate_request
      (state : Session_state.t)
      ~(find_job : job_lookup)
      ~(find_schedule : schedule_lookup)
      (request : Request.t)
  =
  let open Result.Let_syntax in
  let changed () = Error (error Conflict "Goal wake producer changed before routing") in
  match request with
  | Idle -> Ok ()
  | Timer expected ->
    let%bind current = find_schedule expected.id in
    if
      Jsonaf.exactly_equal
        (Agent_protocol.Schedule.to_json current)
        (Agent_protocol.Schedule.to_json expected)
    then Ok ()
    else changed ()
  | Job_completion expected ->
    let%bind current = find_job expected.id in
    if
      Jsonaf.exactly_equal
        (Agent_protocol.Job.to_json current)
        (Agent_protocol.Job.to_json expected)
    then Ok ()
    else changed ()
  | Job_delivery expected ->
    let%bind current =
      Option.bind state.run_state ~f:(fun index ->
        Run_state.find_job_delivery index (Run_job_delivery.key expected))
      |> Result.of_option ~error:(error Conflict "Goal retained Job delivery is absent")
    in
    if Run_job_delivery.equal current expected then Ok () else changed ()
;;

let select
      (state : Session_state.t)
      ~now
      ~subscription_expired
      ~(find_job : job_lookup)
      ~(find_schedule : schedule_lookup)
      (request : Request.t)
  =
  let open Result.Let_syntax in
  let%bind () = validate_request state ~find_job ~find_schedule request in
  let classify_wake =
    classify
      state
      ~producer_ready:
        (producer_ready state ~now ~subscription_expired ~find_job ~find_schedule)
  in
  let wakes =
    Option.value_map state.run_state ~default:[] ~f:(fun index ->
      List.filter_map (Run_state.runs index) ~f:(fun run ->
        match run.Agent_protocol.Run.lifecycle with
        | Waiting wake -> Some wake
        | _ -> None))
  in
  let matching matches =
    match List.filter wakes ~f:matches with
    | [] -> Ok Selection.Ordinary
    | [ wake ] -> classify_wake wake
    | _ -> Error (error Conflict "producer references conflicting retained Wait owners")
  in
  match request with
  | Timer expected ->
    let%bind current = find_schedule expected.id in
    matching (fun wake ->
      match wake.occurrence with
      | Delivered_timer { schedule_id; _ } ->
        Agent_protocol.Id.Schedule.equal schedule_id current.id
      | Job_completion _ | Subscription_delivery _ -> false)
  | Job_completion expected ->
    let%bind current = find_job expected.id in
    matching (fun wake ->
      match wake.occurrence with
      | Job_completion { job_id; attempt } ->
        Agent_protocol.Id.Job.equal job_id current.id && Int.equal attempt current.attempt
      | Delivered_timer _ | Subscription_delivery _ -> false)
  | Job_delivery current ->
    matching (fun wake ->
      Option.exists
        (Run_job_delivery.Key.of_wake wake)
        ~f:(Run_job_delivery.Key.equal (Run_job_delivery.key current)))
  | Idle ->
    let%bind snapshot = Moderator_checkpoint.decode state.moderator in
    let queued =
      Option.exists snapshot ~f:(fun snapshot ->
        not (List.is_empty snapshot.queued_internal_events))
    in
    if queued
    then (
      let%bind heads =
        List.filter_map wakes ~f:(fun wake ->
          match wake.Agent_protocol.Run_wake.occurrence with
          | Subscription_delivery _ -> None
          | Job_completion _ | Delivered_timer _ -> Some wake)
        |> List.map ~f:(fun wake ->
          let%bind run = pending_run state wake in
          let%map matches =
            Option.value_map run ~default:(Ok false) ~f:(fun run ->
              queue_head state ~now ~subscription_expired run wake)
          in
          Option.some_if matches wake)
        |> Result.all
        |> Result.map ~f:List.filter_opt
      in
      match heads with
      | [] -> Ok Selection.Ordinary
      | [ wake ] -> classify_wake wake
      | _ -> Error (error Conflict "queue head has conflicting retained Goal wakes"))
    else (
      (* Same oldest-first durable wake order as Notification_delivery. A Wait
         locator which has no eligible delivery never suppresses other work. *)
      let deliveries =
        List.filter state.deliveries ~f:(fun delivery ->
          Int.equal
            delivery.Agent_protocol.Delivery.context.generation
            state.identity.generation
          && (Option.is_some delivery.context.ownership
              || Option.is_some delivery.completion_projection)
          &&
          match delivery.status, delivery.wake_disposition with
          | Committed _, Some Pending_wake -> true
          | (Pending | Failed _), _
          | Committed _, (None | Some (Accepted_wake _ | Discarded_wake _)) -> false)
        |> List.sort ~compare:(fun (left : Agent_protocol.Delivery.t) right ->
          match
            Agent_protocol.Timestamp.compare
              left.context.created_at
              right.context.created_at
          with
          | 0 -> Agent_protocol.Id.Delivery.compare left.context.id right.context.id
          | order -> order)
      in
      let rec select ~suspended = function
        | [] -> Ok (Option.value suspended ~default:Selection.Ordinary)
        | (delivery : Agent_protocol.Delivery.t) :: remaining ->
          let%bind selection =
            matching (fun wake ->
              match wake.occurrence with
              | Subscription_delivery { delivery_id; _ } ->
                Agent_protocol.Id.Delivery.equal delivery.context.id delivery_id
              | Job_completion _ | Delivered_timer _ -> false)
          in
          (match selection with
           | Ordinary | Wake _ -> Ok selection
           | No_work -> select ~suspended remaining
           | Suspended_goal _ ->
             select
               ~suspended:(Some (Option.value suspended ~default:selection))
               remaining)
      in
      select ~suspended:None deliveries)
;;
