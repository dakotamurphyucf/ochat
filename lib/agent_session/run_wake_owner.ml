open! Core
module P = Agent_protocol

let validate (state : Session_state.t) ~(run : P.Run.t) ~(wake : P.Run_wake.t) =
  let owns key =
    List.exists run.owned_work ~f:(fun work ->
      P.Run_work.Key.equal work.key (Retained key)
      && Int.equal work.generation run.source.generation)
  in
  let source = P.Invocation.equal_observer run.source.observer in
  let valid =
    P.Id.Run.equal run.id wake.run_id
    && P.Run_source.equal run.source wake.source
    &&
    match wake.occurrence with
    | Job_completion { job_id; attempt } ->
      owns (Job { id = job_id; attempt })
      && List.exists state.jobs ~f:(fun (job : P.Job.t) ->
        P.Id.Job.equal job.id job_id
        && Option.exists
             (List.find run.owned_work ~f:(fun work ->
                P.Run_work.Key.equal work.key (Retained (Job { id = job_id; attempt }))))
             ~f:(fun work -> Run_job_occurrence.matches job ~work)
        && Int.equal job.generation wake.source.generation
        && Option.exists job.launch ~f:(fun launch ->
          Option.exists launch.moderator_source ~f:source))
    | Delivered_timer { schedule_id; delivery_count; creator; subscription } ->
      owns (Schedule schedule_id)
      && List.exists state.schedules ~f:(fun (schedule : P.Schedule.t) ->
        P.Id.Schedule.equal schedule.id schedule_id
        && Int.equal schedule.generation wake.source.generation
        && (match schedule.status with
            | Scheduled | Delivering ->
              schedule.delivery_count < Int.max_value
              && Int.equal (Int.succ schedule.delivery_count) delivery_count
            | Delivered -> Int.equal schedule.delivery_count delivery_count
            | Cancelled | Failed _ -> false)
        && Option.is_none schedule.delivery_cancellation
        && Option.exists schedule.ownership ~f:(fun owner ->
          source owner.source
          && P.Job.equal_launch_owner owner.creator creator
          && Option.equal
               (fun (left, left_epoch) (right, right_epoch) ->
                  P.Id.Subscription.equal left right && Int.equal left_epoch right_epoch)
               owner.subscription
               subscription))
    | Subscription_delivery { subscription_id; epoch; delivery_id; creator } ->
      (owns (Subscription subscription_id) || owns (Delivery delivery_id))
      && List.exists state.deliveries ~f:(fun (delivery : P.Delivery.t) ->
        P.Id.Delivery.equal delivery.context.id delivery_id
        && Int.equal delivery.context.generation wake.source.generation
        && Option.exists delivery.context.ownership ~f:(fun owner ->
          source owner.source
          && P.Job.equal_launch_owner owner.creator creator
          && Option.exists owner.subscription_binding ~f:(fun binding ->
            P.Id.Subscription.equal binding.subscription_id subscription_id
            && Int.equal binding.epoch epoch)))
  in
  if valid
  then Ok ()
  else
    Error (P.Error.invalid_request "run wake differs from its retained owner occurrence")
;;

let claim_matches
      (state : Session_state.t)
      ~(run : P.Run.t)
      ~(wake : P.Run_wake.t)
      ~(executing : P.Moderator_execution.t)
  =
  let open Result.Let_syntax in
  let context = executing.context in
  let owner_matches =
    P.Id.Session.equal context.session_id state.identity.session_id
    && P.Id.Session.equal context.session_id (P.Session_ref.session_id run.session)
    && Int.equal context.generation state.identity.generation
    && Int.equal context.generation wake.source.generation
    && P.Invocation.equal_observer context.source wake.source.observer
    && P.Run_source.equal run.source wake.source
  in
  match context.phase, executing.status, context.event with
  | Internal_event, Running, `Object fields when owner_matches ->
    (match List.Assoc.find fields ~equal:String.equal "snapshot" with
     | None -> Ok false
     | Some json ->
       let%bind snapshot =
         Session.Snapshot.of_jsonaf json |> Result.map_error ~f:P.Error.invalid_request
       in
       let%bind value =
         Session.Snapshot.to_value snapshot |> Result.map_error ~f:P.Error.invalid_request
       in
       let%bind matches =
         match wake.occurrence with
         | Job_completion { job_id; attempt } ->
           let%bind frame =
             Chat_response.Background_delivery.decode value
             |> Result.map_error ~f:P.Error.invalid_request
           in
           (match frame with
            | None -> Ok false
            | Some frame ->
              if
                not
                  (P.Id.Job.equal frame.job_id job_id
                   && Int.equal frame.attempt attempt
                   && P.Id.Session.equal frame.session_id context.session_id
                   && Int.equal frame.generation context.generation
                   && P.Invocation.equal_observer frame.source context.source)
              then Ok false
              else (
                let%bind retained =
                  match state.run_state with
                  | None -> Ok None
                  | Some index -> Run_state.enqueued_job_frame index ~frame
                in
                match retained with
                | Some delivery ->
                  Ok
                    (P.Id.Run.equal (Run_job_delivery.run_id delivery) run.id
                     && P.Run_source.equal (Run_job_delivery.source delivery) wake.source
                    )
                | None ->
                  let%map () = validate state ~run ~wake in
                  true))
         | Delivered_timer { schedule_id; delivery_count; creator; subscription } ->
           let%map timer =
             Chat_response.Schedule_delivery.decode value
             |> Result.map_error ~f:P.Error.invalid_request
           in
           Option.exists timer ~f:(fun timer ->
             P.Id.Schedule.equal timer.id schedule_id
             && timer.delivery_count < Int.max_value
             && Int.equal (Int.succ timer.delivery_count) delivery_count
             && P.Id.Session.equal timer.session_id context.session_id
             && Int.equal timer.generation context.generation
             && Option.exists timer.ownership ~f:(fun ownership ->
               P.Invocation.equal_observer ownership.source context.source
               && P.Job.equal_launch_owner ownership.creator creator
               && Option.equal
                    (fun (left, left_epoch) (right, right_epoch) ->
                       P.Id.Subscription.equal left right
                       && Int.equal left_epoch right_epoch)
                    ownership.subscription
                    subscription))
         | Subscription_delivery _ -> Ok false
       in
       if not matches
       then Ok false
       else (
         match wake.occurrence with
         | Job_completion _ -> Ok true
         | Delivered_timer _ | Subscription_delivery _ ->
           let%map () = validate state ~run ~wake in
           true))
  | _ -> Ok false
;;
