open Core
module P = Agent_protocol

let invalid message = Error (P.Error.create Journal_corrupt ~message ~retryable:false ())

let validate ~invocations ~events ~subscriptions (schedule : P.Schedule.t) =
  let open Result.Let_syntax in
  let%bind () = P.Schedule.validate schedule in
  match schedule.ownership with
  | None -> Ok ()
  | Some ownership ->
    let same_session id generation =
      Extension_invariants.owner
        ~session_id:schedule.session_id
        ~generation:schedule.generation
        id
        generation
    in
    let%bind () =
      match ownership.creator with
      | P.Job.Invocation id ->
        (match
           List.find invocations ~f:(fun invocation ->
             P.Id.Invocation.equal invocation.P.Invocation.context.id id)
         with
         | None -> invalid "schedule creating invocation is missing"
         | Some invocation ->
           let%bind () =
             same_session invocation.context.session_id invocation.context.generation
           in
           (match invocation.observation with
            | Some observation
              when not (P.Invocation.equal_observer observation.observer ownership.source)
              -> invalid "schedule source differs from creating invocation observer"
            | _ -> Ok ()))
      | Moderator_event id ->
        (match
           List.find events ~f:(fun event ->
             P.Id.Moderator_execution.equal event.P.Moderator_execution.context.id id)
         with
         | None -> invalid "schedule creating moderator event is missing"
         | Some event ->
           let%bind () = same_session event.context.session_id event.context.generation in
           (match P.Invocation.equal_observer event.context.source ownership.source with
            | true -> Ok ()
            | false -> invalid "schedule source differs from creating moderator event"))
    in
    (match ownership.subscription with
     | None -> Ok ()
     | Some (id, epoch) ->
       (match
          List.find subscriptions ~f:(fun subscription ->
            P.Id.Subscription.equal subscription.P.Subscription.context.id id)
        with
        | None -> invalid "schedule subscription is missing"
        | Some subscription ->
          let%bind () =
            same_session subscription.context.session_id subscription.context.generation
          in
          let%bind () =
            match subscription.context.source with
            | Some source when P.Invocation.equal_observer ownership.source source ->
              Ok ()
            | _ -> invalid "schedule and subscription sources differ"
          in
          let%bind () =
            match epoch <= subscription.epoch with
            | true -> Ok ()
            | false -> invalid "schedule references a future subscription epoch"
          in
          (match schedule.status with
           | Scheduled | Delivering ->
             (match subscription.result, subscription.timer_id with
              | None, Some timer
                when P.Id.Schedule.equal timer schedule.id
                     && Int.equal epoch subscription.epoch -> Ok ()
              | _ -> invalid "active schedule has an obsolete subscription binding")
           | Delivered | Cancelled | Failed _ -> Ok ())))
;;
