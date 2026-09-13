open Core
module P = Agent_protocol

let invalid message = Error (P.Error.create Journal_corrupt ~message ~retryable:false ())

let validate ~invocations ~events ~subscriptions (delivery : P.Delivery.t) =
  let open Result.Let_syntax in
  let%bind () = P.Delivery.validate delivery in
  match delivery.context.ownership with
  | None -> Ok ()
  | Some ownership ->
    let same_session id generation =
      Extension_invariants.owner
        ~session_id:delivery.context.session_id
        ~generation:delivery.context.generation
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
         | None -> invalid "delivery creating invocation is missing"
         | Some invocation ->
           let%bind () =
             same_session invocation.context.session_id invocation.context.generation
           in
           (match invocation.observation with
            | Some observation
              when not (P.Invocation.equal_observer observation.observer ownership.source)
              -> invalid "delivery source differs from its creating invocation observer"
            | _ -> Ok ()))
      | Moderator_event id ->
        (match
           List.find events ~f:(fun event ->
             P.Id.Moderator_execution.equal event.P.Moderator_execution.context.id id)
         with
         | None -> invalid "delivery creating moderator event is missing"
         | Some event ->
           let%bind () = same_session event.context.session_id event.context.generation in
           (match P.Invocation.equal_observer event.context.source ownership.source with
            | true -> Ok ()
            | false -> invalid "delivery source differs from its creating moderator event"))
    in
    (match delivery.context.work with
     | None | Some (Job _) -> Ok ()
     | Some (Subscription id) ->
       (match
          List.find subscriptions ~f:(fun subscription ->
            P.Id.Subscription.equal subscription.P.Subscription.context.id id)
        with
        | None -> invalid "delivery subscription is missing"
        | Some subscription ->
          let%bind () =
            same_session subscription.context.session_id subscription.context.generation
          in
          (match subscription.context.source with
           | Some source when P.Invocation.equal_observer source ownership.source -> Ok ()
           | _ -> invalid "delivery and subscription moderator sources differ")))
;;
