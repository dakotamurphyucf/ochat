open Core
module P = Agent_protocol

type t =
  { subscriptions : P.Subscription.t list
  ; schedules : P.Schedule.t list
  }

let is_empty t = List.is_empty t.subscriptions && List.is_empty t.schedules

let prepare ~state ~mode ~now =
  match mode with
  | P.Session.Graceful -> Ok { subscriptions = []; schedules = [] }
  | Cancel ->
    let open Result.Let_syntax in
    let%bind subscriptions =
      List.filter state.Session_state.subscriptions ~f:(fun subscription ->
        Option.is_some subscription.context.source && Option.is_none subscription.result)
      |> List.map ~f:(fun subscription ->
        (* A wall-clock rollback cannot prevent an explicit stop. Retained
           lifecycle timestamps cannot precede the record's creation. *)
        let at =
          match P.Timestamp.compare now subscription.context.created_at < 0 with
          | true -> subscription.context.created_at
          | false -> now
        in
        P.Subscription.finish
          subscription
          ~expected_epoch:subscription.epoch
          ~now:at
          (Cancelled "session stopped")
        |> Result.map ~f:fst)
      |> Result.all
    in
    let owned =
      List.filter state.schedules ~f:(fun timer -> Option.is_some timer.ownership)
    in
    let%bind claimed =
      match
        List.exists owned ~f:(fun timer ->
          match timer.status, timer.delivery_count, timer.delivery_cancellation with
          | Delivered, 1, None -> true
          | _ -> false)
      with
      | true -> Queued_moderator_event.claimed_timer_ids ~state
      | false -> Ok []
    in
    let claimed = Hash_set.of_list (module P.Id.Schedule) claimed in
    let schedules =
      List.filter_map owned ~f:(fun timer ->
        match timer.status, timer.delivery_count, timer.delivery_cancellation with
        | (Scheduled | Delivering), _, _ -> Some { timer with status = Cancelled }
        | Delivered, 1, None when not (Hash_set.mem claimed timer.id) ->
          Some { timer with delivery_cancellation = Some "session stopped" }
        | _ -> None)
    in
    Ok { subscriptions; schedules }
;;

let deltas t =
  List.map t.subscriptions ~f:(fun value -> Session_delta.Subscription_cancelled value)
  @ List.map t.schedules ~f:(fun value -> Session_delta.Schedule_changed value)
;;

let payloads t =
  List.map t.schedules ~f:(fun value ->
    P.Event.Durable.Payload.Schedule_state_changed value)
;;
