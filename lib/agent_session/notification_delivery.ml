open Core
module P = Agent_protocol
module B = Chat_response.Background_request

type action =
  | Publish of P.Delivery.t
  | Fail of P.Delivery.t

type t =
  { session_id : P.Id.Session.t
  ; generation : int
  ; revision : int64
  ; source : P.Invocation.observer
  ; actions : action list
  }

type idle =
  { pending : t
  ; wakes : P.Delivery.t list
  ; discarded_wakes : P.Delivery.t list
  }

let current_owned (state : Session_state.t) (value : P.Delivery.t) =
  Int.equal value.context.generation state.identity.generation
  && Option.is_some value.context.ownership
;;

let pending_wake (value : P.Delivery.t) =
  match value.status, value.wake_disposition with
  | Committed _, Some Pending_wake -> true
  | _ -> false
;;

let has_idle_work (state : Session_state.t) =
  List.exists state.deliveries ~f:(fun value ->
    current_owned state value
    &&
    match value.status with
    | Pending -> true
    | _ -> pending_wake value)
;;

let oldest values ~max_count =
  List.sort values ~compare:(fun a b ->
    match P.Timestamp.compare a.P.Delivery.context.created_at b.context.created_at with
    | 0 -> P.Id.Delivery.compare a.context.id b.context.id
    | order -> order)
  |> Fn.flip List.take max_count
;;

let access ~(state : Session_state.t) ~current_capabilities ~policy (value : P.Delivery.t)
  =
  let open Result.Let_syntax in
  let%bind selected =
    Script_notification_service.validate_disclosure ~current_capabilities value
  in
  match value.context.work with
  | None | Some (P.Invocation.Subscription _) -> Ok ()
  | Some (Job id) ->
    let%bind job =
      List.find state.jobs ~f:(fun job -> P.Id.Job.equal job.id id)
      |> Result.of_option ~error:(P.Error.invalid_request "missing notification job")
    in
    let%bind request = B.of_json ~policy job.payload in
    B.validate_capabilities request ~capabilities:selected
;;

let failure value code message =
  P.Delivery.fail value { code; message; retryable = false; details = `Null }
  |> Result.map ~f:(fun value -> Some (Fail value))
;;

let prepare ~(state : Session_state.t) ~source ~current_capabilities ~policy ~max_count =
  let open Result.Let_syntax in
  let%bind () =
    if max_count > 0
    then Ok ()
    else Error (P.Error.invalid_request "notification batch limit must be positive")
  in
  let pending =
    List.filter state.deliveries ~f:(fun value ->
      current_owned state value
      &&
      match value.status with
      | Pending -> true
      | Committed _ | Failed _ -> false)
    |> oldest ~max_count
  in
  let%map actions =
    List.map pending ~f:(fun value ->
      let owner = Option.value_exn value.context.ownership in
      match P.Invocation.equal_observer owner.source source with
      | false ->
        failure
          value
          "notification.source_changed"
          "The notification publisher is no longer installed."
      | true ->
        (match access ~state ~current_capabilities ~policy value with
         | Error _ ->
           failure
             value
             "notification.disclosure_denied"
             "The notification is outside the current captured disclosure scope."
         | Ok () ->
           (match
              Notification_readiness.check
                ~invocations:state.invocations
                ~jobs:state.jobs
                ~events:state.moderator_executions
                value
            with
            | Ok () -> Ok (Some (Publish value))
            | Error (Waiting _) -> Ok None
            | Error (Rejected _) ->
              failure
                value
                "notification.acknowledgement_unavailable"
                "The notification acknowledgement cannot be delivered.")))
    |> Result.all
  in
  { session_id = state.identity.session_id
  ; generation = state.identity.generation
  ; revision = state.counters.revision
  ; source
  ; actions = List.filter_opt actions
  }
;;

let prepare_idle ~state ~source ~current_capabilities ~policy ~max_count =
  let open Result.Let_syntax in
  let%bind pending = prepare ~state ~source ~current_capabilities ~policy ~max_count in
  let candidates =
    List.filter state.deliveries ~f:(fun value ->
      current_owned state value && pending_wake value)
    |> oldest ~max_count
  in
  let%map wakes, discarded_wakes =
    List.fold_result candidates ~init:([], []) ~f:(fun (wakes, discarded) value ->
      let owner = Option.value_exn value.context.ownership in
      let permitted =
        P.Invocation.equal_observer owner.source source
        && Result.is_ok (access ~state ~current_capabilities ~policy value)
      in
      match permitted with
      | true -> Ok (value :: wakes, discarded)
      | false ->
        let%map value =
          P.Delivery.discard_wake
            value
            ~reason:
              "notification wake is outside the current publisher or disclosure scope"
        in
        wakes, value :: discarded)
  in
  { pending; wakes = List.rev wakes; discarded_wakes = List.rev discarded_wakes }
;;
