open! Core
module P = Agent_protocol

let invalid message = Error (P.Error.invalid_request message)

type mutation =
  | Changed of P.Delivery.t
  | Committed of P.Delivery.t * P.History.entry
  | Wake of P.Delivery.t

let value = function
  | Changed value | Committed (value, _) | Wake value -> value
;;

let rec selected = function
  | Session_delta.Delivery_changed value -> [ Changed value ]
  | Delivery_committed (value, entry) -> [ Committed (value, entry) ]
  | Delivery_wake_changed value -> [ Wake value ]
  | Batch changes -> List.concat_map changes ~f:selected
  | _ -> []
;;

let has_mutations delta = not (List.is_empty (selected delta))

let owns (run : P.Run.t) (delivery : P.Delivery.t) =
  List.exists run.owned_work ~f:(fun work ->
    Int.equal work.P.Run_work.generation delivery.context.generation
    && P.Run_work.Key.equal work.key (Retained (Delivery delivery.context.id)))
;;

let final = function
  | P.Delivery.Committed _ -> true
  | Failed error -> not error.retryable
  | Pending -> false
;;

let requires_completion_sample (state : Session_state.t) ~delta =
  Option.exists state.run_state ~f:(fun index ->
    List.exists (selected delta) ~f:(fun change ->
      let delivery = value change in
      final delivery.status
      && List.exists state.deliveries ~f:(fun before ->
        P.Id.Delivery.equal before.context.id delivery.context.id
        && P.Delivery.equal_status before.status Pending)
      && List.exists (Run_state.runs index) ~f:(fun run ->
        (match run.P.Run.lifecycle with
         | Admitted | Active | Waiting _ -> true
         | Terminal _ -> false)
        && owns run delivery)))
;;

let check_retries (state : Session_state.t) ~delta =
  let rec restored = function
    | Session_delta.Created state -> [ state ]
    | Batch changes -> List.concat_map changes ~f:restored
    | _ -> []
  in
  let restored = restored delta in
  let indexes =
    List.filter_opt
      (state.run_state
       :: List.map restored ~f:(fun state -> state.Session_state.run_state))
  in
  let deliveries =
    List.map (selected delta) ~f:value
    @ List.concat_map restored ~f:(fun state -> state.Session_state.deliveries)
  in
  List.fold_result deliveries ~init:() ~f:(fun () delivery ->
    match delivery.P.Delivery.status with
    | Pending ->
      let blocked =
        List.exists indexes ~f:(fun index ->
          List.exists (Run_state.runs index) ~f:(fun run ->
            List.exists run.P.Run.terminal_work ~f:(fun proof ->
              P.Run_work.Key.equal
                proof.work.key
                (Retained (Delivery delivery.context.id))
              && P.Run_work.Terminal.equal_outcome proof.outcome Failed)))
      in
      if blocked
      then invalid "immutable failed owned Delivery cannot be retried"
      else Ok ()
    | Committed _ | Failed _ -> Ok ())
;;

let subscription (next : Session_state.t) (delivery : P.Delivery.t) =
  let open Result.Let_syntax in
  match delivery.context.work, delivery.context.ownership with
  | Some (Subscription id), Some ownership ->
    let%bind retained =
      List.find next.subscriptions ~f:(fun value ->
        P.Id.Subscription.equal id value.context.id)
      |> Result.of_option
           ~error:(P.Error.invalid_request "Delivery subscription is not retained")
    in
    if
      P.Id.Session.equal retained.context.session_id delivery.context.session_id
      && Int.equal retained.context.generation delivery.context.generation
      && Option.exists
           retained.context.source
           ~f:(P.Invocation.equal_observer ownership.source)
      && Option.exists
           delivery.context.invocation_id
           ~f:(P.Id.Invocation.equal retained.context.invocation_id)
      && Option.exists ownership.subscription_binding ~f:(fun binding ->
        P.Id.Subscription.equal binding.subscription_id id
        && Int.equal binding.epoch retained.epoch)
      && Option.exists retained.result ~f:(P.Completion.equal delivery.context.completion)
    then Ok ()
    else invalid "Delivery differs from its actual Subscription winner and epoch"
  | Some (Subscription _), None ->
    invalid "owned subscription Delivery has no source binding"
  | (None | Some (Job _)), _ -> Ok ()
;;

let capture
      (state : Session_state.t)
      ~(next : Session_state.t)
      ~delta
      ~(run : P.Run.t)
      ~owned_work
  =
  let open Result.Let_syntax in
  let%bind () = check_retries state ~delta in
  let selected = selected delta in
  Set.of_list (module P.Run_work) (run.owned_work @ owned_work)
  |> Set.to_list
  |> List.fold_result ~init:[] ~f:(fun proofs work ->
    match work.P.Run_work.key with
    | Retained (Delivery id) ->
      let changes =
        List.filter selected ~f:(fun change ->
          P.Id.Delivery.equal id (value change).context.id)
      in
      let before =
        List.find state.deliveries ~f:(fun value ->
          P.Id.Delivery.equal id value.context.id)
      in
      let%bind _, winner =
        List.fold_result changes ~init:(before, None) ~f:(fun (previous, winner) change ->
          let delivery = value change in
          let%bind () = P.Delivery.validate_transition ~previous delivery in
          let%bind () =
            match winner, delivery.status with
            | Some _, Pending ->
              invalid "final owned Delivery was reused in its terminal transaction"
            | _ -> Ok ()
          in
          let%map winner =
            match winner, previous, change with
            | None, Some { status = Pending; _ }, Committed (delivery, entry) ->
              let%bind () = Notification_history.validate ~delivery entry in
              let%map () =
                if
                  List.exists
                    next.conversation.canonical_history
                    ~f:(P.History.equal_entry entry)
                then Ok ()
                else
                  invalid
                    "committed Delivery history is absent from its actual publication"
              in
              Some (delivery, P.Run_work.Terminal.Succeeded)
            | ( None
              , Some { status = Pending; _ }
              , Changed ({ status = Failed error; _ } as delivery) )
              when not error.retryable -> Ok (Some (delivery, Failed))
            | _ -> Ok winner
          in
          Some delivery, winner)
      in
      (match winner with
       | None -> Ok proofs
       | Some (winner, outcome) ->
         let%bind actual =
           List.find next.deliveries ~f:(fun value ->
             P.Id.Delivery.equal id value.context.id)
           |> Result.of_option
                ~error:(P.Error.invalid_request "terminal Delivery disappeared")
         in
         let%bind () = P.Delivery.validate_transition ~previous:(Some winner) actual in
         let%bind () =
           if
             P.Delivery.equal_status winner.status actual.status
             && Int.equal winner.attempt actual.attempt
             && P.Id.Session.equal winner.context.session_id state.identity.session_id
             && Int.equal work.generation winner.context.generation
             && Int.equal work.generation run.source.generation
             && Option.exists winner.context.ownership ~f:(fun owner ->
               P.Invocation.equal_observer owner.source run.source.observer)
           then Ok ()
           else
             invalid
               "Delivery terminal evidence differs from its actual owned publication"
         in
         let%bind () = subscription next winner in
         (match
            List.find run.terminal_work ~f:(fun proof -> P.Run_work.equal proof.work work)
          with
          | Some proof ->
            if P.Run_work.Terminal.equal_outcome proof.outcome outcome
            then Ok proofs
            else invalid "Delivery evidence contradicts an immutable outcome"
          | None ->
            if Int64.equal run.revision Int64.max_value
            then invalid "Run revision exhausted at Delivery completion"
            else (
              let%map proof =
                P.Run_work.Terminal.create
                  ~work
                  ~outcome
                  ~revision:(Int64.succ run.revision)
              in
              proof :: proofs)))
    | Operation _ | Retained _ -> Ok proofs)
;;
