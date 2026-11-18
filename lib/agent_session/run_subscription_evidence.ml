open! Core
module P = Agent_protocol

let invalid message = Error (P.Error.invalid_request message)

let rec selected = function
  | Session_delta.Subscription_changed value
  | Subscription_expired value
  | Subscription_cancelled value -> [ value ]
  | Batch changes -> List.concat_map changes ~f:selected
  | _ -> []
;;

let has_mutations delta = not (List.is_empty (selected delta))

let owns (run : P.Run.t) (subscription : P.Subscription.t) =
  List.exists run.owned_work ~f:(fun work ->
    Int.equal work.P.Run_work.generation subscription.context.generation
    && P.Run_work.Key.equal work.key (Retained (Subscription subscription.context.id)))
;;

let requires_completion_sample (state : Session_state.t) ~delta =
  Option.exists state.run_state ~f:(fun index ->
    List.exists (selected delta) ~f:(fun subscription ->
      Option.is_some subscription.P.Subscription.result
      && List.exists state.subscriptions ~f:(fun before ->
        P.Id.Subscription.equal before.context.id subscription.context.id
        && Option.is_none before.result)
      && List.exists (Run_state.runs index) ~f:(fun run ->
        (match run.P.Run.lifecycle with
         | Admitted | Active | Waiting _ -> true
         | Terminal _ -> false)
        && owns run subscription)))
;;

let capture
      (state : Session_state.t)
      ~(next : Session_state.t)
      ~delta
      ~(run : P.Run.t)
      ~owned_work
  =
  let open Result.Let_syntax in
  let selected = selected delta in
  Set.of_list (module P.Run_work) (run.owned_work @ owned_work)
  |> Set.to_list
  |> List.fold_result ~init:[] ~f:(fun proofs work ->
    match work.P.Run_work.key with
    | Retained (Subscription id) ->
      let changes =
        List.filter selected ~f:(fun value -> P.Id.Subscription.equal id value.context.id)
      in
      let before =
        List.find state.subscriptions ~f:(fun value ->
          P.Id.Subscription.equal id value.context.id)
      in
      let%bind _, winner =
        List.fold_result changes ~init:(before, None) ~f:(fun (previous, winner) value ->
          let%map () = P.Subscription.validate_transition ~previous value in
          let winner =
            match winner, previous, value.result with
            | None, Some { result = None; _ }, Some _ -> Some value
            | _ -> winner
          in
          Some value, winner)
      in
      (match winner with
       | None -> Ok proofs
       | Some winner ->
         let%bind actual =
           List.find next.subscriptions ~f:(fun value ->
             P.Id.Subscription.equal id value.context.id)
           |> Result.of_option
                ~error:(P.Error.invalid_request "winning Subscription disappeared")
         in
         let%bind () =
           if
             P.Subscription.equal winner actual
             && P.Id.Session.equal winner.context.session_id state.identity.session_id
             && Int.equal work.generation winner.context.generation
             && Int.equal work.generation run.source.generation
             && Option.exists
                  winner.context.source
                  ~f:(P.Invocation.equal_observer run.source.observer)
           then Ok ()
           else invalid "Subscription winner differs from its actual owned occurrence"
         in
         let%bind outcome =
           match winner.result with
           | Some (Succeeded _) -> Ok P.Run_work.Terminal.Succeeded
           | Some (Failed _) -> Ok Failed
           | Some (Cancelled _) -> Ok Cancelled
           | Some Expired -> Ok Limited
           | None -> invalid "Subscription evidence has no actual terminal winner"
         in
         (match
            List.find run.terminal_work ~f:(fun proof -> P.Run_work.equal proof.work work)
          with
          | Some proof ->
            if P.Run_work.Terminal.equal_outcome proof.outcome outcome
            then Ok proofs
            else invalid "Subscription evidence contradicts an immutable outcome"
          | None ->
            if Int64.equal run.revision Int64.max_value
            then invalid "Run revision exhausted at Subscription completion"
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
