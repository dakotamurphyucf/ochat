open! Core
module P = Agent_protocol
module G = P.Goal

let invalid message = Error (P.Error.invalid_request message)

let terminal_receipt index (run : P.Run.t) =
  match
    List.filter (Run_state.receipts index) ~f:(fun receipt ->
      P.Run_receipt.Kind.equal receipt.P.Run_receipt.kind Terminal
      && P.Id.Run.equal receipt.run_id run.id
      && Int64.equal receipt.run_revision run.revision)
  with
  | [ receipt ] -> Ok receipt
  | [] | _ :: _ :: _ ->
    invalid "goal settlement requires its exact actual terminal receipt"
;;

let hold goal =
  match G.state goal with
  | Open Enabled -> Result.map (G.hold goal Source_review) ~f:Option.some
  | Open (Held _) | Closed _ -> Ok None
;;

let current_adverse_hold index goal =
  let open Result.Let_syntax in
  match G.state goal, G.episode goal with
  | Open Enabled, Some episode ->
    let%bind run =
      Result.of_option
        (Run_state.find index (P.Goal_episode.admission episode).run_id)
        ~error:(P.Error.invalid_request "associated goal episode Run absent")
    in
    (match run.lifecycle with
     | Terminal (Failed _ | Cancelled | Limited | Interrupted) ->
       let%bind receipt = terminal_receipt index run in
       let%bind _ = P.Goal_episode.Settlement.create episode ~run ~receipt in
       hold goal
     | Admitted | Active | Waiting _ | Terminal (Completed _) -> Ok None)
  | (Open _ | Closed _), (None | Some _) -> Ok None
;;

let settle index goal =
  let open Result.Let_syntax in
  match G.state goal, G.achievement_request goal, G.request_settlement goal with
  | Open _, Some request, None ->
    let episode = P.Goal_achievement.Request.episode request in
    let%bind run =
      Result.of_option
        (Run_state.find index (P.Goal_episode.admission episode).run_id)
        ~error:(P.Error.invalid_request "goal settlement actual Run absent")
    in
    (match run.lifecycle with
     | Admitted | Active | Waiting _ -> Ok None
     | Terminal outcome ->
       let%bind receipt = terminal_receipt index run in
       let%bind _ = P.Goal_episode.Settlement.create episode ~run ~receipt in
       (match outcome with
        | Failed _ | Cancelled | Limited | Interrupted ->
          Result.map (G.retire_request goal ~run ~receipt) ~f:Option.some
        | Completed result ->
          if
            Option.equal
              P.Run_result_reference.equal
              result
              (P.Goal_achievement.Request.requested_result request)
          then (
            let%bind achievement = P.Goal_achievement.create request ~run ~receipt in
            Result.map (G.achieve goal achievement) ~f:Option.some)
          else
            (* Impossible under valid same-request Finish staging. Record actual
               Run outcome, retain unresolved request, and preserve operator hold;
               never invent adverse evidence or claim a different completion. *)
            hold goal))
  | (Open _ | Closed _), (None | Some _), (None | Some _) ->
    current_adverse_hold index goal
;;

let plan_catalog catalog ~runs ~hold_for_review =
  let open Result.Let_syntax in
  let index = Option.value runs ~default:Run_state.empty in
  let%bind goals =
    List.fold_result (Goal_catalog.goals catalog) ~init:[] ~f:(fun changes goal ->
      let%bind changed = settle index goal in
      let%bind changed =
        match changed with
        | Some _ -> Ok changed
        | None -> if hold_for_review then hold goal else Ok None
      in
      match changed with
      | None -> Ok changes
      | Some incoming ->
        let%bind original =
          Result.of_option
            (Goal_catalog.document catalog (G.id goal))
            ~error:(P.Error.invalid_request "goal settlement document absent")
        in
        let%map document =
          Goal_document.update original incoming
          |> Result.map_error ~f:(fun error ->
            P.Error.invalid_request
              (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
        in
        document :: changes)
  in
  Goal_catalog.advance_existing_many catalog ~goals:(List.rev goals)
;;
