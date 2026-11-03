open! Core
module P = Agent_protocol
module G = P.Goal
module E = P.Goal_episode

let invalid message = Error (P.Error.invalid_request message)

let retained_receipt index expected =
  match
    Run_state.receipt
      index
      ~principal_id:expected.P.Run_receipt.principal_id
      ~key:expected.key
      ~request_sha256:expected.request_sha256
  with
  | Ok (Some receipt) when P.Run_receipt.equal expected receipt -> Ok ()
  | Ok None | Ok (Some _) | Error _ ->
    invalid "goal evidence lacks its exact immutable retained Run receipt"
;;

let actual_run index episode =
  match Run_state.find index (E.admission episode).run_id with
  | Some run when E.matches_run episode run -> Ok run
  | None | Some _ -> invalid "goal episode lacks its actual retained Workflow"
;;

let episode index goal episode =
  let open Result.Let_syntax in
  let%bind _ = actual_run index episode in
  let%bind () = retained_receipt index (E.admission episode) in
  match Run_state.goal_link index (E.admission episode).run_id with
  | Some link
    when P.Id.Goal.equal (Run_goal_link.goal_id link) (G.id goal)
         && E.equal (Run_goal_link.episode link) episode -> Ok ()
  | None | Some _ -> invalid "goal episode differs from its permanent Run association"
;;

let settlement index goal proof =
  let open Result.Let_syntax in
  let associated = E.Settlement.episode proof in
  let receipt = E.Settlement.receipt proof in
  let%bind () = episode index goal associated in
  let%bind () = retained_receipt index receipt in
  let%bind run = actual_run index associated in
  Result.map (E.Settlement.create associated ~run ~receipt) ~f:(fun _ -> ())
;;

let goal index goal =
  let open Result.Let_syntax in
  let%bind () =
    Option.value_map (G.episode goal) ~default:(Ok ()) ~f:(episode index goal)
  in
  let%bind () =
    Option.value_map
      (G.previous_episode_settlement goal)
      ~default:(Ok ())
      ~f:(settlement index goal)
  in
  let%bind () =
    match G.achievement_request goal with
    | None -> Ok ()
    | Some request ->
      let associated = P.Goal_achievement.Request.episode request in
      let receipt = P.Goal_achievement.Request.receipt request in
      let%bind () = episode index goal associated in
      let%bind () = retained_receipt index receipt in
      if
        List.exists (Run_state.intents index) ~f:(fun intent ->
          P.Run_receipt.equal intent.Run_intent.receipt receipt
          && P.Run_action.equal intent.action (P.Goal_achievement.Request.action request))
      then Ok ()
      else invalid "goal achievement lacks its actual retained Finish intent"
  in
  let%bind () =
    Option.value_map
      (G.request_settlement goal)
      ~default:(Ok ())
      ~f:(settlement index goal)
  in
  match G.state goal with
  | Open _ | Closed (Abandoned _) -> Ok ()
  | Closed (Achieved achievement) ->
    let request = P.Goal_achievement.request achievement in
    let associated = P.Goal_achievement.Request.episode request in
    (* Abstract Goal validation already identifies this embedded request,
       settlement and current episode with the fields checked above. The lookup
       here makes the additional host-index fact explicit at successful closure. *)
    let%bind () = retained_receipt index (P.Goal_achievement.receipt achievement) in
    let%bind run = actual_run index associated in
    Result.map
      (P.Goal_achievement.create
         request
         ~run
         ~receipt:(P.Goal_achievement.receipt achievement))
      ~f:(fun _ -> ())
;;

let validate ~catalog ~runs =
  let index = Option.value runs ~default:Run_state.empty in
  let catalog = Option.value catalog ~default:Goal_catalog.empty in
  let open Result.Let_syntax in
  let%bind () =
    List.fold_result (Run_state.goal_links index) ~init:() ~f:(fun () link ->
      let associated = Run_goal_link.episode link in
      let admission = E.admission associated in
      match Goal_catalog.find catalog (Run_goal_link.goal_id link) with
      | Some goal
        when P.Session_ref.equal (G.session goal) (E.session associated)
             && P.Id.Principal.equal (G.creator goal) admission.principal_id
             && admission.source.generation <= (G.source goal).generation
             && Int64.(
                  admission.source.installation_epoch
                  <= (G.source goal).installation_epoch)
             && ((not
                    (Int64.equal
                       admission.source.installation_epoch
                       (G.source goal).installation_epoch))
                 || P.Run_source.equal admission.source (G.source goal)) ->
        let%bind _ = actual_run index associated in
        retained_receipt index admission
      | None | Some _ -> invalid "Run association has a foreign or absent retained goal")
  in
  List.fold_result (Goal_catalog.goals catalog) ~init:() ~f:(fun () value ->
    goal index value)
;;

let validate_transition ~previous_runs ~catalog ~runs =
  let open Result.Let_syntax in
  let%bind () = validate ~catalog ~runs in
  let previous = Option.value previous_runs ~default:Run_state.empty in
  let incoming = Option.value runs ~default:Run_state.empty in
  let catalog = Option.value catalog ~default:Goal_catalog.empty in
  List.fold_result (Run_state.goal_links incoming) ~init:() ~f:(fun () link ->
    match Run_state.goal_link previous (Run_goal_link.run_id link) with
    | Some _ -> Ok ()
    | None ->
      (match Goal_catalog.find catalog (Run_goal_link.goal_id link) with
       | Some goal
         when Option.equal E.equal (G.episode goal) (Some (Run_goal_link.episode link))
              && G.State.equal (G.state goal) (Open Enabled) -> Ok ()
       | None | Some _ ->
         invalid "new Run association requires its exact current Enabled goal episode"))
;;
