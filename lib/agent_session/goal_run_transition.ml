open! Core
module P = Agent_protocol
module G = P.Goal

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let prepare_request (state : Session_state.t) ~scope ~prepared ~intent =
  let open Result.Let_syntax in
  let%bind original =
    Result.of_option
      state.run_state
      ~error:(P.Error.invalid_request "goal Run index absent")
  in
  let%bind catalog =
    Result.of_option
      state.goal_catalog
      ~error:(P.Error.invalid_request "goal catalog absent")
  in
  let%bind link =
    Result.of_option
      (Run_state.goal_link original (Run_scope.run_id scope))
      ~error:
        (P.Error.invalid_request "Goal.finish requires its actual associated Workflow")
  in
  let%bind goal =
    Result.of_option
      (Goal_catalog.find catalog (Run_goal_link.goal_id link))
      ~error:(P.Error.invalid_request "associated goal absent")
  in
  let%bind () =
    if
      Run_scope.is_open scope
      && Int64.equal
           (G.revision goal)
           (P.Goal_achievement.Intent.expected_goal_revision intent)
      && P.Id.Principal.equal (G.creator goal) (Run_scope.principal_id scope)
      && P.Run_source.equal (G.source goal) (Run_scope.source scope)
      && Option.equal
           P.Goal_episode.equal
           (G.episode goal)
           (Some (Run_goal_link.episode link))
      &&
      match G.state goal with
      | Open _ -> true
      | Closed _ -> false
    then Ok ()
    else
      conflict
        "goal finish source, principal, episode or revision changed before checkpoint"
  in
  let%bind index =
    match Run_transition.delta prepared with
    | Session_delta.Run_state_changed index -> Ok index
    | _ -> conflict "goal finish lost its actual Run action candidate"
  in
  let run = Run_transition.run prepared in
  let%bind () =
    if P.Id.Run.equal run.id (Run_scope.run_id scope)
    then Ok ()
    else conflict "goal finish candidate belongs to another Run"
  in
  let candidates =
    List.filter (Run_state.intents index) ~f:(fun action ->
      P.Id.Run.equal action.Run_intent.receipt.run_id run.id
      && P.Id.Moderator_execution.equal action.execution_id (Run_scope.execution_id scope)
      && Int64.equal action.receipt.run_revision run.revision
      && P.Run_action.equal action.action (P.Goal_achievement.Intent.action intent))
  in
  let%bind receipt =
    match candidates with
    | [ action ] -> Ok action.Run_intent.receipt
    | [] | _ :: _ :: _ ->
      conflict "goal finish lacks its exact actual prepared Action receipt"
  in
  let%bind request =
    P.Goal_achievement.Request.create
      ~episode:(Run_goal_link.episode link)
      ~run
      ~receipt
      ~action:(P.Goal_achievement.Intent.action intent)
      ~evidence:(P.Goal_achievement.Intent.evidence intent)
  in
  let%bind goal = G.request_achievement goal request in
  let%bind document =
    Result.of_option
      (Goal_catalog.document catalog (G.id goal))
      ~error:(P.Error.invalid_request "associated goal document absent")
  in
  let%bind document =
    Goal_document.update document goal
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
  in
  let%map catalog = Goal_catalog.advance_existing catalog ~goal:document in
  [ Session_delta.Goal_catalog_changed catalog ]
;;
