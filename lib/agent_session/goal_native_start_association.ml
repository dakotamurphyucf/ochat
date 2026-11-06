open! Core
module P = Agent_protocol
module R = P.Goal_native_start_request

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())
let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())

let intent request =
  Goal_episode_admission.intent
    ~goal_id:(R.goal_id request)
    ~expected_goal_revision:(R.expected_goal_revision request)
    ~expected_catalog_revision:(R.expected_catalog_revision request)
;;

let check_previous_episode state goal =
  let open Result.Let_syntax in
  match P.Goal.episode goal with
  | None -> Ok ()
  | Some episode ->
    let%bind index =
      Result.of_option
        state.Session_state.run_state
        ~error:(P.Error.invalid_request "native Goal Start previous Run index is absent")
    in
    let%bind run =
      Result.of_option
        (Run_state.find index (P.Goal_episode.admission episode).run_id)
        ~error:(P.Error.invalid_request "native Goal Start previous Run is absent")
    in
    let receipts =
      List.filter (Run_state.receipts index) ~f:(fun receipt ->
        P.Id.Run.equal receipt.P.Run_receipt.run_id run.id
        && P.Run_receipt.Kind.equal receipt.kind Terminal
        && Int64.equal receipt.run_revision run.revision)
    in
    (match receipts with
     | [ receipt ] ->
       P.Goal_episode.Settlement.create episode ~run ~receipt |> Result.map ~f:ignore
     | [] | _ :: _ :: _ ->
       conflict "native Goal Start requires its previous actual terminal receipt")
;;

let check_request request ~state ~session ~principal_id =
  let open Result.Let_syntax in
  let%bind catalog =
    Result.of_option
      state.Session_state.goal_catalog
      ~error:(P.Error.invalid_request "native Goal Start catalog is absent")
  in
  let%bind goal =
    Result.of_option
      (Goal_catalog.find catalog (R.goal_id request))
      ~error:(P.Error.invalid_request "native Goal Start target is absent")
  in
  let%bind _, source = Goal_catalog_source.capture state in
  let%bind () =
    if
      P.Id.Session.equal (P.Session_ref.session_id session) state.identity.session_id
      && P.Session_ref.equal (P.Goal.session goal) session
      && Int.equal (R.generation request) state.identity.generation
      && P.Run_source.equal (R.source request) source
      && P.Run_source.equal (P.Goal.source goal) source
      && P.Id.Principal.equal (P.Goal.creator goal) principal_id
      && Int64.equal (R.expected_goal_revision request) (P.Goal.revision goal)
      && Int64.equal (R.expected_catalog_revision request) (Goal_catalog.revision catalog)
    then Ok ()
    else conflict "native Goal Start immutable source, owner or target revision changed"
  in
  let%bind () =
    match P.Goal.state goal with
    | Open (Held Operator_pause) ->
      denied "native Goal Start cannot clear an operator pause"
    | Open (Enabled | Held _) -> Ok ()
    | Closed _ -> conflict "native Goal Start target is closed"
  in
  check_previous_episode state goal
;;

let check_admission stored ~state ~session ~(admission : P.Run_receipt.t) =
  let open Result.Let_syntax in
  let request = P.Goal_native_start_intent.request stored in
  let%bind digest = R.digest request in
  let%bind expected_intent = intent request in
  let%bind () =
    if
      P.Run_receipt.Kind.equal admission.kind Admission
      && P.Id.Principal.equal
           admission.principal_id
           (P.Goal_native_start_intent.principal_id stored)
      && P.Idempotency_key.equal admission.key (R.key request)
      && String.equal admission.request_sha256 digest
      && P.Run_source.equal admission.source (R.source request)
    then Ok ()
    else conflict "native Goal Start receipt immutable command association changed"
  in
  let%bind index =
    Result.of_option
      state.Session_state.run_state
      ~error:(P.Error.invalid_request "native Goal Start receipt has no Run index")
  in
  let%bind run =
    Result.of_option
      (Run_state.find index admission.run_id)
      ~error:(P.Error.invalid_request "native Goal Start receipt Run is absent")
  in
  let%bind link =
    Result.of_option
      (Run_state.goal_link index run.id)
      ~error:(P.Error.invalid_request "native Goal Start historical Goal link is absent")
  in
  let episode = Run_goal_link.episode link in
  let%bind () =
    if
      P.Session_ref.equal run.session session
      && P.Id.Session.equal (P.Session_ref.session_id session) state.identity.session_id
      && P.Run.Mode.equal run.mode Workflow
      && P.Id.Principal.equal run.principal_id admission.principal_id
      && P.Run_source.equal run.source (R.source request)
      && P.Id.Goal.equal (Run_goal_link.goal_id link) (R.goal_id request)
      && String.equal
           (Run_goal_link.intent_sha256 link)
           (Goal_episode_admission.digest expected_intent)
      && P.Goal_episode.matches_run episode run
      && P.Run_receipt.equal (P.Goal_episode.admission episode) admission
      && List.exists (Run_state.receipts index) ~f:(P.Run_receipt.equal admission)
    then Ok ()
    else conflict "native Goal Start lost its actual historical admission association"
  in
  Goal_run_evidence.validate ~catalog:state.goal_catalog ~runs:state.run_state
;;
