open! Core
module P = Agent_protocol
module G = P.Goal

type intent =
  { goal_id : P.Id.Goal.t
  ; expected_goal_revision : int64
  ; expected_catalog_revision : int64
  }

let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let intent ~goal_id ~expected_goal_revision ~expected_catalog_revision =
  let open Result.Let_syntax in
  let%bind _ = P.Id.Goal.of_json (P.Id.Goal.to_json goal_id) in
  if Int64.(expected_goal_revision >= 0L && expected_catalog_revision >= 0L)
  then Ok { goal_id; expected_goal_revision; expected_catalog_revision }
  else Error (P.Error.invalid_request "goal admission revisions must be nonnegative")
;;

let digest intent =
  `Object
    [ "version", `String "goal-episode-admission-v1"
    ; "goal_id", P.Id.Goal.to_json intent.goal_id
    ; "expected_goal_revision", `String (Int64.to_string intent.expected_goal_revision)
    ; ( "expected_catalog_revision"
      , `String (Int64.to_string intent.expected_catalog_revision) )
    ]
  |> Jsonaf.to_string
  |> Digestif.SHA256.digest_string
  |> Digestif.SHA256.to_hex
;;

type t =
  { run_state : Run_state.t
  ; catalog : Goal_catalog.t
  ; goal : G.t
  ; link : Run_goal_link.t
  }

let run_state t = t.run_state
let catalog t = t.catalog
let goal t = t.goal
let link t = t.link

let previous_terminal index episode =
  let open Result.Let_syntax in
  let%bind run =
    Result.of_option
      (Run_state.find index (P.Goal_episode.admission episode).run_id)
      ~error:(P.Error.invalid_request "previous goal episode Run is absent")
  in
  let candidates =
    List.filter (Run_state.receipts index) ~f:(fun receipt ->
      P.Id.Run.equal receipt.P.Run_receipt.run_id run.id
      && P.Run_receipt.Kind.equal receipt.kind Terminal
      && Int64.equal receipt.run_revision run.revision)
  in
  match candidates with
  | [ receipt ] ->
    let%map _ = P.Goal_episode.Settlement.create episode ~run ~receipt in
    run, receipt
  | [] | _ :: _ :: _ ->
    conflict "goal episode replacement requires its exact actual terminal receipt"
;;

let prepare_facts
      (state : Session_state.t)
      ~(run : P.Run.t)
      ~index
      ~receipt
      ~(intent : intent)
      ~enable
  =
  let open Result.Let_syntax in
  let original_runs = Option.value state.run_state ~default:Run_state.empty in
  let catalog = Option.value state.goal_catalog ~default:Goal_catalog.empty in
  let%bind goal =
    Result.of_option
      (Goal_catalog.find catalog intent.goal_id)
      ~error:(P.Error.invalid_request "goal admission target is absent")
  in
  let%bind () =
    if
      Int64.equal intent.expected_goal_revision (G.revision goal)
      && Int64.equal intent.expected_catalog_revision (Goal_catalog.revision catalog)
      && (match G.state goal with
          | Open Enabled -> true
          | Open (Held _) -> enable
          | Closed _ -> false)
      && P.Id.Principal.equal (G.creator goal) run.principal_id
      && P.Session_ref.equal (G.session goal) run.session
      && P.Id.Session.equal
           (P.Session_ref.session_id run.session)
           state.identity.session_id
      && P.Run_source.equal (G.source goal) run.source
      && Int.equal run.source.generation state.identity.generation
      && P.Run.Mode.equal run.mode Workflow
      && P.Run.Lifecycle.equal run.lifecycle Admitted
      && Int64.equal run.revision 0L
      && Option.is_none (Run_state.find original_runs run.id)
    then Ok ()
    else
      conflict
        "new goal admission requires its current Enabled goal and exact new Workflow"
  in
  let%bind source =
    Run_source_installation.captured
      (Run_state.installation index)
      ~generation:state.identity.generation
  in
  let%bind () =
    if P.Run_source.equal source run.source
    then Ok ()
    else conflict "goal admission source is no longer current"
  in
  let%bind episode = P.Goal_episode.create ~run ~admission:receipt in
  let%bind previous =
    match G.episode goal with
    | None -> Ok None
    | Some episode -> Result.map (previous_terminal original_runs episode) ~f:Option.some
  in
  let%bind goal =
    if enable
    then G.activate_episode goal episode ~previous
    else G.bind_episode goal episode ~previous
  in
  let%bind document =
    Result.of_option
      (Goal_catalog.document catalog intent.goal_id)
      ~error:(P.Error.invalid_request "goal admission document is absent")
  in
  let%bind document =
    Goal_document.update document goal
    |> Result.map_error ~f:(fun error ->
      P.Error.invalid_request (Sexp.to_string_hum (Document_schema.Error.sexp_of_t error)))
  in
  let%bind catalog = Goal_catalog.advance_existing catalog ~goal:document in
  let%bind link =
    Run_goal_link.create ~goal_id:intent.goal_id ~episode ~intent_sha256:(digest intent)
  in
  let%bind run_state = Run_state.bind_goal index link in
  let%bind () = Run_state.validate_transition ~previous:original_runs run_state in
  let%map () =
    Goal_run_evidence.validate_transition
      ~previous_runs:state.run_state
      ~catalog:(Some catalog)
      ~runs:(Some run_state)
  in
  { run_state; catalog; goal; link }
;;

let prepare state ~admission ~intent =
  prepare_facts
    state
    ~run:(Run_admission.run admission)
    ~index:(Run_admission.index admission)
    ~receipt:(Run_admission.receipt admission)
    ~intent
    ~enable:false
;;

let prepare_held state ~admission ~intent =
  let open Result.Let_syntax in
  let run = Run_admission.Held_plan.run admission in
  let receipt = Run_admission.Held_plan.receipt admission in
  let%bind index =
    Run_state.commit
      (Run_admission.Held_plan.source_index admission)
      ~run
      ~receipt
      ~intent:None
  in
  prepare_facts state ~run ~index ~receipt ~intent ~enable:true
;;
