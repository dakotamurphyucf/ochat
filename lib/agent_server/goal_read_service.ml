open! Core
module A = Agent_session
module P = Agent_protocol
module Q = P.Goal_query
module G = P.Goal
module Projection = A.Goal_native_metadata_view

type t =
  { registry : Session_registry.t
  ; server_id : P.Id.Server.t
  ; execution_enabled : bool
  ; authorize_write :
      Operator_authorization.t -> A.Session_state.t -> (unit, P.Error.t) result
  }

let create ~registry ~server_id ~execution_enabled ~authorize_write =
  { registry; server_id; execution_enabled; authorize_write }
;;

let authorize_actor actor =
  if
    Operator_authorization.is_current actor
    && P.Principal.has_scope
         (Operator_authorization.principal actor)
         View_session_transcript
  then Ok ()
  else
    Error
      (P.Error.create
         Permission_denied
         ~message:"Goal inspection requires current transcript visibility"
         ~retryable:false
         ())
;;

let authorize_session actor summary =
  let open Result.Let_syntax in
  let%bind () = authorize_actor actor in
  if Authorization.session_visible_to (Operator_authorization.principal actor) summary
  then Ok ()
  else
    Error
      (P.Error.create
         Permission_denied
         ~message:"Goal session is not visible to this principal"
         ~retryable:false
         ())
;;

let availability (state : A.Session_state.t) ~selected : Q.Availability.t =
  match state.lifecycle.observed with
  | Stopped -> Stopped
  | Failed _ -> Unavailable
  | Queued_for_slot
  | Starting
  | Recovering
  | Idle
  | Running_turn _
  | Compacting _
  | Waiting_for_permission _
  | Stopping -> if selected then Active else Unavailable
;;

let wire_usage usage =
  let module U = A.Goal_usage in
  let module M = U.Metric in
  let open Result.Let_syntax in
  let metric component =
    let metric = U.component usage component in
    let count = U.Unknown.count (M.unknown metric) in
    P.Inference_query.Metric.create
      ~actual:(M.actual metric)
      ~actual_attempts:(M.actual_attempts metric)
      ~estimated:(M.estimated metric)
      ~estimated_attempts:(M.estimated_attempts metric)
      ~mixed_estimators:(M.mixed_estimators metric)
      ~unknown:
        { not_reported = count Not_reported
        ; explicit_null = count Explicit_null
        ; interrupted = count Interrupted
        ; not_submitted = count Not_submitted
        ; before_tracking = count Before_tracking
        }
  in
  let%bind input = metric Input in
  let%bind output = metric Output in
  let%bind reported_total = metric Reported_total in
  let%bind cached_input = metric Cached_input in
  let%bind cache_write_input = metric Cache_write_input in
  let%bind reasoning_output = metric Reasoning_output in
  Q.Usage.create
    ~attempts:(U.attempts usage)
    ~untracked_attempts:(U.untracked_attempts usage)
    ~components:
      { input; output; reported_total; cached_input; cache_write_input; reasoning_output }
;;

let wire_accounting (row : Projection.row) =
  let module V = A.Goal_policy_totals.View in
  let open Result.Let_syntax in
  match row.accounting, row.governance with
  | None, None -> Ok None
  | Some account, Some view ->
    let%bind local = wire_usage (V.local_usage view) in
    let%bind physical = wire_usage (V.physical_usage view) in
    let%map causal = wire_usage (V.causal_usage view) in
    Some
      Q.Accounting.
        { revision = account.revision
        ; local
        ; physical
        ; causal
        ; physical_unknown = V.physical_unknown view
        ; historical_causal_unknown = V.historical_causal_unknown view
        ; admitted =
            { turns = V.committed view ~dimension:Host_turn
            ; attempts = V.committed view ~dimension:Attempt
            }
        ; pending =
            { turns = V.pending view ~dimension:Host_turn
            ; attempts = V.pending view ~dimension:Attempt
            }
        }
  | None, Some _ | Some _, None ->
    Error
      (P.Error.create
         Invalid_state
         ~message:"Goal accounting projection is incomplete"
         ~retryable:false
         ())
;;

let run_observation (state : A.Session_state.t) ~availability ~selected principal goal
  : Q.Run_observation.t
  =
  match G.episode goal with
  | None -> No_episode
  | Some episode ->
    let admission = P.Goal_episode.admission episode in
    if Run_read_service.can_view principal ~run_owner:admission.principal_id
    then (
      match state.run_state with
      | None -> Unavailable
      | Some index ->
        (match A.Goal_run_evidence.episode_run index goal episode with
         | Error _ -> Unavailable
         | Ok run ->
           let status : Q.Run_observation.status option =
             match run.lifecycle, availability with
             | Terminal _, _ -> Some Terminal
             | Admitted, Q.Availability.Active -> Some Admitted
             | Active, Active -> Some Active
             | Waiting _, Active -> Some Waiting
             | (Admitted | Active | Waiting _), Stopped when selected -> Some Retained
             | (Admitted | Active | Waiting _), (Stopped | Unavailable) -> None
           in
           (match status with
            | Some status -> Observed { run_id = run.id; revision = run.revision; status }
            | None -> Unavailable)))
    else Unavailable
;;

let metadata_actions ~source (goal : Q.Goal_summary.t) : Q.Action.t list =
  match source with
  | None -> [ Hold; Abandon ]
  | Some current when P.Run_source.equal current goal.source ->
    (if goal.achievement_pending then [] else [ Q.Action.Revise_objective ])
    @ [ Record_progress; Change_budget; Hold; Abandon ]
  | Some _ -> [ Adopt_source; Hold; Abandon ]
;;

let policy_blocker governance ~now : Q.Execution_blocker.t option =
  match governance with
  | None -> Some Accounting_unavailable
  | Some governance ->
    (match A.Goal_policy_totals.observe_activation governance ~now with
     | Ok Allow -> None
     | Ok (Hold (Usage_uncertain _)) -> Some Usage_uncertain
     | Ok (Hold _) -> Some Policy_hold
     | Error _ -> Some Unavailable)
;;

type basis =
  { state : A.Session_state.t
  ; session : P.Session_ref.t
  ; availability : Q.Availability.t
  ; selected : bool
  ; source : P.Run_source.t option
  ; principal : P.Principal.t
  ; catalog_goals : P.Goal.t list
  ; can_advance_catalog : bool
  ; has_command_slot : bool
  ; can_write : bool
  ; now : P.Timestamp.t
  }

let common_blockers basis (goal : Q.Goal_summary.t) (row : Projection.row)
  : Q.Execution_blocker.t list
  =
  let lifecycle =
    match basis.availability with
    | Stopped when not basis.selected -> [ Q.Execution_blocker.Stopped ]
    | Unavailable -> [ Unavailable ]
    | Active | Stopped -> []
  in
  let source =
    match basis.source with
    | None -> [ Q.Execution_blocker.Source_unavailable ]
    | Some current when not (P.Run_source.equal current goal.source) -> [ Source_changed ]
    | Some _ -> []
  in
  let another_goal_enabled =
    List.exists basis.catalog_goals ~f:(fun other ->
      (not (P.Id.Goal.equal (G.id other) goal.id))
      && G.State.equal (G.state other) (Open Enabled))
  in
  let policy =
    if another_goal_enabled
    then [ Q.Execution_blocker.Policy_hold ]
    else Option.to_list (policy_blocker row.governance ~now:basis.now)
  in
  lifecycle @ source @ policy
;;

let new_run_blocker basis : Q.Execution_blocker.t option =
  let unfinished =
    let open Option.Let_syntax in
    let%bind source = basis.source in
    let%bind index = basis.state.run_state in
    A.Run_admission.unfinished_workflow index ~source
  in
  match unfinished, A.Run_admission.held_input_availability basis.state with
  | Some run, _ ->
    if
      Q.Availability.equal basis.availability Active
      && Run_read_service.can_view basis.principal ~run_owner:run.principal_id
    then Some Active_run
    else Some Unavailable
  | None, Unavailable -> Some Unavailable
  | None, (Authored_or_submission | Submission_only) -> None
;;

let execution_actions
      basis
      ~can_advance_goal
      (row : Projection.row)
      (goal : Q.Goal_summary.t)
      (run : Q.Run_observation.t)
  : Q.Action.t list * Q.Execution_blocker.t list
  =
  let open Q.Execution_blocker in
  let common = common_blockers basis goal row in
  let with_advice (action : Q.Action.t) =
    if basis.can_write && can_advance_goal && List.is_empty common then [ action ] else []
  in
  let resume_advice blockers =
    match goal.state, row.current_run with
    | _, None -> [], No_current_run :: blockers
    | Open (Held _), Some _ when basis.has_command_slot ->
      (match
         basis.state.lifecycle.observed, basis.state.active_operation, basis.state.failure
       with
       | (Idle | Stopped), None, None -> with_advice Q.Action.Resume, blockers
       | _ -> [], Unavailable :: blockers)
    | Open (Held _), Some _ -> [], Unavailable :: blockers
    | (Open Enabled | Abandoned _ | Achieved), Some _ -> [], blockers
  in
  match run with
  | No_episode | Observed { status = Terminal; _ } ->
    let original_creator = P.Id.Principal.equal (G.creator row.goal) basis.principal.id in
    (match new_run_blocker basis with
     | Some reason -> [], reason :: No_current_run :: common
     | None ->
       let actions =
         if original_creator && not goal.achievement_pending
         then with_advice Q.Action.Start
         else []
       in
       actions, No_current_run :: common)
  | Observed { status = Retained; _ } -> resume_advice common
  | Observed { status = Admitted | Active | Waiting; _ } ->
    resume_advice (Active_run :: common)
  | Unavailable -> [], Unavailable :: common
;;

let wire_row basis (row : Projection.row) =
  let open Result.Let_syntax in
  let goal = Q.Goal_summary.of_goal row.goal in
  let%bind () =
    if P.Session_ref.equal goal.session basis.session
    then Ok ()
    else Error (P.Error.invalid_request "retained Goal belongs to another session host")
  in
  let%map accounting = wire_accounting row in
  let run =
    run_observation
      basis.state
      ~availability:basis.availability
      ~selected:basis.selected
      basis.principal
      row.goal
  in
  let actions, execution_blockers =
    match goal.state with
    | Abandoned _ | Achieved -> [], []
    | Open _ ->
      let can_advance_goal =
        basis.can_advance_catalog && Int64.(goal.revision < max_value)
      in
      let execution, blockers = execution_actions basis ~can_advance_goal row goal run in
      let metadata =
        if basis.can_write && can_advance_goal && basis.has_command_slot
        then metadata_actions ~source:basis.source goal
        else []
      in
      let blockers =
        if can_advance_goal then blockers else Q.Execution_blocker.Unavailable :: blockers
      in
      ( metadata @ execution
      , List.dedup_and_sort blockers ~compare:Q.Execution_blocker.compare )
  in
  Q.View.{ goal; run; accounting; actions; execution_blockers }
;;

let can_write t ~actor ~owned_attachment ~state ~now =
  let principal = Operator_authorization.principal actor in
  t.execution_enabled
  && P.Principal.has_scope principal Send_messages
  && Option.exists owned_attachment ~f:(fun attachment_id ->
    Result.is_ok (A.Writer_attachment.require state ~now ~attachment_id)
    && Result.is_ok (t.authorize_write actor state))
;;

let snapshot t ~actor ~context ~now (request : Q.Request.t) =
  let open Result.Let_syntax in
  let%bind actor = Connection_context.request_actor context (Some actor) in
  let%bind () = authorize_actor actor in
  let session = request.session in
  let%bind () =
    if P.Id.Server.equal (P.Session_ref.server_id session) t.server_id
    then Ok ()
    else Error (P.Error.invalid_request "Goal reference belongs to another host")
  in
  let session_id = P.Session_ref.session_id session in
  let%bind state, origin =
    Session_registry.read_state_with_execution
      t.registry
      ~authorize:(authorize_session actor)
      session_id
  in
  let owned_attachment =
    match origin with
    | Indexed -> None
    | Loaded_unselected | Selected ->
      Option.filter request.attachment_id ~f:(fun attachment_id ->
        Connection_context.owns_attachment context ~session_id ~attachment_id
        && not (Session_registry.lifecycle_reserved t.registry session_id))
  in
  let selected =
    match origin with
    | Selected -> t.execution_enabled
    | Loaded_unselected | Indexed -> false
  in
  let at = now () in
  let can_write = can_write t ~actor ~owned_attachment ~state ~now:(fun () -> at) in
  let principal = Operator_authorization.principal actor in
  let goal_id =
    match request.target with
    | All -> None
    | One id -> Some id
  in
  let%bind projection = Projection.capture_query state ~session ~goal_id in
  let availability = availability state ~selected in
  (* A saved Run installation can be observed without a current moderator
     checkpoint. That historical identity is not a source for new commands. *)
  let current_source = Option.bind state.moderator ~f:(fun _ -> projection.source) in
  let catalog = Option.value state.goal_catalog ~default:A.Goal_catalog.empty in
  let can_advance_catalog = A.Goal_catalog.can_advance_revision catalog in
  let has_command_slot = A.Goal_catalog.has_command_slot catalog in
  let basis =
    { state
    ; session
    ; availability
    ; selected
    ; source = current_source
    ; principal
    ; catalog_goals = A.Goal_catalog.goals catalog
    ; can_advance_catalog
    ; has_command_slot
    ; can_write
    ; now = at
    }
  in
  let%bind goals = List.map projection.goals ~f:(wire_row basis) |> Result.all in
  let%bind view =
    Q.View.create
      ~session
      ~generation:projection.generation
      ~session_revision:state.counters.revision
      ~catalog_revision:projection.catalog_revision
      ~source:current_source
      ~availability
      ~can_create:
        (can_write
         && Option.is_some current_source
         && can_advance_catalog
         && has_command_slot
         && A.Goal_catalog.has_goal_slot catalog)
      ~goals
  in
  let%map () = authorize_session actor (A.Session_state.summary state) in
  view
;;

let query t ~actor ~context ~now (request : Q.Request.t) =
  let result =
    match snapshot t ~actor ~context ~now request with
    | Ok view -> Ok (Q.Result.Available view)
    | Error failure ->
      (match failure.P.Error.code with
       | Session_not_found -> Ok (Q.Result.Unavailable request.session)
       | _ -> Error failure)
  in
  let open Result.Let_syntax in
  let%bind outcome = result in
  let%map () = authorize_actor actor in
  match request.target with
  | All -> Q.Result.List outcome
  | One _ -> Q.Result.Get outcome
;;
