open! Core
module P = Agent_protocol
module Capture = Recurrence_definition.Goal_binding.Capture
module Request = Recurrence_command.Goal_owner_request

type selection =
  | Operator of Request.t
  | Native of Goal_work_attribution.t

let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())
let conflict message = Error (P.Error.create Conflict ~message ~retryable:false ())

let current_goal (state : Session_state.t) ~owner ~source ~goal_id ~now =
  let open Result.Let_syntax in
  let%bind index, installed_source = Goal_catalog_source.capture state in
  let%bind () =
    if
      P.Id.Session.equal state.identity.session_id (P.Session_ref.session_id owner)
      && Option.exists state.run_state ~f:(phys_equal index)
      && P.Run_source.equal source installed_source
    then Ok ()
    else conflict "recurrence Goal capture lost its actual owner or installed source"
  in
  let%bind goal =
    match
      Option.bind state.goal_catalog ~f:(fun catalog -> Goal_catalog.find catalog goal_id)
    with
    | Some goal -> Ok goal
    | None -> denied "recurrence controller does not own the selected Goal"
  in
  let%bind () =
    if
      P.Session_ref.equal owner (P.Goal.session goal)
      && P.Run_source.equal source (P.Goal.source goal)
    then Ok ()
    else conflict "recurrence Goal owner or source changed"
  in
  let%bind policy = Goal_root_policy.create state ~goal_id in
  let%bind decision = Goal_root_policy.fresh policy ~state ~now in
  match decision with
  | Allow -> Ok (index, goal)
  | Hold _ -> denied "recurrence controller Goal is ineligible for fresh activation"
;;

let check_episode index ~goal_id episode =
  let open Result.Let_syntax in
  let admission = P.Goal_episode.admission episode in
  let%bind receipt =
    Run_state.receipt
      index
      ~principal_id:admission.principal_id
      ~key:admission.key
      ~request_sha256:admission.request_sha256
  in
  if
    Option.exists receipt ~f:(P.Run_receipt.equal admission)
    && Option.exists
         (Run_state.find index admission.run_id)
         ~f:(P.Goal_episode.matches_run episode)
    && Option.exists (Run_state.goal_link index admission.run_id) ~f:(fun link ->
      P.Id.Goal.equal goal_id (Run_goal_link.goal_id link)
      && P.Goal_episode.equal episode (Run_goal_link.episode link))
  then Ok ()
  else denied "recurrence Goal lacks the actual retained Workflow admission"
;;

let capture state ~owner ~source ~selection ~now =
  let open Result.Let_syntax in
  let goal_id =
    match selection with
    | Operator request -> request.goal_id
    | Native attribution -> Goal_work_attribution.goal_id attribution
  in
  let%bind index, goal = current_goal state ~owner ~source ~goal_id ~now in
  let%bind creator_episode =
    match P.Goal.episode goal with
    | Some episode -> Ok episode
    | None -> denied "recurrence Goal has no current Workflow admission"
  in
  let goal_revision_at_capture = P.Goal.revision goal in
  let%bind () =
    match selection with
    | Operator request ->
      if Int64.equal request.expected_goal_revision goal_revision_at_capture
      then Ok ()
      else conflict "recurrence Goal revision changed"
    | Native attribution ->
      if P.Goal_episode.equal creator_episode (Goal_work_attribution.episode attribution)
      then Ok ()
      else conflict "recurrence native Goal episode changed"
  in
  let%bind () = check_episode index ~goal_id creator_episode in
  Capture.create ~owner ~goal_id ~creator_episode ~goal_revision_at_capture
;;

let check_binding_owner binding ~owner ~source =
  if
    P.Session_ref.equal owner (Capture.owner binding)
    && P.Run_source.equal
         source
         (P.Goal_episode.admission (Capture.creator_episode binding)).source
  then Ok ()
  else conflict "recurrence Goal binding differs from its original owner or source"
;;

let retain binding state ~owner ~source ~(request : Request.t) ~now =
  let open Result.Let_syntax in
  let%bind () = check_binding_owner binding ~owner ~source in
  let goal_id = Capture.goal_id binding in
  let%bind () =
    if P.Id.Goal.equal request.goal_id goal_id
    then Ok ()
    else conflict "recurrence Goal binding cannot change"
  in
  let%bind _, goal = current_goal state ~owner ~source ~goal_id ~now in
  if Int64.equal request.expected_goal_revision (P.Goal.revision goal)
  then Ok binding
  else conflict "recurrence Goal revision changed"
;;

let check_current binding state ~owner ~source ~now =
  let open Result.Let_syntax in
  let%bind () = check_binding_owner binding ~owner ~source in
  let%map _, _ =
    current_goal state ~owner ~source ~goal_id:(Capture.goal_id binding) ~now
  in
  ()
;;
