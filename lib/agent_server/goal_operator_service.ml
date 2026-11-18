open! Core
module P = Agent_protocol
module A = Agent_session
module Actor = A.Session_actor
module W = P.Goal_control

type t =
  { registry : Session_registry.t
  ; factory : Session_factory.t
  ; server_id : P.Id.Server.t
  ; now : unit -> P.Timestamp.t
  ; authorize_write :
      Operator_authorization.t -> A.Session_state.t -> (unit, P.Error.t) result
  }

let create ~registry ~factory ~server_id ~now ~authorize_write =
  { registry; factory; server_id; now; authorize_write }
;;

let denied message = Error (P.Error.create Permission_denied ~message ~retryable:false ())

let require_current actor =
  let principal = Operator_authorization.principal actor in
  if
    Operator_authorization.is_current actor
    && P.Principal.has_scope principal Send_messages
    && P.Principal.has_scope principal View_session_transcript
  then Ok ()
  else denied "Goal control requires a current send and transcript authority"
;;

let authorize t actor state =
  let open Result.Let_syntax in
  let%bind () = require_current actor in
  let principal = Operator_authorization.principal actor in
  let%bind () =
    if Authorization.session_visible_to principal (A.Session_state.summary state)
    then Ok ()
    else denied "Goal session is unavailable to this operator"
  in
  let%bind () = t.authorize_write actor state in
  require_current actor
;;

let require_entry t context ~session ~writer =
  let open Result.Let_syntax in
  let session_id = P.Session_ref.session_id session in
  let%bind () =
    if P.Id.Server.equal t.server_id (P.Session_ref.server_id session)
    then Ok ()
    else Error (P.Error.invalid_request "Goal control belongs to another host")
  in
  let%bind () =
    if Connection_context.owns_attachment context ~session_id ~attachment_id:writer
    then Ok ()
    else denied "Goal writer attachment is unavailable to this connection"
  in
  match Session_registry.find t.registry session_id with
  | None -> denied "Goal writer attachment is unavailable to this connection"
  | Some entry ->
    let%map () = Session_factory.check_registered_owner t.factory entry ~session in
    entry
;;

let public_failure message (error : P.Error.t) =
  P.Error.create error.code ~message ~retryable:error.retryable ()
;;

let public_command_failure (error : P.Error.t) =
  let message =
    match error.code with
    | Permission_denied | Lease_stale ->
      "Goal metadata requires current writer and session authority"
    | Conflict -> "Goal metadata conflicts with current session, catalog or source state"
    | Idempotency_conflict -> "Goal command key already belongs to a different intent"
    | Invalid_state -> "Current Goal state does not allow this metadata change"
    | Resource_limit -> "Goal metadata exceeds an owner limit"
    | Persistence_error -> "Goal metadata acknowledgement is unavailable"
    | _ -> "Goal metadata command failed"
  in
  public_failure message error
;;

let command t ~actor ~(entry : Session_registry.entry) ~session ~writer request =
  let authorize state =
    let open Result.Let_syntax in
    let%bind () = Session_factory.check_registered_owner t.factory entry ~session in
    let%bind () = authorize t actor state in
    let%bind _ = A.Writer_attachment.require state ~attachment_id:writer ~now:t.now in
    Session_factory.check_registered_owner t.factory entry ~session
  in
  let%map.Result receipt =
    Actor.goal_command entry.actor ~actor ~authorize ~session ~request
    |> Result.map_error ~f:public_command_failure
  in
  W.Result.Command receipt
;;

let callback_control control : W.Callback_control.t =
  let receipt = Actor.Goal_callback_control.receipt control in
  let disposition =
    match Actor.Goal_callback_control.disposition control with
    | Held reason -> W.Callback_control.Held reason
    | Abandoned reason -> W.Callback_control.Abandoned reason
  in
  { goal_id = receipt.goal_id
  ; goal_revision = receipt.goal_revision
  ; catalog_revision = receipt.catalog_revision
  ; disposition
  ; waiting = Option.is_some (Actor.Goal_callback_control.waiting control)
  }
;;

let start_result
      ~(associate : P.Run_receipt.t -> (W.Start_result.Admission.t, P.Error.t) result)
      (result : Goal_run_service.Activation_result.t)
  : (W.Start_result.t, P.Error.t) result
  =
  let open Result.Let_syntax in
  match result with
  | Activated receipt ->
    let%map admission = associate receipt in
    W.Start_result.Activated admission
  | Controlled { admission; control } ->
    let%map admission = associate admission in
    W.Start_result.Controlled { admission; control = callback_control control }
  | Settled { admission; settlement } ->
    let%map admission = associate admission in
    W.Start_result.Settled
      { admission
      ; run_revision = (P.Goal_episode.Settlement.receipt settlement).run_revision
      }
  | Retained receipt ->
    let%map admission = associate receipt in
    W.Start_result.Retained admission
  | Admission_uncertain error ->
    Ok
      (Admission_uncertain
         (public_failure "Goal Start admission acknowledgement is unknown" error))
  | Activation_failed { admission; error } ->
    let%map admission = associate admission in
    W.Start_result.Activation_failed
      { admission
      ; error = public_failure "Goal activation failed after Run admission" error
      }
;;

let start t ~actor ~entry ~session ~writer (intent : W.Start.t) =
  let open Result.Let_syntax in
  let session_id = P.Session_ref.session_id session in
  let%bind request =
    A.Run_admission_request.Headless.create
      ~session_id
      ~generation:intent.generation
      ~expected_revision:intent.expected_revision
      ~mode:Workflow
      ~input:intent.input
      ~key:intent.key
  in
  let%bind request =
    A.Goal_activation_request.create
      ~goal_id:intent.goal_id
      ~expected_goal_revision:intent.expected_goal_revision
      ~expected_catalog_revision:intent.expected_catalog_revision
      ~request
  in
  let request = A.Goal_activation_request.with_expected_source request intent.source in
  (* The fresh writer commits this exact association with the receipt. Both
     retained paths verify the same association against the durable link. *)
  let association_sha256 =
    A.Goal_episode_admission.digest (A.Goal_activation_request.intent request)
  in
  let associate receipt = W.Start_result.Admission.create receipt ~association_sha256 in
  let service = Goal_run_service.create ~factory:t.factory ~authorize:(authorize t) in
  let%bind outcome =
    Goal_run_service.start_episode ~writer service ~entry ~actor ~session ~request
    |> Result.map_error ~f:(public_failure "Goal Start was rejected")
  in
  match outcome with
  | Retained receipt ->
    let%map admission = associate receipt in
    W.Result.Start (Retained admission)
  | Attempted attempt ->
    let%bind result =
      Goal_run_service.Attempt.result attempt
      |> Result.map_error ~f:(public_failure "Goal Start was rejected before admission")
    in
    let%map result = start_result ~associate result in
    W.Result.Start result
;;

let continuation_result
  : Goal_run_service.Resume_result.Continuation.t -> W.Resume_result.continuation
  = function
  | Started operation ->
    Started { operation_id = operation.id; generation = operation.generation }
  | Waiting _ -> Waiting
  | Settled settlement ->
    let receipt = P.Goal_episode.Settlement.receipt settlement in
    Settled { run_id = receipt.run_id; run_revision = receipt.run_revision }
  | Controlled control -> Controlled (callback_control control)
;;

let resume_result : Goal_run_service.Resume_result.t -> W.Resume_result.t = function
  | Resumed { receipt; continuation } ->
    Resumed { receipt; continuation = continuation_result continuation }
  | Retained receipt -> Retained receipt
  | Resume_failed { receipt; error } ->
    Resume_failed
      { receipt
      ; error =
          public_failure "Goal continuation failed after Resume acknowledgement" error
      }
  | Rejected error ->
    Rejected (public_failure "Goal Resume was rejected before acknowledgement" error)
  | Uncertain error ->
    Uncertain (public_failure "Goal Resume acknowledgement is unknown" error)
;;

let resume t ~actor ~entry ~session ~writer request =
  let service = Goal_run_service.create ~factory:t.factory ~authorize:(authorize t) in
  let%map.Result result =
    Goal_run_service.resume_episode ~writer service ~entry ~actor ~session ~request
    |> Result.map_error ~f:(public_failure "Goal Resume was rejected")
  in
  W.Result.Resume (resume_result result)
;;

let execute t ~actor ~context request =
  let open Result.Let_syntax in
  let%bind actor = Connection_context.request_actor context (Some actor) in
  let%bind () = require_current actor in
  let session = W.Request.session request in
  let writer = W.Request.attachment_id request in
  let%bind entry = require_entry t context ~session ~writer in
  match request with
  | Command target -> command t ~actor ~entry ~session ~writer target.intent
  | Start target -> start t ~actor ~entry ~session ~writer target.intent
  | Resume target -> resume t ~actor ~entry ~session ~writer target.intent
;;
