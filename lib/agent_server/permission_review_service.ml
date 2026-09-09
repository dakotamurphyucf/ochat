open! Core

let error code message = Agent_session.Permission_reviewer.Error.{ code; message }

let redacted invocation =
  Agent_session.Permission_policy.
    { invocation with invocation_display = invocation.tool_name ^ "(<redacted>)" }
;;

let job_kind reviewer =
  match Agent_session.Permission_reviewer.kind reviewer with
  | Model -> Agent_protocol.Job.Model_call
  | External -> Async_tool
;;

let job_payload reviewer invocation =
  `Object
    [ "type", `String "permission_review"
    ; "reviewer_id", `String (Agent_session.Permission_reviewer.id reviewer)
    ; "reviewer_revision", `String (Agent_session.Permission_reviewer.revision reviewer)
    ; "tool_name", `String invocation.Agent_session.Permission_policy.tool_name
    ; "identity_digest", `String invocation.identity_digest
    ; "invocation_display", `String invocation.invocation_display
    ; "effects", `Array (List.map invocation.effects ~f:(fun value -> `String value))
    ]
;;

let create_job ~now state reviewer invocation =
  Agent_protocol.Job.
    { id = Agent_protocol.Id.Job.create ()
    ; session_id = state.Agent_session.Session_state.identity.session_id
    ; generation = state.identity.generation
    ; kind = job_kind reviewer
    ; payload = job_payload reviewer invocation
    ; status = Queued
    ; retry_policy = Never
    ; attempt = 0
    ; created_at = now
    ; started_at = None
    ; next_run_at = None
    ; completed_at = None
    ; result = None
    ; delivery = Not_required
    ; launch = None
    }
;;

let persistence_error failure =
  error "reviewer.persistence" failure.Agent_protocol.Error.message
;;

let claim actor job =
  let open Result.Let_syntax in
  let%bind job =
    Agent_session.Session_actor.add_job actor job |> Result.map_error ~f:persistence_error
  in
  Agent_session.Session_actor.claim_job actor ~job_id:job.id ~generation:job.generation
  |> Result.map_error ~f:persistence_error
  |> Result.bind ~f:(function
    | Some claimed -> Ok claimed
    | None -> Error (error "reviewer.persistence" "reviewer job was not claimable"))
;;

let result_json = function
  | Agent_session.Permission_reviewer.Decision.Allow ->
    `Object [ "decision", `String "allow" ]
  | Deny reason -> `Object [ "decision", `String "deny"; "reason", `String reason ]
;;

let complete actor job result =
  let outcome =
    match result with
    | Ok decision -> Agent_session.Runtime_builder.Model_succeeded (result_json decision)
    | Error (failure : Agent_session.Permission_reviewer.Error.t) ->
      Model_failed (failure.code ^ ": " ^ failure.message)
  in
  Agent_session.Session_actor.complete_job
    actor
    ~job_id:job.Agent_protocol.Job.id
    ~generation:job.generation
    ~attempt:job.attempt
    outcome
  |> Result.map_error ~f:persistence_error
  |> Result.map ~f:(fun _ -> result)
  |> Result.join
;;

let interrupt actor job =
  Eio.Cancel.protect (fun () ->
    ignore
      (Agent_session.Session_actor.interrupt_job
         actor
         ~job_id:job.Agent_protocol.Job.id
         ~generation:job.generation
         ~attempt:job.attempt
         ~reason:"permission reviewer was cancelled"
       : (Agent_protocol.Job.t, Agent_protocol.Error.t) result))
;;

let execute actor profile invocation job =
  match Agent_session.Permission_policy.review profile invocation with
  | result -> complete actor job result
  | exception exn ->
    interrupt actor job;
    raise exn
;;

let review ~now ~actor ~profile invocation =
  let invocation = redacted invocation in
  match profile.Agent_session.Permission_policy.reviewer with
  | None -> Error (error "reviewer.unavailable" "permission reviewer is unavailable")
  | Some reviewer ->
    let open Result.Let_syntax in
    let%bind state =
      Agent_session.Session_actor.state actor |> Result.map_error ~f:persistence_error
    in
    let%bind job = claim actor (create_job ~now state reviewer invocation) in
    execute actor profile invocation job
;;

let permission_invocation (permission : Agent_protocol.Permission.t) =
  Agent_session.Permission_policy.
    { tool_name = permission.tool_name
    ; identity_digest =
        Option.value permission.runtime_identity ~default:permission.call_id
    ; invocation_display = permission.invocation_display
    ; effects = permission.effects
    }
;;

let approval_offered permission =
  List.mem
    permission.Agent_protocol.Permission.choices
    Approve_once
    ~equal:Agent_protocol.Permission.equal_choice
;;

let policy_fallback profile invocation =
  match
    Agent_session.Permission_policy.decide profile ~responder_available:false invocation
  with
  | Allow_now -> Agent_protocol.Permission.Approve_once, None
  | Deny_now reason -> Deny, Some reason
  | Request_permission -> Deny, Some "permission timeout still requires human review"
  | Request_review -> Deny, Some "permission reviewer fallback was not executed"
;;

let reviewer_fallback ~now ~actor ~profile invocation =
  match review ~now ~actor ~profile invocation with
  | Ok Allow -> Agent_protocol.Permission.Approve_once, None
  | Ok (Deny reason) -> Deny, Some reason
  | Error failure -> Deny, Some failure.message
;;

let timeout_resolution ~now ~actor ~profile permission invocation =
  let choice, reason =
    match profile.Agent_session.Permission_policy.fallback with
    | Fallback_reviewer _ -> reviewer_fallback ~now ~actor ~profile invocation
    | Fallback_allow | Fallback_deny | Fallback_allow_if_policy ->
      policy_fallback profile invocation
  in
  if
    Agent_protocol.Permission.equal_choice choice Approve_once
    && not (approval_offered permission)
  then
    ( Agent_protocol.Permission.Deny
    , Some "permission fallback requested an unavailable grant scope" )
  else choice, reason
;;

let resolve_timeout ~now ~actor ~profile permission =
  let invocation = permission_invocation permission in
  let choice, reason = timeout_resolution ~now ~actor ~profile permission invocation in
  Agent_session.Session_actor.resolve_permission_as_system
    actor
    ~permission_id:permission.Agent_protocol.Permission.id
    ~permission_generation:permission.generation
    ~choice
    ~reason
;;
