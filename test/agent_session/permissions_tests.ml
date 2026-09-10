open Core
open Fixtures

let permission_invocation =
  Agent_session.Permission_policy.
    { tool_name = "write_file"
    ; identity_digest = "identity"
    ; invocation_display = "write_file(<redacted>)"
    ; effects = [ "filesystem.write" ]
    }
;;

let is_allowed = function
  | Agent_session.Permission_policy.Allow_now -> true
  | Deny_now _ | Request_permission | Request_review -> false
;;

let%expect_test "permission reviewers are advisory and fail closed" =
  let reviewer =
    Agent_session.Permission_reviewer.create
      ~id:"external"
      ~kind:External
      ~revision:"test-v1"
      ~review:(fun _ -> Ok Allow)
    |> protocol_ok
  in
  let fallback = Agent_session.Permission_policy.Fallback_reviewer "external" in
  let ask =
    permission_policy
      ~tool_default:Ask
      ~fallback
      ~evaluator:None
      ~reviewer:(Some reviewer)
  in
  let deny =
    permission_policy
      ~tool_default:Deny
      ~fallback
      ~evaluator:None
      ~reviewer:(Some reviewer)
  in
  let review_requested =
    Agent_session.Permission_policy.decide
      ask
      ~responder_available:false
      permission_invocation
    |> Agent_session.Permission_policy.equal_decision Request_review
  in
  let reviewer_allowed =
    match Agent_session.Permission_policy.review ask permission_invocation with
    | Ok Allow -> true
    | Ok (Deny _) | Error _ -> false
  in
  let hard_deny =
    Agent_session.Permission_policy.decide
      deny
      ~responder_available:false
      permission_invocation
    |> is_allowed
    |> not
  in
  let failing =
    Agent_session.Permission_reviewer.create
      ~id:"failing"
      ~kind:External
      ~revision:"test-v1"
      ~review:(fun _ -> failwith "reviewer unavailable")
    |> protocol_ok
  in
  let failed_closed =
    permission_policy
      ~tool_default:Ask
      ~fallback:(Fallback_reviewer "failing")
      ~evaluator:None
      ~reviewer:(Some failing)
    |> fun policy ->
    Agent_session.Permission_policy.review policy permission_invocation |> Result.is_error
  in
  print_s
    [%sexp
      { review_requested : bool
      ; reviewer_allowed : bool
      ; hard_deny : bool
      ; failed_closed : bool
      }];
  [%expect
    {|
    ((review_requested true) (reviewer_allowed true) (hard_deny true)
     (failed_closed true))
    |}]
;;

let%expect_test "allow-if-policy requires an affirmative deterministic policy" =
  let decide evaluator =
    permission_policy
      ~tool_default:Ask
      ~fallback:Fallback_allow_if_policy
      ~evaluator:(Some evaluator)
      ~reviewer:None
    |> fun policy ->
    Agent_session.Permission_policy.decide
      policy
      ~responder_available:false
      permission_invocation
    |> is_allowed
  in
  let allows = decide (fun _ -> Ok true) in
  let denies = decide (fun _ -> Ok false) |> not in
  let errors_deny = decide (fun _ -> Error "policy unavailable") |> not in
  print_s [%sexp { allows : bool; denies : bool; errors_deny : bool }];
  [%expect {| ((allows true) (denies true) (errors_deny true)) |}]
;;

let permission_request ~id =
  Agent_protocol.Permission.
    { id
    ; session_id
    ; generation = 0
    ; owner = Operation operation_id
    ; call_id = "call-1"
    ; tool_name = "shell"
    ; runtime_identity = Some "runtime"
    ; invocation_display = "echo hello"
    ; rationale = None
    ; effects = [ "process" ]
    ; choices = [ Approve_once; Deny ]
    ; created_at = timestamp
    ; expires_at = None
    ; state = Pending
    ; resolution = None
    }
;;

let%expect_test "permission requests persist before wait and resolve by generation" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:16
          ~compaction_env:None
          ~initial_state:
            (actor_state
               ~workspace_instance
               ~liveness:Process_bound
               ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:None
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id =
                (fun () ->
                  Agent_protocol.Id.Attachment.of_string "att_actor_permission"
                  |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let attachment, _ =
        Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
        |> protocol_ok
      in
      Agent_session.Session_actor.start actor ~attachment_id:attachment.id
      |> protocol_ok
      |> ignore;
      let resolved_choice = ref None in
      Eio.Fiber.both
        (fun () ->
           let resolution =
             Agent_session.Session_actor.request_permission
               actor
               ~permission:(permission_request ~id:permission_id)
               ~timeout_seconds:None
               ~fallback:Deny
             |> protocol_ok
           in
           resolved_choice := Some resolution.choice)
        (fun () ->
           let rec await_pending () =
             let state = Agent_session.Session_actor.state actor |> protocol_ok in
             if List.is_empty state.permissions
             then (
               Eio.Fiber.yield ();
               await_pending ())
           in
           await_pending ();
           Agent_session.Session_actor.respond_permission
             actor
             ~attachment_id:attachment.id
             ~principal_id:(Some principal_id)
             ~permission_id
             ~permission_generation:0
             ~choice:Approve_once
             ~reason:None
           |> protocol_ok
           |> ignore);
      let state = Agent_session.Session_actor.state actor |> protocol_ok in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { resolved_choice = (!resolved_choice : Agent_protocol.Permission.choice option)
          ; observed = (state.lifecycle.observed : Agent_protocol.Session.observed_state)
          ; permission_state =
              ((List.hd_exn state.permissions).state : Agent_protocol.Permission.state)
          }]));
  [%expect
    {|
    ((resolved_choice (Approve_once)) (observed Idle)
     (permission_state Approved))
    |}]
;;

let%expect_test "session approval creates a durable invocation grant" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let grant_permission_id =
        Agent_protocol.Id.Permission.of_string "per_agent_session_grant" |> protocol_ok
      in
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:16
          ~compaction_env:None
          ~initial_state:
            (actor_state
               ~workspace_instance
               ~liveness:Process_bound
               ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:None
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id =
                (fun () ->
                  Agent_protocol.Id.Attachment.of_string "att_actor_grant" |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let attachment, _ =
        Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
        |> protocol_ok
      in
      let request =
        { (permission_request ~id:grant_permission_id) with
          choices = [ Approve_session; Deny ]
        }
      in
      Eio.Fiber.both
        (fun () ->
           Agent_session.Session_actor.request_permission
             actor
             ~permission:request
             ~timeout_seconds:None
             ~fallback:Deny
           |> protocol_ok
           |> ignore)
        (fun () ->
           let rec await_pending () =
             let state = Agent_session.Session_actor.state actor |> protocol_ok in
             if List.is_empty state.permissions
             then (
               Eio.Fiber.yield ();
               await_pending ())
           in
           await_pending ();
           Agent_session.Session_actor.respond_permission
             actor
             ~attachment_id:attachment.id
             ~principal_id:(Some principal_id)
             ~permission_id:grant_permission_id
             ~permission_generation:0
             ~choice:Approve_session
             ~reason:None
           |> protocol_ok
           |> ignore);
      let state = Agent_session.Session_actor.state actor |> protocol_ok in
      Agent_session.Session_actor.shutdown actor;
      let grant = List.hd_exn state.grants in
      print_s
        [%sexp
          { grant_count = (List.length state.grants : int)
          ; scope = (grant.scope : Agent_protocol.Grant.scope)
          ; identity = (grant.identity_digest : string)
          ; state = (grant.state : Agent_protocol.Grant.state)
          }]));
  [%expect
    {|
    ((grant_count 1) (scope Exact_session) (identity runtime) (state Active))
    |}]
;;

let%expect_test "permission timeout applies configured unattended fallback" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let timeout_id =
        Agent_protocol.Id.Permission.of_string "per_agent_session_timeout" |> protocol_ok
      in
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:16
          ~compaction_env:None
          ~initial_state:
            (actor_state
               ~workspace_instance
               ~liveness:Process_bound
               ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:None
          ~services:
            { now = (fun () -> Agent_protocol.Timestamp.now ())
            ; create_attachment_id =
                (fun () ->
                  Agent_protocol.Id.Attachment.of_string "att_actor_timeout"
                  |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let resolution =
        Agent_session.Session_actor.request_permission
          actor
          ~permission:(permission_request ~id:timeout_id)
          ~timeout_seconds:(Some 0.005)
          ~fallback:Deny
        |> protocol_ok
      in
      Agent_session.Session_actor.shutdown actor;
      print_s [%sexp (resolution.choice : Agent_protocol.Permission.choice)]));
  [%expect {| Deny |}]
;;
