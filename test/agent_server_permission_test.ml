open! Core

let () = Mirage_crypto_rng_unix.use_default ()

let protocol_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let store_ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_store.Store_error.t)]
;;

let timestamp env =
  Eio.Time.now (Eio.Stdenv.clock env)
  |> Time_ns.Span.of_sec
  |> Time_ns.of_span_since_epoch
  |> Agent_protocol.Timestamp.of_time_ns
;;

let initial_state env workspace =
  let now = timestamp env in
  let workspace_instance =
    Agent_session.Workspace_resolver.resolve_current
      ~env
      ~instance_id:(Agent_protocol.Id.Workspace_instance.create ())
      ~path:workspace
      ~access:Shared_write
      ~created_at:now
    |> store_ok
  in
  let protocol =
    Agent_protocol.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Local_path "/agent.chatmd")
      ~workspace:Current
      ~liveness:Detached
      ~persistence:Durable
      ~start_immediately:false
      ~labels:[]
      ()
    |> protocol_ok
  in
  let identity =
    Agent_session.Session_state.Identity.
      { session_id = Agent_protocol.Id.Session.create ()
      ; display_name = None
      ; creating_principal = Some (Agent_protocol.Id.Principal.create ())
      ; created_at = now
      ; updated_at = now
      ; labels = []
      ; generation = 0
      }
  in
  let spec =
    Agent_session.Session_state.Spec.
      { protocol
      ; prompt_definition_id = None
      ; prompt_revision_id = Agent_protocol.Id.Prompt_revision.create ()
      ; workspace_instance
      ; permission_profile = "review"
      ; permission_profile_digest = "review-v1"
      ; runtime_policy = None
      ; quota_key = None
      }
  in
  Agent_session.Session_state.create ~identity ~spec ~initial_history:[]
;;

let actor ~sw ~env state =
  let backend =
    Agent_session.Memory_backend.create ~event_capacity:64 ~initial_state:state
  in
  Agent_session.Session_actor.create
    ~sw
    ~clock:(Eio.Stdenv.clock env)
    ~mailbox_capacity:32
    ~compaction_env:None
    ~initial_state:state
    ~persistence:(Agent_session.Memory_backend.persistence backend)
    ~operation_worker:None
    ~services:
      { now = (fun () -> timestamp env)
      ; create_attachment_id = Agent_protocol.Id.Attachment.create
      ; create_reclaim_token = (fun () -> "review-reclaim-token")
      ; job_results = None
      ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
      ; state_committed = (fun _ _ -> ())
      }
;;

let profile reviewer =
  Agent_session.Permission_policy.create
    ~id:"review"
    ~tool_default:Ask
    ~approval_timeout_ms:None
    ~fallback:(Fallback_reviewer (Agent_session.Permission_reviewer.id reviewer))
    ~manifest_authorization:Require_grant
    ~evaluator:None
    ~evaluator_revision:None
    ~reviewer:(Some reviewer)
  |> protocol_ok
;;

let invocation secret =
  Agent_session.Permission_policy.
    { tool_name = "write_file"
    ; identity_digest = "identity-digest"
    ; invocation_display = "write_file(" ^ secret ^ ")"
    ; effects = [ "filesystem.write" ]
    }
;;

let job_succeeded (job : Agent_protocol.Job.t) =
  match job.status, job.delivery with
  | Succeeded, Not_required -> true
  | ( ( Queued
      | Running
      | Waiting_permission _
      | Waiting_completion _
      | Failed _
      | Cancelled
      | Interrupted _ )
    , _ )
  | Succeeded, (Pending | Delivered _) -> false
;;

let job_failed (job : Agent_protocol.Job.t) =
  match job.status, job.delivery with
  | Failed _, Not_required -> true
  | ( ( Queued
      | Running
      | Waiting_permission _
      | Waiting_completion _
      | Succeeded
      | Cancelled
      | Interrupted _ )
    , _ )
  | Failed _, (Pending | Delivered _) -> false
;;

let permission state now secret =
  Agent_protocol.Permission.
    { id = Agent_protocol.Id.Permission.create ()
    ; session_id = state.Agent_session.Session_state.identity.session_id
    ; generation = state.identity.generation
    ; owner = Operation (Agent_protocol.Id.Operation.create ())
    ; call_id = "review-timeout"
    ; tool_name = "write_file"
    ; runtime_identity = Some "identity-digest"
    ; invocation_display = "write_file(" ^ secret ^ ")"
    ; rationale = None
    ; effects = [ "filesystem.write" ]
    ; choices = [ Approve_once; Deny ]
    ; created_at = now
    ; expires_at = Some now
    ; state = Pending
    ; resolution = None
    }
;;

let rec pending_permission actor =
  let state = Agent_session.Session_actor.state actor |> protocol_ok in
  match state.permissions with
  | permission :: _ -> permission
  | [] ->
    Eio.Fiber.yield ();
    pending_permission actor
;;

let%expect_test "reviewer calls are redacted and represented by terminal durable jobs" =
  Eio_main.run (fun env ->
    let workspace = Eio.Path.native_exn (Eio.Stdenv.cwd env) in
    Eio.Switch.run (fun sw ->
      let actor = actor ~sw ~env (initial_state env workspace) in
      let observed = ref None in
      let allowing =
        Agent_session.Permission_reviewer.create
          ~id:"allowing"
          ~kind:External
          ~revision:"allowing-v1"
          ~review:(fun request ->
            observed := Some request;
            Ok Allow)
        |> protocol_ok
      in
      let secret = "sensitive-payload" in
      let allowed =
        match
          Agent_server.Permission_review_service.review
            ~now:(timestamp env)
            ~actor
            ~profile:(profile allowing)
            (invocation secret)
        with
        | Ok Allow -> true
        | Ok (Deny _) | Error _ -> false
      in
      let failing =
        Agent_session.Permission_reviewer.create
          ~id:"failing"
          ~kind:Model
          ~revision:"failing-v1"
          ~review:(fun _ ->
            Error { code = "reviewer.timeout"; message = "review timed out" })
        |> protocol_ok
      in
      let failed_closed =
        Agent_server.Permission_review_service.review
          ~now:(timestamp env)
          ~actor
          ~profile:(profile failing)
          (invocation secret)
        |> Result.is_error
      in
      let state = Agent_session.Session_actor.state actor |> protocol_ok in
      let observed = Option.value_exn !observed in
      let payloads =
        List.map state.jobs ~f:(fun job ->
          Jsonaf.to_string job.Agent_protocol.Job.payload)
        |> String.concat ~sep:"\n"
      in
      print_s
        [%sexp
          { allowed : bool
          ; failed_closed : bool
          ; reviewer_redacted =
              (String.equal observed.invocation_display "write_file(<redacted>)" : bool)
          ; persisted_payload_redacted =
              (not (String.is_substring payloads ~substring:secret) : bool)
          ; succeeded_jobs = (List.count state.jobs ~f:job_succeeded : int)
          ; failed_jobs = (List.count state.jobs ~f:job_failed : int)
          }];
      Agent_session.Session_actor.shutdown actor));
  [%expect
    {|
    ((allowed true) (failed_closed true) (reviewer_redacted true)
     (persisted_payload_redacted true) (succeeded_jobs 1) (failed_jobs 1))
    |}]
;;

let%expect_test "embedded approval timeout runs the durable reviewer before resuming" =
  Eio_main.run (fun env ->
    let workspace = Eio.Path.native_exn (Eio.Stdenv.cwd env) in
    Eio.Switch.run (fun sw ->
      let state = initial_state env workspace in
      let actor = actor ~sw ~env state in
      let reviewer =
        Agent_session.Permission_reviewer.create
          ~id:"timeout-reviewer"
          ~kind:Model
          ~revision:"timeout-v1"
          ~review:(fun _ -> Ok Allow)
        |> protocol_ok
      in
      let profile = profile reviewer in
      let now = timestamp env in
      let invocation = invocation "timeout-secret" in
      let resolution =
        Agent_session.Session_actor.request_permission_with_review_fallback
          actor
          ~permission:(permission state now "timeout-secret")
          ~timeout_seconds:(Some 0.)
          ~fallback:Deny
          ~review_on_timeout:(fun () ->
            Agent_server.Permission_review_service.review
              ~now:(timestamp env)
              ~actor
              ~profile
              invocation)
        |> protocol_ok
      in
      let state = Agent_session.Session_actor.state actor |> protocol_ok in
      let resolved = List.hd_exn state.permissions in
      print_s
        [%sexp
          { approved_once =
              (Agent_protocol.Permission.equal_choice resolution.choice Approve_once
               : bool)
          ; durable_permission_approved =
              (Agent_protocol.Permission.equal_state resolved.state Approved : bool)
          ; reviewer_job_succeeded = (List.exists state.jobs ~f:job_succeeded : bool)
          }];
      Agent_session.Session_actor.shutdown actor));
  [%expect
    {|
    ((approved_once true) (durable_permission_approved true)
     (reviewer_job_succeeded true))
    |}]
;;

let%expect_test "daemon timeout resolution uses the same durable reviewer service" =
  Eio_main.run (fun env ->
    let workspace = Eio.Path.native_exn (Eio.Stdenv.cwd env) in
    Eio.Switch.run (fun sw ->
      let state = initial_state env workspace in
      let actor = actor ~sw ~env state in
      let reviewer =
        Agent_session.Permission_reviewer.create
          ~id:"daemon-timeout-reviewer"
          ~kind:External
          ~revision:"daemon-timeout-v1"
          ~review:(fun _ -> Ok Allow)
        |> protocol_ok
      in
      let profile = profile reviewer in
      let response, resolver = Eio.Promise.create () in
      Eio.Fiber.fork ~sw (fun () ->
        Agent_session.Session_actor.request_permission
          actor
          ~permission:(permission state (timestamp env) "daemon-timeout-secret")
          ~timeout_seconds:None
          ~fallback:Deny
        |> Eio.Promise.resolve resolver);
      let pending = pending_permission actor in
      Agent_server.Permission_review_service.resolve_timeout
        ~now:(timestamp env)
        ~actor
        ~profile
        pending
      |> protocol_ok
      |> ignore;
      let resolution = Eio.Promise.await response |> protocol_ok in
      let final = Agent_session.Session_actor.state actor |> protocol_ok in
      print_s
        [%sexp
          { approved_once =
              (Agent_protocol.Permission.equal_choice resolution.choice Approve_once
               : bool)
          ; terminal_reviewer_job = (List.exists final.jobs ~f:job_succeeded : bool)
          }];
      Agent_session.Session_actor.shutdown actor));
  [%expect {| ((approved_once true) (terminal_reviewer_job true)) |}]
;;
