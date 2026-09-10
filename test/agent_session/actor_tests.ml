open Core
open Fixtures

let%expect_test "idle mailbox reads cancel without closing or consuming the next item" =
  Eio_main.run (fun env ->
    Eio.Switch.run (fun sw ->
      let mailbox = Agent_session.Mailbox.create ~capacity:2 in
      Eio.Fiber.fork_daemon ~sw (fun () ->
        Eio.Time.sleep (Eio.Stdenv.clock env) 2.;
        Agent_session.Mailbox.close mailbox;
        `Stop_daemon);
      let heartbeat =
        Eio.Fiber.first
          (fun () ->
             ignore (Agent_session.Mailbox.pop mailbox : int option);
             false)
          (fun () ->
             Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
             true)
      in
      let reusable = Agent_session.Mailbox.try_push mailbox ~priority:Normal 7 in
      let next = Agent_session.Mailbox.pop mailbox in
      print_s [%sexp { heartbeat : bool; reusable : bool; next : int option }]));
  [%expect {| ((heartbeat true) (reusable true) (next (7))) |}]
;;

let%expect_test "session actor publishes committed events to multiple subscribers" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let commits = ref 0 in
      let command_audits = ref [] in
      let attachment_sequence = ref 0 in
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:
            (actor_state
               ~workspace_instance
               ~liveness:Process_bound
               ~start_immediately:false)
          ~persistence:
            { commit =
                (fun ~command_audit ~previous:_ _transition ->
                  Int.incr commits;
                  command_audits := command_audit :: !command_audits;
                  Ok ())
            }
          ~operation_worker:None
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id =
                (fun () ->
                  Int.incr attachment_sequence;
                  Agent_protocol.Id.Attachment.of_string
                    (sprintf "att_actor_%d" !attachment_sequence)
                  |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let attach () =
        Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:true
        |> protocol_ok
        |> fun (attachment, subscriber) -> attachment, Option.value_exn subscriber
      in
      let first_attachment, first = attach () in
      let _, second = attach () in
      Agent_session.Session_actor.start_with_command_audit
        actor
        ~command_audit:"audited-start"
        ~attachment_id:first_attachment.id
      |> protocol_ok
      |> ignore;
      Agent_session.Session_actor.append_history
        actor
        ~attachment_id:first_attachment.id
        [ actor_entry ]
      |> protocol_ok
      |> ignore;
      Agent_session.Session_actor.add_shell_manifest_grant
        actor
        Session.Shell_state.Manifest_grant.
          { grant_id = "manifest-grant"
          ; manifest_sha256 = "manifest-sha256"
          ; canonical_source_root = "/prompt"
          ; repository_identity = None
          ; source_sha256 = "source-sha256"
          ; signer = None
          ; issuer = Some "test"
          ; audience = []
          ; schema_version = 1
          ; builtin_versions = []
          ; imported_source_sha256 = []
          ; session_id = Some "session"
          ; user_id = None
          ; host_id = None
          ; created_at_ns = 1L
          ; expires_at_ns = None
          ; revoked_at_ns = None
          ; revocation_reason = None
          }
      |> protocol_ok;
      let manifest_grants =
        Agent_session.Session_actor.shell_manifest_grants actor |> protocol_ok
      in
      let security_snapshot = Agent_session.Session_actor.snapshot actor |> protocol_ok in
      let manifest_security_grant =
        List.find_exn security_snapshot.grants ~f:(fun grant ->
          String.equal grant.Agent_protocol.Grant.tool_name "shell.manifest")
      in
      let revoked_grant, _ =
        Agent_session.Session_actor.revoke_grant
          actor
          ~attachment_id:first_attachment.id
          ~grant_id:manifest_security_grant.id
          ~reason:"test revocation"
        |> protocol_ok
      in
      let rec take_history_kind subscriber =
        match Agent_session.Subscriber.take subscriber with
        | Some (Ok (Durable event))
          when Agent_protocol.Event.Durable.equal_kind event.kind History_appended ->
          event.kind
        | Some (Ok (Durable _)) | Some (Ok (Recoverable _)) ->
          take_history_kind subscriber
        | Some (Error _) | None -> raise_s [%sexp "expected durable event"]
      in
      let first_kind = take_history_kind first in
      let second_kind = take_history_kind second in
      let snapshot = Agent_session.Session_actor.snapshot actor |> protocol_ok in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { same_first_event =
              (Agent_protocol.Event.Durable.equal_kind first_kind second_kind : bool)
          ; commits = (!commits : int)
          ; command_audits = (List.filter_opt !command_audits |> List.rev : string list)
          ; history = (List.length snapshot.canonical_history.entries : int)
          ; manifest_grants = (List.length manifest_grants : int)
          ; security_grants = (List.length security_snapshot.grants : int)
          ; revoked =
              (Agent_protocol.Grant.equal_state revoked_grant.state Revoked : bool)
          }]));
  [%expect
    {|
    ((same_first_event true) (commits 7) (command_audits (audited-start))
     (history 1) (manifest_grants 1) (security_grants 1) (revoked true))
    |}]
;;

let%expect_test "session actor enforces its configured attachment limit" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let attachment_sequence = ref 0 in
      let actor =
        Agent_session.Session_actor.create_with_owner_lease_duration
          ~schedule_permission_timeouts:true
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:16
          ~owner_lease_duration_ms:60_000
          ~max_attachments:1
          ~subscriber_capacity:2
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
                  Int.incr attachment_sequence;
                  Agent_protocol.Id.Attachment.of_string
                    (sprintf "att_limited_%d" !attachment_sequence)
                  |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let first =
        Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
        |> Result.is_ok
      in
      let second =
        match
          Agent_session.Session_actor.attach actor ~mode:Read_only ~subscribe:false
        with
        | Error { code = Resource_limit; _ } -> true
        | Ok _ | Error _ -> false
      in
      Agent_session.Session_actor.shutdown actor;
      print_s [%sexp { first : bool; second_rejected = (second : bool) }]));
  [%expect {| ((first true) (second_rejected true)) |}]
;;

let%expect_test "owner-bound actor stops after its owner disconnect grace" =
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
               ~liveness:(Owner_bound { disconnect_grace_ms = 5; stop_mode = Graceful })
               ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:None
          ~services:
            { now = (fun () -> Agent_protocol.Timestamp.now ())
            ; create_attachment_id =
                (fun () ->
                  Agent_protocol.Id.Attachment.of_string "att_actor_owner" |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let attachment, _ =
        Agent_session.Session_actor.attach actor ~mode:Owner_read_write ~subscribe:false
        |> protocol_ok
      in
      Agent_session.Session_actor.start actor ~attachment_id:attachment.id
      |> protocol_ok
      |> ignore;
      Agent_session.Session_actor.detach actor attachment.id |> protocol_ok;
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
      let state = Agent_session.Session_actor.state actor |> protocol_ok in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { desired = (state.lifecycle.desired : Agent_protocol.Session.desired_state)
          ; observed = (state.lifecycle.observed : Agent_protocol.Session.observed_state)
          }]));
  [%expect {| ((desired Stopped) (observed Stopped)) |}]
;;

let%expect_test "owner-bound actor permits one owner and supports grace reclaim" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let attachment_sequence = ref 0 in
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:16
          ~compaction_env:None
          ~initial_state:
            (actor_state
               ~workspace_instance
               ~liveness:(Owner_bound { disconnect_grace_ms = 20; stop_mode = Graceful })
               ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:None
          ~services:
            { now = (fun () -> Agent_protocol.Timestamp.now ())
            ; create_attachment_id =
                (fun () ->
                  Int.incr attachment_sequence;
                  Agent_protocol.Id.Attachment.of_string
                    (sprintf "att_actor_owner_%d" !attachment_sequence)
                  |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let first, _, _, reclaim_token =
        Agent_session.Session_actor.attach_with_snapshot
          actor
          ~principal_id:(Some principal_id)
          ~reclaim_token:None
          ~mode:Owner_read_write
          ~subscribe:false
        |> protocol_ok
      in
      Agent_session.Session_actor.start actor ~attachment_id:first.id
      |> protocol_ok
      |> ignore;
      let simultaneous_rejected =
        Agent_session.Session_actor.attach_with_snapshot
          actor
          ~principal_id:(Some principal_id)
          ~reclaim_token:None
          ~mode:Owner_read_write
          ~subscribe:false
        |> Result.is_error
      in
      Agent_session.Session_actor.detach actor first.id |> protocol_ok;
      let reclaimed, _, _, rotated_token =
        Agent_session.Session_actor.attach_with_snapshot
          actor
          ~principal_id:(Some second_principal_id)
          ~reclaim_token
          ~mode:Owner_read_write
          ~subscribe:false
        |> protocol_ok
      in
      Eio.Time.sleep (Eio.Stdenv.clock env) 0.04;
      let state = Agent_session.Session_actor.state actor |> protocol_ok in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { simultaneous_rejected : bool
          ; reclaimed_new_attachment =
              (Agent_protocol.Id.Attachment.compare first.id reclaimed.id <> 0 : bool)
          ; reclaim_token_rotated = (Option.is_some rotated_token : bool)
          ; remains_running =
              (Agent_protocol.Session.equal_desired_state state.lifecycle.desired Running
               : bool)
          }]));
  [%expect
    {|
    ((simultaneous_rejected true) (reclaimed_new_attachment true)
     (reclaim_token_rotated true) (remains_running true))
    |}]
;;

let%expect_test "read-only attachments cannot mutate actor state" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:8
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
                  Agent_protocol.Id.Attachment.of_string "att_actor_read_only"
                  |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let attachment, _ =
        Agent_session.Session_actor.attach actor ~mode:Read_only ~subscribe:false
        |> protocol_ok
      in
      let code =
        match Agent_session.Session_actor.start actor ~attachment_id:attachment.id with
        | Ok _ -> "accepted"
        | Error error -> Agent_protocol.Error.code_to_string error.code
      in
      Agent_session.Session_actor.shutdown actor;
      print_endline code));
  [%expect {| permission_denied |}]
;;

let%expect_test "history IDs are allocated only from actor-committed blocks" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let commits = ref 0 in
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:8
          ~compaction_env:None
          ~initial_state:
            (actor_state
               ~workspace_instance
               ~liveness:Process_bound
               ~start_immediately:false)
          ~persistence:
            { commit =
                (fun ~command_audit:_ ~previous:_ _ ->
                  Int.incr commits;
                  Ok ())
            }
          ~operation_worker:None
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id =
                (fun () ->
                  Agent_protocol.Id.Attachment.of_string "att_actor_unused" |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let source =
        Agent_session.History_id_source.create
          ~namespace:"reserved"
          ~block_size:2
          ~reserve:(fun ~count ->
            Agent_session.Session_actor.reserve_history_block actor ~count)
        |> protocol_ok
      in
      let allocate () =
        Agent_session.History_id_source.allocate source
        |> protocol_ok
        |> History_entry.Id.sequence
      in
      let first = allocate () in
      let second = allocate () in
      let third = allocate () in
      let state = Agent_session.Session_actor.state actor |> protocol_ok in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { allocated = ([ first; second; third ] : int list)
          ; reservation_commits = (!commits : int)
          ; durable_high_water = (state.conversation.next_history_sequence : int64)
          }]));
  [%expect
    {|
    ((allocated (0 1 2)) (reservation_commits 2) (durable_high_water 4))
    |}]
;;

let%expect_test "durable history source adapts the response engine contract" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:8
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
                  Agent_protocol.Id.Attachment.of_string "att_source_unused"
                  |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let source =
        Agent_session.History_id_source.create
          ~namespace:"durable-source"
          ~block_size:2
          ~reserve:(fun ~count ->
            Agent_session.Session_actor.reserve_history_block actor ~count)
        |> protocol_ok
      in
      let adapted = Agent_session.History_id_source.as_history_entry_source source in
      let id = History_entry.Id_source.allocate adapted |> Result.ok_or_failwith in
      let item =
        Openai.Responses.Item.Input_message
          { role = User
          ; content = [ Text { text = "hello"; _type = "input_text" } ]
          ; _type = "message"
          }
      in
      let entry = History_entry.create_with_id ~id item in
      let validation = History_entry.Id_source.validate adapted [ entry ] in
      let invalid_id =
        History_entry.Id.create ~namespace:"durable-source" ~sequence:2
        |> Result.ok_or_failwith
      in
      let invalid = History_entry.create_with_id ~id:invalid_id item in
      let invalid_validation = History_entry.Id_source.validate adapted [ invalid ] in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { namespace = (History_entry.Id_source.namespace adapted : string)
          ; first_sequence = (History_entry.Id.sequence id : int)
          ; valid = (Result.is_ok validation : bool)
          ; outside_reservation_rejected = (Result.is_error invalid_validation : bool)
          }]));
  [%expect
    {|
    ((namespace durable-source) (first_sequence 0) (valid true)
     (outside_reservation_rejected true))
    |}]
;;

let%expect_test "foreground worker commits history before terminal operation" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let worker_started, started = Eio.Promise.create () in
      let release_worker, release = Eio.Promise.create () in
      let attachment_id =
        Agent_protocol.Id.Attachment.of_string "att_actor_worker" |> protocol_ok
      in
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:
            (actor_state
               ~workspace_instance
               ~liveness:Process_bound
               ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:(Some (completed_worker ~started ~release:release_worker))
          ~services:
            { now = (fun () -> Agent_protocol.Timestamp.now ())
            ; create_attachment_id = (fun () -> attachment_id)
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
      let source =
        Agent_session.History_id_source.create
          ~namespace:(Agent_protocol.Id.Session.to_string session_id)
          ~block_size:2
          ~reserve:(fun ~count ->
            Agent_session.Session_actor.reserve_history_block actor ~count)
        |> protocol_ok
      in
      let user_id = Agent_session.History_id_source.allocate source |> protocol_ok in
      let user_entry =
        Agent_session.History_codec.user_text ~id:user_id "hello"
        |> Agent_session.History_codec.to_protocol
      in
      let submission =
        Agent_session.Session_actor.submit_message
          actor
          ~attachment_id:attachment.id
          user_entry
        |> protocol_ok
      in
      Eio.Promise.await worker_started;
      let deferred_id = Agent_session.History_id_source.allocate source |> protocol_ok in
      let deferred_entry =
        Agent_session.History_codec.user_text ~id:deferred_id "later"
        |> Agent_session.History_codec.to_protocol
      in
      let deferred =
        Agent_session.Session_actor.submit_message
          actor
          ~attachment_id:attachment.id
          deferred_entry
        |> protocol_ok
      in
      Eio.Promise.resolve release ();
      let state = await_idle actor in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { disposition =
              (submission.disposition
               : Agent_protocol.Method_result.Send_message.disposition)
          ; operation_started = (Option.is_some submission.operation_id : bool)
          ; deferred_disposition =
              (deferred.disposition
               : Agent_protocol.Method_result.Send_message.disposition)
          ; deferred_operation = (Option.is_some deferred.operation_id : bool)
          ; deferred_count = (List.length state.conversation.deferred_user_entries : int)
          ; history_count = (List.length state.conversation.canonical_history : int)
          ; high_water = (state.conversation.next_history_sequence : int64)
          ; active_operation = (Option.is_some state.active_operation : bool)
          }]));
  [%expect
    {|
    ((disposition Started) (operation_started true)
     (deferred_disposition Deferred) (deferred_operation false)
     (deferred_count 1) (history_count 2) (high_water 66)
     (active_operation false))
    |}]
;;

let%expect_test
    "mid-operation attachment snapshots retain nested calls and clear on completion"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let ready, ready_u = Eio.Promise.create () in
      let release, release_u = Eio.Promise.create () in
      let worker =
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input capabilities ->
          List.iter [ "tool-a"; "agent-b" ] ~f:(fun call_id ->
            capabilities.publish_live
              ~kind:Tool_started
              ~payload:
                (`Object
                    [ "call_id", `String call_id
                    ; "name", `String "child"
                    ; "kind", `String "function"
                    ; "payload", `String "{}"
                    ; "agent_page_kind", `String "subagent"
                    ]));
          Eio.Promise.resolve ready_u ();
          Eio.Promise.await release;
          List.iter [ "tool-a"; "agent-b" ] ~f:(fun call_id ->
            capabilities.publish_live
              ~kind:Tool_finished
              ~payload:
                (`Object
                    [ "call_id", `String call_id
                    ; "outcome", `String "returned"
                    ; "output", `Null
                    ]));
          Completed
            { final_history = input.history
            ; runtime_requests = []
            ; moderator_snapshot = None
            })
      in
      let actor =
        Agent_session.Session_actor.create
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:
            (actor_state
               ~workspace_instance
               ~liveness:Process_bound
               ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:(Some worker)
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id = Agent_protocol.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "test-token")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let writer, _ =
        Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
        |> protocol_ok
      in
      ignore
        (Agent_session.Session_actor.start actor ~attachment_id:writer.id |> protocol_ok
         : Agent_protocol.Session.t);
      let entry =
        Agent_session.History_codec.user_text ~id:history_id "start"
        |> Agent_session.History_codec.to_protocol
      in
      ignore
        (Agent_session.Session_actor.submit_message actor ~attachment_id:writer.id entry
         |> protocol_ok
         : Agent_session.Session_actor.submission);
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
        Eio.Promise.await ready);
      let _, _, snapshot, _ =
        Agent_session.Session_actor.attach_with_snapshot
          actor
          ~principal_id:None
          ~reclaim_token:None
          ~mode:Read_only
          ~subscribe:true
        |> protocol_ok
      in
      assert (List.length snapshot.active_tool_calls = 2);
      assert (List.length snapshot.active_agent_calls = 2);
      Eio.Promise.resolve release_u ();
      ignore (await_idle actor : Agent_session.Session_state.t);
      let finished = Agent_session.Session_actor.snapshot actor |> protocol_ok in
      assert (
        List.is_empty finished.active_tool_calls
        && List.is_empty finished.active_agent_calls);
      Agent_session.Session_actor.shutdown actor;
      print_endline "mid-call attach: 2 active tools and agents; completion: 0"));
  [%expect {| mid-call attach: 2 active tools and agents; completion: 0 |}]
;;

let%expect_test
    "writer preflight and job cancellation reject stale owner leases without state \
     changes"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let now = ref timestamp in
      let actor =
        Agent_session.Session_actor.create
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:
            (actor_state
               ~workspace_instance
               ~liveness:(Owner_bound { disconnect_grace_ms = 1000; stop_mode = Cancel })
               ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:None
          ~services:
            { now = (fun () -> !now)
            ; create_attachment_id = Agent_protocol.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "lease-test")
            ; job_results = None
            ; schedule_limits = Agent_session.Staged_schedules.default_limits
            ; subscription_limits = Agent_session.Staged_subscriptions.default_limits
            ; state_committed = (fun _ _ -> ())
            }
      in
      let owner, _ =
        Agent_session.Session_actor.attach actor ~mode:Owner_read_write ~subscribe:false
        |> protocol_ok
      in
      Agent_session.Session_actor.authorize_writer actor ~attachment_id:owner.id
      |> protocol_ok;
      let job =
        Agent_protocol.Job.
          { id = Agent_protocol.Id.Job.create ()
          ; session_id
          ; generation = 0
          ; kind = Model_call
          ; payload = `Object []
          ; status = Queued
          ; retry_policy = Never
          ; attempt = 0
          ; created_at = timestamp
          ; started_at = None
          ; next_run_at = None
          ; completed_at = None
          ; result = None
          ; delivery = Pending
          ; launch = None
          ; progress = None
          }
      in
      ignore
        (Agent_session.Session_actor.add_job actor job |> protocol_ok
         : Agent_protocol.Job.t);
      let reader, _ =
        Agent_session.Session_actor.attach actor ~mode:Read_only ~subscribe:false
        |> protocol_ok
      in
      let before = Agent_session.Session_actor.snapshot actor |> protocol_ok in
      let denied =
        Agent_session.Session_actor.cancel_job
          actor
          ~attachment_id:reader.id
          ~job_id:job.id
          ()
      in
      (match denied with
       | Error error ->
         assert (Agent_protocol.Error.equal_code error.code Permission_denied)
       | Ok _ -> failwith "read-only cancellation accepted");
      now := (Option.value_exn owner.owner_lease).expires_at;
      let denied =
        Agent_session.Session_actor.authorize_writer actor ~attachment_id:owner.id
      in
      (match denied with
       | Error error -> assert (Agent_protocol.Error.equal_code error.code Lease_stale)
       | Ok _ -> failwith "expired lease accepted");
      let denied =
        Agent_session.Session_actor.cancel_job
          actor
          ~attachment_id:owner.id
          ~job_id:job.id
          ()
      in
      (match denied with
       | Error error -> assert (Agent_protocol.Error.equal_code error.code Lease_stale)
       | Ok _ -> failwith "expired owner cancellation accepted");
      let after = Agent_session.Session_actor.snapshot actor |> protocol_ok in
      assert (Int64.equal before.revision after.revision);
      assert (Int64.equal before.latest_event_sequence after.latest_event_sequence);
      assert (Poly.equal (List.hd_exn after.jobs).status Agent_protocol.Job.Queued);
      Agent_session.Session_actor.shutdown actor;
      print_endline "read-only and expired owner denied; queued job and session unchanged"));
  [%expect {| read-only and expired owner denied; queued job and session unchanged |}]
;;
