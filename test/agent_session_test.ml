open Core

let () = Mirage_crypto_rng_unix.use_default ()

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

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "unexpected protocol error", (error : Agent_protocol.Error.t)]
;;

let store_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "unexpected store error", (error : Agent_store.Store_error.t)]
;;

(* Compare the complete snapshots, including counters and recovery metadata. *)
let assert_same_session_snapshot expected actual =
  [%test_eq: Sexp.t]
    (Agent_session.Session_state.sexp_of_t expected)
    (Agent_session.Session_state.sexp_of_t actual)
;;

let workspace_id =
  Agent_protocol.Id.Workspace_definition.of_string "wsd_agent_session_test" |> protocol_ok
;;

let instance_id =
  Agent_protocol.Id.Workspace_instance.of_string "wsi_agent_session_test" |> protocol_ok
;;

let second_instance_id =
  Agent_protocol.Id.Workspace_instance.of_string "wsi_agent_session_second" |> protocol_ok
;;

let session_id =
  Agent_protocol.Id.Session.of_string "ses_agent_session_test" |> protocol_ok
;;

let second_session_id =
  Agent_protocol.Id.Session.of_string "ses_agent_session_second" |> protocol_ok
;;

let third_session_id =
  Agent_protocol.Id.Session.of_string "ses_agent_session_third" |> protocol_ok
;;

let prompt_id =
  Agent_protocol.Id.Prompt_definition.of_string "prd_agent_session_test" |> protocol_ok
;;

let transaction_id =
  Agent_protocol.Id.Transaction.of_string "txn_agent_session_test" |> protocol_ok
;;

let prompt_revision_id =
  Agent_protocol.Id.Prompt_revision.of_string "prv_agent_session_test" |> protocol_ok
;;

let permission_id =
  Agent_protocol.Id.Permission.of_string "per_agent_session_test" |> protocol_ok
;;

let operation_id =
  Agent_protocol.Id.Operation.of_string "op_agent_session_test" |> protocol_ok
;;

let history_id =
  History_entry.Id.create ~namespace:"actor" ~sequence:0
  |> function
  | Ok value -> value
  | Error error -> failwith error
;;

let second_prompt_id =
  Agent_protocol.Id.Prompt_definition.of_string "prd_agent_session_second" |> protocol_ok
;;

let principal_id =
  Agent_protocol.Id.Principal.of_string "pri_agent_session_test" |> protocol_ok
;;

let second_principal_id =
  Agent_protocol.Id.Principal.of_string "pri_agent_session_second" |> protocol_ok
;;

let timestamp = Agent_protocol.Timestamp.of_string "2026-08-15T12:00:00Z" |> protocol_ok

let with_temp_directory f =
  Eio_main.run (fun env ->
    let temporary =
      Filename.concat
        (Sys.getenv "TMPDIR" |> Option.value ~default:"/tmp")
        ("ochat-agent-session."
         ^ (Agent_protocol.Id.Transaction.create ()
            |> Agent_protocol.Id.Transaction.to_string))
    in
    let root = Eio.Path.(Eio.Stdenv.fs env / temporary) in
    Eio.Path.mkdir ~perm:0o700 root;
    Exn.protect
      ~f:(fun () -> f env temporary)
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root))
;;

let permission_invocation =
  Agent_session.Permission_policy.
    { tool_name = "write_file"
    ; identity_digest = "identity"
    ; invocation_display = "write_file(<redacted>)"
    ; effects = [ "filesystem.write" ]
    }
;;

let permission_policy ~tool_default ~fallback ~evaluator ~reviewer =
  Agent_session.Permission_policy.create
    ~id:"test.permission"
    ~tool_default
    ~approval_timeout_ms:None
    ~fallback
    ~manifest_authorization:Require_grant
    ~evaluator
    ~evaluator_revision:(Option.some_if (Option.is_some evaluator) "test-v1")
    ~reviewer
  |> protocol_ok
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

let%expect_test "physical workspace resolution captures and verifies filesystem identity" =
  with_temp_directory (fun env physical_root ->
    let definition =
      Agent_session.Workspace_definition.create
        ~id:workspace_id
        ~config_name:"physical"
        ~source:(Physical { configured_root = physical_root })
        ~access:Shared_write
        ~conflict_domain:None
        ~prompt_limits:[]
      |> store_ok
    in
    let instance =
      Agent_session.Workspace_resolver.resolve
        ~env
        ~instance_id
        ~session_directory:physical_root
        definition
      |> store_ok
    in
    let available =
      Agent_session.Workspace_resolver.verify_available ~env instance |> Result.is_ok
    in
    print_s
      [%sexp
        { source_kind =
            (instance.source_kind : Agent_session.Workspace_instance.source_kind)
        ; server_created = (instance.server_created : bool)
        ; conflict_domain_is_set = (not (String.is_empty instance.conflict_domain) : bool)
        ; available : bool
        }]);
  [%expect
    {|
    ((source_kind Physical) (server_created false) (conflict_domain_is_set true)
     (available true))
    |}]
;;

let%expect_test
    "temporary workspaces are unique and guarded cleanup removes only managed paths"
  =
  with_temp_directory (fun env temporary ->
    let managed_root = Filename.concat temporary "managed" in
    let session_directory = Filename.concat temporary "session" in
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / managed_root);
    Eio.Path.mkdir ~perm:0o700 Eio.Path.(Eio.Stdenv.fs env / session_directory);
    let definition =
      Agent_session.Workspace_definition.create
        ~id:workspace_id
        ~config_name:"temporary"
        ~source:
          (Temporary
             { location = System_tmp
             ; cleanup = On_session_delete
             ; managed_root = Some managed_root
             })
        ~access:Exclusive
        ~conflict_domain:None
        ~prompt_limits:[]
      |> store_ok
    in
    let resolve instance_id =
      Agent_session.Workspace_resolver.resolve
        ~env
        ~instance_id
        ~session_directory
        definition
      |> store_ok
    in
    let first = resolve instance_id in
    let second = resolve second_instance_id in
    let unique =
      not
        (String.equal first.canonical_root.native_path second.canonical_root.native_path)
    in
    let cleaned =
      Agent_session.Workspace_cleanup.cleanup
        ~env
        ~protected_roots:
          { data_root = Filename.concat temporary "data"
          ; physical_workspaces = []
          ; managed_roots = [ managed_root ]
          }
        ~expected_path:first.canonical_root.native_path
        ~has_active_lease:(fun ~conflict_domain:_ -> false)
        ~event:Session_delete
        ~now:timestamp
        first
      |> store_ok
    in
    let removed =
      not
        (Eio.Path.is_directory
           Eio.Path.(Eio.Stdenv.fs env / first.canonical_root.native_path))
    in
    print_s
      [%sexp
        { unique : bool
        ; removed : bool
        ; cleanup_recorded = (Option.is_some cleaned.cleanup_completion : bool)
        }]);
  [%expect {| ((unique true) (removed true) (cleanup_recorded true)) |}]
;;

let%expect_test "workspace leases allow shared holders and exclude exclusive holders" =
  Eio_main.run (fun _env ->
    let leases = Agent_session.Workspace_lease.create () in
    let first =
      Agent_session.Workspace_lease.acquire
        leases
        ~conflict_domain:"project"
        ~session_id
        ~mode:Shared
      |> protocol_ok
    in
    let second =
      Agent_session.Workspace_lease.acquire
        leases
        ~conflict_domain:"project"
        ~session_id:second_session_id
        ~mode:Shared
      |> protocol_ok
    in
    let exclusive_blocked =
      Agent_session.Workspace_lease.acquire
        leases
        ~conflict_domain:"project"
        ~session_id
        ~mode:Exclusive
      |> Result.is_error
    in
    Agent_session.Workspace_lease.release leases first;
    Agent_session.Workspace_lease.release leases second;
    let exclusive =
      Agent_session.Workspace_lease.acquire
        leases
        ~conflict_domain:"project"
        ~session_id
        ~mode:Exclusive
      |> protocol_ok
    in
    let shared_blocked =
      Agent_session.Workspace_lease.acquire
        leases
        ~conflict_domain:"project"
        ~session_id:second_session_id
        ~mode:Shared
      |> Result.is_error
    in
    Agent_session.Workspace_lease.release leases exclusive;
    print_s [%sexp { exclusive_blocked : bool; shared_blocked : bool }]);
  [%expect {| ((exclusive_blocked true) (shared_blocked true)) |}]
;;

let%expect_test "quota acquisition enforces limits and releases construction separately" =
  Eio_main.run (fun _env ->
    let workspace_leases = Agent_session.Workspace_lease.create () in
    let manager =
      Agent_session.Quota_manager.create
        ~limits:
          { global_running_sessions = 2
          ; per_principal_running_sessions = 1
          ; runtime_construction = 1
          }
        ~workspace_leases
      |> protocol_ok
    in
    let quota_key = Agent_session.Quota_key.{ conflict_domain = "project"; prompt_id } in
    let request session_id principal_id overflow =
      Agent_session.Quota_manager.
        { session_id
        ; principal_id
        ; quota_key
        ; prompt_limit = 2
        ; overflow
        ; workspace_lease_mode = None
        }
    in
    let first =
      match
        Agent_session.Quota_manager.try_acquire
          manager
          (request session_id principal_id Reject)
      with
      | Acquired acquisition -> acquisition
      | Queue_required _ | Rejected _ -> raise_s [%sexp "expected first acquisition"]
    in
    let same_principal_queued =
      match
        Agent_session.Quota_manager.try_acquire
          manager
          (request second_session_id principal_id Queue)
      with
      | Queue_required _ -> true
      | Acquired _ | Rejected _ -> false
    in
    Agent_session.Quota_manager.runtime_ready manager first;
    let second =
      match
        Agent_session.Quota_manager.try_acquire
          manager
          (request second_session_id second_principal_id Reject)
      with
      | Acquired acquisition -> acquisition
      | Queue_required _ | Rejected _ -> raise_s [%sexp "expected second acquisition"]
    in
    let global_rejected =
      match
        Agent_session.Quota_manager.try_acquire
          manager
          (request third_session_id principal_id Reject)
      with
      | Rejected _ -> true
      | Acquired _ | Queue_required _ -> false
    in
    let running_before_release = Agent_session.Quota_manager.running_sessions manager in
    Agent_session.Quota_manager.release manager second;
    Agent_session.Quota_manager.release manager first;
    let running_after_release = Agent_session.Quota_manager.running_sessions manager in
    print_s
      [%sexp
        { same_principal_queued : bool
        ; global_rejected : bool
        ; running_before_release : int
        ; running_after_release : int
        }]);
  [%expect
    {|
    ((same_principal_queued true) (global_rejected true)
     (running_before_release 2) (running_after_release 0))
    |}]
;;

let%expect_test "quota acquisition respects exclusive workspace leases" =
  Eio_main.run (fun _env ->
    let workspace_leases = Agent_session.Workspace_lease.create () in
    let manager =
      Agent_session.Quota_manager.create
        ~limits:
          { global_running_sessions = 3
          ; per_principal_running_sessions = 3
          ; runtime_construction = 3
          }
        ~workspace_leases
      |> protocol_ok
    in
    let quota_key =
      Agent_session.Quota_key.{ conflict_domain = "exclusive"; prompt_id }
    in
    let request session_id =
      Agent_session.Quota_manager.
        { session_id
        ; principal_id
        ; quota_key
        ; prompt_limit = 3
        ; overflow = Queue
        ; workspace_lease_mode = Some Exclusive
        }
    in
    let first =
      match Agent_session.Quota_manager.try_acquire manager (request session_id) with
      | Acquired acquisition -> acquisition
      | Queue_required _ | Rejected _ -> raise_s [%sexp "expected acquisition"]
    in
    let second_queued =
      match
        Agent_session.Quota_manager.try_acquire manager (request second_session_id)
      with
      | Queue_required _ -> true
      | Acquired _ | Rejected _ -> false
    in
    Agent_session.Quota_manager.release manager first;
    print_s
      [%sexp
        { second_queued : bool
        ; active_leases =
            (Agent_session.Workspace_lease.active
               workspace_leases
               ~conflict_domain:"exclusive"
             : int)
        }]);
  [%expect {| ((second_queued true) (active_leases 0)) |}]
;;

let%expect_test "start queue is FIFO per key and rotates keys fairly" =
  Eio_main.run (fun _env ->
    let queue = Agent_session.Start_queue.create () in
    let first_key = Agent_session.Quota_key.{ conflict_domain = "first"; prompt_id } in
    let second_key =
      Agent_session.Quota_key.{ conflict_domain = "second"; prompt_id = second_prompt_id }
    in
    let ticket session_id accepted_command_sequence quota_key =
      Agent_session.Start_queue.
        { session_id; accepted_command_sequence; quota_key; created_at = timestamp }
    in
    Agent_session.Start_queue.enqueue queue (ticket session_id 1L first_key)
    |> protocol_ok;
    Agent_session.Start_queue.enqueue queue (ticket second_session_id 2L first_key)
    |> protocol_ok;
    Agent_session.Start_queue.enqueue queue (ticket third_session_id 3L second_key)
    |> protocol_ok;
    let take () =
      Agent_session.Start_queue.take_eligible queue ~eligible:(fun _ -> true)
      |> Option.value_exn
      |> fun (ticket : Agent_session.Start_queue.ticket) ->
      ticket.accepted_command_sequence
    in
    let first = take () in
    let second = take () in
    let third = take () in
    let order = [ first; second; third ] in
    print_s
      [%sexp
        { order : int64 list; remaining = (Agent_session.Start_queue.length queue : int) }]);
  [%expect {| ((order (1 3 2)) (remaining 0)) |}]
;;

let%expect_test "start queue retries preserve per-key FIFO and key rotation" =
  Eio_main.run (fun _env ->
    let queue = Agent_session.Start_queue.create () in
    let first_key = Agent_session.Quota_key.{ conflict_domain = "first"; prompt_id } in
    let second_key =
      Agent_session.Quota_key.{ conflict_domain = "second"; prompt_id = second_prompt_id }
    in
    let ticket session_id accepted_command_sequence quota_key =
      Agent_session.Start_queue.
        { session_id; accepted_command_sequence; quota_key; created_at = timestamp }
    in
    let first_ticket = ticket session_id 1L first_key in
    Agent_session.Start_queue.enqueue queue first_ticket |> protocol_ok;
    Agent_session.Start_queue.enqueue queue (ticket second_session_id 2L first_key)
    |> protocol_ok;
    Agent_session.Start_queue.enqueue queue (ticket third_session_id 3L second_key)
    |> protocol_ok;
    let take () =
      Agent_session.Start_queue.take_eligible queue ~eligible:(fun _ -> true)
      |> Option.value_exn
    in
    let retried = take () in
    Agent_session.Start_queue.requeue queue retried |> protocol_ok;
    let first = (take ()).accepted_command_sequence in
    let second = (take ()).accepted_command_sequence in
    let third = (take ()).accepted_command_sequence in
    let order = [ first; second; third ] in
    print_s
      [%sexp
        { retried = (retried.accepted_command_sequence : int64)
        ; order : int64 list
        ; remaining = (Agent_session.Start_queue.length queue : int)
        }]);
  [%expect {| ((retried 1) (order (3 1 2)) (remaining 0)) |}]
;;

let%expect_test "start queue inspection rotates only after completion" =
  Eio_main.run (fun _env ->
    let queue = Agent_session.Start_queue.create () in
    let first_key = Agent_session.Quota_key.{ conflict_domain = "first"; prompt_id } in
    let second_key =
      Agent_session.Quota_key.{ conflict_domain = "second"; prompt_id = second_prompt_id }
    in
    let ticket session_id accepted_command_sequence quota_key =
      Agent_session.Start_queue.
        { session_id; accepted_command_sequence; quota_key; created_at = timestamp }
    in
    let first_ticket = ticket session_id 1L first_key in
    Agent_session.Start_queue.enqueue queue first_ticket |> protocol_ok;
    Agent_session.Start_queue.enqueue queue (ticket second_session_id 2L first_key)
    |> protocol_ok;
    Agent_session.Start_queue.enqueue queue (ticket third_session_id 3L second_key)
    |> protocol_ok;
    let heads () =
      Agent_session.Start_queue.heads queue
      |> List.map ~f:(fun ticket -> ticket.accepted_command_sequence)
    in
    let before = heads () in
    let unchanged = heads () in
    let completed = Agent_session.Start_queue.complete queue first_ticket in
    let after = heads () in
    print_s
      [%sexp
        { before : int64 list
        ; unchanged : int64 list
        ; completed : bool
        ; after : int64 list
        }]);
  [%expect
    {|
    ((before (1 3)) (unchanged (1 3)) (completed true) (after (3 2)))
    |}]
;;

let%expect_test "pinned prompt revisions restore after live sources disappear" =
  with_temp_directory (fun env temporary ->
    let prompt_root = Filename.concat temporary "prompt" in
    let imported_directory = Filename.concat prompt_root "parts" in
    let artifact_root = Filename.concat temporary "artifacts" in
    Eio.Path.mkdirs
      ~exists_ok:true
      ~perm:0o700
      Eio.Path.(Eio.Stdenv.fs env / imported_directory);
    let root_file = Filename.concat prompt_root "root.chatmd" in
    let imported_file = Filename.concat imported_directory "developer.chatmd" in
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.fs env / root_file)
      "<import src=\"parts/developer.chatmd\"/>";
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(Eio.Stdenv.fs env / imported_file)
      "<tool name=\"read_file\"><read id=\"source\" path=\"${source_dir}\"/></tool>\n\
       <tool name=\"nested\" agent=\"nested/child.chatmd\" local/>\n\
       <user><agent src=\"nested/child.chatmd\" local/></user>";
    let nested = Eio.Path.(Eio.Stdenv.fs env / imported_directory / "nested") in
    Eio.Path.mkdir ~perm:0o700 nested;
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(nested / "child.chatmd")
      "<import src=\"helper.chatmd\"/><tool name=\"back\" agent=\"../../root.chatmd\" \
       local/>";
    Eio.Path.save
      ~create:(`Exclusive 0o600)
      Eio.Path.(nested / "helper.chatmd")
      "<developer>pinned-child-marker</developer>";
    let definition =
      Agent_session.Prompt_definition.create
        ~id:prompt_id
        ~config_name:"pinned"
        ~root_file
        ~allowed_workspaces:[ workspace_id ]
        ~permission_profile:"interactive"
        ~runtime_policy:None
        ~enabled:true
        ~description:None
      |> store_ok
    in
    let artifact_store =
      Agent_store.Prompt_artifact_store.create ~env ~root:artifact_root |> store_ok
    in
    let revision =
      Agent_session.Prompt_revision_builder.build
        ~env
        ~artifact_store
        ~transaction_id
        ~created_at:timestamp
        definition
      |> function
      | Ok value -> value
      | Error diagnostics ->
        raise_s
          [%sexp (diagnostics : Agent_session.Prompt_revision_builder.Diagnostic.t list)]
    in
    let revision_id = Agent_session.Prompt_revision.id revision in
    let source_dir revision =
      Agent_session.Prompt_revision.elements revision
      |> List.find_map_exn ~f:(function
        | Prompt.Chat_markdown.Tool (Read_file specification) ->
          Some specification.source.source_dir
        | _ -> None)
    in
    let expected_source_dir =
      Filename.concat
        (Eio.Path.native_exn (Agent_session.Prompt_revision.materialized_tree revision))
        "parts"
    in
    let fresh_materialized = String.equal (source_dir revision) expected_source_dir in
    Eio.Path.rmtree Eio.Path.(Eio.Stdenv.fs env / prompt_root);
    let restored =
      Agent_session.Prompt_revision_builder.restore ~artifact_store definition revision_id
      |> function
      | Ok value -> value
      | Error diagnostics ->
        raise_s
          [%sexp (diagnostics : Agent_session.Prompt_revision_builder.Diagnostic.t list)]
    in
    let artifact = Agent_session.Prompt_revision.artifact restored in
    let child =
      Agent_session.Prompt_revision.elements restored
      |> List.find_map_exn ~f:(function
        | Prompt.Chat_markdown.Tool (Agent tool) -> Some tool.agent
        | _ -> None)
    in
    assert (String.is_prefix child ~prefix:expected_source_dir);
    assert (
      String.is_substring
        (Eio.Path.load Eio.Path.(Eio.Stdenv.fs env / child))
        ~substring:"helper.chatmd");
    assert (
      String.is_substring
        (Eio.Path.load
           Eio.Path.(Eio.Stdenv.fs env / Filename.dirname child / "helper.chatmd"))
        ~substring:"pinned-child-marker");
    print_s
      [%sexp
        { restored_elements =
            (List.length (Agent_session.Prompt_revision.elements restored) : int)
        ; captured_sources = (List.length artifact.sources : int)
        ; fresh_materialized : bool
        ; restored_materialized =
            (String.equal (source_dir restored) expected_source_dir : bool)
        ; pinned_revision =
            (Agent_protocol.Id.Prompt_revision.compare
               revision_id
               (Agent_session.Prompt_revision.id restored)
             = 0
             : bool)
        }]);
  [%expect
    {|
    ((restored_elements 3) (captured_sources 3) (fresh_materialized true)
     (restored_materialized true) (pinned_revision true))
    |}]
;;

let actor_state ~workspace_instance ~liveness ~start_immediately =
  let execution_host, persistence =
    match liveness with
    | Agent_protocol.Session.Process_bound ->
      Agent_protocol.Session.Embedded, Agent_protocol.Session.Transient
    | Detached | Owner_bound _ -> Daemon, Durable
  in
  let protocol =
    Agent_protocol.Session.Spec.create
      ~execution_host
      ~prompt:(Local_path "/prompt.chatmd")
      ~workspace:Current
      ~liveness
      ~persistence
      ~start_immediately
      ~labels:[]
      ()
    |> protocol_ok
  in
  let identity =
    Agent_session.Session_state.Identity.
      { session_id
      ; display_name = Some "actor"
      ; creating_principal = Some principal_id
      ; created_at = timestamp
      ; updated_at = timestamp
      ; labels = []
      ; generation = 0
      }
  in
  let spec =
    Agent_session.Session_state.Spec.
      { protocol
      ; prompt_definition_id = None
      ; prompt_revision_id
      ; workspace_instance
      ; permission_profile = "interactive"
      ; permission_profile_digest = "profile-digest"
      ; runtime_policy = None
      ; quota_key = None
      }
  in
  Agent_session.Session_state.create ~identity ~spec ~initial_history:[]
;;

let actor_entry =
  Agent_protocol.History.
    { id = history_id
    ; role = User
    ; kind = Message
    ; payload = `Object [ "text", `String "hello" ]
    ; provenance = Canonical
    ; redacted = false
    }
;;

let with_actor_workspace f =
  with_temp_directory (fun env temporary ->
    let workspace_instance =
      Agent_session.Workspace_resolver.resolve_current
        ~env
        ~instance_id
        ~path:temporary
        ~access:Shared_write
        ~created_at:timestamp
      |> store_ok
    in
    f env workspace_instance)
;;

let invocation_fixture () =
  Agent_protocol.Invocation.create
    { id = Agent_protocol.Id.Invocation.of_string "inv_session_test" |> protocol_ok
    ; session_id
    ; generation = 0
    ; origin = Script
    ; provider_call_id = None
    ; call_entry_id = None
    ; parent_invocation = None
    ; parent_job = None
    ; tool_name = "read_file"
    ; implementation_revision = "revision-1"
    ; capability_fingerprint = "capability-1"
    ; input = `Null
    ; created_at = timestamp
    ; deadline = None
    }
  |> protocol_ok
;;

let%expect_test "invocation deltas replay through durable transactions and snapshots" =
  with_actor_workspace (fun _env workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let admitted = invocation_fixture () in
    let dispatched = Agent_protocol.Invocation.dispatch admitted |> protocol_ok in
    let resolved =
      Agent_protocol.Invocation.resolve
        dispatched
        ~session_id
        ~generation:0
        (Complete (`String "done"))
      |> protocol_ok
    in
    let delta =
      Agent_session.Session_delta.Batch
        [ Invocation_changed admitted
        ; Invocation_changed dispatched
        ; Invocation_changed resolved
        ]
    in
    let transaction =
      Agent_store.Transaction.create
        ~session_id
        ~generation:0
        ~transaction_sequence:1L
        ~previous_transaction_hash:None
        ~session_revision:1L
        ~first_event_sequence:None
        ~last_event_sequence:None
        ~accepted_at_ns:
          (Agent_protocol.Timestamp.to_time_ns timestamp
           |> Time_ns.to_int_ns_since_epoch
           |> Int64.of_int)
        ~command_audit:None
        ~delta:(Sexp.to_string_mach (Agent_session.Session_delta.sexp_of_t delta))
        ~durable_events:[]
      |> store_ok
    in
    let transaction =
      Agent_store.Transaction.decode (Agent_store.Transaction.encode transaction)
      |> store_ok
    in
    let replayed =
      Agent_session.Session_persistence.apply_transaction initial transaction |> store_ok
    in
    let restored =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t replayed))
      |> store_ok
    in
    let invocation = List.hd_exn restored.invocations in
    print_s [%sexp (invocation.status : Agent_protocol.Invocation.status)];
    let published = Agent_protocol.Invocation.publish invocation |> protocol_ok in
    let final =
      Agent_session.Session_delta.apply restored (Invocation_changed published)
      |> protocol_ok
    in
    print_s
      [%sexp ((List.hd_exn final.invocations).status : Agent_protocol.Invocation.status)];
    let repeated_resolution =
      Agent_session.Session_delta.apply final (Invocation_changed resolved)
    in
    print_s [%sexp (Result.is_error repeated_resolution : bool)]);
  [%expect
    {|
    (Resolved (Complete (String done)))
    (Published (Complete (String done)))
    true |}]
;;

let%test_unit "bound publication journal replay validates the retained call and output" =
  with_actor_workspace (fun _env workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let id sequence =
      History_entry.Id.create ~namespace:"publication" ~sequence |> Result.ok_or_failwith
    in
    let call =
      History_entry.create_with_id
        ~id:(id 0)
        (Openai.Responses.Item.Function_call
           { name = "read_file"
           ; arguments = "{}"
           ; call_id = "call"
           ; _type = "function_call"
           ; id = None
           ; status = None
           })
    in
    let admitted =
      Agent_protocol.Invocation.create
        ~routing:
          (let fingerprint payload =
             Agent_protocol.Invocation.
               { sha256 = Chatmd_shell_spec.Source_ref.digest payload
               ; byte_length = String.length payload
               }
           in
           { kind = Function
           ; original_name = "alias"
           ; original_payload = fingerprint "original private input"
           ; final_payload = fingerprint "private execution input"
           ; canonical_payload = Some (fingerprint "{}")
           ; preparation = Passed
           })
        { (invocation_fixture ()).context with
          origin = Model
        ; provider_call_id = Some "call"
        ; call_entry_id = Some (id 0)
        }
      |> protocol_ok
    in
    let routing = Option.value_exn admitted.routing in
    List.iter
      [ { routing with kind = Custom }
      ; { routing with
          canonical_payload = Some { sha256 = String.make 64 'a'; byte_length = 2 }
        }
      ]
      ~f:(fun routing ->
        let wrong =
          Agent_protocol.Invocation.create ~routing admitted.context |> protocol_ok
        in
        assert (
          Result.is_error
            (Agent_session.Session_delta.apply
               initial
               (Batch
                  [ Canonical_entries_appended
                      [ Agent_session.History_codec.to_protocol call ]
                  ; Invocation_changed wrong
                  ]))));
    let dispatched = Agent_protocol.Invocation.dispatch admitted |> protocol_ok in
    let resolved =
      Agent_protocol.Invocation.resolve
        dispatched
        ~session_id
        ~generation:0
        (Complete (`String "done"))
      |> protocol_ok
    in
    let output =
      History_entry.create_with_id
        ~id:(id 1)
        (Openai.Responses.Item.Function_call_output
           { output =
               Text
                 (Jsonaf.to_string
                    (Agent_protocol.Invocation.outcome_to_json
                       (Complete (`String "done"))))
           ; call_id = "call"
           ; _type = "function_call_output"
           ; id = None
           ; status = None
           })
    in
    let published =
      Agent_protocol.Invocation.publish_with_history resolved ~output_entry_id:(id 1)
      |> protocol_ok
    in
    let prefix =
      Agent_session.Session_delta.
        [ Canonical_entries_appended [ Agent_session.History_codec.to_protocol call ]
        ; Invocation_changed admitted
        ; Invocation_changed dispatched
        ; Invocation_changed resolved
        ]
    in
    assert (
      Result.is_error
        (Agent_session.Session_delta.apply
           initial
           (Batch (prefix @ [ Invocation_changed published ]))));
    let delta =
      Agent_session.Session_delta.Batch
        (prefix
         @ [ Canonical_entries_appended [ Agent_session.History_codec.to_protocol output ]
           ; Invocation_changed published
           ])
    in
    let transaction =
      Agent_store.Transaction.create
        ~session_id
        ~generation:0
        ~transaction_sequence:1L
        ~previous_transaction_hash:None
        ~session_revision:1L
        ~first_event_sequence:None
        ~last_event_sequence:None
        ~accepted_at_ns:
          (Agent_protocol.Timestamp.to_time_ns timestamp
           |> Time_ns.to_int_ns_since_epoch
           |> Int64.of_int)
        ~command_audit:None
        ~delta:(Sexp.to_string_mach (Agent_session.Session_delta.sexp_of_t delta))
        ~durable_events:[]
      |> store_ok
    in
    let transaction =
      Agent_store.Transaction.decode (Agent_store.Transaction.encode transaction)
      |> store_ok
    in
    let replayed =
      Agent_session.Session_persistence.apply_transaction initial transaction |> store_ok
    in
    let restored =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t replayed))
      |> store_ok
    in
    assert (Poly.equal restored.invocations [ published ]);
    let changed_call =
      match History_entry.item call with
      | Function_call value ->
        History_entry.create_with_id
          ~id:(id 0)
          (Function_call { value with arguments = "changed" })
      | _ -> assert false
    in
    let changed =
      { restored with
        conversation =
          { restored.conversation with
            canonical_history =
              List.map [ changed_call; output ] ~f:Agent_session.History_codec.to_protocol
          }
      }
    in
    assert (
      Result.is_error
        (Agent_session.Session_persistence.restore_snapshot
           (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t changed))));
    let compacted =
      { restored with
        conversation = { restored.conversation with canonical_history = [] }
      }
    in
    let compacted =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t compacted))
      |> store_ok
    in
    assert (Poly.equal compacted.invocations [ published ]);
    let wrong =
      Agent_session.History_codec.user_text ~id:(id 1) "forged result"
      |> Agent_session.History_codec.to_protocol
    in
    let corrupted =
      { restored with
        conversation =
          { restored.conversation with
            canonical_history = [ Agent_session.History_codec.to_protocol call; wrong ]
          }
      }
    in
    assert (
      Result.is_error
        (Agent_session.Session_persistence.restore_snapshot
           (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t corrupted)))))
;;

let%expect_test
    "legacy state migration preserves data and rejects invalid invocation ownership"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let legacy =
      match Agent_session.Session_state.sexp_of_t { initial with schema_version = 2 } with
      | Sexp.List fields ->
        Sexp.List
          (List.filter fields ~f:(function
             | Sexp.List (Sexp.Atom "invocations" :: _) -> false
             | _ -> true))
      | _ -> assert false
    in
    let migrated =
      Agent_session.Session_persistence.restore_snapshot (Sexp.to_string_mach legacy)
      |> store_ok
    in
    print_s
      [%sexp
        { version = (migrated.schema_version : int)
        ; records = (List.length migrated.invocations : int)
        }];
    let invocation = invocation_fixture () in
    let foreign =
      Agent_protocol.Invocation.create
        { invocation.context with session_id = second_session_id }
      |> protocol_ok
    in
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_delta.apply initial (Invocation_changed foreign))
         : bool)];
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_state.validate
              { initial with invocations = [ foreign ] })
         : bool)];
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_state.validate
              { initial with invocations = [ invocation; invocation ] })
         : bool)];
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_state.upgrade_schema
              { initial with schema_version = 5 })
         : bool)]);
  [%expect
    {|
    ((version 4) (records 0))
    true
    true
    true
    true |}]
;;

let%expect_test "pre-extension compaction archives remain readable after state migration" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let state =
        actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
      in
      let legacy = { state with schema_version = 2 } in
      let store =
        Agent_store.Session_store.create
          ~env
          ~sw
          ~root:
            (Filename.concat
               workspace_instance.canonical_root.native_path
               "archive-store")
          ~server_id:(Agent_protocol.Id.Server.of_string "srv_archive_test" |> protocol_ok)
          ~process_start_identity:None
          ~lock_nonce:"archive-store-lock"
        |> store_ok
      in
      let metadata =
        Agent_store.Session_store.Metadata.
          { schema_version = Agent_store.Session_store.current_schema_version
          ; session = Agent_session.Session_state.summary legacy
          ; prompt_artifact =
              Agent_protocol.Id.Prompt_revision.to_string prompt_revision_id
          ; workspace_identity = workspace_instance.conflict_domain
          ; data_schema_version = 2
          }
      in
      let handle =
        Agent_store.Session_store.create_session
          store
          ~sw
          ~transaction_id
          ~actor_lock_nonce:"archive-actor-lock"
          metadata
        |> store_ok
      in
      let reference = Agent_session.Compaction_archive.reference legacy operation_id in
      Agent_session.Compaction_archive.write
        ~env
        ~handle
        ~max_payload_length:1048576
        reference
        legacy
      |> protocol_ok;
      let restored =
        Agent_session.Compaction_archive.read
          ~env
          ~handle
          ~max_payload_length:1048576
          reference
        |> protocol_ok
      in
      print_s
        [%sexp
          { version = (restored.schema_version : int)
          ; records = (List.length restored.invocations : int)
          }];
      Agent_store.Session_store.close_session store handle |> store_ok;
      Agent_store.Session_store.close store |> store_ok));
  [%expect {| ((version 4) (records 0)) |}]
;;

let extension_fixture workspace_instance =
  let initial =
    actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
  in
  let admitted = invocation_fixture () in
  let dispatched = Agent_protocol.Invocation.dispatch admitted |> protocol_ok in
  let subscription =
    Agent_protocol.Subscription.create
      { id = Agent_protocol.Id.Subscription.of_string "sub_atomic" |> protocol_ok
      ; session_id
      ; generation = 0
      ; invocation_id = admitted.context.id
      ; kind = "fixture"
      ; created_at = timestamp
      ; deadline =
          Agent_protocol.Timestamp.of_string "2026-08-15T13:00:00Z" |> protocol_ok
      ; completion_schema = None
      ; wake = Request_turn
      ; ingress_capability = None
      }
    |> protocol_ok
  in
  let finished, _ =
    Agent_protocol.Subscription.finish
      subscription
      ~expected_epoch:0
      ~now:timestamp
      (Succeeded (`String "ready"))
    |> protocol_ok
  in
  let resolved =
    Agent_protocol.Invocation.resolve
      dispatched
      ~session_id
      ~generation:0
      (Pending (Subscription subscription.context.id, `String "accepted"))
    |> protocol_ok
  in
  let delivery =
    Agent_protocol.Delivery.create
      { id = Agent_protocol.Id.Delivery.of_string "dlv_atomic" |> protocol_ok
      ; session_id
      ; generation = 0
      ; invocation_id = Some admitted.context.id
      ; work = Some (Subscription subscription.context.id)
      ; correlation = "fixture"
      ; source = Moderator
      ; completion = Succeeded (`String "ready")
      ; wake = Request_turn
      ; created_at = timestamp
      }
    |> protocol_ok
  in
  let delta =
    Agent_session.Session_delta.Batch
      [ Invocation_changed admitted
      ; Invocation_changed dispatched
      ; Subscription_changed subscription
      ; Subscription_changed finished
      ; Invocation_changed resolved
      ; Delivery_changed delivery
      ]
  in
  let staged =
    Agent_session.Session_transition.apply ~now:timestamp initial ~delta ~payloads:[]
    |> protocol_ok
  in
  initial, staged.state, resolved, finished, delivery
;;

let notification_entry delivery =
  let id =
    History_entry.Id.create ~namespace:"notification" ~sequence:0 |> Result.ok_or_failwith
  in
  let entry =
    Agent_session.History_codec.user_text ~id "Ochat runtime result data: ready"
  in
  Agent_session.History_codec.to_protocol
    ~provenance:(Runtime_notification delivery.Agent_protocol.Delivery.context.id)
    entry
;;

let%expect_test
    "fast completion stays pending until acknowledgement then commits exactly one \
     history entry"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let _, staged, resolved, _, delivery = extension_fixture workspace_instance in
    let restored =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t staged))
      |> store_ok
    in
    let entry = notification_entry delivery in
    let committed =
      Agent_protocol.Delivery.commit delivery ~history_id:entry.id ~now:timestamp
      |> protocol_ok
    in
    let transition state delta =
      Agent_session.Session_transition.apply ~now:timestamp state ~delta ~payloads:[]
    in
    print_s
      [%sexp
        (Result.is_error (transition restored (Delivery_committed (committed, entry)))
         : bool)];
    print_s [%sexp (List.length restored.conversation.canonical_history : int)];
    let published = Agent_protocol.Invocation.publish resolved |> protocol_ok in
    let ready = transition restored (Invocation_changed published) |> protocol_ok in
    let delivered =
      transition ready.state (Delivery_committed (committed, entry)) |> protocol_ok
    in
    let repeated =
      transition delivered.state (Delivery_committed (committed, entry)) |> protocol_ok
    in
    let status_updates transition =
      List.filter_map transition.Agent_session.Session_transition.events ~f:(fun event ->
        Agent_protocol.Event.Durable.extension_status event |> protocol_ok)
    in
    assert (
      Poly.equal
        (status_updates ready)
        [ Agent_session.Session_state.extension_status ready.state ]);
    assert (
      Poly.equal
        (status_updates delivered)
        [ Agent_session.Session_state.extension_status delivered.state ]);
    assert (List.is_empty (status_updates repeated));
    print_s [%sexp (List.length repeated.state.conversation.canonical_history : int)];
    let restored =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t repeated.state))
      |> store_ok
    in
    print_s
      [%sexp
        ((List.hd_exn restored.conversation.canonical_history).provenance
         : Agent_protocol.History.provenance)];
    print_s
      [%sexp (Result.is_error (transition restored (Delivery_changed committed)) : bool)]);
  [%expect
    {|
    true
    0
    1
    (Runtime_notification dlv_atomic)
    true |}]
;;

let%expect_test
    "invalid delivery transaction cannot publish its acknowledgement or forge human \
     provenance"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let _, staged, resolved, _, delivery = extension_fixture workspace_instance in
    let published = Agent_protocol.Invocation.publish resolved |> protocol_ok in
    let entry = { (notification_entry delivery) with provenance = Canonical } in
    let committed =
      Agent_protocol.Delivery.commit delivery ~history_id:entry.id ~now:timestamp
      |> protocol_ok
    in
    let delta =
      Agent_session.Session_delta.Batch
        [ Invocation_changed published; Delivery_committed (committed, entry) ]
    in
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_transition.apply
              ~now:timestamp
              staged
              ~delta
              ~payloads:[])
         : bool)];
    print_s
      [%sexp ((List.hd_exn staged.invocations).status : Agent_protocol.Invocation.status)];
    print_s [%sexp (List.length staged.conversation.canonical_history : int)]);
  [%expect
    {|
    true
    (Resolved (Pending (Subscription sub_atomic) (String accepted)))
    0 |}]
;;

let%expect_test "extension references reject missing work and competing delivery owners" =
  with_actor_workspace (fun _env workspace_instance ->
    let _, staged, _, subscription, delivery = extension_fixture workspace_instance in
    let original = List.hd_exn staged.invocations in
    let wrong_ack =
      Agent_protocol.Invocation.create original.context
      |> protocol_ok
      |> Agent_protocol.Invocation.dispatch
      |> protocol_ok
    in
    let wrong_ack =
      Agent_protocol.Invocation.resolve
        wrong_ack
        ~session_id
        ~generation:0
        (Complete `Null)
      |> protocol_ok
    in
    assert (
      Result.is_error
        (Agent_session.Session_state.validate { staged with invocations = [ wrong_ack ] }));
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_state.validate { staged with subscriptions = [] })
         : bool)];
    let duplicate =
      Agent_protocol.Delivery.create
        { delivery.context with
          id = Agent_protocol.Id.Delivery.of_string "dlv_competing" |> protocol_ok
        }
      |> protocol_ok
    in
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_state.validate
              { staged with deliveries = duplicate :: staged.deliveries })
         : bool)];
    let wrong =
      Agent_protocol.Delivery.create
        { delivery.context with completion = Succeeded (`String "forged") }
      |> protocol_ok
    in
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_state.validate { staged with deliveries = [ wrong ] })
         : bool)];
    let foreign =
      Agent_protocol.Subscription.create
        { subscription.context with session_id = second_session_id }
      |> protocol_ok
    in
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_delta.apply staged (Subscription_changed foreign))
         : bool)];
    let stale =
      Agent_protocol.Subscription.create { subscription.context with generation = 1 }
      |> protocol_ok
    in
    print_s
      [%sexp
        (Result.is_error
           (Agent_session.Session_delta.apply staged (Subscription_changed stale))
         : bool)]);
  [%expect
    {|
    true
    true
    true
    true
    true |}]
;;

let%expect_test "schema-3 invocation snapshots migrate without losing pending publication"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let state =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let invocation =
      invocation_fixture () |> Agent_protocol.Invocation.dispatch |> protocol_ok
    in
    let invocation =
      Agent_protocol.Invocation.resolve
        invocation
        ~session_id
        ~generation:0
        (Complete `Null)
      |> protocol_ok
    in
    let legacy = { state with schema_version = 3; invocations = [ invocation ] } in
    let restored =
      Agent_session.Session_persistence.restore_snapshot
        (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t legacy))
      |> store_ok
    in
    print_s
      [%sexp
        { version = (restored.schema_version : int)
        ; invocations = (List.length restored.invocations : int)
        ; subscriptions = (List.length restored.subscriptions : int)
        ; deliveries = (List.length restored.deliveries : int)
        }];
    print_s
      [%sexp
        ((List.hd_exn restored.invocations).status : Agent_protocol.Invocation.status)]);
  [%expect
    {|
    ((version 4) (invocations 1) (subscriptions 0) (deliveries 0))
    (Resolved (Complete Null)) |}]
;;

let%expect_test
    "actor extension commit exposes neither queued work nor notification before \
     persistence succeeds"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let _, staged, resolved, _, delivery = extension_fixture workspace_instance in
      let staged = { staged with lifecycle = { desired = Running; observed = Idle } } in
      let fail_commit = ref true in
      let callbacks = ref 0 in
      let history_events = ref 0 in
      let actor =
        Agent_session.Session_actor.create
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:staged
          ~operation_worker:None
          ~persistence:
            { commit =
                (fun ~command_audit:_ ~previous:_ _ ->
                  if !fail_commit
                  then
                    Error
                      (Agent_protocol.Error.create
                         Persistence_error
                         ~message:"injected disk failure"
                         ~retryable:true
                         ())
                  else Ok ())
            }
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id =
                (fun () ->
                  Agent_protocol.Id.Attachment.of_string "att_extension" |> protocol_ok)
            ; create_reclaim_token = (fun () -> "fixture")
            ; state_committed =
                (fun _ events ->
                  Int.incr callbacks;
                  List.iter events ~f:(fun event ->
                    if Agent_protocol.Event.Durable.equal_kind event.kind History_appended
                    then Int.incr history_events))
            }
      in
      let published = Agent_protocol.Invocation.publish resolved |> protocol_ok in
      let entry = notification_entry delivery in
      let committed =
        Agent_protocol.Delivery.commit delivery ~history_id:entry.id ~now:timestamp
        |> protocol_ok
      in
      let job =
        Agent_protocol.Job.
          { id = Agent_protocol.Id.Job.of_string "job_atomic" |> protocol_ok
          ; session_id
          ; generation = 0
          ; kind = Async_tool
          ; payload = `Null
          ; status = Queued
          ; retry_policy = Never
          ; attempt = 0
          ; created_at = timestamp
          ; started_at = None
          ; next_run_at = None
          ; completed_at = None
          ; result = None
          ; delivery = Not_required
          }
      in
      let changes =
        Agent_session.Session_actor.Extension_change.
          [ Start_job job; Invocation published; Publish (committed, entry) ]
      in
      let commit expected_revision changes =
        Agent_session.Session_actor.commit_extensions
          actor
          ~generation:0
          ~expected_revision
          changes
      in
      print_s [%sexp (Result.is_error (commit staged.counters.revision changes) : bool)];
      let failed = Agent_session.Session_actor.state actor |> protocol_ok in
      print_s
        [%sexp
          { jobs = (List.length failed.jobs : int)
          ; history = (List.length failed.conversation.canonical_history : int)
          ; callbacks = (!callbacks : int)
          }];
      fail_commit := false;
      let after = commit staged.counters.revision changes |> protocol_ok in
      print_s [%sexp (Result.is_error (commit staged.counters.revision changes) : bool)];
      commit
        after.revision
        Agent_session.Session_actor.Extension_change.
          [ Invocation published; Publish (committed, entry) ]
      |> protocol_ok
      |> ignore;
      let final = Agent_session.Session_actor.state actor |> protocol_ok in
      print_s
        [%sexp
          { jobs = (List.length final.jobs : int)
          ; history = (List.length final.conversation.canonical_history : int)
          ; history_events = (!history_events : int)
          }];
      Agent_session.Session_actor.shutdown actor));
  [%expect
    {|
    true
    ((jobs 0) (history 0) (callbacks 0))
    true
    ((jobs 1) (history 1) (history_events 1)) |}]
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

let%expect_test "schedule delivery is generation-checked and actor-committed" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:16
          ~compaction_env:None
          ~initial_state:
            (actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:None
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id =
                (fun () ->
                  Agent_protocol.Id.Attachment.of_string "att_schedule_test"
                  |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; state_committed = (fun _ _ -> ())
            }
      in
      let attachment, _ =
        Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
        |> protocol_ok
      in
      let schedule =
        Agent_protocol.Schedule.
          { id = Agent_protocol.Id.Schedule.of_string "sch_actor_test" |> protocol_ok
          ; session_id
          ; generation = 0
          ; payload = `Object [ "event", `String "wake" ]
          ; created_at = timestamp
          ; next_due_at = timestamp
          ; misfire = Deliver_once_immediately
          ; status = Scheduled
          ; delivery_count = 0
          ; last_delivery_at = None
          }
      in
      Agent_session.Session_actor.change_schedule
        actor
        ~attachment_id:attachment.id
        ~event:`Created
        schedule
      |> protocol_ok
      |> ignore;
      let stale_rejected =
        Agent_session.Session_actor.claim_schedule
          actor
          ~schedule_id:schedule.id
          ~generation:1
        |> Result.is_error
      in
      let claimed =
        Agent_session.Session_actor.claim_schedule
          actor
          ~schedule_id:schedule.id
          ~generation:0
        |> protocol_ok
        |> Option.value_exn
      in
      let retried =
        Agent_session.Session_actor.retry_schedule
          actor
          ~schedule_id:schedule.id
          ~generation:0
        |> protocol_ok
      in
      let reclaimed =
        Agent_session.Session_actor.claim_schedule
          actor
          ~schedule_id:schedule.id
          ~generation:0
        |> protocol_ok
        |> Option.value_exn
      in
      let completed =
        Agent_session.Session_actor.complete_schedule
          actor
          ~schedule_id:schedule.id
          ~generation:0
          ~moderator_snapshot:(Some (`Object [ "queued", `True ]))
        |> protocol_ok
      in
      let state = Agent_session.Session_actor.state actor |> protocol_ok in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { stale_rejected : bool
          ; claimed = (claimed.status : Agent_protocol.Schedule.status)
          ; retried = (retried.status : Agent_protocol.Schedule.status)
          ; reclaimed = (reclaimed.status : Agent_protocol.Schedule.status)
          ; completed = (completed.status : Agent_protocol.Schedule.status)
          ; delivery_count = (completed.delivery_count : int)
          ; moderator_persisted = (Option.is_some state.moderator : bool)
          }]));
  [%expect
    {|
    ((stale_rejected true) (claimed Delivering) (retried Scheduled)
     (reclaimed Delivering) (completed Delivered) (delivery_count 1)
     (moderator_persisted true))
    |}]
;;

let%expect_test "model jobs are claimed, completed, and delivered atomically" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:16
          ~compaction_env:None
          ~initial_state:
            (actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:None
          ~services:
            { now = (fun () -> timestamp)
            ; create_attachment_id =
                (fun () ->
                  Agent_protocol.Id.Attachment.of_string "att_job_test" |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; state_committed = (fun _ _ -> ())
            }
      in
      let job =
        Agent_protocol.Job.
          { id = Agent_protocol.Id.Job.of_string "job_actor_test" |> protocol_ok
          ; session_id
          ; generation = 0
          ; kind = Model_call
          ; payload =
              `Object
                [ "recipe", `String "agent_prompt_v1"
                ; "payload", `Object [ "input", `String "test" ]
                ]
          ; status = Queued
          ; retry_policy = Never
          ; attempt = 0
          ; created_at = timestamp
          ; started_at = None
          ; next_run_at = None
          ; completed_at = None
          ; result = None
          ; delivery = Pending
          }
      in
      Agent_session.Session_actor.add_job actor job |> protocol_ok |> ignore;
      let stale_rejected =
        Agent_session.Session_actor.claim_job actor ~job_id:job.id ~generation:1
        |> Result.is_error
      in
      let claimed =
        Agent_session.Session_actor.claim_job actor ~job_id:job.id ~generation:0
        |> protocol_ok
        |> Option.value_exn
      in
      let completed =
        Agent_session.Session_actor.complete_job
          actor
          ~job_id:job.id
          ~generation:0
          (Agent_session.Runtime_builder.Model_succeeded
             (`Object [ "answer", `String "done" ]))
        |> protocol_ok
      in
      let delivered =
        Agent_session.Session_actor.deliver_job
          actor
          ~job_id:job.id
          ~generation:0
          ~moderator_snapshot:(Some (`Object [ "queued", `True ]))
        |> protocol_ok
      in
      let repeated_cancel =
        Agent_session.Session_actor.cancel_job_internal actor ~job_id:job.id
        |> protocol_ok
      in
      let state = Agent_session.Session_actor.state actor |> protocol_ok in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { stale_rejected : bool
          ; claimed = (claimed.status : Agent_protocol.Job.status)
          ; attempt = (claimed.attempt : int)
          ; completed = (completed.status : Agent_protocol.Job.status)
          ; delivered = (delivered.delivery : Agent_protocol.Job.delivery)
          ; repeated_cancel = (repeated_cancel.status : Agent_protocol.Job.status)
          ; result_persisted = (Option.is_some completed.result : bool)
          ; moderator_persisted = (Option.is_some state.moderator : bool)
          }]));
  [%expect
    {|
    ((stale_rejected true) (claimed Running) (attempt 1) (completed Succeeded)
     (delivered (Delivered 2026-08-15T12:00:00.000000000Z))
     (repeated_cancel Succeeded) (result_persisted true)
     (moderator_persisted true))
    |}]
;;

let%expect_test "durable job retry policy persists backoff before terminal delivery" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let now = ref timestamp in
      let actor =
        Agent_session.Session_actor.create
          ~sw:switch
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:16
          ~compaction_env:None
          ~initial_state:
            (actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false)
          ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
          ~operation_worker:None
          ~services:
            { now = (fun () -> !now)
            ; create_attachment_id =
                (fun () ->
                  Agent_protocol.Id.Attachment.of_string "att_retry_test" |> protocol_ok)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
            ; state_committed = (fun _ _ -> ())
            }
      in
      let job =
        Agent_protocol.Job.
          { id = Agent_protocol.Id.Job.of_string "job_retry_test" |> protocol_ok
          ; session_id
          ; generation = 0
          ; kind = Model_call
          ; payload = `Object []
          ; status = Queued
          ; retry_policy = Safe_retry { max_attempts = 2; backoff_ms = 500 }
          ; attempt = 0
          ; created_at = timestamp
          ; started_at = None
          ; next_run_at = None
          ; completed_at = None
          ; result = None
          ; delivery = Pending
          }
      in
      Agent_session.Session_actor.add_job actor job |> protocol_ok |> ignore;
      Agent_session.Session_actor.claim_job actor ~job_id:job.id ~generation:0
      |> protocol_ok
      |> Option.value_exn
      |> ignore;
      let retry =
        Agent_session.Session_actor.complete_job
          actor
          ~job_id:job.id
          ~generation:0
          (Agent_session.Runtime_builder.Model_failed "temporary")
        |> protocol_ok
      in
      let early_claim =
        Agent_session.Session_actor.claim_job actor ~job_id:job.id ~generation:0
        |> protocol_ok
      in
      now
      := Agent_protocol.Timestamp.to_time_ns timestamp
         |> Fn.flip Time_ns.add (Time_ns.Span.of_sec 1.)
         |> Agent_protocol.Timestamp.of_time_ns;
      let second_claim =
        Agent_session.Session_actor.claim_job actor ~job_id:job.id ~generation:0
        |> protocol_ok
        |> Option.value_exn
      in
      let terminal =
        Agent_session.Session_actor.complete_job
          actor
          ~job_id:job.id
          ~generation:0
          (Agent_session.Runtime_builder.Model_failed "permanent")
        |> protocol_ok
      in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { retry_status = (retry.status : Agent_protocol.Job.status)
          ; retry_attempt = (retry.attempt : int)
          ; retry_due = (retry.next_run_at : Agent_protocol.Timestamp.t option)
          ; retry_result = (retry.result : Jsonaf.t option)
          ; early_claim_blocked = (Option.is_none early_claim : bool)
          ; second_attempt = (second_claim.attempt : int)
          ; terminal_status = (terminal.status : Agent_protocol.Job.status)
          ; terminal_delivery = (terminal.delivery : Agent_protocol.Job.delivery)
          }]));
  [%expect
    {|
    ((retry_status Queued) (retry_attempt 1)
     (retry_due (2026-08-15T12:00:00.500000000Z))
     (retry_result ((Object ((last_error (String temporary))))))
     (early_claim_blocked true) (second_attempt 2)
     (terminal_status
      (Failed
       ((code Internal_error) (message permanent) (retryable false)
        (data (Object ())))))
     (terminal_delivery Pending))
    |}]
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

let worker_output_item =
  Openai.Responses.Item.Output_message
    { role = Assistant
    ; id = "worker-output"
    ; content = [ { annotations = []; text = "done"; _type = "output_text" } ]
    ; status = "completed"
    ; phase = None
    ; _type = "message"
    }
;;

let completed_worker_result
      (input : Agent_session.Operation_worker.Input.t)
      (capabilities : Agent_session.Operation_worker.Capabilities.t)
  =
  let open Result.Let_syntax in
  let%bind id =
    History_entry.Id_source.allocate capabilities.id_source
    |> Result.map_error ~f:(fun message ->
      Agent_protocol.Error.create Internal_error ~message ~retryable:false ())
  in
  let entry = History_entry.create_with_id ~id worker_output_item in
  let%map () = capabilities.commit_entry entry in
  Agent_session.Operation_worker.Summary.
    { final_history = input.history @ [ entry ]
    ; runtime_requests = []
    ; moderator_snapshot = None
    }
;;

let completed_worker ~started ~release =
  Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input capabilities ->
    Eio.Promise.resolve started ();
    Eio.Promise.await release;
    match completed_worker_result input capabilities with
    | Ok result -> Agent_session.Operation_worker.Completed result
    | Error failure -> Agent_session.Operation_worker.Failed failure)
;;

let rec await_idle actor =
  let state = Agent_session.Session_actor.state actor |> protocol_ok in
  match state.active_operation, state.lifecycle.observed with
  | None, Idle -> state
  | _ ->
    Eio.Fiber.yield ();
    await_idle actor
;;

let handoff_error message =
  Agent_protocol.Error.create Internal_error ~message ~retryable:false ()
;;

let handoff_snapshot count =
  Session.Moderator_state.Identity_snapshot.
    { script_id = "handoff"
    ; script_source_hash = "fixture"
    ; current_state = Session.Snapshot.Int count
    ; queued_internal_events = []
    ; halted = false
    ; revision = 0
    ; next_change_id = 0
    ; prepended_items = []
    ; appended_items = []
    ; replacements = []
    ; tombstones = []
    ; halted_reason = None
    }
;;

let with_handoff_actor ?(reject = fun _ -> false) ~make_worker f =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let actor_ready, actor_ready_u = Eio.Promise.create () in
      let initial =
        actor_state ~workspace_instance ~liveness:Process_bound ~start_immediately:false
      in
      let backend =
        Agent_session.Memory_backend.create ~event_capacity:128 ~initial_state:initial
      in
      let persistence = Agent_session.Memory_backend.persistence backend in
      let worker = make_worker env actor_ready in
      let actor =
        Agent_session.Session_actor.create
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~mailbox_capacity:32
          ~compaction_env:None
          ~initial_state:initial
          ~operation_worker:(Some worker)
          ~persistence:
            { commit =
                (fun ~command_audit ~previous next ->
                  if reject next
                  then Error (handoff_error "injected invocation save failure")
                  else persistence.commit ~command_audit ~previous next)
            }
          ~services:
            { now = Agent_protocol.Timestamp.now
            ; create_attachment_id = Agent_protocol.Id.Attachment.create
            ; create_reclaim_token = (fun () -> "handoff-test")
            ; state_committed = (fun _ _ -> ())
            }
      in
      Eio.Promise.resolve actor_ready_u actor;
      Exn.protect
        ~finally:(fun () -> Agent_session.Session_actor.shutdown actor)
        ~f:(fun () ->
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
            let writer, _ =
              Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
              |> protocol_ok
            in
            Agent_session.Session_actor.start actor ~attachment_id:writer.id
            |> protocol_ok
            |> ignore;
            let entry =
              Agent_session.History_codec.user_text ~id:history_id "handoff"
              |> Agent_session.History_codec.to_protocol
            in
            Agent_session.Session_actor.submit_message
              actor
              ~attachment_id:writer.id
              entry
            |> protocol_ok
            |> ignore;
            f env actor writer backend))))
;;

let%test_unit
    "invocation recovery preserves results, repairs pairs and never replays work"
  =
  List.iter [ false; true ] ~f:(fun custom ->
    List.iter
      [ `Admitted
      ; `Dispatching
      ; `Resolved
      ; `Cancelled
      ; `Existing
      ; `Published
      ; `Removed
      ; `Old_generation
      ; `Bad_output
      ; `Reused_id
      ; `Collision
      ]
      ~f:(fun mode ->
        with_actor_workspace (fun _env workspace_instance ->
          let module I = Agent_protocol.Invocation in
          let module D = Agent_session.Session_delta in
          let initial =
            actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
          in
          let id n =
            History_entry.Id.create ~namespace:"recover" ~sequence:n
            |> Result.ok_or_failwith
          in
          let call_item =
            if custom
            then
              Openai.Responses.Item.Custom_tool_call
                { name = "read_file"
                ; input = "null"
                ; call_id = "same"
                ; _type = "custom_tool_call"
                ; id = None
                }
            else
              Function_call
                { name = "read_file"
                ; arguments = "null"
                ; call_id = "same"
                ; _type = "function_call"
                ; id = None
                ; status = None
                }
          in
          let call =
            History_entry.create_with_id ~id:(id 0) call_item
            |> Agent_session.History_codec.to_protocol
          in
          let admitted =
            I.create
              { (invocation_fixture ()).context with
                origin = Model
              ; provider_call_id = Some "same"
              ; call_entry_id = Some (id 0)
              }
            |> protocol_ok
          in
          let dispatched = I.dispatch admitted |> protocol_ok in
          let resolved =
            I.resolve dispatched ~session_id ~generation:0 (Complete (`String "saved"))
            |> protocol_ok
          in
          let output =
            Openai.Responses.Tool_output.Output.Text
              (if Poly.equal mode `Bad_output
               then "wrong"
               else Jsonaf.to_string (I.outcome_to_json (Complete (`String "saved"))))
          in
          let output_item =
            if custom
            then
              Openai.Responses.Item.Custom_tool_call_output
                { output; call_id = "same"; _type = "custom_tool_call_output"; id = None }
            else
              Function_call_output
                { output
                ; call_id = "same"
                ; _type = "function_call_output"
                ; id = None
                ; status = None
                }
          in
          let output =
            History_entry.create_with_id ~id:(id 1) output_item
            |> Agent_session.History_codec.to_protocol
          in
          let invocation =
            match mode with
            | `Admitted -> admitted
            | `Dispatching -> dispatched
            | `Cancelled -> I.cancel dispatched ~reason:"already cancelled" |> protocol_ok
            | `Published ->
              I.publish_with_history resolved ~output_entry_id:(id 1) |> protocol_ok
            | _ -> resolved
          in
          let history =
            match mode with
            | `Removed -> []
            | `Existing | `Bad_output | `Published -> [ call; output ]
            | `Reused_id ->
              [ call
              ; History_entry.create_with_id ~id:(id 1) call_item
                |> Agent_session.History_codec.to_protocol
              ]
            | `Collision ->
              let other =
                match call_item with
                | Openai.Responses.Item.Function_call c ->
                  Openai.Responses.Item.Function_call { c with call_id = "other" }
                | Custom_tool_call c -> Custom_tool_call { c with call_id = "other" }
                | _ -> assert false
              in
              [ call
              ; History_entry.create_with_id ~id:(id 8) other
                |> Agent_session.History_codec.to_protocol
              ]
            | _ -> [ call ]
          in
          let state =
            { initial with
              invocations = [ invocation ]
            ; identity =
                { initial.identity with
                  generation = (if Poly.equal mode `Old_generation then 1 else 0)
                }
            ; conversation =
                { initial.conversation with
                  canonical_history = history
                ; next_history_sequence = 8L
                ; reserved_history_through = 8L
                }
            }
          in
          Agent_session.Session_state.validate state |> protocol_ok;
          let plan state =
            Agent_session.Invocation_recovery.plan
              ~state
              ~namespace:"recover"
              ~first_sequence:(Int64.to_int_exn state.conversation.next_history_sequence)
              ~reason:"restart"
          in
          if
            Poly.equal mode `Bad_output
            || Poly.equal mode `Reused_id
            || Poly.equal mode `Collision
          then assert (Result.is_error (plan state))
          else (
            List.iter [ false; true ] ~f:(fun keep_history ->
              let candidate =
                Agent_session.Administration.reset
                  state
                  { keep_history
                  ; keep_tasks = false
                  ; keep_grants = false
                  ; keep_labels = true
                  ; workspace_instance = None
                  }
                |> protocol_ok
              in
              let administrative =
                Agent_session.Administration.archive ~previous:state candidate Reset
                |> protocol_ok
              in
              Agent_session.Session_state.validate administrative |> protocol_ok;
              let archive = List.hd_exn administrative.conversation.compaction_archives in
              assert (List.is_empty administrative.invocations);
              assert (
                List.length archive.invocation_dispositions
                = if Poly.equal mode `Published then 0 else 1);
              if not keep_history
              then assert (List.is_empty administrative.conversation.canonical_history);
              List.iter archive.invocation_dispositions ~f:(fun disposition ->
                assert (
                  Agent_protocol.Id.Invocation.compare
                    disposition.invocation_id
                    invocation.context.id
                  = 0);
                if keep_history && not (Poly.equal mode `Removed)
                then assert (Option.is_some disposition.output_entry_id)
                else assert (Option.is_some disposition.publication_discarded));
              let restored_admin =
                Agent_session.Session_persistence.restore_snapshot
                  (Sexp.to_string_mach
                     (Agent_session.Session_state.sexp_of_t administrative))
                |> store_ok
              in
              assert (
                Sexp.equal
                  (Agent_session.Session_state.sexp_of_t administrative)
                  (Agent_session.Session_state.sexp_of_t restored_admin)));
            let result = plan state |> protocol_ok in
            let delta =
              D.Batch
                (History_block_reserved (Int64.of_int result.next_sequence)
                 :: result.deltas)
            in
            let delta = D.t_of_sexp (D.sexp_of_t delta) in
            let restored = D.apply state delta |> protocol_ok in
            Agent_session.Session_state.validate restored |> protocol_ok;
            let restored =
              Agent_session.Session_persistence.restore_snapshot
                (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t restored))
              |> store_ok
            in
            let actual = List.hd_exn restored.invocations in
            (match mode, actual.status with
             | (`Admitted | `Dispatching), Published (Cancelled "restart") -> ()
             | `Cancelled, Published (Cancelled "already cancelled") -> ()
             | `Removed, Resolved (Complete (`String "saved")) ->
               assert (Option.is_some actual.publication_discarded)
             | _, Published (Complete (`String "saved")) -> ()
             | _ -> assert false);
            let reused = Poly.equal mode `Existing || Poly.equal mode `Published in
            assert (
              List.length result.appended
              = if reused || Poly.equal mode `Removed then 0 else 1);
            if reused
            then
              assert (
                Option.equal History_entry.Id.equal actual.output_entry_id (Some (id 1)));
            if (not reused) && not (Poly.equal mode `Removed)
            then
              assert (
                Option.equal History_entry.Id.equal actual.output_entry_id (Some (id 8)));
            let again = plan restored |> protocol_ok in
            assert (List.is_empty again.deltas && List.is_empty again.appended);
            assert (again.next_sequence = result.next_sequence);
            assert (
              Result.is_error
                (Agent_session.Invocation_recovery.plan
                   ~state:restored
                   ~namespace:"recover"
                   ~first_sequence:0
                   ~reason:"restart"));
            if Poly.equal mode `Removed
            then (
              assert (Result.is_error (I.publish actual));
              assert (
                Result.is_error
                  (Agent_session.Session_state.validate
                     { restored with
                       conversation =
                         { restored.conversation with canonical_history = [ call ] }
                     })))))))
;;

let%test_unit
    "recovery does not fabricate provider outputs for scripts or unbound legacy calls"
  =
  with_actor_workspace (fun _env workspace_instance ->
    List.iter [ false; true ] ~f:(fun model ->
      List.iter [ false; true ] ~f:(fun resolved ->
        let module I = Agent_protocol.Invocation in
        let initial =
          actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
        in
        let invocation =
          I.create
            { (invocation_fixture ()).context with
              origin = (if model then Model else Script)
            ; provider_call_id = (if model then Some "legacy" else None)
            }
          |> protocol_ok
          |> I.dispatch
          |> protocol_ok
        in
        let invocation =
          if resolved
          then
            I.resolve invocation ~session_id ~generation:0 (Complete `Null) |> protocol_ok
          else invocation
        in
        let state = { initial with invocations = [ invocation ] } in
        let plan =
          Agent_session.Invocation_recovery.plan
            ~state
            ~namespace:"unbound"
            ~first_sequence:(Int64.to_int_exn state.conversation.next_history_sequence)
            ~reason:"restart"
          |> protocol_ok
        in
        assert (List.is_empty plan.appended);
        let result =
          Agent_session.Session_delta.apply state (Batch plan.deltas) |> protocol_ok
        in
        Agent_session.Session_state.validate result |> protocol_ok;
        let actual = List.hd_exn result.invocations in
        assert (Bool.equal (Option.is_some actual.publication_discarded) model);
        assert (Option.is_none actual.output_entry_id);
        match resolved, actual.status with
        | true, Resolved (Complete `Null) | false, Resolved (Cancelled "restart") -> ()
        | _ -> assert false)))
;;

let%test_unit
    "restart preserves waiting observations and fails interrupted handling without replay"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let module I = Agent_protocol.Invocation in
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let observer : I.observer =
      { script_id = "moderator"; source_sha256 = String.make 64 'a' }
    in
    let admitted =
      I.create
        ~observer
        { (invocation_fixture ()).context with
          origin = Moderator
        ; provider_call_id = None
        ; parent_invocation = Some (Agent_protocol.Id.Invocation.create ())
        }
      |> protocol_ok
    in
    let dispatched = I.dispatch admitted |> protocol_ok in
    let resolved =
      I.resolve dispatched ~session_id ~generation:0 (Complete (`String "saved"))
      |> protocol_ok
    in
    let observing = I.claim_observation resolved |> protocol_ok in
    let observed = I.complete_observation observing |> protocol_ok in
    let pending_actions =
      I.complete_observation
        observing
        ~follow_up:{ request_turn = true; request_compaction = true; end_session = None }
      |> protocol_ok
    in
    let applied_actions = I.apply_observation_follow_up pending_actions |> protocol_ok in
    let failed = I.fail_observation observing ~reason:"handler failed" |> protocol_ok in
    let published = I.publish observing |> protocol_ok in
    List.iter
      [ admitted
      ; dispatched
      ; resolved
      ; observing
      ; observed
      ; failed
      ; published
      ; pending_actions
      ; applied_actions
      ]
      ~f:(fun invocation ->
        let state = { initial with invocations = [ invocation ] } in
        let plan state =
          Agent_session.Invocation_recovery.plan
            ~state
            ~namespace:"observation-restart"
            ~first_sequence:(Int64.to_int_exn state.conversation.next_history_sequence)
            ~reason:"restart"
          |> protocol_ok
        in
        let recovery = plan state in
        assert (List.is_empty recovery.appended);
        let repaired =
          Agent_session.Session_delta.apply state (Batch recovery.deltas) |> protocol_ok
        in
        Agent_session.Session_state.validate repaired |> protocol_ok;
        let actual = List.hd_exn repaired.invocations in
        (match invocation.status, actual.status with
         | (Admitted | Dispatching), Resolved (Cancelled "restart") -> ()
         | old, current -> assert (I.equal_status old current));
        (match invocation.observation, actual.observation with
         | ( Some { status = Observing; observer = old }
           , Some { status = Observation_failed _; observer = current } ) ->
           assert (I.equal_observer old current);
           assert (Result.is_error (I.claim_observation actual))
         | old, current -> assert (Option.equal I.equal_observation old current));
        assert (List.is_empty (plan repaired).deltas);
        (* Recovery may fail interrupted handling, but must never claim successful execution. *)
        assert (
          Result.is_error
            (Agent_session.Session_delta.apply state (Invocation_reconciled observed)))))
;;

let%test_unit "foreground recovery leaves script and background invocations running" =
  with_actor_workspace (fun _env workspace_instance ->
    let module I = Agent_protocol.Invocation in
    let initial =
      actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
    in
    let make origin parent_job =
      I.create
        { (invocation_fixture ()).context with
          id = Agent_protocol.Id.Invocation.create ()
        ; origin
        ; provider_call_id = (if I.equal_origin origin Model then Some "legacy" else None)
        ; parent_job
        }
      |> protocol_ok
      |> I.dispatch
      |> protocol_ok
    in
    let foreground = make Model None in
    let script = make Script None in
    let background = make Model (Some (Agent_protocol.Id.Job.create ())) in
    let state = { initial with invocations = [ script; foreground; background ] } in
    let plan =
      Agent_session.Invocation_recovery.plan_foreground
        ~state
        ~namespace:"foreground"
        ~first_sequence:(Int64.to_int_exn state.conversation.next_history_sequence)
        ~reason:"worker stopped"
      |> protocol_ok
    in
    let repaired =
      Agent_session.Session_delta.apply state (Batch plan.deltas) |> protocol_ok
    in
    assert (List.is_empty plan.appended);
    List.iter [ script; background ] ~f:(fun invocation ->
      assert (List.mem repaired.invocations invocation ~equal:Poly.equal));
    let actual =
      List.find_exn repaired.invocations ~f:(fun invocation ->
        Agent_protocol.Id.Invocation.compare invocation.context.id foreground.context.id
        = 0)
    in
    assert (Poly.equal actual.status (Resolved (Cancelled "worker stopped")));
    assert (Option.is_some actual.publication_discarded);
    let again =
      Agent_session.Invocation_recovery.plan_foreground
        ~state:repaired
        ~namespace:"foreground"
        ~first_sequence:plan.next_sequence
        ~reason:"worker stopped"
      |> protocol_ok
    in
    assert (List.is_empty again.deltas))
;;

let publication_call caps ?(custom = false) () =
  let id =
    History_entry.Id_source.allocate
      caps.Agent_session.Operation_worker.Capabilities.id_source
    |> Result.ok_or_failwith
  in
  let item =
    if custom
    then
      Openai.Responses.Item.Custom_tool_call
        { name = "read_file"
        ; input = "{}"
        ; call_id = "reused"
        ; _type = "custom_tool_call"
        ; id = None
        }
    else
      Openai.Responses.Item.Function_call
        { name = "read_file"
        ; arguments = "{}"
        ; call_id = "reused"
        ; _type = "function_call"
        ; id = None
        ; status = None
        }
  in
  let call = History_entry.create_with_id ~id item in
  let invocation =
    Agent_protocol.Invocation.create
      { (invocation_fixture ()).context with
        id = Agent_protocol.Id.Invocation.create ()
      ; origin = Model
      ; provider_call_id = Some "reused"
      ; call_entry_id = Some id
      }
    |> protocol_ok
  in
  call, invocation
;;

let publication_output
      caps
      ?(custom = false)
      ?(text = "{\"type\":\"complete\",\"value\":\"done\"}")
      ()
  =
  let id =
    History_entry.Id_source.allocate
      caps.Agent_session.Operation_worker.Capabilities.id_source
    |> Result.ok_or_failwith
  in
  let item =
    if custom
    then
      Openai.Responses.Item.Custom_tool_call_output
        { output = Text text
        ; call_id = "reused"
        ; _type = "custom_tool_call_output"
        ; id = None
        }
    else
      Openai.Responses.Item.Function_call_output
        { output = Text text
        ; call_id = "reused"
        ; _type = "function_call_output"
        ; id = None
        ; status = None
        }
  in
  History_entry.create_with_id ~id item
;;

let resolve_publication caps invocation =
  caps.Agent_session.Operation_worker.Capabilities.with_moderator_invocation
    ~invocation
    (fun ~dispatched ~commit ->
       let resolved =
         Agent_protocol.Invocation.resolve
           dispatched
           ~session_id:dispatched.context.session_id
           ~generation:dispatched.context.generation
           (Complete (`String "done"))
         |> protocol_ok
       in
       commit ~resolved ~snapshot:(handoff_snapshot 1))
;;

let%test_unit
    "model call intent and history commit atomically and retries preserve outcomes"
  =
  let reject = ref true in
  with_handoff_actor
    ~reject:(fun next ->
      if
        !reject
        && not (List.is_empty next.Agent_session.Session_transition.state.invocations)
      then (
        reject := false;
        true)
      else false)
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let call, invocation = publication_call caps () in
        let before = Agent_session.Session_actor.state actor |> protocol_ok in
        let save () = caps.commit_invocation_call ~invocation call in
        assert (Result.is_error (save ()));
        let failed = Agent_session.Session_actor.state actor |> protocol_ok in
        assert_same_session_snapshot before failed;
        Eio.Fiber.both
          (fun () -> save () |> protocol_ok)
          (fun () -> save () |> protocol_ok);
        let admitted = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (List.length admitted.conversation.canonical_history = 2);
        assert (List.length admitted.invocations = 1);
        (match (List.hd_exn admitted.invocations).status with
         | Admitted -> ()
         | _ -> assert false);
        assert (
          Int64.equal admitted.counters.revision Int64.(before.counters.revision + 1L));
        let changed =
          Agent_protocol.Invocation.create
            { invocation.context with input = `String "different" }
          |> protocol_ok
        in
        assert (Result.is_error (caps.commit_invocation_call ~invocation:changed call));
        let competing =
          Agent_protocol.Invocation.create
            { invocation.context with id = Agent_protocol.Id.Invocation.create () }
          |> protocol_ok
        in
        assert (Result.is_error (caps.commit_invocation_call ~invocation:competing call));
        resolve_publication caps invocation |> protocol_ok;
        let output = publication_output caps () in
        caps.publish_invocation_output ~invocation_id:invocation.context.id output
        |> protocol_ok;
        let published = Agent_session.Session_actor.state actor |> protocol_ok in
        save () |> protocol_ok;
        let retried = Agent_session.Session_actor.state actor |> protocol_ok in
        assert_same_session_snapshot published retried;
        Completed
          { final_history = input.history @ [ call; output ]
          ; runtime_requests = []
          ; moderator_snapshot = published.moderator
          }))
    (fun _env actor _writer backend ->
       let state = await_idle actor in
       assert (List.length state.conversation.canonical_history = 3);
       assert (List.length state.invocations = 1);
       assert_same_session_snapshot state (Agent_session.Memory_backend.state backend))
;;

let%test_unit
    "invocation publication saves history and receipt atomically and retries only once"
  =
  let done_, done_u = Eio.Promise.create () in
  let reject_publication = ref true in
  let stale_publish = ref None in
  with_handoff_actor
    ~reject:(fun next ->
      let publishing =
        List.exists next.Agent_session.Session_transition.state.invocations ~f:(fun i ->
          Option.is_some i.output_entry_id)
      in
      if publishing && !reject_publication
      then (
        reject_publication := false;
        true)
      else false)
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let call, invocation = publication_call caps () in
        (* Binding a nonexistent call must not invoke the handler. *)
        assert (Result.is_error (resolve_publication caps invocation));
        caps.commit_entry call |> protocol_ok;
        let output = publication_output caps () in
        let publish entry =
          caps.publish_invocation_output ~invocation_id:invocation.context.id entry
        in
        assert (Result.is_error (publish output));
        resolve_publication caps invocation |> protocol_ok;
        let duplicate =
          Agent_protocol.Invocation.create
            { invocation.context with id = Agent_protocol.Id.Invocation.create () }
          |> protocol_ok
        in
        assert (Result.is_error (resolve_publication caps duplicate));
        let wrong = publication_output caps ~text:"wrong" () in
        assert (Result.is_error (publish wrong));
        let wrong_kind = publication_output caps ~custom:true () in
        assert (Result.is_error (publish wrong_kind));
        let before = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (Result.is_error (publish output));
        let failed = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (
          Sexp.equal
            (Agent_session.Session_state.sexp_of_t before)
            (Agent_session.Session_state.sexp_of_t failed));
        Eio.Fiber.both
          (fun () -> publish output |> protocol_ok)
          (fun () -> publish output |> protocol_ok);
        let published = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (
          Int64.equal published.counters.revision Int64.(before.counters.revision + 1L));
        assert (List.length published.conversation.canonical_history = 3);
        assert (
          Option.equal
            History_entry.Id.equal
            (List.hd_exn published.invocations).output_entry_id
            (Some (History_entry.id output)));
        assert (Result.is_error (publish (publication_output caps ())));
        assert (
          Result.is_error
            (publish (History_entry.with_item output (History_entry.item wrong))));
        let restored =
          Agent_session.Session_persistence.restore_snapshot
            (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t published))
          |> store_ok
        in
        assert (Poly.equal restored.invocations published.invocations);
        (* History compaction retains the receipt and permits no new publication identity. *)
        let compacted =
          Agent_session.Session_delta.apply restored (Canonical_history_replaced [])
          |> protocol_ok
        in
        let repeated =
          Agent_session.Session_delta.apply
            compacted
            (Invocation_changed (List.hd_exn published.invocations))
          |> protocol_ok
        in
        assert (List.is_empty repeated.conversation.canonical_history);
        Agent_session.Session_state.validate repeated |> protocol_ok;
        stale_publish := Some (fun () -> publish output);
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = input.history @ [ call; output ]
          ; runtime_requests = []
          ; moderator_snapshot = published.moderator
          }))
    (fun _env actor _writer backend ->
       Eio.Promise.await done_;
       ignore (await_idle actor);
       assert (Result.is_error ((Option.value_exn !stale_publish) ()));
       let saved = Agent_session.Memory_backend.state backend in
       assert (List.length saved.conversation.canonical_history = 3);
       assert (List.length saved.invocations = 1))
;;

let%test_unit
    "publication binds repeated provider IDs to their exact function or custom call"
  =
  let done_, done_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let history = ref input.history in
        List.iter [ false; true; false ] ~f:(fun custom ->
          let call, invocation = publication_call caps ~custom () in
          caps.commit_entry call |> protocol_ok;
          resolve_publication caps invocation |> protocol_ok;
          let output = publication_output caps ~custom () in
          caps.publish_invocation_output ~invocation_id:invocation.context.id output
          |> protocol_ok;
          history := !history @ [ call; output ]);
        let old_call, old_invocation = publication_call caps () in
        caps.commit_entry old_call |> protocol_ok;
        let next_call, _ = publication_call caps () in
        caps.commit_entry next_call |> protocol_ok;
        assert (Result.is_error (resolve_publication caps old_invocation));
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (List.length state.invocations = 3);
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = !history @ [ old_call; next_call ]
          ; runtime_requests = []
          ; moderator_snapshot = state.moderator
          }))
    (fun _env actor _writer _backend ->
       Eio.Promise.await done_;
       ignore (await_idle actor))
;;

let%test_unit
    "cancelled publishers cannot append late results and committed receipts survive \
     cancellation"
  =
  List.iter [ false; true ] ~f:(fun publish_first ->
    let ready, ready_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    with_handoff_actor
      ~make_worker:(fun _env _actor_ready ->
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
          let call, invocation = publication_call caps () in
          caps.commit_entry call |> protocol_ok;
          resolve_publication caps invocation |> protocol_ok;
          let output = publication_output caps () in
          let publish () =
            caps.publish_invocation_output ~invocation_id:invocation.context.id output
          in
          if publish_first then publish () |> protocol_ok;
          Eio.Promise.resolve ready_u publish;
          Eio.Promise.await never))
      (fun _env actor writer backend ->
         let publish = Eio.Promise.await ready in
         Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Cancel
         |> protocol_ok
         |> ignore;
         assert (Result.is_error (publish ()));
         let rec finished () =
           let s = Agent_session.Session_actor.state actor |> protocol_ok in
           if Option.is_none s.active_operation
           then s
           else (
             Eio.Fiber.yield ();
             finished ())
         in
         let state = finished () in
         assert (List.length state.conversation.canonical_history = 3);
         let invocation = List.hd_exn state.invocations in
         assert (Option.is_some invocation.output_entry_id);
         assert (
           match invocation.status with
           | Published (Complete (`String "done")) -> true
           | _ -> false);
         assert (
           Poly.equal
             state.invocations
             (Agent_session.Memory_backend.state backend).invocations)))
;;

let%test_unit "worker cancellation publishes an interrupted handler result exactly once" =
  List.iter [ false; true ] ~f:(fun custom ->
    let ready, ready_u = Eio.Promise.create () in
    let never, _ = Eio.Promise.create () in
    let calls = ref 0 in
    with_handoff_actor
      ~make_worker:(fun _env _actor_ready ->
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
          let call, invocation = publication_call caps ~custom () in
          caps.commit_entry call |> protocol_ok;
          ignore
            (caps.with_moderator_invocation ~invocation (fun ~dispatched:_ ~commit:_ ->
               Int.incr calls;
               Eio.Promise.resolve ready_u ();
               Eio.Promise.await never)
             : (unit, Agent_protocol.Error.t) result);
          assert false))
      (fun _env actor writer backend ->
         Eio.Promise.await ready;
         let running = Agent_session.Session_actor.state actor |> protocol_ok in
         assert (Option.is_some running.active_operation);
         Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Cancel
         |> protocol_ok
         |> ignore;
         let rec finished () =
           let state = Agent_session.Session_actor.state actor |> protocol_ok in
           if Option.is_none state.active_operation
           then state
           else (
             Eio.Fiber.yield ();
             finished ())
         in
         let state = finished () in
         let invocation = List.hd_exn state.invocations in
         assert (!calls = 1);
         assert (
           match invocation.status with
           | Published (Cancelled _) -> true
           | _ -> false);
         assert (List.length state.conversation.canonical_history = 3);
         ignore
           (Agent_session.Invocation_history.recover_output
              ~history:state.conversation.canonical_history
              invocation
            |> protocol_ok);
         let events =
           Agent_session.Memory_backend.events_after backend 0L |> protocol_ok
         in
         assert (
           List.count events ~f:(fun event ->
             Agent_protocol.Event.Durable.equal_kind event.kind Operation_cancelled)
           = 1);
         assert (Poly.equal state (Agent_session.Memory_backend.state backend))))
;;

let%test_unit "worker failure repairs a transient publication failure without replay" =
  let publication_attempts = ref 0 in
  let handler_calls = ref 0 in
  with_handoff_actor
    ~reject:(fun next ->
      if
        List.exists
          next.Agent_session.Session_transition.state.invocations
          ~f:(fun invocation -> Option.is_some invocation.output_entry_id)
      then (
        Int.incr publication_attempts;
        !publication_attempts = 1)
      else false)
    ~make_worker:(fun _env _actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ caps ->
        let call, invocation = publication_call caps () in
        caps.commit_entry call |> protocol_ok;
        Int.incr handler_calls;
        resolve_publication caps invocation |> protocol_ok;
        match
          caps.publish_invocation_output
            ~invocation_id:invocation.context.id
            (publication_output caps ())
        with
        | Ok () -> assert false
        | Error failure -> Failed failure))
    (fun _env actor _writer backend ->
       let state = await_idle actor in
       assert (!handler_calls = 1 && !publication_attempts = 2);
       assert (Option.is_none state.failure);
       assert (List.length state.conversation.canonical_history = 3);
       let invocation = List.hd_exn state.invocations in
       assert (Poly.equal invocation.status (Published (Complete (`String "done"))));
       ignore
         (Agent_session.Invocation_history.recover_output
            ~history:state.conversation.canonical_history
            invocation
          |> protocol_ok);
       let events = Agent_session.Memory_backend.events_after backend 0L |> protocol_ok in
       assert (
         List.count events ~f:(fun event ->
           Agent_protocol.Event.Durable.equal_kind event.kind Operation_failed)
         = 1);
       assert (Poly.equal state (Agent_session.Memory_backend.state backend)))
;;

let native_registry ?(custom = false) ?(on_call = fun () -> ()) calls ~raises =
  let module Definition = struct
    type input = string

    let name = "read_file"
    let description = Some "native invocation fixture"
    let type_ = if custom then "custom" else "function"
    let parameters = `Object [ "type", `String (if custom then "string" else "object") ]
    let input_of_string input = input
  end
  in
  let implementation =
    Ochat_function.create_function
      (module Definition)
      (fun input ->
         Int.incr calls;
         on_call ();
         assert (String.equal input "{}");
         if raises then failwith "private runner diagnostic";
         Openai.Responses.Tool_output.Output.Text "private output")
  in
  Chat_response.Tool_capability.create
    ~owner:"fixture"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "resources")
    [ Chatmd_shell_spec.Source_ref.digest "native v1", implementation ]
  |> Result.map_error ~f:(fun error -> error.Chat_response.Tool_capability.message)
  |> Result.ok_or_failwith
;;

let native_context ?(input = `Object []) registry invocation =
  let module C = Chat_response.Tool_capability in
  let reference = List.hd_exn (C.references registry) in
  let invocation =
    Agent_protocol.Invocation.create
      { invocation.Agent_protocol.Invocation.context with
        input
      ; implementation_revision = reference.implementation_revision
      ; capability_fingerprint = C.fingerprint registry
      }
    |> protocol_ok
  in
  reference, invocation
;;

let%test_unit
    "native invocation policy and disclosure are shared by model and script calls"
  =
  List.iter [ false; true ] ~f:(fun model ->
    List.iter
      [ `Success; `Deny; `Revoke; `Replace; `Input; `Raise; `Disclosure; `Output ]
      ~f:(fun mode ->
        let calls = ref 0
        and authorized = ref 0
        and disclosed = ref 0 in
        let registry = ref (native_registry calls ~raises:(Poly.equal mode `Raise)) in
        let stale = ref None in
        with_handoff_actor
          ~make_worker:(fun _env actor_ready ->
            Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
              let actor = Eio.Promise.await actor_ready in
              let call, invocation =
                if model
                then (
                  let call, invocation = publication_call caps () in
                  caps.commit_entry call |> protocol_ok;
                  Some call, invocation)
                else None, invocation_fixture ()
              in
              let reference, invocation = native_context !registry invocation in
              let invocation =
                if Poly.equal mode `Input
                then
                  Agent_protocol.Invocation.create
                    { invocation.context with input = `Null }
                  |> protocol_ok
                else invocation
              in
              let run () =
                Agent_session.Native_tool_invocation.run
                  ~is_halted:(fun () -> false)
                  ~capabilities:caps
                  ~registry:(fun () -> !registry)
                  ~reference
                  ~invocation
                  ~authorize:(fun dispatched _binding ->
                    assert (Poly.equal dispatched.status Dispatching);
                    Int.incr authorized;
                    Eio.Fiber.yield ();
                    (match mode with
                     | `Revoke ->
                       registry
                       := Chat_response.Tool_capability.select !registry ~names:[]
                          |> Result.map_error ~f:(fun error ->
                            error.Chat_response.Tool_capability.message)
                          |> Result.ok_or_failwith
                     | `Replace -> registry := native_registry calls ~raises:false
                     | _ -> ());
                    if Poly.equal mode `Deny
                    then Error (handoff_error "private denial")
                    else Ok ())
                  ~prepare_output:(fun _ ->
                    Int.incr disclosed;
                    if Poly.equal mode `Disclosure
                    then Error (handoff_error "private disclosure diagnostic")
                    else if Poly.equal mode `Output
                    then Ok (`Object [ "duplicate", `Null; "duplicate", `Null ])
                    else Ok (`String "disclosed"))
              in
              let recorded = run () |> protocol_ok in
              stale := Some run;
              let expected =
                match mode with
                | `Success -> None
                | `Deny -> Some "invocation.permission_denied"
                | `Revoke | `Replace -> Some "invocation.stale_binding"
                | `Input -> Some "invocation.invalid_input"
                | `Raise -> Some "invocation.handler_failed"
                | `Disclosure -> Some "invocation.disclosure_rejected"
                | `Output -> Some "invocation.invalid_output"
              in
              let outcome =
                match recorded.status, expected with
                | Resolved (Complete (`String "disclosed") as outcome), None -> outcome
                | Resolved (Fail error as outcome), Some code ->
                  assert (String.equal error.code code);
                  assert (not (String.is_substring error.message ~substring:"private"));
                  outcome
                | _ -> assert false
              in
              let tail =
                match call with
                | None -> []
                | Some call ->
                  let output =
                    publication_output
                      caps
                      ~text:
                        (Jsonaf.to_string
                           (Agent_protocol.Invocation.outcome_to_json outcome))
                      ()
                  in
                  caps.publish_invocation_output ~invocation_id:recorded.context.id output
                  |> protocol_ok;
                  [ call; output ]
              in
              let state = Agent_session.Session_actor.state actor |> protocol_ok in
              Completed
                { final_history = input.history @ tail
                ; runtime_requests = []
                ; moderator_snapshot = state.moderator
                }))
          (fun _env actor _writer backend ->
             let state = await_idle actor in
             assert (Result.is_error ((Option.value_exn !stale) ()));
             assert (!authorized = if Poly.equal mode `Input then 0 else 1);
             let runs =
               List.mem [ `Success; `Raise; `Disclosure; `Output ] mode ~equal:Poly.equal
             in
             assert (!calls = if runs then 1 else 0);
             assert (!disclosed = if runs && not (Poly.equal mode `Raise) then 1 else 0);
             assert (
               List.length state.conversation.canonical_history = if model then 3 else 1);
             assert (List.length state.invocations = 1);
             assert (Poly.equal state (Agent_session.Memory_backend.state backend)))))
;;

let%test_unit
    "native custom tools receive raw strings without provider history for scripts"
  =
  List.iter [ false; true ] ~f:(fun model ->
    let calls = ref 0 in
    let registry = native_registry ~custom:true calls ~raises:false in
    with_handoff_actor
      ~make_worker:(fun _env actor_ready ->
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          let actor = Eio.Promise.await actor_ready in
          let call, invocation =
            if model
            then (
              let call, invocation = publication_call caps ~custom:true () in
              caps.commit_entry call |> protocol_ok;
              Some call, invocation)
            else None, invocation_fixture ()
          in
          let reference, invocation =
            native_context ~input:(`String "{}") registry invocation
          in
          let result =
            Agent_session.Native_tool_invocation.run
              ~is_halted:(fun () -> false)
              ~capabilities:caps
              ~registry:(fun () -> registry)
              ~reference
              ~invocation
              ~authorize:(fun _ _ -> Ok ())
              ~prepare_output:(fun _ -> Ok `Null)
            |> protocol_ok
          in
          assert (Poly.equal result.status (Resolved (Complete `Null)));
          let tail =
            match call with
            | None -> []
            | Some call ->
              let output =
                publication_output
                  caps
                  ~custom:true
                  ~text:
                    (Jsonaf.to_string
                       (Agent_protocol.Invocation.outcome_to_json (Complete `Null)))
                  ()
              in
              caps.publish_invocation_output ~invocation_id:result.context.id output
              |> protocol_ok;
              [ call; output ]
          in
          let state = Agent_session.Session_actor.state actor |> protocol_ok in
          Completed
            { final_history = input.history @ tail
            ; runtime_requests = []
            ; moderator_snapshot = state.moderator
            }))
      (fun _env actor _writer _backend ->
         let state = await_idle actor in
         assert (!calls = 1);
         assert (List.length state.conversation.canonical_history = if model then 3 else 1)))
;;

let%test_unit "independent ordinary invocations run concurrently outside the actor" =
  let started = ref 0 in
  let ready, ready_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let run () =
          let invocation =
            Agent_protocol.Invocation.create
              { (invocation_fixture ()).context with
                id = Agent_protocol.Id.Invocation.create ()
              }
            |> protocol_ok
          in
          let result =
            caps.with_invocation ~invocation (fun ~dispatched:_ ->
              Int.incr started;
              if !started = 2 then Eio.Promise.resolve ready_u ();
              Eio.Promise.await ready;
              let live = Agent_session.Session_actor.state actor |> protocol_ok in
              assert (List.length live.invocations = 2);
              Ok (Complete `Null))
            |> protocol_ok
          in
          assert (Poly.equal result.status (Resolved (Complete `Null)))
        in
        Eio.Fiber.both run run;
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot = state.moderator
          }))
    (fun _env actor _writer backend ->
       let state = await_idle actor in
       assert (List.length state.invocations = 2);
       assert (List.length state.conversation.canonical_history = 1);
       assert (Poly.equal state (Agent_session.Memory_backend.state backend)))
;;

let%test_unit "native nested invocation persists without reentering a borrowed moderator" =
  List.iter [ false; true ] ~f:(fun parent_fails ->
    let calls = ref 0 in
    let registry = native_registry calls ~raises:false in
    with_handoff_actor
      ~make_worker:(fun _env actor_ready ->
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          let actor = Eio.Promise.await actor_ready in
          let parent = invocation_fixture () in
          let result =
            caps.with_moderator_invocation ~invocation:parent (fun ~dispatched ~commit ->
              let reference, child =
                native_context
                  registry
                  (Agent_protocol.Invocation.create
                     { parent.context with
                       id = Agent_protocol.Id.Invocation.create ()
                     ; origin = Moderator
                     ; parent_invocation = Some dispatched.context.id
                     }
                   |> protocol_ok)
              in
              let child_result =
                Agent_session.Native_tool_invocation.run
                  ~is_halted:(fun () -> false)
                  ~capabilities:caps
                  ~registry:(fun () -> registry)
                  ~reference
                  ~invocation:child
                  ~authorize:(fun _ _ -> Ok ())
                  ~prepare_output:(fun _ -> Ok `Null)
                |> protocol_ok
              in
              assert (Poly.equal child_result.status (Resolved (Complete `Null)));
              let resolved =
                Agent_protocol.Invocation.resolve
                  dispatched
                  ~session_id:input.session_id
                  ~generation:input.session_generation
                  (Complete `Null)
                |> protocol_ok
              in
              if parent_fails
              then Error (handoff_error "parent handler failed after native effect")
              else commit ~resolved ~snapshot:(handoff_snapshot 1))
          in
          assert (Bool.equal (Result.is_error result) parent_fails);
          let stale_child =
            Agent_protocol.Invocation.create
              { parent.context with
                id = Agent_protocol.Id.Invocation.create ()
              ; parent_invocation = Some parent.context.id
              }
            |> protocol_ok
          in
          assert (
            Result.is_error
              (caps.with_invocation ~invocation:stale_child (fun ~dispatched:_ ->
                 assert false)));
          let state = Agent_session.Session_actor.state actor |> protocol_ok in
          Completed
            { final_history = input.history
            ; runtime_requests = []
            ; moderator_snapshot = state.moderator
            }))
      (fun _env actor _writer backend ->
         let state = await_idle actor in
         assert (!calls = 1);
         assert (List.length state.invocations = 2);
         List.iter state.invocations ~f:(fun invocation ->
           if Option.is_some invocation.context.parent_invocation
           then assert (Poly.equal invocation.status (Resolved (Complete `Null)))
           else
             assert (
               match invocation.status with
               | Resolved (Complete `Null) -> not parent_fails
               | Resolved (Fail _) -> parent_fails
               | _ -> false));
         assert (List.length state.conversation.canonical_history = 1);
         assert (Poly.equal state (Agent_session.Memory_backend.state backend))))
;;

let%test_unit
    "ordinary invocation cancellation and persistence failures retain terminal evidence"
  =
  List.iter [ false; true ] ~f:(fun model ->
    List.iter [ `Cancel; `Reject_result; `Error; `Raise; `Malformed ] ~f:(fun mode ->
      let ready, ready_u = Eio.Promise.create () in
      let never, _ = Eio.Promise.create () in
      let calls = ref 0 in
      with_handoff_actor
        ~reject:(fun next ->
          Poly.equal mode `Reject_result
          && List.exists
               next.Agent_session.Session_transition.state.invocations
               ~f:(fun invocation ->
                 match invocation.status with
                 | Resolved (Complete _) -> true
                 | _ -> false))
        ~make_worker:(fun _env actor_ready ->
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let history, invocation =
              if model
              then (
                let call, invocation = publication_call caps () in
                caps.commit_entry call |> protocol_ok;
                input.history @ [ call ], invocation)
              else input.history, invocation_fixture ()
            in
            let result =
              caps.with_invocation ~invocation (fun ~dispatched ->
                Int.incr calls;
                let live = Agent_session.Session_actor.state actor |> protocol_ok in
                assert (List.mem live.invocations dispatched ~equal:Poly.equal);
                (* Neither a concurrent extension transaction nor a duplicate claim
                 may replace the live callback's record. *)
                let replacement =
                  Agent_protocol.Invocation.cancel dispatched ~reason:"forged"
                  |> protocol_ok
                in
                assert (
                  Result.is_error
                    (Agent_session.Session_actor.commit_extensions
                       actor
                       ~generation:input.session_generation
                       ~expected_revision:live.counters.revision
                       [ Invocation replacement ]));
                assert (
                  Result.is_error
                    (caps.with_invocation ~invocation (fun ~dispatched:_ -> assert false)));
                Eio.Promise.resolve ready_u ();
                match mode with
                | `Cancel -> Eio.Promise.await never
                | `Error -> Error (handoff_error "private callback diagnostic")
                | `Raise -> failwith "private callback diagnostic"
                | `Malformed ->
                  Ok (Complete (`Object [ "duplicate", `Null; "duplicate", `Null ]))
                | `Reject_result -> Ok (Complete `Null))
            in
            let stale_child =
              Agent_protocol.Invocation.create
                { invocation.context with
                  id = Agent_protocol.Id.Invocation.create ()
                ; origin = Script
                ; provider_call_id = None
                ; call_entry_id = None
                ; parent_invocation = Some invocation.context.id
                }
              |> protocol_ok
            in
            assert (
              Result.is_error
                (caps.with_invocation ~invocation:stale_child (fun ~dispatched:_ ->
                   assert false)));
            match result with
            | Error failure -> Failed failure
            | Ok _ ->
              let state = Agent_session.Session_actor.state actor |> protocol_ok in
              Completed
                { final_history = history
                ; runtime_requests = []
                ; moderator_snapshot = state.moderator
                }))
        (fun _env actor writer backend ->
           Eio.Promise.await ready;
           if Poly.equal mode `Cancel
           then
             Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Cancel
             |> protocol_ok
             |> ignore;
           let rec finished () =
             let state = Agent_session.Session_actor.state actor |> protocol_ok in
             if Option.is_none state.active_operation
             then state
             else (
               Eio.Fiber.yield ();
               finished ())
           in
           let state = finished () in
           assert (!calls = 1);
           let invocation = List.hd_exn state.invocations in
           let outcome =
             match invocation.status with
             | Published outcome ->
               assert model;
               outcome
             | Resolved outcome ->
               assert (not model);
               outcome
             | _ -> assert false
           in
           (match mode, outcome with
            | (`Cancel | `Reject_result), Cancelled _ -> ()
            | (`Error | `Raise | `Malformed), Fail failure ->
              assert (
                String.equal
                  failure.code
                  (if Poly.equal mode `Malformed
                   then "invocation.invalid_output"
                   else "invocation.handler_failed"));
              assert (not (String.is_substring failure.message ~substring:"private"))
            | _ -> assert false);
           assert (
             List.length state.conversation.canonical_history = if model then 3 else 1);
           assert (Poly.equal state (Agent_session.Memory_backend.state backend)))))
;;

let%test_unit
    "graceful stop allows an admitted invocation to publish its initial response"
  =
  let ready, ready_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let done_, done_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let call, invocation = publication_call caps () in
        caps.commit_entry call |> protocol_ok;
        resolve_publication caps invocation |> protocol_ok;
        let output = publication_output caps () in
        Eio.Promise.resolve ready_u ();
        Eio.Promise.await release;
        caps.publish_invocation_output ~invocation_id:invocation.context.id output
        |> protocol_ok;
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = input.history @ [ call; output ]
          ; runtime_requests = []
          ; moderator_snapshot = state.moderator
          }))
    (fun _env actor writer _backend ->
       Eio.Promise.await ready;
       Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Graceful
       |> protocol_ok
       |> ignore;
       Eio.Promise.resolve release_u ();
       Eio.Promise.await done_;
       let state = Agent_session.Session_actor.state actor |> protocol_ok in
       assert (Option.is_some (List.hd_exn state.invocations).output_entry_id))
;;

let handoff_definition
      ?capability_registry
      ?(declare_tool = true)
      ?(events = "| _ -> Task.pure(state)")
      ?(script_limits = "")
      ?(schema = "true")
      ?(finish = "Task.pure(state)")
      ?(resolve = "Invocation.resolve(p.context.invocation_id, `Complete(`Null))")
      ?(moderator_capabilities = Chat_response.Moderation.Capabilities.default)
      env
  =
  let module EC = Chat_response.Extension_compiler in
  let module M = Chat_response.Moderator_manager in
  let module C = Chat_response.Tool_capability in
  let dir = Eio.Stdenv.cwd env in
  let source =
    {|<script id="handoff" language="chatml" kind="moderator" api="extensibility-v1" |}
    ^ script_limits
    ^ {|>
    let initial_state = [0]
    let on_event = fun ctx state event -> match event with
    | `Tool_invoked(p) ->
      let ignored = state[0] <- state[0] + 1 in
      Task.bind(Runtime.emit(`String("committed")), fun ignored ->
      Task.bind(|}
    ^ resolve
    ^ {|, fun ignored -> |}
    ^ finish
    ^ {|))
    |}
    ^ events
    ^ "\n</script>"
    ^
    match declare_tool with
    | true ->
      {|<tool name="counter" type="moderator" moderator="handoff"
      input_schema="schema.json" output_schema="schema.json"/>|}
    | false -> ""
  in
  let loader =
    Source_loader.captured_filesystem ~root:dir ~sources:[ "schema.json", schema ]
  in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs ~dir ~source_loader:loader source
  in
  let capabilities =
    match capability_registry with
    | Some registry -> registry
    | None ->
      C.create
        ~owner:"handoff"
        ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "fixture")
        []
      |> Result.map_error ~f:(fun e -> e.C.message)
      |> Result.ok_or_failwith
  in
  let definition =
    EC.prepare_definition_in_domain ~env ~capabilities elements
    |> Result.map_error ~f:(fun ds ->
      String.concat ~sep:"; " (List.map ds ~f:Chatmd_shell_spec.Diagnostic.to_string))
    |> Result.ok_or_failwith
  in
  let _, artifact =
    M.Registry.of_definition M.Registry.empty definition |> Result.ok_or_failwith
  in
  let allocator =
    History_entry.Allocator.create ~namespace:"handoff-overlay" ~next_sequence:0
    |> Result.ok_or_failwith
  in
  let manager =
    M.create_entries
      ~artifact:(Option.value_exn artifact)
      ~capabilities:moderator_capabilities
      ~allocator
      ()
    |> Result.ok_or_failwith
  in
  let invocation () =
    let tool = List.hd_exn (EC.prepared_tools definition) in
    Agent_protocol.Invocation.create
      { (invocation_fixture ()).context with
        id = Agent_protocol.Id.Invocation.create ()
      ; tool_name = "counter"
      ; implementation_revision = EC.fingerprint tool
      ; capability_fingerprint = C.fingerprint (EC.capabilities tool)
      }
    |> protocol_ok
  in
  manager, invocation, definition
;;

let handoff_manager env =
  let manager, invocation, _ = handoff_definition env in
  manager, invocation
;;

let%expect_test
    "follow-up scheduling survives save failure and reload without repeating compaction"
  =
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  List.iter
    [ `Continue; `Reload; `Stop; `End; `Obsolete; `Cancel; `Failure; `Interrupted ]
    ~f:(fun mode ->
      with_actor_workspace (fun env workspace_instance ->
        Eio.Switch.run (fun sw ->
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
            let source = String.make 64 'a' in
            let observer : I.observer =
              { script_id = "handoff"; source_sha256 = source }
            in
            let parent = invocation_fixture () |> I.dispatch |> protocol_ok in
            let parent =
              I.resolve parent ~session_id ~generation:0 (Complete `Null) |> protocol_ok
            in
            let child id follow_up =
              I.create
                ~observer
                { parent.context with
                  id = Agent_protocol.Id.Invocation.of_string id |> protocol_ok
                ; origin = Moderator
                ; parent_invocation = Some parent.context.id
                }
              |> protocol_ok
              |> I.dispatch
              |> protocol_ok
              |> fun child ->
              I.resolve child ~session_id ~generation:0 (Complete (`String "native"))
              |> protocol_ok
              |> I.claim_observation
              |> protocol_ok
              |> I.complete_observation ~follow_up
              |> protocol_ok
            in
            let initial =
              actor_state ~workspace_instance ~liveness:Detached ~start_immediately:false
            in
            let snapshot =
              { (handoff_snapshot 1) with
                script_source_hash = source
              ; halted =
                  (match mode with
                   | `End -> true
                   | _ -> false)
              ; halted_reason =
                  (match mode with
                   | `End -> Some "done"
                   | _ -> None)
              }
            in
            let initial =
              { initial with
                lifecycle = { desired = Running; observed = Idle }
              ; moderator =
                  Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)
              ; invocations =
                  ((parent
                    :: [ child
                           "inv_follow_both"
                           { request_turn = true
                           ; request_compaction = true
                           ; end_session = None
                           }
                       ; child
                           "inv_follow_turn"
                           { request_turn = true
                           ; request_compaction = false
                           ; end_session = None
                           }
                       ])
                   @
                   match mode with
                   | `End ->
                     [ child
                         "inv_follow_end"
                         { request_turn = false
                         ; request_compaction = false
                         ; end_session = Some "done"
                         }
                     ]
                   | _ -> [])
              }
            in
            let initial =
              match mode with
              | `Obsolete ->
                { initial with identity = { initial.identity with generation = 1 } }
              | `Cancel | `Failure | `Interrupted ->
                let other =
                  child
                    "inv_follow_other"
                    { request_turn = true; request_compaction = true; end_session = None }
                  |> I.accept_observation_compaction
                       ~operation_id:
                         (Agent_protocol.Id.Operation.of_string "op_previous_compaction"
                          |> protocol_ok)
                  |> protocol_ok
                in
                { initial with invocations = other :: initial.invocations }
              | _ -> initial
            in
            let restore (state : Agent_session.Session_state.t) =
              let state =
                { state with
                  Agent_session.Session_state.invocations =
                    List.map state.invocations ~f:(fun invocation ->
                      I.of_json (I.to_json invocation) |> protocol_ok)
                }
              in
              Agent_session.Session_persistence.restore_snapshot
                (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t state))
              |> store_ok
            in
            let starts = ref []
            and model_runs = ref 0
            and reject = ref true in
            let reject_completion =
              ref
                (match mode with
                 | `Failure -> true
                 | _ -> false)
            in
            let captured_compaction = ref None in
            let cancel_ready, cancel_ready_u = Eio.Promise.create () in
            let create (initial : Agent_session.Session_state.t) =
              let backend =
                Agent_session.Memory_backend.create
                  ~event_capacity:128
                  ~initial_state:initial
              in
              let persistence = Agent_session.Memory_backend.persistence backend in
              let actor =
                A.create
                  ~sw
                  ~clock:(Eio.Stdenv.clock env)
                  ~mailbox_capacity:32
                  ~compaction_env:None
                  ~initial_state:initial
                  ~persistence:
                    { commit =
                        (fun ~command_audit ~previous next ->
                          if !reject
                          then (
                            reject := false;
                            Error (handoff_error "injected follow-up save failure"))
                          else if
                            !reject_completion
                            && Option.is_none
                                 next.Agent_session.Session_transition.state
                                   .active_operation
                            &&
                            match previous.active_operation with
                            | Some { kind = Compaction; _ } -> true
                            | _ -> false
                          then (
                            reject_completion := false;
                            Error (handoff_error "injected compaction checkpoint failure"))
                          else persistence.commit ~command_audit ~previous next)
                    }
                  ~operation_worker:
                    (Some
                       (Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input _ ->
                          Int.incr model_runs;
                          Completed
                            { final_history = input.history
                            ; moderator_snapshot = initial.moderator
                            ; runtime_requests = []
                            })))
                  ~services:
                    { now = Agent_protocol.Timestamp.now
                    ; create_attachment_id = Agent_protocol.Id.Attachment.create
                    ; create_reclaim_token = (fun () -> "follow-up-test")
                    ; state_committed =
                        (fun committed events ->
                          List.iter events ~f:(fun event ->
                            match
                              Agent_protocol.Event.Durable.Payload.of_json
                                ~kind:event.kind
                                event.payload
                              |> protocol_ok
                            with
                            | Operation_started operation ->
                              starts := !starts @ [ operation.kind ];
                              (match operation.kind with
                               | Compaction ->
                                 captured_compaction := Some committed;
                                 let bound =
                                   List.find_exn
                                     committed.invocations
                                     ~f:(fun invocation ->
                                       String.equal
                                         (Agent_protocol.Id.Invocation.to_string
                                            invocation.context.id)
                                         "inv_follow_both")
                                 in
                                 assert (
                                   Option.equal
                                     Agent_protocol.Id.Operation.equal
                                     (Option.value_exn bound.observation)
                                       .compaction_operation_id
                                     (Some operation.id));
                                 (match mode with
                                  | `Cancel ->
                                    Eio.Promise.resolve cancel_ready_u operation.id;
                                    Eio.Fiber.yield ()
                                  | _ -> ())
                               | _ -> ())
                            | _ -> ()))
                    }
              in
              actor, backend
            in
            let actor, backend = create (restore initial) in
            (match mode with
             | `Cancel ->
               reject := false;
               let writer, _ =
                 A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
               in
               reject := true;
               Eio.Fiber.fork ~sw (fun () ->
                 let operation_id = Eio.Promise.await cancel_ready in
                 A.cancel_operation actor ~attachment_id:writer.id ~operation_id
                 |> protocol_ok
                 |> ignore)
             | _ -> ());
            let before = A.state actor |> protocol_ok in
            assert (Result.is_error (A.apply_observation_follow_up actor));
            assert_same_session_snapshot before (A.state actor |> protocol_ok);
            assert (List.is_empty !starts && !model_runs = 0);
            assert (A.apply_observation_follow_up actor |> protocol_ok);
            let actor, backend =
              match mode with
              | `End -> actor, backend
              | _ ->
                let compacted = await_idle actor in
                (match mode with
                 | `Reload ->
                   A.shutdown actor;
                   create (restore compacted)
                 | `Interrupted ->
                   A.shutdown actor;
                   let captured = restore (Option.value_exn !captured_compaction) in
                   let first_sequence =
                     Int64.to_int_exn
                       (Int64.max
                          captured.conversation.next_history_sequence
                          captured.conversation.reserved_history_through)
                   in
                   let recover state =
                     Agent_session.Invocation_recovery.plan
                       ~state
                       ~first_sequence
                       ~namespace:(Agent_protocol.Id.Session.to_string session_id)
                       ~reason:"simulated restart"
                     |> protocol_ok
                   in
                   let recovered =
                     List.fold
                       (recover captured).deltas
                       ~init:captured
                       ~f:(fun state delta ->
                         Agent_session.Session_delta.apply state delta |> protocol_ok)
                   in
                   let recovered =
                     { recovered with
                       active_operation = None
                     ; lifecycle = { desired = Running; observed = Idle }
                     }
                   in
                   assert (List.is_empty (recover recovered).deltas);
                   create (restore recovered)
                 | _ -> actor, backend)
            in
            (match mode with
             | `Stop ->
               let writer, _ =
                 A.attach actor ~mode:Read_write ~subscribe:false |> protocol_ok
               in
               A.stop actor ~attachment_id:writer.id ~mode:Cancel |> protocol_ok |> ignore;
               A.start actor ~attachment_id:writer.id |> protocol_ok |> ignore
             | `Continue | `Reload | `Cancel | `Failure | `Interrupted ->
               (* Compaction acceptance retains and coalesces the two requested turns. *)
               assert (A.apply_observation_follow_up actor |> protocol_ok);
               ignore (await_idle actor : Agent_session.Session_state.t)
             | `End | `Obsolete -> ());
            assert (not (A.apply_observation_follow_up actor |> protocol_ok));
            let state = A.state actor |> protocol_ok in
            [%test_eq: int]
              (match mode with
               | `Continue | `Reload | `Stop -> 1
               | _ -> 0)
              state.conversation.compaction_generation;
            assert_same_session_snapshot
              state
              (Agent_session.Memory_backend.state backend);
            let receipts =
              List.filter_map state.invocations ~f:(fun invocation ->
                Option.bind invocation.observation ~f:(fun observation ->
                  Option.map observation.follow_up ~f:(fun receipt ->
                    assert (
                      I.equal_status
                        invocation.status
                        (Resolved (Complete (`String "native"))));
                    Agent_protocol.Id.Invocation.to_string invocation.context.id, receipt)))
              |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
            in
            print_s
              [%sexp
                { mode =
                    ((match mode with
                      | `Continue -> "continue"
                      | `Reload -> "reload"
                      | `Stop -> "stop then restart"
                      | `End -> "end overrides work"
                      | `Obsolete -> "old generation"
                      | `Cancel -> "compaction cancelled"
                      | `Failure -> "compaction checkpoint failed"
                      | `Interrupted -> "compaction interrupted")
                     : string)
                ; starts = (!starts : Agent_protocol.Operation.kind list)
                ; model_runs = (!model_runs : int)
                ; receipts : (string * I.follow_up_status) list
                }];
            A.shutdown actor))));
  [%expect
    {|
    ((mode continue) (starts (Compaction (Turn Moderator_request)))
     (model_runs 1)
     (receipts
      ((inv_follow_both
        (Applied_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))))
       (inv_follow_turn
        (Applied_follow_up
         ((request_turn true) (request_compaction false) (end_session ())))))))
    ((mode reload) (starts (Compaction (Turn Moderator_request))) (model_runs 1)
     (receipts
      ((inv_follow_both
        (Applied_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))))
       (inv_follow_turn
        (Applied_follow_up
         ((request_turn true) (request_compaction false) (end_session ())))))))
    ((mode "stop then restart") (starts (Compaction)) (model_runs 0)
     (receipts
      ((inv_follow_both
        (Discarded_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))
         "session stopped"))
       (inv_follow_turn
        (Discarded_follow_up
         ((request_turn true) (request_compaction false) (end_session ()))
         "session stopped")))))
    ((mode "end overrides work") (starts ()) (model_runs 0)
     (receipts
      ((inv_follow_both
        (Discarded_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))
         "moderator ended session"))
       (inv_follow_end
        (Applied_follow_up
         ((request_turn false) (request_compaction false) (end_session (done)))))
       (inv_follow_turn
        (Discarded_follow_up
         ((request_turn true) (request_compaction false) (end_session ()))
         "moderator ended session")))))
    ((mode "old generation") (starts ()) (model_runs 0)
     (receipts
      ((inv_follow_both
        (Discarded_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))
         "observation owner is no longer installed"))
       (inv_follow_turn
        (Discarded_follow_up
         ((request_turn true) (request_compaction false) (end_session ()))
         "observation owner is no longer installed")))))
    ((mode "compaction cancelled") (starts (Compaction (Turn Moderator_request)))
     (model_runs 1)
     (receipts
      ((inv_follow_both
        (Discarded_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))
         "compaction cancelled"))
       (inv_follow_other
        (Applied_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))))
       (inv_follow_turn
        (Applied_follow_up
         ((request_turn true) (request_compaction false) (end_session ())))))))
    ((mode "compaction checkpoint failed")
     (starts (Compaction (Turn Moderator_request))) (model_runs 1)
     (receipts
      ((inv_follow_both
        (Discarded_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))
         "compaction failed"))
       (inv_follow_other
        (Applied_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))))
       (inv_follow_turn
        (Applied_follow_up
         ((request_turn true) (request_compaction false) (end_session ())))))))
    ((mode "compaction interrupted")
     (starts (Compaction (Turn Moderator_request))) (model_runs 1)
     (receipts
      ((inv_follow_both
        (Discarded_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))
         "compaction interrupted before durable completion"))
       (inv_follow_other
        (Applied_follow_up
         ((request_turn true) (request_compaction true) (end_session ()))))
       (inv_follow_turn
        (Applied_follow_up
         ((request_turn true) (request_compaction false) (end_session ())))))))
    |}]
;;

let%expect_test
    "bounded observation drains select atomically and leave unrelated intent alone"
  =
  List.iter [ `Budget; `Concurrent; `Failure; `End ] ~f:(fun mode ->
    let module I = Agent_protocol.Invocation in
    let module M = Chat_response.Moderator_manager in
    let calls = ref []
    and native_calls = ref 0 in
    with_handoff_actor
      ~make_worker:(fun env actor_ready ->
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          let actor = Eio.Promise.await actor_ready in
          let finish =
            match mode with
            | `Failure -> "Task.fail(\"observer failed\")"
            | `End ->
              "Task.bind(Runtime.end_session(\"observations done\"), fun ignored -> \
               Task.pure(state))"
            | _ -> "Task.pure(state)"
          in
          let manager, _, definition =
            handoff_definition
              env
              ~events:
                ("| `Tool_observed(p) -> let ignored = state[0] <- state[0] + 1 in \
                  Task.bind(Tool.call(p.invocation_id, p.outcome), fun ignored -> "
                 ^ finish
                 ^ ") | _ -> Task.pure(state)")
          in
          let script =
            Chat_response.Extension_compiler.script
              (List.hd_exn (Chat_response.Extension_compiler.prepared_tools definition))
          in
          let observer : I.observer =
            { script_id = script.id; source_sha256 = script.source_sha256 }
          in
          caps.commit_moderator
            (Some
               (Agent_session.Runtime_builder.encode_moderator_snapshot
                  (M.identity_snapshot manager |> Result.ok_or_failwith)))
          |> protocol_ok;
          let parent = invocation_fixture () in
          caps.with_invocation ~invocation:parent (fun ~dispatched:_ ->
            List.iter [ 2; 0; 1; 3 ] ~f:(fun index ->
              let observer =
                match index with
                | 3 -> { observer with script_id = "unrelated" }
                | _ -> observer
              in
              let child =
                I.create
                  ~observer
                  { parent.context with
                    id =
                      Agent_protocol.Id.Invocation.of_string
                        ("inv_queue_" ^ Int.to_string index)
                      |> protocol_ok
                  ; origin = Moderator
                  ; parent_invocation = Some parent.context.id
                  }
                |> protocol_ok
              in
              caps.with_invocation ~invocation:child (fun ~dispatched:_ ->
                Int.incr native_calls;
                Ok (Complete (`Number (Int.to_string index))))
              |> protocol_ok
              |> ignore);
            assert (
              not
                (caps.with_next_moderator_observation
                   ~observer
                   (fun ~observing:_ ~commit:_ -> assert false)
                 |> protocol_ok));
            Ok (Complete `Null))
          |> protocol_ok
          |> ignore;
          let drain max_observations =
            Agent_session.Moderator_observation.drain
              ~max_observations
              ~capabilities:caps
              ~observer
              ~manager
              ~history:(fun () ->
                (Agent_session.Session_actor.state actor |> protocol_ok).conversation
                  .canonical_history
                |> Agent_session.History_codec.all_of_protocol
                |> protocol_ok)
              ~available_tools:[]
              ~session_meta:`Null
              ~now:Agent_protocol.Timestamp.now
              ~on_tool_call:(fun ~name ~args:_ ->
                calls := !calls @ [ name ];
                Eio.Fiber.yield ();
                Ok (Tool_ok `Null))
              ()
          in
          assert (Result.is_error (drain 0));
          assert (Result.is_error (drain 257));
          let outcomes =
            match mode with
            | `Budget ->
              let first = drain 2 |> protocol_ok in
              assert first.budget_exhausted;
              [%test_eq: int] 2 (List.length first.outcomes);
              let second = drain 2 |> protocol_ok in
              assert (not second.budget_exhausted);
              [%test_eq: int] 1 (List.length second.outcomes);
              let empty = drain 2 |> protocol_ok in
              assert (List.is_empty empty.outcomes && not empty.budget_exhausted);
              first.outcomes @ second.outcomes
            | `Concurrent ->
              let results = ref [] in
              Eio.Fiber.both
                (fun () ->
                   let result = drain 256 |> protocol_ok in
                   results := result :: !results)
                (fun () ->
                   let result = drain 256 |> protocol_ok in
                   results := result :: !results);
              assert (
                List.for_all !results ~f:(fun result ->
                  not result.Agent_session.Moderator_observation.budget_exhausted));
              let outcomes =
                List.concat_map !results ~f:(fun result ->
                  result.Agent_session.Moderator_observation.outcomes)
              in
              [%test_eq: int] 3 (List.length outcomes);
              outcomes
            | `Failure ->
              assert (Result.is_error (drain 32));
              []
            | `End ->
              let result = drain 32 |> protocol_ok in
              assert (not result.budget_exhausted);
              [%test_eq: int] 1 (List.length result.outcomes);
              let halted = drain 32 |> protocol_ok in
              assert (List.is_empty halted.outcomes && not halted.budget_exhausted);
              result.outcomes
          in
          let state = Agent_session.Session_actor.state actor |> protocol_ok in
          let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
          (match snapshot.current_state with
           | Session.Snapshot.Array [ Int count ] ->
             [%test_eq: int]
               (match mode with
                | `Failure -> 0
                | `End -> 1
                | _ -> 3)
               count
           | _ -> assert false);
          assert (
            Option.equal
              Jsonaf.exactly_equal
              state.moderator
              (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)));
          Completed
            { final_history = input.history
            ; moderator_snapshot = state.moderator
            ; runtime_requests =
                List.concat_map outcomes ~f:(fun outcome ->
                  outcome.Chat_response.Moderation.Outcome.runtime_requests)
            }))
      (fun _env actor _writer backend ->
         let rec finished () =
           let state = Agent_session.Session_actor.state actor |> protocol_ok in
           match state.active_operation with
           | None -> state
           | Some _ ->
             Eio.Fiber.yield ();
             finished ()
         in
         let state = finished () in
         let observations =
           List.filter_map state.invocations ~f:(fun invocation ->
             Option.map invocation.observation ~f:(fun observation ->
               ( Agent_protocol.Id.Invocation.to_string invocation.context.id
               , observation.status )))
           |> List.sort ~compare:(fun (a, _) (b, _) -> String.compare a b)
         in
         let mode =
           match mode with
           | `Budget -> "budget"
           | `Concurrent -> "concurrent"
           | `Failure -> "failure"
           | `End -> "end"
         in
         print_s
           [%sexp
             { mode : string
             ; native_calls = (!native_calls : int)
             ; handled = (!calls : string list)
             ; observations : (string * I.observation_status) list
             }];
         [%test_eq: int] 1 (List.length state.conversation.canonical_history);
         assert_same_session_snapshot state (Agent_session.Memory_backend.state backend)));
  [%expect
    {|
    ((mode budget) (native_calls 4)
     (handled (inv_queue_0 inv_queue_1 inv_queue_2))
     (observations
      ((inv_queue_0 Observed) (inv_queue_1 Observed) (inv_queue_2 Observed)
       (inv_queue_3 Awaiting))))
    ((mode concurrent) (native_calls 4)
     (handled (inv_queue_0 inv_queue_1 inv_queue_2))
     (observations
      ((inv_queue_0 Observed) (inv_queue_1 Observed) (inv_queue_2 Observed)
       (inv_queue_3 Awaiting))))
    ((mode failure) (native_calls 4) (handled (inv_queue_0))
     (observations
      ((inv_queue_0
        (Observation_failed "observation handler failed before acknowledgement"))
       (inv_queue_1 Awaiting) (inv_queue_2 Awaiting) (inv_queue_3 Awaiting))))
    ((mode end) (native_calls 4) (handled (inv_queue_0))
     (observations
      ((inv_queue_0 Observed) (inv_queue_1 Awaiting) (inv_queue_2 Awaiting)
       (inv_queue_3 Awaiting))))
    |}]
;;

let%expect_test "runtime owner drains observation batches and applies durable termination"
  =
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  let module M = Chat_response.Moderator_manager in
  let module B = Agent_session.Runtime_builder in
  let prepared = ref None in
  let native_calls = ref 0 in
  with_handoff_actor
    ~make_worker:(fun env _ ->
      let manager, _, _ =
        handoff_definition
          env
          ~events:
            {| | `Tool_observed(p) ->
                   let ignored = state[0] <- state[0] + 1 in
                   Task.bind(Runtime.emit(`String("observed")), fun ignored ->
                   match state[0] with
                   | 35 ->
                     Task.bind(Runtime.end_session("all observed"), fun ignored -> Task.pure(state))
                   | _ -> Task.pure(state))
                 | _ -> Task.pure(state) |}
      in
      let observer = M.invocation_observer manager |> Option.value_exn in
      let snapshot =
        Some
          (B.encode_moderator_snapshot
             (M.identity_snapshot manager |> Result.ok_or_failwith))
      in
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        caps.commit_moderator snapshot |> protocol_ok;
        let parent = invocation_fixture () in
        caps.with_invocation ~invocation:parent (fun ~dispatched:_ ->
          List.iter (List.range 0 36) ~f:(fun n ->
            let observer =
              match n with
              | 35 -> { observer with source_sha256 = String.make 64 'b' }
              | _ -> observer
            in
            let child =
              I.create
                ~observer
                { parent.context with
                  id = Agent_protocol.Id.Invocation.create ()
                ; origin = Moderator
                ; parent_invocation = Some parent.context.id
                }
              |> protocol_ok
            in
            caps.with_invocation ~invocation:child (fun ~dispatched:_ ->
              Int.incr native_calls;
              Ok (Complete (`String "native result")))
            |> protocol_ok
            |> ignore);
          Ok (Complete `Null))
        |> protocol_ok
        |> ignore;
        prepared := Some manager;
        Completed
          { final_history = input.history
          ; moderator_snapshot = snapshot
          ; runtime_requests = []
          }))
    (fun _env actor _writer backend ->
       let initial = await_idle actor in
       let manager = Option.value_exn !prepared in
       let snapshot () =
         Some
           (B.encode_moderator_snapshot
              (M.identity_snapshot manager |> Result.ok_or_failwith))
       in
       let internal_batches = ref 0 in
       let runtime : B.t =
         { worker =
             Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input:_ _ ->
               failwith "unexpected model turn")
         ; parse_user_content = (fun ~id:_ _ -> failwith "unexpected input")
         ; initial_history = []
         ; initial_prompt_entry_count = 0
         ; reserved_history_through = 0
         ; moderator_snapshot = snapshot ()
         ; moderator_manager = Some manager
         ; moderator_tools = []
         ; start_moderator = (fun () -> failwith "unexpected startup")
         ; enqueue_internal_event = (fun _ -> failwith "unexpected external event")
         ; drain_internal_events =
             (fun history ->
               Int.incr internal_batches;
               let outcomes =
                 M.drain_internal_events_entries
                   manager
                   ~session_id:
                     (Agent_protocol.Id.Session.to_string initial.identity.session_id)
                   ~now_ms:0
                   ~history
                   ~available_tools:[]
                   ~session_meta:`Null
                 |> Result.ok_or_failwith
               in
               Ok
                 { moderator_snapshot = snapshot ()
                 ; runtime_requests =
                     List.concat_map outcomes ~f:(fun outcome ->
                       outcome.Chat_response.Moderation.Outcome.runtime_requests)
                 ; notifications = []
                 ; remaining_events =
                     B.moderator_snapshot_has_queued_events (snapshot ()) |> protocol_ok
                 })
         ; execute_model_job =
             (fun ~recipe:_ ~payload:_ -> failwith "unexpected model job")
         ; enqueue_model_job_completion = (fun _ -> failwith "unexpected completion")
         ; close = (fun () -> ())
         }
       in
       let owner =
         Agent_server.Runtime_owner.create
           ~actor
           ~initial:(Some runtime)
           ~build:(fun () -> failwith "unexpected runtime rebuild")
       in
       let poll () =
         Agent_server.Runtime_owner.drain_idle_moderator owner |> protocol_ok
       in
       let summarize more =
         let state = A.state actor |> protocol_ok in
         let count status =
           List.count state.invocations ~f:(fun invocation ->
             Option.exists invocation.observation ~f:(fun observation ->
               I.equal_observation_status observation.status status))
         in
         assert (Option.is_none state.active_operation);
         [%test_eq: int] 1 (List.length state.conversation.canonical_history);
         assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
         print_s
           [%sexp
             { more : bool
             ; observed = (count Observed : int)
             ; awaiting = (count Awaiting : int)
             ; desired = (state.lifecycle.desired : Agent_protocol.Session.desired_state)
             ; native_calls = (!native_calls : int)
             ; internal_batches = (!internal_batches : int)
             }]
       in
       summarize (poll ());
       summarize (poll ());
       summarize (poll ()));
  [%expect
    {|
    ((more true) (observed 32) (awaiting 4) (desired Running) (native_calls 36)
     (internal_batches 1))
    ((more true) (observed 35) (awaiting 1) (desired Stopped) (native_calls 36)
     (internal_batches 1))
    ((more false) (observed 35) (awaiting 1) (desired Stopped) (native_calls 36)
     (internal_batches 1))
    |}]
;;

let%expect_test "idle moderator tools preserve authority, outcomes and scope lifetime" =
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  let module M = Chat_response.Moderator_manager in
  let module C = Chat_response.Tool_capability in
  List.iter
    [ `Success
    ; `Deny
    ; `Revoke
    ; `Reentrant
    ; `Handler_fail
    ; `Save_fail
    ; `Cancel_native
    ; `Forged_parent
    ]
    ~f:(fun mode ->
      let prepared = ref None in
      let native_calls = ref 0
      and authorizations = ref 0
      and rejected = ref false in
      let on_native = ref (fun () -> ()) in
      let registry =
        ref
          (native_registry ~on_call:(fun () -> !on_native ()) native_calls ~raises:false)
      in
      with_handoff_actor
        ~reject:(fun next ->
          match mode, !rejected with
          | `Save_fail, false
            when List.exists
                   next.Agent_session.Session_transition.state.invocations
                   ~f:(fun invocation ->
                     String.equal invocation.context.tool_name "read_file"
                     &&
                     match invocation.status with
                     | Resolved (Complete _) -> true
                     | _ -> false) ->
            rejected := true;
            true
          | _ -> false)
        ~make_worker:(fun env _ ->
          let finish =
            match mode with
            | `Handler_fail -> "Task.fail(\"after native effect\")"
            | _ -> "Task.pure(state)"
          in
          let manager, _, definition =
            handoff_definition
              env
              ~capability_registry:!registry
              ~declare_tool:false
              ~events:
                ("| `Tool_observed(p) -> let ignored = state[0] <- state[0] + 1 in "
                 ^ "Task.bind(Tool.call(\"read_file\", `Object([])), fun ignored -> "
                 ^ finish
                 ^ ") | _ -> Task.pure(state)")
          in
          let observer = M.invocation_observer manager |> Option.value_exn in
          let snapshot =
            Some
              (Agent_session.Runtime_builder.encode_moderator_snapshot
                 (M.identity_snapshot manager |> Result.ok_or_failwith))
          in
          prepared := Some (manager, definition, observer);
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            caps.commit_moderator snapshot |> protocol_ok;
            let parent =
              I.create { (invocation_fixture ()).context with tool_name = "root" }
              |> protocol_ok
            in
            caps.with_invocation ~invocation:parent (fun ~dispatched:_ ->
              let child =
                I.create
                  ~observer
                  { parent.context with
                    id = Agent_protocol.Id.Invocation.create ()
                  ; origin = Moderator
                  ; parent_invocation = Some parent.context.id
                  ; tool_name = "seed"
                  }
                |> protocol_ok
              in
              caps.with_invocation ~invocation:child (fun ~dispatched:_ ->
                Ok (Complete (`String "seed")))
              |> protocol_ok
              |> ignore;
              Ok (Complete `Null))
            |> protocol_ok
            |> ignore;
            Completed
              { final_history = input.history
              ; moderator_snapshot = snapshot
              ; runtime_requests = []
              }))
        (fun _env actor writer backend ->
           let initial = await_idle actor in
           let manager, definition, observer = Option.value_exn !prepared in
           let seed =
             List.find_exn initial.invocations ~f:(fun i ->
               String.equal i.context.tool_name "seed")
           in
           (on_native
            := fun () ->
                 let owned = A.state actor |> protocol_ok in
                 assert (Option.is_none owned.active_operation);
                 match mode with
                 | `Cancel_native ->
                   A.stop actor ~attachment_id:writer.id ~mode:Cancel
                   |> protocol_ok
                   |> ignore;
                   assert (Result.is_error (A.start actor ~attachment_id:writer.id));
                   Eio.Fiber.yield ()
                 | _ -> ());
           let tools =
             Agent_session.Script_tool_calls.create
               ~registry:(fun () -> !registry)
               ~moderator_names:String.Set.empty
               ~now:Agent_protocol.Timestamp.now
               ~is_halted:(fun () ->
                 let state = A.state actor |> protocol_ok in
                 match state.lifecycle.desired with
                 | Running -> state.halted
                 | Stopped -> true)
               ~requires_active_moderator:(fun _ ->
                 match mode with
                 | `Reentrant -> true
                 | _ -> false)
               ~authorize:(fun _ _ ->
                 Int.incr authorizations;
                 Eio.Fiber.yield ();
                 match mode with
                 | `Deny -> Error (handoff_error "denied")
                 | `Revoke ->
                   registry := native_registry native_calls ~raises:false;
                   Ok ()
                 | _ -> Ok ())
               ~prepare_output:(fun _ -> Ok (`String "disclosed"))
               ~defer_observation:(fun _ -> Ok ())
           in
           let escaped = ref None
           and escaped_executor = ref None in
           let forged_rejected = ref false in
           let run () =
             A.with_idle_moderator_observation_tools
               actor
               ~observer
               (fun ~observing ~execute ~commit ->
                  let reference = List.hd_exn (C.references !registry) in
                  let child () =
                    I.create
                      ~observer
                      { observing.context with
                        id = Agent_protocol.Id.Invocation.create ()
                      ; parent_invocation = Some observing.context.id
                      ; tool_name = "read_file"
                      ; input = `Object []
                      ; implementation_revision = reference.implementation_revision
                      ; capability_fingerprint = C.fingerprint !registry
                      }
                    |> protocol_ok
                  in
                  escaped_executor := Some (execute, child);
                  (match mode with
                   | `Forged_parent ->
                     let invocation = child () in
                     let invocation =
                       I.create
                         ~observer
                         { invocation.context with
                           parent_invocation = seed.context.parent_invocation
                         }
                       |> protocol_ok
                     in
                     forged_rejected
                     := Result.is_error
                          (execute ~invocation (fun ~dispatched:_ -> assert false))
                   | _ -> ());
                  Agent_session.Script_tool_calls.with_observation
                    tools
                    ~definition
                    ~execute
                    ~observing
                    (fun call ->
                       escaped := Some call;
                       let handled =
                         M.handle_observation_entries
                           manager
                           ~retain_follow_up:true
                           ~on_tool_call:call
                           ~invocation:observing
                           ~history:
                             (Agent_session.History_codec.all_of_protocol
                                initial.conversation.canonical_history
                              |> protocol_ok)
                           ~available_tools:[]
                           ~session_meta:`Null
                           ~now_ms:0
                           ~prepare_observation:(fun ~observed ~outcome:_ ~snapshot ->
                             commit ~resolved:observed ~snapshot
                             |> Result.map_error ~f:(fun error ->
                               error.Agent_protocol.Error.message)
                             |> Result.map ~f:(fun () -> fun () -> ()))
                       in
                       (match handled with
                        | Ok _ ->
                          assert (
                            match call ~name:"read_file" ~args:(`Object []) with
                            | Ok (Tool_error "invocation.admission_failed") -> true
                            | _ -> false)
                        | Error _ -> ());
                       handled
                       |> Result.map ~f:ignore
                       |> Result.map_error ~f:handoff_error))
           in
           let completed =
             try Result.is_ok (run ()) with
             | Eio.Cancel.Cancelled _ -> false
           in
           let calls_before_escape = !native_calls in
           let call = Option.value_exn !escaped in
           assert (
             match call ~name:"read_file" ~args:(`Object []) with
             | Ok (Tool_error "invocation.inactive_scope") -> true
             | _ -> false);
           let execute, child = Option.value_exn !escaped_executor in
           assert (
             Result.is_error
               (execute ~invocation:(child ()) (fun ~dispatched:_ -> assert false)));
           [%test_eq: int] calls_before_escape !native_calls;
           let state = A.state actor |> protocol_ok in
           let observed =
             List.find_exn state.invocations ~f:(fun i ->
               String.equal i.context.tool_name "seed")
           in
           let native =
             List.filter state.invocations ~f:(fun i ->
               String.equal i.context.tool_name "read_file")
           in
           assert (I.equal_status observed.status seed.status);
           assert (Option.is_none state.active_operation);
           assert (
             List.equal
               Agent_protocol.History.equal_entry
               initial.conversation.canonical_history
               state.conversation.canonical_history);
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           assert (Result.is_ok (A.change_moderator actor state.moderator));
           let count =
             match
               (M.identity_snapshot manager |> Result.ok_or_failwith).current_state
             with
             | Session.Snapshot.Array [ Int n ] -> n
             | _ -> assert false
           in
           print_s
             [%sexp
               { mode : [ `Success
                        | `Deny
                        | `Revoke
                        | `Reentrant
                        | `Handler_fail
                        | `Save_fail
                        | `Cancel_native
                        | `Forged_parent
                        ]
               ; completed : bool
               ; native_calls = (!native_calls : int)
               ; authorizations = (!authorizations : int)
               ; forged_rejected = (!forged_rejected : bool)
               ; observation =
                   ((Option.value_exn observed.observation).status : I.observation_status)
               ; native = (List.map native ~f:(fun i -> i.status) : I.status list)
               ; state_count = (count : int)
               }]));
  [%expect
    {|
    ((mode Success) (completed true) (native_calls 1) (authorizations 1)
     (forged_rejected false) (observation Observed)
     (native ((Resolved (Complete (String disclosed))))) (state_count 1))
    ((mode Deny) (completed true) (native_calls 0) (authorizations 1)
     (forged_rejected false) (observation Observed)
     (native
      ((Resolved
        (Fail
         ((code invocation.permission_denied)
          (message "Tool execution was not authorized.") (retryable false)
          (details Null))))))
     (state_count 1))
    ((mode Revoke) (completed true) (native_calls 0) (authorizations 1)
     (forged_rejected false) (observation Observed)
     (native
      ((Resolved
        (Fail
         ((code invocation.stale_binding)
          (message "The selected tool capability is no longer valid.")
          (retryable false) (details Null))))))
     (state_count 1))
    ((mode Reentrant) (completed true) (native_calls 0) (authorizations 0)
     (forged_rejected false) (observation Observed)
     (native
      ((Resolved
        (Fail
         ((code moderator_reentrancy)
          (message
           "Tool execution requires a decision from the active moderator.")
          (retryable false) (details Null))))))
     (state_count 1))
    ((mode Handler_fail) (completed false) (native_calls 1) (authorizations 1)
     (forged_rejected false)
     (observation
      (Observation_failed "observation handler failed before acknowledgement"))
     (native ((Resolved (Complete (String disclosed))))) (state_count 0))
    ((mode Save_fail) (completed false) (native_calls 1) (authorizations 1)
     (forged_rejected false)
     (observation
      (Observation_failed "observation handler failed before acknowledgement"))
     (native
      ((Resolved
        (Cancelled
         "idle moderator exited before recording the invocation outcome"))))
     (state_count 0))
    ((mode Cancel_native) (completed false) (native_calls 1) (authorizations 1)
     (forged_rejected false)
     (observation
      (Observation_failed "observation handler cancelled before acknowledgement"))
     (native ((Resolved (Cancelled "idle moderator cancelled"))))
     (state_count 0))
    ((mode Forged_parent) (completed true) (native_calls 1) (authorizations 1)
     (forged_rejected true) (observation Observed)
     (native ((Resolved (Complete (String disclosed))))) (state_count 1))
    |}]
;;

let%expect_test "idle observations own state without starting a model operation" =
  let module A = Agent_session.Session_actor in
  let module I = Agent_protocol.Invocation in
  let module M = Chat_response.Moderator_manager in
  List.iter
    [ `Success
    ; `Request
    ; `End
    ; `Concurrent
    ; `Reentrant
    ; `Handler_fail
    ; `Claim_rejected
    ; `Ack_rejected
    ; `Cancelled
    ; `Stopped
    ; `Stop_cancel
    ; `Graceful_then_cancel
    ]
    ~f:(fun mode ->
      let prepared = ref None
      and calls = ref 0
      and rejected = ref false in
      with_handoff_actor
        ~reject:(fun next ->
          let matches =
            List.exists
              next.Agent_session.Session_transition.state.invocations
              ~f:(fun invocation ->
                match mode, invocation.observation with
                | `Claim_rejected, Some { status = Observing; _ }
                | `Ack_rejected, Some { status = Observed; _ } -> true
                | _ -> false)
          in
          match matches && not !rejected with
          | true ->
            rejected := true;
            true
          | false -> false)
        ~make_worker:(fun env _ ->
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let finish =
              match mode with
              | `Request ->
                "Task.bind(Runtime.request_turn(), fun ignored -> Task.pure(state))"
              | `End ->
                "Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
                 Task.pure(state))"
              | `Handler_fail -> "Task.fail(\"observer failed\")"
              | _ -> "Task.pure(state)"
            in
            let manager, _, definition =
              handoff_definition
                env
                ~events:
                  ("| `Tool_observed(p) -> let ignored = state[0] <- state[0] + 1 in "
                   ^ "Task.bind(Tool.call(\"probe\", p.outcome), fun ignored -> "
                   ^ finish
                   ^ ") | _ -> Task.pure(state)")
            in
            let script =
              Chat_response.Extension_compiler.script
                (List.hd_exn (Chat_response.Extension_compiler.prepared_tools definition))
            in
            let observer : I.observer =
              { script_id = script.id; source_sha256 = script.source_sha256 }
            in
            let snapshot =
              Some
                (Agent_session.Runtime_builder.encode_moderator_snapshot
                   (M.identity_snapshot manager |> Result.ok_or_failwith))
            in
            caps.commit_moderator snapshot |> protocol_ok;
            let parent = invocation_fixture () in
            caps.with_invocation ~invocation:parent (fun ~dispatched:_ ->
              let child =
                I.create
                  ~observer
                  { parent.context with
                    id =
                      Agent_protocol.Id.Invocation.of_string "inv_idle_observation"
                      |> protocol_ok
                  ; origin = Moderator
                  ; parent_invocation = Some parent.context.id
                  }
                |> protocol_ok
              in
              caps.with_invocation ~invocation:child (fun ~dispatched:_ ->
                Ok (Complete (`String "native result")))
              |> protocol_ok
              |> ignore;
              Ok (Complete `Null))
            |> protocol_ok
            |> ignore;
            prepared := Some (manager, observer);
            Completed
              { final_history = input.history
              ; moderator_snapshot = snapshot
              ; runtime_requests = []
              }))
        (fun _env actor writer backend ->
           let initial = await_idle actor in
           let manager, observer = Option.value_exn !prepared in
           let cancellation = ref None in
           let run () =
             Agent_session.Moderator_observation.drain_idle
               ~claim:(A.with_idle_moderator_observation actor ~observer)
               ~manager
               ~history:(fun () ->
                 (A.state actor |> protocol_ok).conversation.canonical_history
                 |> Agent_session.History_codec.all_of_protocol
                 |> protocol_ok)
               ~available_tools:[]
               ~session_meta:`Null
               ~now:Agent_protocol.Timestamp.now
               ~on_tool_call:(fun ~name:_ ~args:_ ->
                 Int.incr calls;
                 let owned = A.state actor |> protocol_ok in
                 assert (Option.is_none owned.active_operation);
                 assert (Result.is_error (A.change_moderator actor None));
                 assert (
                   Result.is_error
                     (A.commit_extensions
                        actor
                        ~generation:owned.identity.generation
                        ~expected_revision:owned.counters.revision
                        [ Moderator_state None ]));
                 assert (Result.is_error (A.set_operation_worker actor None));
                 assert (Option.is_none (A.claim_idle_moderator actor |> protocol_ok));
                 assert (
                   Result.is_error (A.fail_idle_moderator actor (handoff_error "foreign")));
                 assert (
                   Result.is_error
                     (A.complete_idle_moderator
                        actor
                        { moderator_snapshot = None
                        ; runtime_requests = []
                        ; notifications = []
                        ; remaining_events = false
                        }));
                 Eio.Fiber.yield ();
                 (match mode with
                  | `Reentrant ->
                    assert (
                      Result.is_error
                        (A.with_idle_moderator_observation
                           actor
                           ~observer
                           (fun ~observing:_ ~commit:_ -> assert false)))
                  | `Cancelled ->
                    Eio.Cancel.cancel (Option.value_exn !cancellation) Exit;
                    Eio.Fiber.yield ();
                    assert false
                  | `Stopped ->
                    A.stop actor ~attachment_id:writer.id ~mode:Graceful
                    |> protocol_ok
                    |> ignore;
                    assert (Result.is_error (A.start actor ~attachment_id:writer.id))
                  | `Stop_cancel | `Graceful_then_cancel ->
                    (match mode with
                     | `Graceful_then_cancel ->
                       A.stop actor ~attachment_id:writer.id ~mode:Graceful
                       |> protocol_ok
                       |> ignore
                     | _ -> ());
                    A.stop actor ~attachment_id:writer.id ~mode:Cancel
                    |> protocol_ok
                    |> ignore;
                    Eio.Fiber.yield ();
                    assert false
                  | _ -> ());
                 Ok (Tool_ok `Null))
               ()
           in
           let safe_run () =
             try
               match mode with
               | `Cancelled ->
                 Eio.Cancel.sub (fun context ->
                   cancellation := Some context;
                   run ())
               | _ -> run ()
             with
             | Eio.Cancel.Cancelled _ -> Error (handoff_error "cancelled")
           in
           let results = ref [] in
           (match mode with
            | `Concurrent ->
              Eio.Fiber.both
                (fun () ->
                   let result = safe_run () in
                   results := result :: !results)
                (fun () ->
                   let result = safe_run () in
                   results := result :: !results)
            | _ -> results := [ safe_run () ]);
           let state = A.state actor |> protocol_ok in
           let child =
             List.find_exn state.invocations ~f:(fun invocation ->
               I.equal_origin invocation.context.origin Moderator)
           in
           let observation = Option.value_exn child.observation in
           let count =
             match
               (M.identity_snapshot manager |> Result.ok_or_failwith).current_state
             with
             | Session.Snapshot.Array [ Int count ] -> count
             | _ -> assert false
           in
           assert (Option.is_none state.active_operation);
           assert (
             I.equal_status child.status (Resolved (Complete (`String "native result"))));
           assert (
             List.equal
               Agent_protocol.History.equal_entry
               initial.conversation.canonical_history
               state.conversation.canonical_history);
           assert (
             Option.equal
               Jsonaf.exactly_equal
               state.moderator
               (Some
                  (Agent_session.Runtime_builder.encode_moderator_snapshot
                     (M.identity_snapshot manager |> Result.ok_or_failwith))));
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend);
           let mode =
             match mode with
             | `Success -> "success"
             | `Request -> "request"
             | `End -> "end"
             | `Concurrent -> "concurrent"
             | `Reentrant -> "reentrant"
             | `Handler_fail -> "handler failure"
             | `Claim_rejected -> "claim rejected"
             | `Ack_rejected -> "ack rejected"
             | `Cancelled -> "cancelled"
             | `Stopped -> "stopped"
             | `Stop_cancel -> "cancel stop"
             | `Graceful_then_cancel -> "graceful then cancel"
           in
           print_s
             [%sexp
               { mode : string
               ; callbacks = (!calls : int)
               ; state = (count : int)
               ; errors = (List.count !results ~f:Result.is_error : int)
               ; observation = (observation.status : I.observation_status)
               ; follow_up = (observation.follow_up : I.follow_up_status option)
               }];
           (* The legacy borrow is available again even after callback failure. *)
           match mode with
           | "stopped" | "cancel stop" | "graceful then cancel" ->
             assert (Option.is_none (A.claim_idle_moderator actor |> protocol_ok))
           | _ ->
             assert (Option.is_some (A.claim_idle_moderator actor |> protocol_ok));
             A.complete_idle_moderator
               actor
               { moderator_snapshot = state.moderator
               ; runtime_requests = []
               ; notifications = []
               ; remaining_events = false
               }
             |> protocol_ok));
  [%expect
    {|
    ((mode success) (callbacks 1) (state 1) (errors 0) (observation Observed)
     (follow_up ()))
    ((mode request) (callbacks 1) (state 1) (errors 0) (observation Observed)
     (follow_up
      ((Pending_follow_up
        ((request_turn true) (request_compaction false) (end_session ()))))))
    ((mode end) (callbacks 1) (state 1) (errors 0) (observation Observed)
     (follow_up
      ((Pending_follow_up
        ((request_turn false) (request_compaction false) (end_session (done)))))))
    ((mode concurrent) (callbacks 1) (state 1) (errors 0) (observation Observed)
     (follow_up ()))
    ((mode reentrant) (callbacks 1) (state 1) (errors 0) (observation Observed)
     (follow_up ()))
    ((mode "handler failure") (callbacks 1) (state 0) (errors 1)
     (observation
      (Observation_failed "observation handler failed before acknowledgement"))
     (follow_up ()))
    ((mode "claim rejected") (callbacks 0) (state 0) (errors 1)
     (observation Awaiting) (follow_up ()))
    ((mode "ack rejected") (callbacks 1) (state 0) (errors 1)
     (observation
      (Observation_failed "observation handler failed before acknowledgement"))
     (follow_up ()))
    ((mode cancelled) (callbacks 1) (state 0) (errors 1)
     (observation
      (Observation_failed "observation handler cancelled before acknowledgement"))
     (follow_up ()))
    ((mode stopped) (callbacks 1) (state 0) (errors 1)
     (observation
      (Observation_failed "observation handler failed before acknowledgement"))
     (follow_up ()))
    ((mode "cancel stop") (callbacks 1) (state 0) (errors 1)
     (observation
      (Observation_failed "observation handler cancelled before acknowledgement"))
     (follow_up ()))
    ((mode "graceful then cancel") (callbacks 1) (state 0) (errors 1)
     (observation
      (Observation_failed "observation handler cancelled before acknowledgement"))
     (follow_up ()))
    |}]
;;

let%test_unit
    "claimed observations commit moderator state atomically and never replay tools"
  =
  List.iter
    [ `Success
    ; `Handler_fail
    ; `Raise
    ; `Cancelled
    ; `Claim_rejected
    ; `Ack_rejected
    ; `Wrong_source
    ; `Wrong_snapshot
    ; `Resolve_again
    ; `Concurrent
    ; `After_commit_error
    ]
    ~f:(fun mode ->
      let module I = Agent_protocol.Invocation in
      let module M = Chat_response.Moderator_manager in
      let native_calls = ref 0
      and observer_calls = ref 0
      and rejected = ref false in
      let succeeded =
        match mode with
        | `Success | `Concurrent | `After_commit_error -> true
        | _ -> false
      in
      with_handoff_actor
        ~reject:(fun next ->
          let matches =
            List.exists
              next.Agent_session.Session_transition.state.invocations
              ~f:(fun invocation ->
                match mode, invocation.observation with
                | `Claim_rejected, Some { status = Observing; _ }
                | `Ack_rejected, Some { status = Observed; _ } -> true
                | _ -> false)
          in
          if matches && not !rejected
          then (
            rejected := true;
            true)
          else false)
        ~make_worker:(fun env actor_ready ->
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let finish =
              match mode with
              | `Handler_fail -> "Task.fail(\"observer failed\")"
              | `Resolve_again ->
                "Task.bind(Invocation.resolve(p.invocation_id, `Complete(`Null)), fun \
                 ignored -> Task.pure(state))"
              | _ -> "Task.pure(state)"
            in
            let manager, _, definition =
              handoff_definition
                env
                ~events:
                  ("| `Tool_observed(p) -> let ignored = state[0] <- state[0] + 1 in "
                   ^ "Task.bind(Tool.call(\"observe\", p.outcome), fun ignored -> "
                   ^ "Task.bind(Runtime.emit(p.outcome), fun ignored -> "
                   ^ finish
                   ^ ")) | _ -> Task.pure(state)")
            in
            let script =
              Chat_response.Extension_compiler.script
                (List.hd_exn (Chat_response.Extension_compiler.prepared_tools definition))
            in
            let initial_snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
            caps.commit_moderator
              (Some
                 (Agent_session.Runtime_builder.encode_moderator_snapshot
                    initial_snapshot))
            |> protocol_ok;
            let parent = invocation_fixture () in
            let child =
              I.create
                ~observer:
                  { script_id = script.id
                  ; source_sha256 =
                      (match mode with
                       | `Wrong_source -> String.make 64 'f'
                       | _ -> script.source_sha256)
                  }
                { parent.context with
                  id = Agent_protocol.Id.Invocation.create ()
                ; origin = Moderator
                ; parent_invocation = Some parent.context.id
                }
              |> protocol_ok
            in
            caps.with_invocation ~invocation:parent (fun ~dispatched:_ ->
              caps.with_invocation ~invocation:child (fun ~dispatched:_ ->
                Int.incr native_calls;
                Ok (Complete (`String "native result")))
              |> protocol_ok
              |> ignore;
              assert (
                Result.is_error
                  (caps.with_moderator_observation
                     ~invocation_id:child.context.id
                     (fun ~observing:_ ~commit:_ -> assert false)));
              Ok (Complete `Null))
            |> protocol_ok
            |> ignore;
            let escaped = ref None in
            let cancellation = ref None in
            let run () =
              caps.with_moderator_observation
                ~invocation_id:child.context.id
                (fun ~observing ~commit ->
                   let state = Agent_session.Session_actor.state actor |> protocol_ok in
                   assert (List.mem state.invocations observing ~equal:I.equal);
                   assert (
                     Result.is_error
                       (caps.with_moderator_observation
                          ~invocation_id:child.context.id
                          (fun ~observing:_ ~commit:_ -> assert false)));
                   let result =
                     M.handle_observation_entries
                       manager
                       ~invocation:observing
                       ~history:input.history
                       ~available_tools:[]
                       ~session_meta:`Null
                       ~now_ms:0
                       ~on_tool_call:(fun ~name ~args ->
                         [%test_eq: string] "observe" name;
                         assert (
                           Jsonaf.exactly_equal
                             args
                             (I.outcome_to_json (Complete (`String "native result"))));
                         Int.incr observer_calls;
                         Eio.Fiber.yield ();
                         match mode with
                         | `Raise -> failwith "observer external helper raised"
                         | `Cancelled ->
                           Eio.Cancel.cancel (Option.value_exn !cancellation) Exit;
                           Eio.Fiber.yield ();
                           assert false
                         | _ -> Ok (Tool_ok `Null))
                       ~prepare_observation:(fun ~observed ~outcome:_ ~snapshot ->
                         let snapshot =
                           match mode with
                           | `Wrong_snapshot -> { snapshot with script_id = "foreign" }
                           | _ -> snapshot
                         in
                         let save () = commit ~resolved:observed ~snapshot in
                         escaped := Some save;
                         save ()
                         |> Result.map_error ~f:(fun e -> e.Agent_protocol.Error.message)
                         |> Result.map ~f:(fun () -> fun () -> ()))
                     |> Result.map_error ~f:handoff_error
                   in
                   match result, mode with
                   | Ok _, `After_commit_error ->
                     Error (handoff_error "after acknowledgement")
                   | Ok _, _ -> Ok ()
                   | Error e, _ -> Error e)
            in
            let safe_run () =
              let execute () =
                match mode with
                | `Cancelled ->
                  Eio.Cancel.sub (fun context ->
                    cancellation := Some context;
                    run ())
                | _ -> run ()
              in
              match execute () with
              | result -> result
              | exception Failure message
                when String.equal message "observer external helper raised" ->
                Error (handoff_error "observer raised")
              | exception Eio.Cancel.Cancelled _ ->
                Error (handoff_error "observer cancelled")
            in
            (match mode with
             | `Concurrent ->
               let a = ref None
               and b = ref None in
               Eio.Fiber.both
                 (fun () -> a := Some (safe_run ()))
                 (fun () -> b := Some (safe_run ()));
               assert (
                 Bool.equal
                   (Result.is_ok (Option.value_exn !a))
                   (Result.is_error (Option.value_exn !b)))
             | _ ->
               let result = safe_run () in
               assert (
                 Bool.equal
                   (Result.is_ok result)
                   (match mode with
                    | `Success -> true
                    | _ -> false)));
            Option.iter !escaped ~f:(fun save -> assert (Result.is_error (save ())));
            (match mode with
             | `Claim_rejected -> ()
             | _ -> assert (Result.is_error (safe_run ())));
            let snapshot = M.identity_snapshot manager |> Result.ok_or_failwith in
            (match snapshot.current_state with
             | Session.Snapshot.Array [ Int value ] ->
               [%test_eq: int] (if succeeded then 1 else 0) value
             | _ -> assert false);
            let state = Agent_session.Session_actor.state actor |> protocol_ok in
            assert (
              Option.equal
                Jsonaf.exactly_equal
                state.moderator
                (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)));
            Completed
              { final_history = input.history
              ; runtime_requests = []
              ; moderator_snapshot = state.moderator
              }))
        (fun _env actor _writer backend ->
           let state = await_idle actor in
           [%test_eq: int] 1 !native_calls;
           [%test_eq: int]
             (match mode with
              | `Claim_rejected | `Wrong_source -> 0
              | _ -> 1)
             !observer_calls;
           let child =
             List.find_exn state.invocations ~f:(fun invocation ->
               I.equal_origin invocation.context.origin Moderator)
           in
           assert (
             I.equal_status child.status (Resolved (Complete (`String "native result"))));
           (match child.observation with
            | Some { status = Observed; _ } when succeeded -> ()
            | Some { status = Awaiting; _ } ->
              assert (
                match mode with
                | `Claim_rejected -> true
                | _ -> false)
            | Some { status = Observation_failed _; _ } when not succeeded -> ()
            | _ -> assert false);
           [%test_eq: int] 1 (List.length state.conversation.canonical_history);
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend)))
;;

let%test_unit "compiled moderator Tool.call uses persisted scoped native routing" =
  let cases =
    [ `Success
    ; `Custom
    ; `Denied
    ; `Revoked
    ; `Replaced
    ; `Requires_moderator
    ; `Self
    ; `Unknown
    ; `Unselected
    ; `Observation_failed
    ; `Observation_raised
    ; `Invalid
    ; `Disclosure
    ; `Output_limit
    ; `Parent_failed
    ]
  in
  List.iter
    (List.cartesian_product [ false; true ] cases)
    ~f:(fun (observe_nested, mode) ->
      let calls = ref 0
      and authorized = ref 0
      and observations = ref []
      and legacy_calls = ref 0 in
      let custom =
        match mode with
        | `Custom -> true
        | _ -> false
      in
      let registry = ref (native_registry ~custom calls ~raises:false) in
      with_handoff_actor
        ~make_worker:(fun env actor_ready ->
          Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let selected =
              match mode with
              | `Unselected ->
                Chat_response.Tool_capability.select !registry ~names:[]
                |> Result.map_error ~f:(fun e -> e.Chat_response.Tool_capability.message)
                |> Result.ok_or_failwith
              | _ -> !registry
            in
            let name =
              match mode with
              | `Self -> "counter"
              | `Unknown -> "missing"
              | _ -> "read_file"
            in
            let argument =
              match mode with
              | `Invalid -> "`Null"
              | _ -> if custom then "`String(\"{}\")" else "`Object([])"
            in
            let resolve =
              "Task.bind(Tool.call(\""
              ^ name
              ^ "\", "
              ^ argument
              ^ "), fun result -> "
              ^ "match result with | `Ok(value) -> \
                 Invocation.resolve(p.context.invocation_id, `Complete(value)) "
              ^ "| `Error(code) -> Invocation.resolve(p.context.invocation_id, "
              ^ "`Fail({code = code; message = \"nested call failed\"; retryable = \
                 false; details = `Null})))"
            in
            let finish =
              match mode with
              | `Parent_failed -> "Task.fail(\"parent failed after child\")"
              | _ -> "Task.pure(state)"
            in
            let manager, _, definition =
              handoff_definition
                ~capability_registry:selected
                ~script_limits:{|max_value="256KiB"|}
                ~resolve
                ~finish
                ~events:
                  "| `Internal_event(x) -> Task.bind(Tool.call(\"legacy\", `Null), fun \
                   ignored -> Task.pure(state)) | _ -> Task.pure(state)"
                ~moderator_capabilities:
                  { Chat_response.Moderation.Capabilities.default with
                    on_tool_call =
                      (fun ~name ~args:_ ->
                        assert (String.equal name "legacy");
                        Int.incr legacy_calls;
                        Ok (Tool_ok `Null))
                  }
                env
            in
            let tools =
              Agent_session.Script_tool_calls.create
                ~registry:(fun () -> !registry)
                ~moderator_names:(String.Set.singleton "counter")
                ~now:Agent_protocol.Timestamp.now
                ~is_halted:(fun () ->
                  let state = Agent_session.Session_actor.state actor |> protocol_ok in
                  state.halted)
                ~requires_active_moderator:(fun _ ->
                  match mode with
                  | `Requires_moderator -> true
                  | _ -> false)
                ~authorize:(fun child _ ->
                  assert (
                    Agent_protocol.Invocation.equal_origin child.context.origin Moderator);
                  assert (Option.is_some child.context.parent_invocation);
                  Int.incr authorized;
                  Eio.Fiber.yield ();
                  match mode with
                  | `Denied -> Error (handoff_error "private denial")
                  | `Revoked ->
                    registry
                    := Chat_response.Tool_capability.select !registry ~names:[]
                       |> Result.map_error ~f:(fun e ->
                         e.Chat_response.Tool_capability.message)
                       |> Result.ok_or_failwith;
                    Ok ()
                  | `Replaced ->
                    registry := native_registry ~custom calls ~raises:false;
                    Ok ()
                  | _ -> Ok ())
                ~prepare_output:(fun _ ->
                  match mode with
                  | `Disclosure -> Error (handoff_error "private disclosure")
                  | `Output_limit -> Ok (`String (String.make (300 * 1024) 'x'))
                  | _ -> Ok (`String "disclosed child"))
                ~defer_observation:(fun child ->
                  let state = Agent_session.Session_actor.state actor |> protocol_ok in
                  assert (
                    List.mem
                      state.invocations
                      child
                      ~equal:Agent_protocol.Invocation.equal);
                  observations := child :: !observations;
                  (match child.observation with
                   | Some { status = Awaiting; observer } ->
                     let prepared =
                       List.hd_exn
                         (Chat_response.Extension_compiler.prepared_tools definition)
                     in
                     let script = Chat_response.Extension_compiler.script prepared in
                     [%test_eq: string] script.id observer.script_id;
                     [%test_eq: string] script.source_sha256 observer.source_sha256
                   | _ -> failwith "saved child lost its deferred observation intent");
                  match mode with
                  | `Observation_failed ->
                    Error (handoff_error "private observer failure")
                  | `Observation_raised -> failwith "private observer exception"
                  | _ -> Ok ())
            in
            let call_id = "nested-parent" in
            let id =
              History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
            in
            let call =
              History_entry.create_with_id
                ~id
                (Chat_response.Tool_call.call_item
                   ~kind:Function
                   ~name:"counter"
                   ~payload:"null"
                   ~call_id
                   ~id:None)
            in
            let dispatch =
              Agent_session.Moderator_tool_dispatch.create
                ~script_tools:tools
                ~observe_nested
                ~definition
                ~manager
                ~input
                ~capabilities:caps
                ~available_tools:[]
                ~session_meta:`Null
                ~now:Agent_protocol.Timestamp.now
                ~validate_work:(fun _ -> Error "pending disabled")
                ~admit:(fun _ -> Ok ())
                ~prepare_outcome:(fun _ -> Ok ())
                ()
            in
            let request =
              Chat_response.In_memory_stream.Tool_dispatch.
                { kind = Function
                ; original_name = "counter"
                ; original_payload = "null"
                ; name = "counter"
                ; payload = "null"
                ; rejection = None
                ; call
                ; history = input.history @ [ call ]
                ; source = None
                ; parent_call_id = None
                }
            in
            let result = dispatch.run request ~authorize:ignore |> Option.value_exn in
            let output_id =
              History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
            in
            let output =
              History_entry.create_with_id
                ~id:output_id
                (Chat_response.Tool_call.output_item
                   ~kind:Function
                   ~call_id
                   ~output:result.output)
            in
            (Option.value_exn result.commit_output) output;
            let snapshot =
              Chat_response.Moderator_manager.identity_snapshot manager
              |> Result.ok_or_failwith
            in
            (match snapshot.current_state with
             | Session.Snapshot.Array [ Int count ] ->
               assert (
                 count
                 =
                 match mode with
                 | `Parent_failed -> 0
                 | _ -> 1)
             | _ -> assert false);
            Chat_response.Moderator_manager.handle_event_entries
              manager
              ~session_id:(Agent_protocol.Id.Session.to_string input.session_id)
              ~now_ms:0
              ~history:(input.history @ [ call; output ])
              ~available_tools:[]
              ~session_meta:`Null
              ~event:
                (Internal_event
                   (Chatml.Chatml_lang.VVariant
                      ( "Internal_event"
                      , [ Chatml.Chatml_value_codec.jsonaf_to_value
                            (`String "check restored callback")
                        ] )))
            |> Result.ok_or_failwith
            |> ignore;
            caps.commit_moderator
              (Some
                 (Agent_session.Runtime_builder.encode_moderator_snapshot
                    (Chat_response.Moderator_manager.identity_snapshot manager
                     |> Result.ok_or_failwith)))
            |> protocol_ok;
            let state = Agent_session.Session_actor.state actor |> protocol_ok in
            Completed
              { final_history = input.history @ [ call; output ]
              ; runtime_requests = []
              ; moderator_snapshot = state.moderator
              }))
        (fun _env actor _writer backend ->
           let state = await_idle actor in
           let parent =
             List.find_exn state.invocations ~f:(fun i ->
               Option.is_none i.context.parent_invocation)
           in
           let children =
             List.filter state.invocations ~f:(fun i ->
               Option.is_some i.context.parent_invocation)
           in
           let expected =
             match mode with
             | `Success | `Custom -> None
             | `Denied -> Some "invocation.permission_denied"
             | `Revoked | `Replaced -> Some "invocation.stale_binding"
             | `Requires_moderator | `Self -> Some "moderator_reentrancy"
             | `Unknown | `Unselected -> Some "invocation.unselected_tool"
             | `Observation_failed | `Observation_raised ->
               Some "invocation.observation_failed"
             | `Invalid -> Some "invocation.invalid_input"
             | `Disclosure | `Output_limit -> Some "invocation.disclosure_rejected"
             | `Parent_failed -> Some "invocation.handler_failed"
           in
           (match parent.status, expected with
            | Published (Complete (`String "disclosed child")), None -> ()
            | Published (Fail error), Some code -> [%test_eq: string] code error.code
            | _ ->
              raise_s
                [%sexp
                  "unexpected parent outcome", (parent : Agent_protocol.Invocation.t)]);
           let has_child =
             match mode with
             | `Self | `Unknown | `Unselected -> false
             | _ -> true
           in
           assert (List.length children = if has_child then 1 else 0);
           assert (List.length !observations = List.length children);
           List.iter children ~f:(fun child ->
             (match observe_nested, child.observation with
              | false, Some { status = Awaiting; _ } | true, Some { status = Observed; _ }
                -> ()
              | _ -> failwith "saved child lost its deferred observation intent");
             assert (Option.is_none child.context.provider_call_id);
             assert (Option.is_none child.context.call_entry_id);
             assert (Option.is_none child.output_entry_id);
             assert (
               Option.equal
                 Agent_protocol.Id.Invocation.equal
                 child.context.parent_invocation
                 (Some parent.context.id));
             match mode, child.status with
             | ( ( `Denied
                 | `Revoked
                 | `Replaced
                 | `Requires_moderator
                 | `Invalid
                 | `Disclosure
                 | `Output_limit )
               , Resolved (Fail error) ) ->
               assert (Option.equal String.equal (Some error.code) expected)
             | ( ( `Success
                 | `Custom
                 | `Observation_failed
                 | `Observation_raised
                 | `Parent_failed )
               , Resolved (Complete (`String "disclosed child")) ) -> ()
             | _ -> assert false);
           let executed =
             match mode with
             | `Success
             | `Custom
             | `Observation_failed
             | `Observation_raised
             | `Disclosure
             | `Output_limit
             | `Parent_failed -> true
             | _ -> false
           in
           assert (!calls = if executed then 1 else 0);
           assert (
             !authorized
             =
             match mode with
             | `Self | `Unknown | `Unselected | `Requires_moderator | `Invalid -> 0
             | _ -> 1);
           assert (List.length state.conversation.canonical_history = 3);
           assert (!legacy_calls = 1);
           assert_same_session_snapshot state (Agent_session.Memory_backend.state backend)))
;;

let%test_unit "script call scopes bound attempts and reject escaped or closed parents" =
  let calls = ref 0
  and observations = ref 0 in
  let registry = native_registry calls ~raises:false in
  let expect_error code = function
    | Ok (Chat_response.Moderation.Capabilities.Tool_error actual) ->
      assert (String.equal code actual)
    | _ -> assert false
  in
  with_handoff_actor
    ~make_worker:(fun env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let _, make_parent, definition =
          handoff_definition
            ~capability_registry:registry
            ~script_limits:{|max_value="256KiB"|}
            env
        in
        let prepared =
          List.hd_exn (Chat_response.Extension_compiler.prepared_tools definition)
        in
        let parent = make_parent () in
        let tools =
          Agent_session.Script_tool_calls.create
            ~registry:(fun () -> registry)
            ~moderator_names:(String.Set.singleton "counter")
            ~now:Agent_protocol.Timestamp.now
            ~is_halted:(fun () -> false)
            ~requires_active_moderator:(fun _ -> false)
            ~authorize:(fun _ _ -> Ok ())
            ~prepare_output:(fun _ -> Ok (`String "done"))
            ~defer_observation:(fun _ ->
              Int.incr observations;
              Ok ())
        in
        caps.with_moderator_invocation ~invocation:parent (fun ~dispatched ~commit ->
          let scope f =
            Agent_session.Script_tool_calls.with_invocation
              tools
              ~prepared
              ~capabilities:caps
              ~parent:dispatched
              f
          in
          let escaped =
            scope (fun call ->
              for _ = 1 to 100 do
                call ~name:"missing" ~args:`Null
                |> expect_error "invocation.unselected_tool"
              done;
              call ~name:"read_file" ~args:(`Object [])
              |> expect_error "invocation.nested_call_limit";
              call)
          in
          escaped ~name:"read_file" ~args:(`Object [])
          |> expect_error "invocation.inactive_scope";
          assert (!calls = 0);
          scope (fun call ->
            call ~name:"read_file" ~args:(`String (String.make (300 * 1024) 'x'))
            |> expect_error "invocation.invalid_input";
            match call ~name:"read_file" ~args:(`Object []) with
            | Ok (Tool_ok (`String "done")) -> ()
            | _ -> assert false);
          let resolved =
            Agent_protocol.Invocation.resolve
              dispatched
              ~session_id:input.session_id
              ~generation:input.session_generation
              (Complete `Null)
            |> protocol_ok
          in
          commit ~resolved ~snapshot:(handoff_snapshot 1) |> protocol_ok;
          scope (fun call ->
            call ~name:"read_file" ~args:(`Object [])
            |> expect_error "invocation.admission_failed");
          Ok ())
        |> protocol_ok;
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot = state.moderator
          }))
    (fun _env actor _writer backend ->
       let state = await_idle actor in
       assert (!calls = 1 && !observations = 1);
       assert (List.length state.invocations = 2);
       assert (List.length state.conversation.canonical_history = 1);
       assert_same_session_snapshot state (Agent_session.Memory_backend.state backend))
;;

let%test_unit "streamed native and moderator services share pre and post routing" =
  List.iter
    [ `Success
    ; `Custom
    ; `Invalid
    ; `Rewrite_invalid
    ; `Redirect
    ; `Deny
    ; `Pre_reject
    ; `Pre_fail
    ; `Post_fail
    ; `Revoked
    ; `Halt_wait
    ; `Disclosure
    ; `Mixed
    ; `Pre_end
    ; `Pre_reject_end
    ; `Revoked_before
    ; `Kind_mismatch
    ; `Invalid_json
    ; `Redacted
    ; `Publish_rejected
    ; `Call_save_rejected
    ; `Dispatch_rejected
    ; `Observer_failed
    ; `Moderator_call_save_rejected
    ; `Moderator_dispatch_rejected
    ; `Moderator_observer_failed
    ; `Custom_call_save_rejected
    ; `Custom_dispatch_rejected
    ; `Custom_observer_failed
    ]
    ~f:(fun mode ->
      let calls = ref 0
      and admitted = ref 0
      and post_calls = ref 0
      and requests = ref 0 in
      let halted = ref false in
      let custom =
        match mode with
        | `Custom
        | `Custom_call_save_rejected
        | `Custom_dispatch_rejected
        | `Custom_observer_failed -> true
        | _ -> false
      in
      let mixed =
        match mode with
        | `Mixed -> true
        | _ -> false
      in
      let publication_rejected =
        match mode with
        | `Publish_rejected -> true
        | _ -> false
      in
      let redacted =
        match mode with
        | `Redacted -> true
        | _ -> false
      in
      let post_failed =
        match mode with
        | `Post_fail -> true
        | _ -> false
      in
      let call_save_rejected =
        match mode with
        | `Call_save_rejected | `Moderator_call_save_rejected | `Custom_call_save_rejected
          -> true
        | _ -> false
      in
      let dispatch_rejected =
        match mode with
        | `Dispatch_rejected | `Moderator_dispatch_rejected | `Custom_dispatch_rejected ->
          true
        | _ -> false
      in
      let observer_failed =
        match mode with
        | `Observer_failed | `Moderator_observer_failed | `Custom_observer_failed -> true
        | _ -> false
      in
      let moderator_target =
        match mode with
        | `Moderator_call_save_rejected
        | `Moderator_dispatch_rejected
        | `Moderator_observer_failed -> true
        | _ -> false
      in
      let original_name =
        match mode with
        | `Redirect -> "counter"
        | _ -> if moderator_target then "counter" else "read_file"
      in
      let before_execution_failure =
        call_save_rejected || dispatch_rejected || observer_failed
      in
      let registry = ref (native_registry ~custom calls ~raises:false) in
      with_handoff_actor
        ~reject:(fun next ->
          List.exists
            next.Agent_session.Session_transition.state.invocations
            ~f:(fun invocation ->
              (publication_rejected && Option.is_some invocation.output_entry_id)
              ||
              match invocation.status with
              | Admitted -> call_save_rejected
              | Dispatching -> dispatch_rejected
              | Resolved _ | Published _ -> false))
        ~make_worker:(fun env actor_ready ->
          let pre =
            match mode with
            | `Invalid | `Invalid_json | `Kind_mismatch ->
              "Task.fail(\"invalid input reached pre hook\")"
            | `Revoked_before ->
              "Task.bind(Tool.call(\"revoke\", `Null), fun ignored -> Task.pure(state))"
            | `Rewrite_invalid ->
              "Task.bind(Tool.rewrite_args(`Null), fun ignored -> Task.pure(state))"
            | `Redirect ->
              "Task.bind(Tool.redirect(\"read_file\", `Object([])), fun ignored -> \
               Task.pure(state))"
            | `Pre_reject ->
              "Task.bind(Tool.reject(\"private rejection\"), fun ignored -> \
               Task.pure(state))"
            | `Pre_reject_end ->
              "Task.bind(Tool.reject(\"private rejection\"), fun ignored -> \
               Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
               Task.pure(state)))"
            | `Pre_fail -> "Task.fail(\"private pre failure\")"
            | `Pre_end ->
              "Task.bind(Runtime.end_session(\"done\"), fun ignored -> Task.pure(state))"
            | _ -> "Task.pure(state)"
          in
          let events =
            "| `Pre_tool_call(c) -> "
            ^ pre
            ^ " | `Post_tool_response(r) -> Task.bind(Tool.call(\"observe\", `Null), fun \
               ignored -> "
            ^ (if post_failed
               then "Task.fail(\"private post failure\")"
               else "Task.pure(state)")
            ^ ")"
            ^ (if observer_failed
               then
                 " | `Item_appended(item) -> (match Json.get_field(item.value, \"type\") \
                  with | `Some(`String(\"function_call\")) -> Task.fail(\"private \
                  observer failure\") | `Some(`String(\"custom_tool_call\")) -> \
                  Task.fail(\"private observer failure\") | _ -> Task.pure(state))"
               else "")
            ^ " | _ -> Task.pure(state)"
          in
          let moderator_capabilities =
            { Chat_response.Moderation.Capabilities.default with
              on_tool_call =
                (fun ~name ~args:_ ->
                  if String.equal name "observe"
                  then Int.incr post_calls
                  else (
                    assert (String.equal name "revoke");
                    registry
                    := Chat_response.Tool_capability.select !registry ~names:[]
                       |> Result.map_error ~f:(fun error ->
                         error.Chat_response.Tool_capability.message)
                       |> Result.ok_or_failwith);
                  Ok (Tool_ok `Null))
            }
          in
          let manager, _, definition =
            handoff_definition ~events ~moderator_capabilities env
          in
          Agent_session.Operation_worker.create ~run:(fun ~sw ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let state = Agent_session.Session_actor.state actor |> protocol_ok in
            let response_dir =
              Eio.Path.(
                Eio.Stdenv.fs env
                / state.spec.workspace_instance.canonical_root.native_path
                / "response")
            in
            Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 response_dir;
            let post_stream ~sw:_ ~inputs =
              Int.incr requests;
              if !requests > 1
              then (
                assert (
                  List.exists inputs ~f:(function
                    | Openai.Responses.Item.Function_call_output _
                    | Custom_tool_call_output _ -> true
                    | _ -> false));
                Stdlib.Seq.empty)
              else
                let open Openai.Responses.Response_stream in
                let call ~custom ~name ~payload ~index =
                  let item_id = "native-item-" ^ Int.to_string index in
                  let call_id = "native-call-" ^ Int.to_string index in
                  [ Output_item_added
                      { item =
                          (if custom
                           then
                             Custom_function
                               { name
                               ; input = ""
                               ; call_id
                               ; _type = "custom_tool_call"
                               ; id = Some item_id
                               }
                           else
                             Function_call
                               { name
                               ; arguments = ""
                               ; call_id
                               ; _type = "function_call"
                               ; id = Some item_id
                               ; status = None
                               })
                      ; output_index = index
                      ; type_ = "response.output_item.added"
                      }
                  ; (if custom
                     then
                       Custom_tool_call_input_done
                         { input = payload
                         ; item_id
                         ; output_index = index
                         ; type_ = "response.custom_tool_call_input.done"
                         }
                     else
                       Function_call_arguments_done
                         { arguments = payload
                         ; item_id
                         ; output_index = index
                         ; type_ = "response.function_call_arguments.done"
                         })
                  ]
                in
                let initial =
                  call
                    ~custom:
                      (match mode with
                       | `Kind_mismatch -> true
                       | _ -> custom)
                    ~name:original_name
                    ~payload:
                      (match mode with
                       | `Invalid -> "null"
                       | `Invalid_json -> "{"
                       | _ -> "{}")
                    ~index:0
                in
                Stdlib.List.to_seq
                  (initial
                   @
                   if mixed
                   then call ~custom:false ~name:"counter" ~payload:"null" ~index:1
                   else [])
            in
            let dispatch_tool ~input ~capabilities =
              let native =
                Agent_session.Native_tool_dispatch.create
                  ~input
                  ~capabilities
                  ~registry:(fun () -> !registry)
                  ~now:Agent_protocol.Timestamp.now
                  ~is_halted:(fun () ->
                    !halted
                    || Chat_response.Moderator_manager.is_halted manager
                       |> Result.ok_or_failwith)
                  ~admit:(fun _ _ ->
                    Int.incr admitted;
                    Eio.Fiber.yield ();
                    (match mode with
                     | `Revoked ->
                       registry
                       := Chat_response.Tool_capability.select !registry ~names:[]
                          |> Result.map_error ~f:(fun error ->
                            error.Chat_response.Tool_capability.message)
                          |> Result.ok_or_failwith
                     | `Halt_wait -> halted := true
                     | _ -> ());
                    Ok ())
                  ~prepare_output:(fun _ ->
                    match mode with
                    | `Disclosure -> Error (handoff_error "private disclosure")
                    | _ -> Ok (`String "disclosed"))
              in
              let moderator =
                Agent_session.Moderator_tool_dispatch.create
                  ~definition
                  ~manager
                  ~input
                  ~capabilities
                  ~available_tools:[]
                  ~session_meta:`Null
                  ~now:Agent_protocol.Timestamp.now
                  ~validate_work:(fun _ -> Error "pending disabled")
                  ~admit:(fun _ -> Ok ())
                  ~prepare_outcome:(fun _ -> Ok ())
                  ()
              in
              Chat_response.In_memory_stream.Tool_dispatch.chain [ moderator; native ]
            in
            let tool_tbl = String.Table.create () in
            Hashtbl.set tool_tbl ~key:"read_file" ~data:(fun ~invocation:_ _ ->
              failwith "native adapter fell through");
            let worker =
              Agent_session.Turn_worker.create
                ~dispatch_tool
                { env
                ; response_dir
                ; tools = []
                ; tool_tbl
                ; temperature = None
                ; max_output_tokens = None
                ; reasoning = None
                ; moderator =
                    Some
                      { manager
                      ; session_id = Agent_protocol.Id.Session.to_string input.session_id
                      ; session_meta = `Null
                      ; runtime_policy = Chat_response.Runtime_semantics.default_policy
                      }
                ; permission_profile =
                    permission_policy
                      ~tool_default:
                        (match mode with
                         | `Deny -> Deny
                         | _ -> Allow)
                      ~fallback:Fallback_deny
                      ~evaluator:None
                      ~reviewer:None
                ; review_permission = (fun _ -> assert false)
                ; history_compaction = false
                ; parallel_tool_calls = true
                ; model = Openai.Responses.Request.O3
                ; prompt_cache_key = None
                ; prompt_cache_retention = None
                ; post_stream = Some post_stream
                ; agent_page_classifications = []
                ; delegated_permission_tools = String.Set.empty
                ; redact_tool_payload =
                    (fun ~name:_ value -> if redacted then "\"hidden\"" else value)
                }
            in
            Agent_session.Operation_worker.run worker ~sw ~input caps))
        (fun _env actor _writer backend ->
           let rec finished () =
             let state = Agent_session.Session_actor.state actor |> protocol_ok in
             if Option.is_none state.active_operation
             then state
             else (
               Eio.Fiber.yield ();
               finished ())
           in
           let state = finished () in
           let expected_count =
             if call_save_rejected then 0 else if mixed then 2 else 1
           in
           if List.length state.invocations <> expected_count
           then
             failwithf
               "invocation count: expected %d, got %d (save=%b dispatch=%b observer=%b \
                moderator=%b requests=%d)"
               expected_count
               (List.length state.invocations)
               call_save_rejected
               dispatch_rejected
               observer_failed
               moderator_target
               !requests
               ();
           if call_save_rejected
           then (
             assert (!calls = 0 && !admitted = 0 && !post_calls = 0 && !requests = 1);
             assert (List.length state.conversation.canonical_history = 1);
             assert_same_session_snapshot
               state
               (Agent_session.Memory_backend.state backend))
           else (
             let native =
               List.find_exn state.invocations ~f:(fun invocation ->
                 String.equal
                   invocation.context.tool_name
                   (if moderator_target then "counter" else "read_file"))
             in
             let expected =
               match mode with
               | `Success
               | `Custom
               | `Redirect
               | `Post_fail
               | `Mixed
               | `Redacted
               | `Publish_rejected -> None
               | `Invalid | `Rewrite_invalid | `Invalid_json | `Kind_mismatch ->
                 Some "invocation.invalid_input"
               | `Deny -> Some "invocation.permission_denied"
               | `Pre_reject | `Pre_reject_end -> Some "invocation.pre_tool_rejected"
               | `Pre_fail -> Some "invocation.pre_tool_failed"
               | `Revoked | `Revoked_before -> Some "invocation.stale_binding"
               | `Halt_wait | `Pre_end -> Some "invocation.session_ended"
               | `Disclosure -> Some "invocation.disclosure_rejected"
               | `Call_save_rejected
               | `Moderator_call_save_rejected
               | `Custom_call_save_rejected -> assert false
               | `Dispatch_rejected
               | `Observer_failed
               | `Moderator_dispatch_rejected
               | `Moderator_observer_failed
               | `Custom_dispatch_rejected
               | `Custom_observer_failed -> Some "interrupted"
             in
             (match native.status, expected with
              | Published (Complete (`String "disclosed")), None -> ()
              | Resolved (Complete (`String "disclosed")), None ->
                assert publication_rejected
              | Published (Fail error), Some code -> assert (String.equal error.code code)
              | Published (Cancelled _), Some "interrupted" -> ()
              | _ -> assert false);
             let executed =
               match mode with
               | `Disclosure -> true
               | _ -> Option.is_none expected
             in
             assert (!calls = if executed then 1 else 0);
             assert (
               !admitted
               =
               if
                 before_execution_failure
                 ||
                 match mode with
                 | `Invalid
                 | `Invalid_json
                 | `Kind_mismatch
                 | `Revoked_before
                 | `Rewrite_invalid
                 | `Pre_reject
                 | `Pre_reject_end
                 | `Pre_fail
                 | `Pre_end -> true
                 | _ -> false
               then 0
               else 1);
             assert (
               !post_calls
               =
               if
                 before_execution_failure
                 ||
                 match mode with
                 | `Pre_end | `Pre_reject_end | `Publish_rejected -> true
                 | _ -> false
               then 0
               else if mixed
               then 2
               else 1);
             assert (
               !requests
               =
               if
                 before_execution_failure
                 ||
                 match mode with
                 | `Post_fail | `Publish_rejected | `Pre_end | `Pre_reject_end -> true
                 | _ -> false
               then 1
               else 2);
             assert (
               List.length state.conversation.canonical_history
               = if mixed then 5 else if publication_rejected then 2 else 3);
             let routing = Option.value_exn native.routing in
             assert (String.equal routing.original_name original_name);
             if redacted
             then (
               let canonical = Option.value_exn routing.canonical_payload in
               assert (
                 String.equal
                   canonical.sha256
                   (Chatmd_shell_spec.Source_ref.digest "\"hidden\""));
               assert (
                 String.equal
                   routing.final_payload.sha256
                   (Chatmd_shell_spec.Source_ref.digest "{}")));
             let failures =
               Agent_session.Memory_backend.events_after backend 0L
               |> protocol_ok
               |> List.count ~f:(fun event ->
                 Agent_protocol.Event.Durable.equal_kind event.kind Operation_failed)
             in
             assert (
               failures
               =
               if before_execution_failure || post_failed || publication_rejected
               then 1
               else 0);
             assert (Bool.equal (Option.is_some state.failure) publication_rejected);
             assert_same_session_snapshot
               state
               (Agent_session.Memory_backend.state backend))))
;;

let%test_unit
    "streamed moderator tools use actor publication and preserve post-hook failures"
  =
  List.iter
    [ `Success
    ; `Deny
    ; `Disclosure
    ; `Post_fail
    ; `Publish_rejected
    ; `Invalid_json
    ; `Redirect
    ; `Redirect_bad
    ; `Revoked
    ; `End_session
    ; `Unhandled
    ; `Duplicate
    ; `Wrong_id
    ; `Invalid_output
    ; `Forged_error
    ; `Result_rejected
    ; `Pre_reject
    ; `Pre_reject_end
    ; `Pre_reject_post_fail
    ; `Custom_success
    ; `Pre_reject_custom
    ; `Original_invalid
    ; `Custom_invalid
    ; `Rewrite_bad
    ; `Rewrite_ok
    ; `Redacted_input
    ; `Pre_fail
    ; `Pre_host_exception
    ; `Pre_invalid_action
    ; `Pre_custom_fail
    ; `Original_array_limit
    ; `Original_depth_limit
    ; `Original_bytes_limit
    ; `Custom_bytes_limit
    ; `Rewrite_limit
    ; `Redirect_limit
    ; `Pre_end_multi
    ; `Pre_reject_end_multi
    ; `End_session_multi
    ]
    ~f:(fun mode ->
      let request_count = ref 0 in
      let admitted = ref 0 in
      let host_calls = ref 0 in
      let live_snapshot = ref None in
      let native_calls = ref 0 in
      let multi =
        List.mem
          [ `Pre_end_multi; `Pre_reject_end_multi; `End_session_multi ]
          mode
          ~equal:Poly.equal
      in
      let pre_end = Poly.equal mode `Pre_end_multi in
      let implementation_end =
        Poly.equal mode `End_session || Poly.equal mode `End_session_multi
      in
      let pre_failed =
        List.mem
          [ `Pre_fail; `Pre_host_exception; `Pre_invalid_action; `Pre_custom_fail ]
          mode
          ~equal:Poly.equal
      in
      let redirected =
        List.mem [ `Redirect; `Redirect_bad; `Redirect_limit ] mode ~equal:Poly.equal
      in
      let rewritten =
        List.mem [ `Rewrite_bad; `Rewrite_ok; `Rewrite_limit ] mode ~equal:Poly.equal
      in
      let original_limit =
        List.mem
          [ `Original_array_limit
          ; `Original_depth_limit
          ; `Original_bytes_limit
          ; `Custom_bytes_limit
          ]
          mode
          ~equal:Poly.equal
      in
      let final_limit =
        Poly.equal mode `Rewrite_limit || Poly.equal mode `Redirect_limit
      in
      let over_array = `Array (List.init 257 ~f:(fun _ -> `Null)) in
      let array_expression =
        "`Array([" ^ String.concat ~sep:"," (List.init 257 ~f:(fun _ -> "`Null")) ^ "])"
      in
      let original_payload =
        match mode with
        | `Invalid_json -> "[broken"
        | `Original_invalid -> "\"wrong\""
        | `Original_array_limit -> Jsonaf.to_string over_array
        | `Original_depth_limit ->
          Jsonaf.to_string
            (List.fold (List.init 17 ~f:Fn.id) ~init:`Null ~f:(fun value _ ->
               `Array [ value ]))
        | `Original_bytes_limit ->
          Jsonaf.to_string (`String (String.make (256 * 1024) 'x'))
        | `Custom_bytes_limit -> String.make (256 * 1024) 'x'
        | _ -> if redirected then "{}" else "null"
      in
      let invalid_original =
        original_limit
        || List.mem
             [ `Invalid_json; `Original_invalid; `Custom_invalid ]
             mode
             ~equal:Poly.equal
      in
      let pre_rejected =
        List.mem
          [ `Pre_reject
          ; `Pre_reject_end
          ; `Pre_reject_post_fail
          ; `Pre_reject_custom
          ; `Pre_reject_end_multi
          ]
          mode
          ~equal:Poly.equal
      in
      let ends_session = implementation_end || Poly.equal mode `Pre_reject_end || multi in
      let custom =
        Poly.equal mode `Custom_success
        || Poly.equal mode `Pre_reject_custom
        || Poly.equal mode `Custom_invalid
        || Poly.equal mode `Pre_custom_fail
        || Poly.equal mode `Custom_bytes_limit
      in
      let post_fails =
        Poly.equal mode `Post_fail || Poly.equal mode `Pre_reject_post_fail
      in
      with_handoff_actor
        ~reject:(fun next ->
          List.exists
            next.Agent_session.Session_transition.state.invocations
            ~f:(fun inv ->
              (Poly.equal mode `Publish_rejected && Option.is_some inv.output_entry_id)
              || (Poly.equal mode `Result_rejected
                  &&
                  match inv.status with
                  | Resolved (Complete _) -> true
                  | _ -> false)))
        ~make_worker:(fun env actor_ready ->
          let events =
            if pre_end
            then
              "| `Pre_tool_call(c) -> Task.bind(Runtime.end_session(\"done\"), fun \
               ignored -> Task.pure(state)) | _ -> Task.pure(state)"
            else if pre_failed
            then
              "| `Pre_tool_call(c) -> let ignored = state[0] <- 99 in "
              ^ "Task.bind(Runtime.emit(`String(\"uncommitted\")), fun ignored -> "
              ^ "Task.bind(Turn.prepend_system(\"uncommitted\"), fun ignored -> "
              ^ (if Poly.equal mode `Pre_host_exception
                 then
                   "Task.bind(Tool.call(\"explode\", `Null), fun ignored -> \
                    Task.pure(state))"
                 else if Poly.equal mode `Pre_invalid_action
                 then
                   "Task.bind(Tool.reject(\"rejected\"), fun ignored -> \
                    Task.bind(Tool.redirect(\"counter\", `Null), fun ignored -> \
                    Task.pure(state)))"
                 else "Task.fail(\"private diagnostic\")")
              ^ ")) | _ -> Task.pure(state)"
            else if invalid_original
            then
              "| `Pre_tool_call(c) -> Task.fail(\"invalid input reached pre handler\") | \
               _ -> Task.pure(state)"
            else if rewritten
            then
              "| `Pre_tool_call(c) -> Task.bind(Tool.rewrite_args("
              ^ (if final_limit
                 then array_expression
                 else if Poly.equal mode `Rewrite_bad
                 then "`String(\"wrong\")"
                 else "`Null")
              ^ "), fun ignored -> Task.pure(state)) | _ -> Task.pure(state)"
            else if pre_rejected
            then
              "| `Pre_tool_call(c) -> Task.bind(Tool.reject(\"private diagnostic\"), fun \
               ignored -> "
              ^ (if
                   Poly.equal mode `Pre_reject_end
                   || Poly.equal mode `Pre_reject_end_multi
                 then
                   "Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
                    Task.pure(state)))"
                 else "Task.pure(state))")
              ^ (if post_fails
                 then
                   " | `Post_tool_response(r) -> let ignored = state[0] <- 99 in \
                    Task.fail(\"post hook failed\")"
                 else "")
              ^ " | _ -> Task.pure(state)"
            else if Poly.equal mode `Post_fail
            then
              "| `Post_tool_response(r) -> let ignored = state[0] <- 99 in \
               Task.fail(\"post hook failed\") | _ -> Task.pure(state)"
            else if redirected
            then
              "| `Pre_tool_call(c) -> Task.bind(Tool.redirect(\"counter\", "
              ^ (if final_limit
                 then array_expression
                 else if Poly.equal mode `Redirect_bad
                 then "`String(\"wrong\")"
                 else "`Null")
              ^ "), fun ignored -> Task.pure(state)) | _ -> Task.pure(state)"
            else "| _ -> Task.pure(state)"
          in
          let manager, _, definition =
            handoff_definition
              ~events
              ~script_limits:
                (if original_limit || final_limit
                 then {|max_array_items="256" max_depth="32" max_value="256KiB"|}
                 else "")
              ~moderator_capabilities:
                { Chat_response.Moderation.Capabilities.default with
                  on_tool_call =
                    (fun ~name:_ ~args:_ ->
                      Int.incr host_calls;
                      failwith "private diagnostic from host")
                }
              ~schema:
                (if original_limit || final_limit
                 then "true"
                 else if
                   redirected
                   || rewritten
                   || invalid_original
                   || Poly.equal mode `Invalid_output
                 then "{\"type\":\"null\"}"
                 else "true")
              ~resolve:
                (match mode with
                 | `Unhandled -> "Task.pure(())"
                 | `Duplicate ->
                   "Task.bind(Invocation.resolve(p.context.invocation_id, \
                    `Complete(`Null)), fun ignored -> \
                    Invocation.resolve(p.context.invocation_id, `Complete(`Null)))"
                 | `Wrong_id -> "Invocation.resolve(\"other\", `Complete(`Null))"
                 | `Invalid_output ->
                   "Invocation.resolve(p.context.invocation_id, \
                    `Complete(`String(\"wrong\")))"
                 | `Forged_error ->
                   "Task.fail(\"invocation.unhandled: private diagnostic\")"
                 | _ -> "Invocation.resolve(p.context.invocation_id, `Complete(`Null))")
              ~finish:
                (if implementation_end
                 then
                   "Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
                    Task.pure(state))"
                 else "Task.pure(state)")
              env
          in
          live_snapshot
          := Some
               (fun () ->
                 Chat_response.Moderator_manager.identity_snapshot manager
                 |> Result.ok_or_failwith);
          Agent_session.Operation_worker.create ~run:(fun ~sw ~input caps ->
            let actor = Eio.Promise.await actor_ready in
            let state = Agent_session.Session_actor.state actor |> protocol_ok in
            let response_dir =
              Eio.Path.(
                Eio.Stdenv.fs env
                / state.spec.workspace_instance.canonical_root.native_path
                / "response")
            in
            Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 response_dir;
            let post_stream ~sw:_ ~inputs =
              Int.incr request_count;
              if !request_count = 1
              then (
                let initial =
                  Stdlib.List.to_seq
                    Openai.Responses.Response_stream.
                      [ Output_item_added
                          { item =
                              (if custom
                               then
                                 Custom_function
                                   { name = "counter"
                                   ; input = ""
                                   ; call_id = "counter-call"
                                   ; _type = "custom_tool_call"
                                   ; id = Some "counter-item"
                                   }
                               else
                                 Function_call
                                   { name = (if redirected then "alias" else "counter")
                                   ; arguments = ""
                                   ; call_id = "counter-call"
                                   ; _type = "function_call"
                                   ; id = Some "counter-item"
                                   ; status = None
                                   })
                          ; output_index = 0
                          ; type_ = "response.output_item.added"
                          }
                      ; (if custom
                         then
                           Custom_tool_call_input_done
                             { input = original_payload
                             ; item_id = "counter-item"
                             ; output_index = 0
                             ; type_ = "response.custom_tool_call_input.done"
                             }
                         else
                           Function_call_arguments_done
                             { arguments = original_payload
                             ; item_id = "counter-item"
                             ; output_index = 0
                             ; type_ = "response.function_call_arguments.done"
                             })
                      ]
                in
                if not multi
                then initial
                else
                  Stdlib.Seq.append initial (fun () ->
                    (* Force later provider items to arrive after the first handler
                     has halted, including the asynchronous implementation path. *)
                    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
                      let rec halted () =
                        if
                          Chat_response.Moderator_manager.is_halted manager
                          |> Result.ok_or_failwith
                        then ()
                        else (
                          Eio.Fiber.yield ();
                          halted ())
                      in
                      halted ());
                    let open Openai.Responses.Response_stream in
                    let message =
                      match worker_output_item with
                      | Openai.Responses.Item.Output_message message -> message
                      | _ -> assert false
                    in
                    Stdlib.List.to_seq
                      [ Output_item_added
                          { item =
                              Custom_function
                                { name = "counter"
                                ; input = ""
                                ; call_id = "later-custom"
                                ; _type = "custom_tool_call"
                                ; id = Some "later-custom-item"
                                }
                          ; output_index = 1
                          ; type_ = "response.output_item.added"
                          }
                      ; Custom_tool_call_input_done
                          { input = "null"
                          ; item_id = "later-custom-item"
                          ; output_index = 1
                          ; type_ = "response.custom_tool_call_input.done"
                          }
                      ; Output_item_added
                          { item =
                              Function_call
                                { name = "native"
                                ; arguments = ""
                                ; call_id = "later-native"
                                ; _type = "function_call"
                                ; id = Some "later-native-item"
                                ; status = None
                                }
                          ; output_index = 2
                          ; type_ = "response.output_item.added"
                          }
                      ; Function_call_arguments_done
                          { arguments = "null"
                          ; item_id = "later-native-item"
                          ; output_index = 2
                          ; type_ = "response.function_call_arguments.done"
                          }
                      ; Output_item_done
                          { item = Output_message message
                          ; output_index = 3
                          ; type_ = "response.output_item.done"
                          }
                      ]
                      ()))
              else (
                assert (
                  List.exists inputs ~f:(function
                    | Openai.Responses.Item.Function_call_output _ -> not custom
                    | Custom_tool_call_output _ -> custom
                    | _ -> false));
                Seq.empty)
            in
            let dispatch_tool ~input ~capabilities =
              Agent_session.Moderator_tool_dispatch.create
                ~definition
                ~manager
                ~input
                ~capabilities
                ~available_tools:[]
                ~session_meta:`Null
                ~now:Agent_protocol.Timestamp.now
                ~validate_work:(fun _ -> Error "no pending work")
                ~admit:(fun request ->
                  Int.incr admitted;
                  assert (String.equal request.name "counter");
                  if redirected
                  then (
                    assert (String.equal request.original_name "alias");
                    assert (String.equal request.original_payload "{}");
                    assert (String.equal request.payload "null"));
                  if Poly.equal mode `Revoked
                  then Error "capability was revoked"
                  else Ok ())
                ~prepare_outcome:(fun _ ->
                  if Poly.equal mode `Disclosure then Error "blocked" else Ok ())
                ()
            in
            let worker =
              let tool_tbl = String.Table.create () in
              Hashtbl.set tool_tbl ~key:"native" ~data:(fun ~invocation:_ _ ->
                Int.incr native_calls;
                Openai.Responses.Tool_output.Output.Text "unexpected execution");
              Agent_session.Turn_worker.create
                ~dispatch_tool
                { env
                ; response_dir
                ; tools = []
                ; tool_tbl
                ; temperature = None
                ; max_output_tokens = None
                ; reasoning = None
                ; moderator =
                    Some
                      { manager
                      ; session_id = Agent_protocol.Id.Session.to_string input.session_id
                      ; session_meta = `Null
                      ; runtime_policy = Chat_response.Runtime_semantics.default_policy
                      }
                ; permission_profile =
                    permission_policy
                      ~tool_default:(if Poly.equal mode `Deny then Deny else Allow)
                      ~fallback:Fallback_deny
                      ~evaluator:None
                      ~reviewer:None
                ; review_permission = (fun _ -> assert false)
                ; history_compaction = false
                ; parallel_tool_calls = true
                ; model = Openai.Responses.Request.O3
                ; prompt_cache_key = None
                ; prompt_cache_retention = None
                ; post_stream = Some post_stream
                ; agent_page_classifications = []
                ; delegated_permission_tools = String.Set.empty
                ; redact_tool_payload =
                    (fun ~name:_ value ->
                      if Poly.equal mode `Redacted_input then "\"redacted\"" else value)
                }
            in
            Agent_session.Operation_worker.run worker ~sw ~input caps))
        (fun _env actor writer backend ->
           let rec finished () =
             let state = Agent_session.Session_actor.state actor |> protocol_ok in
             if Option.is_some state.active_operation
             then (
               Eio.Fiber.yield ();
               finished ())
             else state
           in
           let state = finished () in
           assert (!native_calls = 0);
           if Poly.equal mode `Publish_rejected
           then (
             assert (Option.is_some state.failure);
             assert (
               match state.lifecycle.observed with
               | Failed _ -> true
               | _ -> false);
             let entry =
               let id =
                 History_entry.Id.create ~namespace:"rejected-next-turn" ~sequence:0
                 |> Result.ok_or_failwith
               in
               Agent_session.History_codec.user_text ~id "must not start"
               |> Agent_session.History_codec.to_protocol
             in
             assert (
               Result.is_error
                 (Agent_session.Session_actor.submit_message
                    actor
                    ~attachment_id:writer.id
                    entry));
             let after = Agent_session.Session_actor.state actor |> protocol_ok in
             assert (Poly.equal state after));
           assert (List.length state.invocations = if multi then 2 else 1);
           let invocation =
             List.find_exn state.invocations ~f:(fun inv ->
               Option.equal
                 String.equal
                 inv.context.provider_call_id
                 (Some "counter-call"))
           in
           if multi
           then (
             let later =
               List.find_exn state.invocations ~f:(fun inv ->
                 Option.equal
                   String.equal
                   inv.context.provider_call_id
                   (Some "later-custom"))
             in
             assert (Option.is_some later.output_entry_id);
             assert (
               Poly.equal
                 (Option.value_exn later.routing).preparation
                 Agent_protocol.Invocation.Session_ended);
             match later.status with
             | Published (Fail error) ->
               assert (String.equal error.code "invocation.session_ended")
             | _ -> assert false);
           let routing = Option.value_exn invocation.routing in
           let final_payload =
             if final_limit
             then Jsonaf.to_string over_array
             else if Poly.equal mode `Redirect_bad || Poly.equal mode `Rewrite_bad
             then "\"wrong\""
             else if redirected || rewritten
             then "null"
             else original_payload
           in
           let fingerprint payload =
             Agent_protocol.Invocation.
               { sha256 = Chatmd_shell_spec.Source_ref.digest payload
               ; byte_length = String.length payload
               }
           in
           assert (
             String.equal
               routing.original_name
               (if redirected then "alias" else "counter"));
           assert (
             Poly.equal
               routing.kind
               (if custom then Agent_protocol.Invocation.Custom else Function));
           assert (Poly.equal routing.original_payload (fingerprint original_payload));
           assert (Poly.equal routing.final_payload (fingerprint final_payload));
           assert (
             Poly.equal
               routing.canonical_payload
               (Some
                  (fingerprint
                     (if Poly.equal mode `Redacted_input
                      then "\"redacted\""
                      else final_payload))));
           let expected_preparation =
             if pre_end
             then Agent_protocol.Invocation.Session_ended
             else if invalid_original
             then Agent_protocol.Invocation.Invalid_input
             else if pre_rejected
             then Pre_tool_rejected
             else if pre_failed
             then Pre_tool_failed
             else Passed
           in
           if not (Poly.equal routing.preparation expected_preparation)
           then
             failwithf
               "unexpected preparation for input %s/%d: %s, expected %s"
               (Chatmd_shell_spec.Source_ref.digest original_payload)
               (String.length original_payload)
               (Sexp.to_string
                  (Agent_protocol.Invocation.sexp_of_preparation routing.preparation))
               (Sexp.to_string
                  (Agent_protocol.Invocation.sexp_of_preparation expected_preparation))
               ();
           let expected_count =
             if
               Poly.equal mode `Success
               || Poly.equal mode `Custom_success
               || Poly.equal mode `Rewrite_ok
               || Poly.equal mode `Redacted_input
               || Poly.equal mode `Post_fail
               || Poly.equal mode `Publish_rejected
               || Poly.equal mode `Redirect
               || Poly.equal mode `End_session
               || Poly.equal mode `End_session_multi
             then 1
             else 0
           in
           let saved_snapshot =
             match state.moderator with
             | Some (`Object [ ("identity_snapshot_sexp", `String encoded) ]) ->
               Session.Moderator_state.Identity_snapshot.t_of_sexp
                 (Sexp.of_string encoded)
             | _ -> assert false
           in
           let live = (Option.value_exn !live_snapshot) () in
           assert (Poly.equal live.current_state saved_snapshot.current_state);
           assert (!host_calls = if Poly.equal mode `Pre_host_exception then 1 else 0);
           if pre_failed
           then (
             assert (List.is_empty live.prepended_items);
             assert (List.is_empty live.queued_internal_events));
           assert (
             Poly.equal
               saved_snapshot.current_state
               (Session.Snapshot.Array [ Int expected_count ]));
           assert (
             Bool.equal
               (Option.is_some invocation.output_entry_id)
               (not (Poly.equal mode `Publish_rejected)));
           assert (
             match invocation.status with
             | Published (Complete `Null) ->
               Poly.equal mode `Success
               || Poly.equal mode `Custom_success
               || Poly.equal mode `Rewrite_ok
               || Poly.equal mode `Redacted_input
               || Poly.equal mode `Post_fail
               || Poly.equal mode `Redirect
               || Poly.equal mode `End_session
               || Poly.equal mode `End_session_multi
             | Published (Fail error) ->
               let expected =
                 match mode with
                 | `Deny | `Revoked -> "invocation.permission_denied"
                 | `Disclosure -> "invocation.disclosure_rejected"
                 | `Invalid_json
                 | `Redirect_bad
                 | `Original_invalid
                 | `Custom_invalid
                 | `Original_array_limit
                 | `Original_depth_limit
                 | `Original_bytes_limit
                 | `Custom_bytes_limit
                 | `Rewrite_limit
                 | `Redirect_limit
                 | `Rewrite_bad -> "invocation.invalid_input"
                 | `Unhandled -> "invocation.unhandled"
                 | `Duplicate -> "invocation.duplicate_resolution"
                 | `Wrong_id -> "invocation.wrong_id"
                 | `Invalid_output -> "invocation.invalid_output"
                 | `Forged_error -> "invocation.handler_failed"
                 | `Result_rejected -> "invocation.commit_failed"
                 | `Pre_fail
                 | `Pre_host_exception
                 | `Pre_invalid_action
                 | `Pre_custom_fail -> "invocation.pre_tool_failed"
                 | `Pre_reject
                 | `Pre_reject_end
                 | `Pre_reject_post_fail
                 | `Pre_reject_custom -> "invocation.pre_tool_rejected"
                 | `Pre_reject_end_multi -> "invocation.pre_tool_rejected"
                 | `Pre_end_multi -> "invocation.session_ended"
                 | _ -> assert false
               in
               assert (not error.retryable);
               assert (Poly.equal error.details `Null);
               assert (
                 not (String.is_substring error.message ~substring:"private diagnostic"));
               String.equal error.code expected
             | Resolved (Complete `Null) -> Poly.equal mode `Publish_rejected
             | _ -> false);
           assert (
             List.length state.conversation.canonical_history
             = if multi then 8 else if Poly.equal mode `Publish_rejected then 2 else 3);
           let failed = post_fails || Poly.equal mode `Publish_rejected in
           assert (!request_count = if failed || ends_session then 1 else 2);
           if ends_session then assert saved_snapshot.halted;
           assert (
             !admitted
             =
             if
               pre_end
               || pre_rejected
               || pre_failed
               || invalid_original
               || final_limit
               || Poly.equal mode `Rewrite_bad
               || Poly.equal mode `Redirect_bad
             then 0
             else 1);
           let events =
             Agent_session.Memory_backend.events_after backend 0L |> protocol_ok
           in
           let failures =
             List.filter events ~f:(fun e ->
               Agent_protocol.Event.Durable.equal_kind e.kind Operation_failed)
           in
           assert (List.length failures = if failed then 1 else 0);
           if post_fails
           then (
             let event = List.hd_exn failures in
             match
               Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
               |> protocol_ok
             with
             | Operation_failed { state = Failed error; _ } ->
               assert (not error.retryable);
               assert (
                 String.is_substring
                   (Jsonaf.to_string error.data)
                   ~substring:"post_tool_response");
               assert (
                 String.is_substring
                   (Jsonaf.to_string error.data)
                   ~substring:
                     (History_entry.Id.to_string
                        (Option.value_exn invocation.output_entry_id)))
             | _ -> assert false);
           assert (
             Poly.equal
               state.invocations
               (Agent_session.Memory_backend.state backend).invocations)))
;;

let%test_unit
    "routed calls recheck revoked policy and release cancelled handlers and waits"
  =
  List.iter [ `Revoked; `Cancelled; `Active_cancel; `Session_ended ] ~f:(fun mode ->
    let done_, done_u = Eio.Promise.create () in
    with_handoff_actor
      ~make_worker:(fun env actor_ready ->
        let manager, _, definition =
          handoff_definition
            ~finish:
              (if Poly.equal mode `Session_ended
               then
                 "Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
                  Task.pure(state))"
               else "Task.pure(state)")
            env
        in
        Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
          let actor = Eio.Promise.await actor_ready in
          let first_held, first_held_u = Eio.Promise.create () in
          let release, release_u = Eio.Promise.create () in
          let cancel, cancel_u = Eio.Promise.create () in
          let cancelled, cancelled_u = Eio.Promise.create () in
          let attempted, attempted_u = Eio.Promise.create () in
          let admitted = ref [] in
          let revoked = ref false in
          let prepared = ref 0 in
          let history = ref input.history in
          let allocate item =
            let id =
              History_entry.Id_source.allocate caps.id_source |> Result.ok_or_failwith
            in
            History_entry.create_with_id ~id item
          in
          let request call_id =
            let call =
              allocate
                (Openai.Responses.Item.Function_call
                   { name = "counter"
                   ; arguments = "null"
                   ; call_id
                   ; _type = "function_call"
                   ; id = None
                   ; status = None
                   })
            in
            caps.commit_entry call |> protocol_ok;
            history := !history @ [ call ];
            Chat_response.In_memory_stream.Tool_dispatch.
              { kind = Function
              ; original_name = "counter"
              ; original_payload = "null"
              ; name = "counter"
              ; payload = "null"
              ; rejection = None
              ; call
              ; history = !history
              ; source = None
              ; parent_call_id = None
              }
          in
          let dispatch =
            Agent_session.Moderator_tool_dispatch.create
              ~definition
              ~manager
              ~input
              ~capabilities:caps
              ~available_tools:[]
              ~session_meta:`Null
              ~now:Agent_protocol.Timestamp.now
              ~validate_work:(fun _ -> Error "no pending work")
              ~admit:(fun request ->
                let call_id =
                  match History_entry.item request.call with
                  | Function_call c -> c.call_id
                  | _ -> assert false
                in
                admitted := !admitted @ [ call_id ];
                if !revoked then Error "revoked while queued" else Ok ())
              ~prepare_outcome:(fun _ ->
                Int.incr prepared;
                if !prepared = 1
                then (
                  Eio.Promise.resolve first_held_u ();
                  Eio.Promise.await release);
                Ok ())
              ()
          in
          let run request =
            let result = dispatch.run request ~authorize:ignore |> Option.value_exn in
            let call_id =
              match History_entry.item request.call with
              | Function_call c -> c.call_id
              | _ -> assert false
            in
            let output =
              allocate
                (Openai.Responses.Item.Function_call_output
                   { output = result.output
                   ; call_id
                   ; _type = "function_call_output"
                   ; id = None
                   ; status = None
                   })
            in
            (Option.value_exn result.commit_output) output;
            history := !history @ [ output ]
          in
          let first = request "first" in
          let second = request "second" in
          Eio.Switch.run (fun sw ->
            Eio.Fiber.fork ~sw (fun () ->
              if Poly.equal mode `Active_cancel
              then (
                let result =
                  Eio.Fiber.first
                    (fun () ->
                       run first;
                       `Completed)
                    (fun () ->
                       Eio.Promise.await cancel;
                       `Cancelled)
                in
                assert (Poly.equal result `Cancelled);
                Eio.Promise.resolve cancelled_u ())
              else run first);
            Eio.Promise.await first_held;
            Eio.Fiber.fork ~sw (fun () ->
              if Poly.equal mode `Cancelled
              then (
                let result =
                  Eio.Fiber.first
                    (fun () ->
                       Eio.Promise.resolve attempted_u ();
                       run second;
                       `Completed)
                    (fun () ->
                       Eio.Promise.await cancel;
                       `Cancelled)
                in
                assert (Poly.equal result `Cancelled);
                Eio.Promise.resolve cancelled_u ())
              else (
                Eio.Promise.resolve attempted_u ();
                run second));
            Eio.Promise.await attempted;
            Eio.Fiber.yield ();
            (* These mailbox requests must remain responsive while the first
               handler owns the moderator and the second call waits. *)
            let queued = Agent_session.Session_actor.state actor |> protocol_ok in
            assert (List.length queued.invocations = 2);
            assert (Poly.equal !admitted [ "first" ]);
            if Poly.equal mode `Cancelled
            then (
              Eio.Promise.resolve cancel_u ();
              Eio.Promise.await cancelled;
              let after = Agent_session.Session_actor.state actor |> protocol_ok in
              assert (List.length after.invocations = 2))
            else if Poly.equal mode `Active_cancel
            then (
              Eio.Promise.resolve cancel_u ();
              Eio.Promise.await cancelled)
            else if Poly.equal mode `Revoked
            then revoked := true;
            Eio.Promise.resolve release_u ());
          let state = Agent_session.Session_actor.state actor |> protocol_ok in
          assert (!prepared = if Poly.equal mode `Active_cancel then 2 else 1);
          assert (
            Poly.equal
              state.moderator
              (Some
                 (Agent_session.Runtime_builder.encode_moderator_snapshot
                    (Chat_response.Moderator_manager.identity_snapshot manager
                     |> Result.ok_or_failwith))));
          assert (
            Poly.equal
              (Chat_response.Moderator_manager.identity_snapshot manager
               |> Result.ok_or_failwith)
                .current_state
              (Session.Snapshot.Array [ Int 1 ]));
          let failed =
            List.filter state.invocations ~f:(fun inv ->
              match inv.status with
              | Published (Fail error) ->
                assert (
                  String.equal
                    error.code
                    (if Poly.equal mode `Session_ended
                     then "invocation.session_ended"
                     else "invocation.permission_denied"));
                true
              | Published (Complete `Null) -> false
              | Resolved (Cancelled _) ->
                assert (Poly.equal mode `Active_cancel);
                false
              | Admitted ->
                (match mode with
                 | `Cancelled -> ()
                 | _ -> assert false);
                false
              | _ -> assert false)
          in
          assert (
            List.length failed
            = if Poly.equal mode `Revoked || Poly.equal mode `Session_ended then 1 else 0);
          assert (
            Poly.equal
              !admitted
              (if Poly.equal mode `Cancelled || Poly.equal mode `Session_ended
               then [ "first" ]
               else [ "first"; "second" ]));
          revoked := false;
          run (request "third");
          assert (
            !prepared
            =
            if Poly.equal mode `Session_ended
            then 1
            else if Poly.equal mode `Active_cancel
            then 3
            else 2);
          let snapshot =
            Chat_response.Moderator_manager.identity_snapshot manager
            |> Result.ok_or_failwith
          in
          assert (
            Poly.equal
              snapshot.current_state
              (Session.Snapshot.Array
                 [ Int (if Poly.equal mode `Session_ended then 1 else 2) ]));
          let state = Agent_session.Session_actor.state actor |> protocol_ok in
          Eio.Promise.resolve done_u ();
          Completed
            { final_history = !history
            ; runtime_requests = []
            ; moderator_snapshot = state.moderator
            }))
      (fun _env actor _writer backend ->
         Eio.Promise.await done_;
         let state = await_idle actor in
         assert (
           Poly.equal
             state.invocations
             (Agent_session.Memory_backend.state backend).invocations)))
;;

let%test_unit "actor handoff persists actual manager state and resolution atomically" =
  let done_, done_u = Eio.Promise.create () in
  let reject_once = ref true in
  let reject transition =
    if
      !reject_once
      && List.exists
           transition.Agent_session.Session_transition.state.invocations
           ~f:(fun invocation ->
             match invocation.Agent_protocol.Invocation.status with
             | Resolved (Complete _) -> true
             | _ -> false)
    then (
      reject_once := false;
      true)
    else false
  in
  with_handoff_actor
    ~reject
    ~make_worker:(fun env actor_ready ->
      let manager, invocation = handoff_manager env in
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let invoke () =
          caps.with_moderator_invocation
            ~invocation:(invocation ())
            (fun ~dispatched ~commit ->
               Chat_response.Moderator_manager.handle_invocation_entries
                 manager
                 ~invocation:dispatched
                 ~history:input.history
                 ~available_tools:[]
                 ~session_meta:`Null
                 ~now_ms:0
                 ~validate_work:(fun _ -> Error "no pending work")
                 ~prepare_resolution:(fun ~resolved ~outcome:_ ~snapshot ->
                   commit ~resolved ~snapshot
                   |> Result.map_error ~f:(fun e -> e.Agent_protocol.Error.message)
                   |> Result.map ~f:(fun () -> ignore))
               |> Result.map ~f:(fun _ -> ())
               |> Result.map_error ~f:handoff_error)
        in
        assert (Result.is_error (invoke ()));
        let failed = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (Option.is_none failed.moderator);
        assert (
          match (List.hd_exn failed.invocations).status with
          | Resolved (Fail _) -> true
          | _ -> false);
        let snapshot =
          Chat_response.Moderator_manager.identity_snapshot manager
          |> Result.ok_or_failwith
        in
        assert (Poly.equal snapshot.current_state (Session.Snapshot.Array [ Int 0 ]));
        assert (List.is_empty snapshot.queued_internal_events);
        invoke () |> protocol_ok;
        let snapshot =
          Chat_response.Moderator_manager.identity_snapshot manager
          |> Result.ok_or_failwith
        in
        let encoded =
          Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)
        in
        assert (Poly.equal snapshot.current_state (Session.Snapshot.Array [ Int 1 ]));
        assert (List.length snapshot.queued_internal_events = 1);
        let saved = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (Poly.equal saved.moderator encoded);
        Eio.Promise.resolve done_u encoded;
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot = encoded
          }))
    (fun _env actor _writer backend ->
       let expected = Eio.Promise.await done_ in
       let final = await_idle actor in
       let saved = Agent_session.Memory_backend.state backend in
       assert (Poly.equal saved.moderator expected);
       assert (Poly.equal final.moderator expected);
       assert (List.length saved.invocations = 2);
       assert (List.length saved.conversation.canonical_history = 1);
       let restored =
         Agent_session.Session_persistence.restore_snapshot
           (Sexp.to_string_mach (Agent_session.Session_state.sexp_of_t saved))
         |> store_ok
       in
       assert (Poly.equal restored.moderator expected);
       assert (List.length restored.invocations = 2))
;;

let%test_unit
    "active moderator borrow rejects reentrancy and stale saved commit callbacks"
  =
  let done_, done_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let saved_commit = ref None in
        let saved_resolution = ref None in
        let invocation = invocation_fixture () in
        caps.with_moderator_invocation ~invocation (fun ~dispatched ~commit ->
          saved_commit := Some commit;
          (* Actor reads and unrelated mailbox work continue during the borrow. *)
          let state = Agent_session.Session_actor.state actor |> protocol_ok in
          assert (List.length state.invocations = 1);
          let nested =
            Agent_protocol.Invocation.create
              { invocation.context with id = Agent_protocol.Id.Invocation.create () }
            |> protocol_ok
          in
          let ran = ref false in
          assert (
            Result.is_error
              (caps.with_moderator_invocation
                 ~invocation:nested
                 (fun ~dispatched:_ ~commit:_ ->
                    ran := true;
                    Ok ())));
          assert (not !ran);
          assert (Result.is_error (caps.commit_moderator None));
          assert (
            Result.is_error (Agent_session.Session_actor.change_moderator actor None));
          assert (
            Result.is_error (Agent_session.Session_actor.set_operation_worker actor None));
          assert (
            Result.is_error
              (Agent_session.Session_actor.commit_extensions
                 actor
                 ~generation:0
                 ~expected_revision:state.counters.revision
                 [ Moderator_state None ]));
          let resolved =
            Agent_protocol.Invocation.resolve
              dispatched
              ~session_id:input.session_id
              ~generation:input.session_generation
              (Complete `Null)
            |> protocol_ok
          in
          saved_resolution := Some resolved;
          commit ~resolved ~snapshot:(handoff_snapshot 1) |> protocol_ok;
          assert (Result.is_error (commit ~resolved ~snapshot:(handoff_snapshot 2)));
          Ok ())
        |> protocol_ok;
        let commit = Option.value_exn !saved_commit in
        assert (
          Result.is_error
            (commit
               ~resolved:(Option.value_exn !saved_resolution)
               ~snapshot:(handoff_snapshot 3)));
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot = state.moderator
          }))
    (fun _env actor _writer _backend ->
       Eio.Promise.await done_;
       ignore (await_idle actor))
;;

let%test_unit
    "cancelled worker releases its borrow and persists cancellation without state"
  =
  let entered, entered_u = Eio.Promise.create () in
  let never, _ = Eio.Promise.create () in
  let stale_commit = ref None in
  let stale_resolution = ref None in
  with_handoff_actor
    ~make_worker:(fun _env _actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        caps.with_moderator_invocation
          ~invocation:(invocation_fixture ())
          (fun ~dispatched ~commit ->
             stale_commit := Some commit;
             stale_resolution
             := Some
                  (Agent_protocol.Invocation.resolve
                     dispatched
                     ~session_id:input.session_id
                     ~generation:input.session_generation
                     (Complete `Null)
                   |> protocol_ok);
             Eio.Promise.resolve entered_u ();
             Eio.Promise.await never)
        |> protocol_ok;
        failwith "cancelled handler returned"))
    (fun _env actor writer backend ->
       Eio.Promise.await entered;
       let before = Agent_session.Session_actor.state actor |> protocol_ok in
       assert (
         match (List.hd_exn before.invocations).status with
         | Dispatching -> true
         | _ -> false);
       Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Cancel
       |> protocol_ok
       |> ignore;
       let rec await_stopped () =
         let state = Agent_session.Session_actor.state actor |> protocol_ok in
         if Option.is_none state.active_operation
         then state
         else (
           Eio.Fiber.yield ();
           await_stopped ())
       in
       let after = await_stopped () in
       assert (Option.is_none after.moderator);
       assert (
         match (List.hd_exn after.invocations).status with
         | Resolved (Cancelled _) -> true
         | _ -> false);
       let commit = Option.value_exn !stale_commit in
       assert (
         Result.is_error
           (commit
              ~resolved:(Option.value_exn !stale_resolution)
              ~snapshot:(handoff_snapshot 1)));
       assert (Option.is_none (Agent_session.Memory_backend.state backend).moderator))
;;

let%test_unit "cancellation after the atomic commit preserves its recorded outcome" =
  let committed, committed_u = Eio.Promise.create () in
  let never, _ = Eio.Promise.create () in
  let snapshot = handoff_snapshot 7 in
  with_handoff_actor
    ~make_worker:(fun _env _actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        caps.with_moderator_invocation
          ~invocation:(invocation_fixture ())
          (fun ~dispatched ~commit ->
             let resolved =
               Agent_protocol.Invocation.resolve
                 dispatched
                 ~session_id:input.session_id
                 ~generation:input.session_generation
                 (Complete (`String "already saved"))
               |> protocol_ok
             in
             commit ~resolved ~snapshot |> protocol_ok;
             Eio.Promise.resolve committed_u ();
             Eio.Promise.await never)
        |> protocol_ok;
        failwith "cancelled worker returned"))
    (fun _env actor writer backend ->
       Eio.Promise.await committed;
       Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Cancel
       |> protocol_ok
       |> ignore;
       let rec finished () =
         let state = Agent_session.Session_actor.state actor |> protocol_ok in
         if Option.is_some state.active_operation
         then (
           Eio.Fiber.yield ();
           finished ())
         else state
       in
       let state = finished () in
       assert (
         match (List.hd_exn state.invocations).status with
         | Resolved (Complete (`String "already saved")) -> true
         | _ -> false);
       assert (
         Poly.equal
           state.moderator
           (Some (Agent_session.Runtime_builder.encode_moderator_snapshot snapshot)));
       assert (
         Poly.equal state.moderator (Agent_session.Memory_backend.state backend).moderator))
;;

let%test_unit "independent worker calls queue before actor admission" =
  let done_, done_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let first_held, first_held_u = Eio.Promise.create () in
        let release, release_u = Eio.Promise.create () in
        let attempted, attempted_u = Eio.Promise.create () in
        let second_entered = ref false in
        let fresh () =
          Agent_protocol.Invocation.create
            { (invocation_fixture ()).context with
              id = Agent_protocol.Id.Invocation.create ()
            }
          |> protocol_ok
        in
        let run ~invocation count wait =
          caps.with_moderator_invocation ~invocation (fun ~dispatched ~commit ->
            wait ();
            let resolved =
              Agent_protocol.Invocation.resolve
                dispatched
                ~session_id:input.session_id
                ~generation:input.session_generation
                (Complete `Null)
              |> protocol_ok
            in
            commit ~resolved ~snapshot:(handoff_snapshot count))
          |> protocol_ok
        in
        Eio.Switch.run (fun sw ->
          Eio.Fiber.fork ~sw (fun () ->
            run ~invocation:(fresh ()) 1 (fun () ->
              Eio.Promise.resolve first_held_u ();
              Eio.Promise.await release));
          Eio.Promise.await first_held;
          let second = fresh () in
          Eio.Fiber.fork ~sw (fun () ->
            Eio.Promise.resolve attempted_u ();
            run ~invocation:second 2 (fun () -> second_entered := true));
          Eio.Promise.await attempted;
          Eio.Fiber.yield ();
          let while_queued = Agent_session.Session_actor.state actor |> protocol_ok in
          assert (List.length while_queued.invocations = 1);
          assert (not !second_entered);
          Eio.Promise.resolve release_u ());
        assert !second_entered;
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (List.length state.invocations = 2);
        assert (
          List.for_all state.invocations ~f:(fun inv ->
            match inv.status with
            | Resolved (Complete _) -> true
            | _ -> false));
        assert (
          Poly.equal
            state.moderator
            (Some
               (Agent_session.Runtime_builder.encode_moderator_snapshot
                  (handoff_snapshot 2))));
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot = state.moderator
          }))
    (fun _env actor _writer _backend ->
       Eio.Promise.await done_;
       ignore (await_idle actor))
;;

let%test_unit "failed admission and handler errors cannot strand a moderator borrow" =
  let done_, done_u = Eio.Promise.create () in
  let reject_once = ref true in
  let reject next =
    if
      !reject_once
      && not (List.is_empty next.Agent_session.Session_transition.state.invocations)
    then (
      reject_once := false;
      true)
    else false
  in
  with_handoff_actor
    ~reject
    ~make_worker:(fun _env actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        let actor = Eio.Promise.await actor_ready in
        let fresh () =
          Agent_protocol.Invocation.create
            { (invocation_fixture ()).context with
              id = Agent_protocol.Id.Invocation.create ()
            }
          |> protocol_ok
        in
        let admitted = fresh () in
        let ran = ref false in
        assert (
          Result.is_error
            (caps.with_moderator_invocation
               ~invocation:admitted
               (fun ~dispatched:_ ~commit:_ ->
                  ran := true;
                  Ok ())));
        assert (not !ran);
        assert (
          List.is_empty
            (Agent_session.Session_actor.state actor |> protocol_ok).invocations);
        (* An admission with no effects/commit may be retried with its original ID. *)
        assert (
          Result.is_error
            (caps.with_moderator_invocation
               ~invocation:admitted
               (fun ~dispatched:_ ~commit:_ -> Ok ())));
        List.iter
          [ ""; String.make 20_000 'x' ]
          ~f:(fun message ->
            assert (
              Result.is_error
                (caps.with_moderator_invocation
                   ~invocation:(fresh ())
                   (fun ~dispatched:_ ~commit:_ -> Error (handoff_error message)))));
        (match
           caps.with_moderator_invocation
             ~invocation:(fresh ())
             (fun ~dispatched:_ ~commit:_ -> raise Exit)
         with
         | _ -> assert false
         | exception Exit -> ());
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        assert (List.length state.invocations = 4);
        assert (
          List.for_all state.invocations ~f:(fun invocation ->
            match invocation.status with
            | Resolved (Fail _) -> true
            | _ -> false));
        caps.with_moderator_invocation ~invocation:(fresh ()) (fun ~dispatched ~commit ->
          let resolved =
            Agent_protocol.Invocation.resolve
              dispatched
              ~session_id:input.session_id
              ~generation:input.session_generation
              (Complete `Null)
            |> protocol_ok
          in
          commit ~resolved ~snapshot:(handoff_snapshot 1))
        |> protocol_ok;
        let state = Agent_session.Session_actor.state actor |> protocol_ok in
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot = state.moderator
          }))
    (fun _env actor _writer _backend ->
       Eio.Promise.await done_;
       ignore (await_idle actor))
;;

let%test_unit "graceful stop lets an admitted moderator invocation commit" =
  let entered, entered_u = Eio.Promise.create () in
  let release, release_u = Eio.Promise.create () in
  let done_, done_u = Eio.Promise.create () in
  with_handoff_actor
    ~make_worker:(fun _env _actor_ready ->
      Agent_session.Operation_worker.create ~run:(fun ~sw:_ ~input caps ->
        caps.with_moderator_invocation
          ~invocation:(invocation_fixture ())
          (fun ~dispatched ~commit ->
             Eio.Promise.resolve entered_u ();
             Eio.Promise.await release;
             let resolved =
               Agent_protocol.Invocation.resolve
                 dispatched
                 ~session_id:input.session_id
                 ~generation:input.session_generation
                 (Complete `Null)
               |> protocol_ok
             in
             commit ~resolved ~snapshot:(handoff_snapshot 1))
        |> protocol_ok;
        Eio.Promise.resolve done_u ();
        Completed
          { final_history = input.history
          ; runtime_requests = []
          ; moderator_snapshot =
              Some
                (Agent_session.Runtime_builder.encode_moderator_snapshot
                   (handoff_snapshot 1))
          }))
    (fun _env actor writer _backend ->
       Eio.Promise.await entered;
       Agent_session.Session_actor.stop actor ~attachment_id:writer.id ~mode:Graceful
       |> protocol_ok
       |> ignore;
       Eio.Promise.resolve release_u ();
       Eio.Promise.await done_;
       let rec finished () =
         let state = Agent_session.Session_actor.state actor |> protocol_ok in
         if Option.is_some state.active_operation
         then (
           Eio.Fiber.yield ();
           finished ())
         else state
       in
       let state = finished () in
       assert (
         match state.lifecycle.observed with
         | Stopped -> true
         | _ -> false);
       assert (
         match (List.hd_exn state.invocations).status with
         | Resolved (Complete _) -> true
         | _ -> false);
       assert (Option.is_some state.moderator))
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

let%expect_test "compaction atomically replaces history and advances its generation" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun switch ->
      let attachment_id =
        Agent_protocol.Id.Attachment.of_string "att_actor_compaction" |> protocol_ok
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
          ~operation_worker:None
          ~services:
            { now = (fun () -> Agent_protocol.Timestamp.now ())
            ; create_attachment_id = (fun () -> attachment_id)
            ; create_reclaim_token = (fun () -> "test-reclaim-token")
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
      let initial_entry =
        Agent_session.History_codec.user_text ~id:history_id "remember this"
        |> Agent_session.History_codec.to_protocol
      in
      Agent_session.Session_actor.append_history
        actor
        ~attachment_id:attachment.id
        [ initial_entry ]
      |> protocol_ok
      |> ignore;
      let before = Agent_session.Session_actor.state actor |> protocol_ok in
      let stale_revision_rejected =
        Agent_session.Session_actor.compact
          actor
          ~attachment_id:attachment.id
          ~expected_revision:(Some Int64.(before.counters.revision - 1L))
        |> Result.is_error
      in
      Agent_session.Session_actor.compact
        actor
        ~attachment_id:attachment.id
        ~expected_revision:(Some before.counters.revision)
      |> protocol_ok
      |> ignore;
      let state = await_idle actor in
      let reminder = List.hd_exn state.conversation.canonical_history in
      Agent_session.Session_actor.shutdown actor;
      print_s
        [%sexp
          { stale_revision_rejected : bool
          ; history_count = (List.length state.conversation.canonical_history : int)
          ; compaction_generation = (state.conversation.compaction_generation : int)
          ; high_water = (state.conversation.next_history_sequence : int64)
          ; reminder_namespace = (History_entry.Id.namespace reminder.id : string)
          ; active_operation = (Option.is_some state.active_operation : bool)
          }]));
  [%expect
    {|
    ((stale_revision_rejected true) (history_count 1) (compaction_generation 1)
     (high_water 1) (reminder_namespace ses_agent_session_test)
     (active_operation false))
    |}]
;;

let compaction_cancel_state workspace_instance =
  let initial =
    actor_state ~workspace_instance ~liveness:Process_bound ~start_immediately:false
  in
  let entry =
    Agent_session.History_codec.user_text ~id:history_id "preserve cancelled history"
    |> Agent_session.History_codec.to_protocol
  in
  { initial with
    conversation = { initial.conversation with canonical_history = [ entry ] }
  }
;;

let compaction_cancel_actor ~sw env workspace_instance state_committed =
  let services : Agent_session.Session_actor.services =
    { now = (fun () -> timestamp)
    ; create_attachment_id =
        (fun () ->
          Agent_protocol.Id.Attachment.of_string "att_compact_cancel" |> protocol_ok)
    ; create_reclaim_token = (fun () -> "compaction-cancel-reclaim")
    ; state_committed
    }
  in
  Agent_session.Session_actor.create
    ~sw
    ~clock:(Eio.Stdenv.clock env)
    ~mailbox_capacity:32
    ~compaction_env:None
    ~initial_state:(compaction_cancel_state workspace_instance)
    ~persistence:{ commit = (fun ~command_audit:_ ~previous:_ _ -> Ok ()) }
    ~operation_worker:None
    ~services
;;

let compaction_cancel_events recorded started _state events =
  recorded := !recorded @ events;
  List.iter events ~f:(fun (event : Agent_protocol.Event.Durable.t) ->
    match
      Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
      |> protocol_ok
    with
    | Operation_started operation ->
      Eio.Promise.resolve started operation.id;
      Eio.Fiber.yield ()
    | _ -> ())
;;

let compaction_cancel_on_start ~sw actor attachment_id started =
  Eio.Fiber.fork ~sw (fun () ->
    let operation_id = Eio.Promise.await started in
    ignore
      (Agent_session.Session_actor.cancel_operation actor ~attachment_id ~operation_id
       |> protocol_ok
       : Agent_protocol.Session.t))
;;

let compaction_cancel_run ~sw env actor started =
  let attachment, _subscriber =
    Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
    |> protocol_ok
  in
  ignore
    (Agent_session.Session_actor.start actor ~attachment_id:attachment.id |> protocol_ok
     : Agent_protocol.Session.t);
  compaction_cancel_on_start ~sw actor attachment.id started;
  ignore
    (Agent_session.Session_actor.compact
       actor
       ~attachment_id:attachment.id
       ~expected_revision:None
     |> protocol_ok
     : Agent_protocol.Session.t);
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () -> await_idle actor)
;;

let compaction_cancel_terminals events =
  List.filter_map events ~f:(fun (event : Agent_protocol.Event.Durable.t) ->
    match
      Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
      |> protocol_ok
    with
    | Operation_completed operation
    | Operation_cancelled operation
    | Operation_failed operation -> Some (event.kind, operation.state)
    | _ -> None)
;;

let compaction_cancel_report before after events =
  let history state =
    [%sexp_of: Agent_protocol.History.entry list]
      state.Agent_session.Session_state.conversation.canonical_history
  in
  print_s
    [%sexp
      { history_preserved = (Sexp.equal (history before) (history after) : bool)
      ; compaction_generation = (after.conversation.compaction_generation : int)
      ; terminals =
          (compaction_cancel_terminals events
           : (Agent_protocol.Event.Durable.kind * Agent_protocol.Operation.state) list)
      ; history_replaced =
          (List.exists events ~f:(fun event ->
             Agent_protocol.Event.Durable.equal_kind
               event.Agent_protocol.Event.Durable.kind
               History_replaced)
           : bool)
      ; active_operation = (Option.is_some after.active_operation : bool)
      }]
;;

let%expect_test "compaction cancellation before worker readiness preserves history" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let recorded = ref [] in
      let started, notify_started = Eio.Promise.create () in
      let actor =
        compaction_cancel_actor
          ~sw
          env
          workspace_instance
          (compaction_cancel_events recorded notify_started)
      in
      let before = Agent_session.Session_actor.state actor |> protocol_ok in
      let after = compaction_cancel_run ~sw env actor started in
      Agent_session.Session_actor.shutdown actor;
      compaction_cancel_report before after !recorded));
  [%expect
    {|
    ((history_preserved true) (compaction_generation 0)
     (terminals ((Operation_cancelled Cancelled))) (history_replaced false)
     (active_operation false))
    |}]
;;

let%expect_test "actor consumes late compaction success without changing cancelled state" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let recorded = ref [] in
      let started, notify_started = Eio.Promise.create () in
      let actor =
        compaction_cancel_actor
          ~sw
          env
          workspace_instance
          (compaction_cancel_events recorded notify_started)
      in
      let cancelled = compaction_cancel_run ~sw env actor started in
      let events_before = !recorded in
      let history =
        [ Agent_session.History_codec.user_text
            ~id:history_id
            "late summary must never replace history"
        ]
      in
      Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
        Agent_session.Session_actor.For_testing.deliver_compaction_result
          actor
          ~operation_id:(Eio.Promise.await started)
          ~history
        |> protocol_ok);
      let after = Agent_session.Session_actor.state actor |> protocol_ok in
      print_s
        [%sexp
          { exact_state_unchanged =
              (Sexp.equal
                 ([%sexp_of: Agent_session.Session_state.t] cancelled)
                 ([%sexp_of: Agent_session.Session_state.t] after)
               : bool)
          ; exact_events_unchanged =
              (Sexp.equal
                 ([%sexp_of: Agent_protocol.Event.Durable.t list] events_before)
                 ([%sexp_of: Agent_protocol.Event.Durable.t list] !recorded)
               : bool)
          }];
      Agent_session.Session_actor.shutdown actor));
  [%expect {| ((exact_state_unchanged true) (exact_events_unchanged true)) |}]
;;

let permission_request ~id =
  Agent_protocol.Permission.
    { id
    ; session_id
    ; generation = 0
    ; operation_id
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

let durable_event session_id sequence =
  Agent_protocol.Event.Durable.of_payload
    ~session_id
    ~sequence
    ~revision:sequence
    ~timestamp
    (Agent_protocol.Event.Durable.Payload.Moderator_notification
       (`Object [ "sequence", `Number (Int64.to_string sequence) ]))
;;

let replay_shape = function
  | Agent_session.Durable_event_log.Snapshot_required -> "snapshot"
  | Available events ->
    List.map events ~f:(fun event -> Int64.to_string event.sequence)
    |> String.concat ~sep:","
;;

let%expect_test "durable event replay detects retained and expired cursors" =
  Eio_main.run (fun _env ->
    let session_id =
      Agent_protocol.Id.Session.of_string "ses_event_replay" |> protocol_ok
    in
    let log =
      Agent_session.Durable_event_log.create
        ~capacity:2
        [ durable_event session_id 1L; durable_event session_id 2L ]
      |> protocol_ok
    in
    Agent_session.Durable_event_log.append log [ durable_event session_id 3L ];
    let expired =
      Agent_session.Durable_event_log.replay log ~after_sequence:0L ~through_sequence:3L
    in
    let retained =
      Agent_session.Durable_event_log.replay log ~after_sequence:1L ~through_sequence:3L
    in
    print_s
      [%sexp
        { expired = (replay_shape expired : string)
        ; retained = (replay_shape retained : string)
        ; oldest = (Agent_session.Durable_event_log.oldest_sequence log : int64 option)
        ; latest = (Agent_session.Durable_event_log.latest_sequence log : int64 option)
        }]);
  [%expect
    {|
    ((expired snapshot) (retained 2,3) (oldest (2)) (latest (3)))
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

let audit_actor ?(with_invocation = false) ~sw ~env ~workspace_instance ~reject_archive ()
  =
  let initial =
    actor_state ~workspace_instance ~liveness:Process_bound ~start_immediately:false
  in
  let entry =
    Agent_session.History_codec.user_text ~id:history_id "original history"
    |> Agent_session.History_codec.to_protocol
  in
  let initial =
    { initial with
      conversation = { initial.conversation with canonical_history = [ entry ] }
    }
  in
  let initial =
    if not with_invocation
    then initial
    else (
      let call =
        History_entry.create_with_id
          ~id:history_id
          (Openai.Responses.Item.Function_call
             { name = "read_file"
             ; arguments = "null"
             ; call_id = "audit-call"
             ; id = None
             ; status = None
             ; _type = "function_call"
             })
        |> Agent_session.History_codec.to_protocol
      in
      let inv =
        Agent_protocol.Invocation.create
          { (invocation_fixture ()).context with
            origin = Model
          ; provider_call_id = Some "audit-call"
          ; call_entry_id = Some history_id
          }
        |> protocol_ok
        |> Agent_protocol.Invocation.dispatch
        |> protocol_ok
      in
      let inv =
        Agent_protocol.Invocation.resolve inv ~session_id ~generation:0 (Complete `Null)
        |> protocol_ok
      in
      { initial with
        invocations = [ inv ]
      ; conversation = { initial.conversation with canonical_history = [ call ] }
      })
  in
  let backend =
    Agent_session.Memory_backend.create ~event_capacity:64 ~initial_state:initial
  in
  let persistence = Agent_session.Memory_backend.persistence backend in
  let persistence =
    Agent_session.Session_actor.
      { commit =
          (fun ~command_audit ~previous transition ->
            if
              reject_archive
              && List.length
                   transition.Agent_session.Session_transition.state.conversation
                     .compaction_archives
                 > List.length previous.conversation.compaction_archives
            then
              Error
                (Agent_protocol.Error.create
                   Persistence_error
                   ~message:"injected archive failure"
                   ~retryable:false
                   ())
            else persistence.commit ~command_audit ~previous transition)
      }
  in
  let actor =
    Agent_session.Session_actor.create
      ~sw
      ~clock:(Eio.Stdenv.clock env)
      ~mailbox_capacity:32
      ~compaction_env:None
      ~initial_state:initial
      ~persistence
      ~operation_worker:None
      ~services:
        { now = Agent_protocol.Timestamp.now
        ; create_attachment_id = Agent_protocol.Id.Attachment.create
        ; create_reclaim_token = (fun () -> "audit-token")
        ; state_committed = (fun _ _ -> ())
        }
  in
  actor, backend
;;

let%expect_test
    "administrative commit failure and stale candidates preserve complete state"
  =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let actor, backend =
        audit_actor
          ~with_invocation:true
          ~sw
          ~env
          ~workspace_instance
          ~reject_archive:true
          ()
      in
      let writer, _ =
        Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
        |> protocol_ok
      in
      let before = Agent_session.Session_actor.state actor |> protocol_ok in
      let target =
        Agent_protocol.Id.Prompt_revision.of_string "prv_prepared_admin" |> protocol_ok
      in
      let candidate = Agent_session.Administration.rebuild before target |> protocol_ok in
      let commit revision =
        Agent_session.Session_actor.commit_administration
          actor
          ~command_audit:None
          ~attachment_id:writer.id
          ~expected_revision:revision
          ~kind:Rebuild
          candidate
      in
      assert (Result.is_error (commit Int64.(before.counters.revision - 1L)));
      assert (Result.is_error (commit before.counters.revision));
      let after = Agent_session.Session_actor.state actor |> protocol_ok in
      assert (
        Sexp.equal
          (Agent_session.Session_state.sexp_of_t before)
          (Agent_session.Session_state.sexp_of_t after));
      assert (
        List.is_empty
          (Agent_session.Memory_backend.events_after
             backend
             before.counters.event_sequence
           |> protocol_ok));
      Agent_session.Session_actor.shutdown actor));
  print_endline "stale and failed commits preserve complete state and event position";
  [%expect {| stale and failed commits preserve complete state and event position |}]
;;

let%expect_test "history deletion pairs occurrences, not reused provider call IDs" =
  let open Openai.Responses in
  let call =
    Item.Function_call
      { name = "test"
      ; arguments = "{}"
      ; call_id = "reused"
      ; _type = "function_call"
      ; id = None
      ; status = None
      }
  in
  let output =
    Item.Function_call_output
      { output = Text "done"
      ; call_id = "reused"
      ; _type = "function_call_output"
      ; id = None
      ; status = None
      }
  in
  let custom =
    Item.Custom_tool_call
      { name = "custom"
      ; input = "input"
      ; call_id = "reused"
      ; _type = "custom_tool_call"
      ; id = None
      }
  in
  let custom_output =
    Item.Custom_tool_call_output
      { output = Text "custom done"
      ; call_id = "reused"
      ; _type = "custom_tool_call_output"
      ; id = None
      }
  in
  let entries =
    List.mapi
      [ call; custom; output; custom_output; call; output ]
      ~f:(fun sequence item ->
        History_entry.create_with_id
          ~id:
            (History_entry.Id.create ~namespace:"pairs" ~sequence |> Result.ok_or_failwith)
          item)
  in
  List.iter [ 0; 2; 1; 3; 4; 5 ] ~f:(fun index ->
    let retained =
      History_entry.remove_with_tool_pair
        entries
        ~entry_id:(History_entry.id (List.nth_exn entries index))
      |> Result.ok_or_failwith
    in
    print_s
      [%sexp
        (List.map retained ~f:(fun entry ->
           History_entry.Id.sequence (History_entry.id entry))
         : int list)]);
  [%expect
    {|
    (1 3 4 5)
    (1 3 4 5)
    (0 2 4 5)
    (0 2 4 5)
    (0 1 2 3)
    (0 1 2 3)
    |}]
;;

let rec next_replacement subscriber =
  match Agent_session.Subscriber.take subscriber with
  | Some (Ok (Durable event)) ->
    (match
       Agent_protocol.Event.Durable.Payload.of_json ~kind:event.kind event.payload
       |> protocol_ok
     with
     | History_replaced history -> history.entries
     | _ -> next_replacement subscriber)
  | Some (Ok _) -> next_replacement subscriber
  | _ -> failwith "subscriber ended before history replacement"
;;

let%expect_test "history deletion is authoritative, revision checked and broadcast" =
  with_actor_workspace (fun env workspace_instance ->
    Eio.Switch.run (fun sw ->
      let actor, backend =
        audit_actor ~sw ~env ~workspace_instance ~reject_archive:false ()
      in
      let writer, first =
        Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:true
        |> protocol_ok
      in
      let reader, second =
        Agent_session.Session_actor.attach actor ~mode:Read_only ~subscribe:true
        |> protocol_ok
      in
      let before = Agent_session.Session_actor.state actor |> protocol_ok in
      let remove attachment_id expected_revision =
        Agent_session.Session_actor.delete_history
          actor
          ~attachment_id
          ~expected_revision
          history_id
      in
      let denied = Result.is_error (remove reader.id before.counters.revision) in
      let stale =
        Result.is_error (remove writer.id Int64.(before.counters.revision - 1L))
      in
      ignore
        (remove writer.id before.counters.revision |> protocol_ok
         : Agent_protocol.Session.t);
      let events =
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 2. (fun () ->
          List.map [ first; second ] ~f:(fun subscriber ->
            List.length (next_replacement (Option.value_exn subscriber))))
      in
      printf
        "denied=%b stale=%b durable=%d clients=%s\n"
        denied
        stale
        (List.length
           (Agent_session.Memory_backend.state backend).conversation.canonical_history)
        (Sexp.to_string ([%sexp_of: int list] events));
      Agent_session.Session_actor.shutdown actor));
  [%expect {| denied=true stale=true durable=0 clients=(0 0) |}]
;;

let%expect_test
    "compaction archive survives memory event retention and failures terminate safely"
  =
  with_actor_workspace (fun env workspace_instance ->
    List.iter [ false; true ] ~f:(fun reject_archive ->
      Eio.Switch.run (fun sw ->
        let actor, backend =
          audit_actor ~sw ~env ~workspace_instance ~reject_archive ()
        in
        let writer, _ =
          Agent_session.Session_actor.attach actor ~mode:Read_write ~subscribe:false
          |> protocol_ok
        in
        ignore
          (Agent_session.Session_actor.start actor ~attachment_id:writer.id |> protocol_ok
           : Agent_protocol.Session.t);
        ignore
          (Agent_session.Session_actor.compact
             actor
             ~attachment_id:writer.id
             ~expected_revision:None
           |> protocol_ok
           : Agent_protocol.Session.t);
        let state =
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 3. (fun () -> await_idle actor)
        in
        for _ = 1 to 70 do
          let attachment, _ =
            Agent_session.Session_actor.attach actor ~mode:Read_only ~subscribe:false
            |> protocol_ok
          in
          Agent_session.Session_actor.detach actor attachment.id |> protocol_ok
        done;
        let original =
          match state.conversation.compaction_archives with
          | [] ->
            List.exists state.conversation.canonical_history ~f:(fun entry ->
              Agent_protocol.History.Id.compare entry.id history_id = 0)
          | reference :: _ ->
            Option.is_some
              (Agent_session.Memory_backend.archived_state
                 backend
                 ~revision:reference.revision)
        in
        printf
          "failure=%b archives=%d original_preserved=%b idle=%b\n"
          reject_archive
          (List.length state.conversation.compaction_archives)
          original
          (Option.is_none state.active_operation);
        Agent_session.Session_actor.shutdown actor)));
  [%expect
    {|
    failure=false archives=1 original_preserved=true idle=true
    failure=true archives=0 original_preserved=true idle=true
    |}]
;;

let%expect_test
    "extension status journal replay preserves projection and rejects future codecs"
  =
  with_actor_workspace (fun _env workspace_instance ->
    let _, staged, resolved, _, _ = extension_fixture workspace_instance in
    let published = Agent_protocol.Invocation.publish resolved |> protocol_ok in
    let transition =
      Agent_session.Session_transition.apply
        ~now:timestamp
        staged
        ~delta:(Invocation_changed published)
        ~payloads:[]
      |> protocol_ok
    in
    let transaction events =
      Agent_store.Transaction.create
        ~session_id
        ~generation:0
        ~transaction_sequence:transition.state.counters.transaction_sequence
        ~previous_transaction_hash:None
        ~session_revision:transition.state.counters.revision
        ~first_event_sequence:
          (Some (List.hd_exn events).Agent_protocol.Event.Durable.sequence)
        ~last_event_sequence:
          (Some (List.last_exn events).Agent_protocol.Event.Durable.sequence)
        ~accepted_at_ns:
          (Agent_protocol.Timestamp.to_time_ns timestamp
           |> Time_ns.to_int_ns_since_epoch
           |> Int64.of_int)
        ~command_audit:None
        ~delta:
          (Sexp.to_string_mach (Agent_session.Session_delta.sexp_of_t transition.delta))
        ~durable_events:
          (List.map events ~f:(fun event ->
             Sexp.to_string_mach (Agent_protocol.Event.Durable.sexp_of_t event)))
      |> store_ok
      |> Agent_store.Transaction.encode
      |> Agent_store.Transaction.decode
      |> store_ok
    in
    let journal = transaction transition.events in
    let replayed =
      Agent_session.Session_persistence.apply_transaction staged journal |> store_ok
    in
    let statuses = Agent_session.Session_state.extension_status replayed in
    let events = Agent_session.Session_persistence.durable_events journal |> store_ok in
    let updates =
      List.filter_map events ~f:(fun event ->
        Agent_protocol.Event.Durable.extension_status event |> protocol_ok)
    in
    assert (Poly.equal updates [ statuses ]);
    let corrupt =
      List.map events ~f:(fun event ->
        match event.kind, event.payload with
        | Session_updated, `Object fields ->
          { event with
            payload =
              `Object
                (("extension_status", `Array [ `Object [ "version", `Number "99" ] ])
                 :: List.filter fields ~f:(fun (name, _) ->
                   not (String.equal name "extension_status")))
          }
        | _ -> event)
    in
    assert (
      Result.is_error
        (Agent_session.Session_persistence.durable_events (transaction corrupt))));
  print_endline "journal state/status agree; future status codec rejected during replay";
  [%expect {| journal state/status agree; future status codec rejected during replay |}]
;;

let%expect_test
    "extension schemas and scripts restore from actual pinned artifact closure"
  =
  with_temp_directory (fun env temporary ->
    let directory = Eio.Path.(Eio.Stdenv.fs env / temporary / "prompt") in
    Eio.Path.mkdirs ~exists_ok:true ~perm:0o700 Eio.Path.(directory / "parts");
    let save path text =
      Eio.Path.save ~create:(`Exclusive 0o600) Eio.Path.(directory / path) text
    in
    save "root.chatmd" {|<import src="parts/definition.chatmd" namespace="lab"/>|};
    save
      "parts/definition.chatmd"
      {|<script id="worker" language="chatml" kind="tool" src="worker.chatml"/><tool name="work" type="chatml" script="worker" entrypoint="run" input_schema="schema.json" output_schema="schema.json"/>|};
    save "parts/worker.chatml" "let run = fun ctx input -> Task.pure(`Complete(input))";
    save "parts/schema.json" {|{"type":"object"}|};
    let definition =
      Agent_session.Prompt_definition.create
        ~id:prompt_id
        ~config_name:"extensions"
        ~root_file:(Filename.concat temporary "prompt/root.chatmd")
        ~allowed_workspaces:[ workspace_id ]
        ~permission_profile:"interactive"
        ~runtime_policy:None
        ~enabled:true
        ~description:None
      |> store_ok
    in
    let artifact_store =
      Agent_store.Prompt_artifact_store.create
        ~env
        ~root:(Filename.concat temporary "artifacts")
      |> store_ok
    in
    let get = function
      | Ok value -> value
      | Error errors ->
        raise_s [%sexp (errors : Agent_session.Prompt_revision_builder.Diagnostic.t list)]
    in
    let revision =
      Agent_session.Prompt_revision_builder.build
        ~env
        ~artifact_store
        ~transaction_id
        ~created_at:timestamp
        definition
      |> get
    in
    let revision_id = Agent_session.Prompt_revision.id revision in
    Eio.Path.rmtree directory;
    let restored =
      Agent_session.Prompt_revision_builder.restore ~artifact_store definition revision_id
      |> get
    in
    let elements = Agent_session.Prompt_revision.elements restored in
    let tool =
      List.find_map_exn elements ~f:(function
        | Prompt.Chat_markdown.Tool (Extension tool) -> Some tool
        | _ -> None)
    in
    assert (String.equal tool.input_schema.source_text {|{"type":"object"}|});
    assert (List.is_empty tool.uses);
    let script =
      List.find_map_exn elements ~f:(function
        | Prompt.Chat_markdown.Extension_script script -> Some script
        | _ -> None)
    in
    assert (String.equal script.id "lab:worker");
    assert (
      String.equal
        (Chatmd_shell_spec.Extension_spec.script_text script)
        "let run = fun ctx input -> Task.pure(`Complete(input))");
    let materialized =
      Eio.Path.native_exn (Agent_session.Prompt_revision.materialized_tree restored)
    in
    assert (
      String.equal
        tool.input_schema.source_ref.source_dir
        (Filename.concat materialized "parts"));
    let artifact = Agent_session.Prompt_revision.artifact restored in
    assert (artifact.parser_schema_version = 4);
    assert (List.length artifact.sources = 3);
    let restore_fixture ~suffix ~version ~root ~sources =
      let id =
        Agent_protocol.Id.Prompt_revision.of_string ("prv_compat_" ^ suffix)
        |> protocol_ok
      in
      let fixture =
        Agent_store.Prompt_artifact_store.Artifact.create
          ~revision_id:id
          ~root_relative_path:"root.chatmd"
          ~root_chatmd:root
          ~sources
          ~parser_schema_version:version
          ~runtime_schema_version:1
          ~created_at:timestamp
          ()
        |> store_ok
      in
      Agent_store.Prompt_artifact_store.install artifact_store ~transaction_id fixture
      |> store_ok;
      Agent_session.Prompt_revision_builder.restore ~artifact_store definition id
    in
    assert (
      Result.is_ok
        (restore_fixture
           ~suffix:"v2_extension"
           ~version:2
           ~root:artifact.root_chatmd
           ~sources:artifact.sources));
    assert (
      Result.is_error
        (restore_fixture
           ~suffix:"v1_extension"
           ~version:1
           ~root:artifact.root_chatmd
           ~sources:artifact.sources));
    assert (
      Result.is_ok
        (restore_fixture
           ~suffix:"v1_inline_markup"
           ~version:1
           ~root:{|<user>Example: <authoring_context policy="manual"/></user>|}
           ~sources:[]));
    let inherited = {|<tool type="inherited" name="read_file"/>|} in
    let authored_help =
      {|<authoring_help tool="custom" package="one-off" tasks="one_off_script" topics="chatml/basics"/>|}
    in
    List.iter [ 1; 2; 3 ] ~f:(fun version ->
      let result =
        restore_fixture
          ~suffix:(sprintf "help_%d" version)
          ~version
          ~root:authored_help
          ~sources:[]
      in
      match result with
      | Ok _ -> failwith "old artifact accepted new help declarations"
      | Error errors ->
        assert (
          List.exists errors ~f:(fun diagnostic ->
            String.is_substring
              diagnostic.Agent_session.Prompt_revision_builder.Diagnostic.message
              ~substring:
                "authoring help declarations require prompt parser schema version 4")));
    assert (
      Result.is_ok
        (restore_fixture ~suffix:"help_v4" ~version:4 ~root:authored_help ~sources:[]));
    List.iter [ 1; 2 ] ~f:(fun version ->
      let assert_floor result =
        match result with
        | Ok _ -> failwith "old parser accepted inherited reference"
        | Error diagnostics ->
          assert (
            List.exists diagnostics ~f:(fun diagnostic ->
              String.is_substring
                diagnostic.Agent_session.Prompt_revision_builder.Diagnostic.message
                ~substring:
                  "inherited tool references require prompt parser schema version 3"))
      in
      restore_fixture
        ~suffix:(sprintf "root_%d" version)
        ~version
        ~root:inherited
        ~sources:[]
      |> assert_floor;
      let source path contents =
        Agent_store.Prompt_artifact_store.Source.create ~relative_path:path ~contents
        |> store_ok
      in
      restore_fixture
        ~suffix:(sprintf "nested_%d" version)
        ~version
        ~root:{|<tool name="child" agent="child.chatmd" local/>|}
        ~sources:
          [ source "child.chatmd" {|<import src="parts/refs.chatmd"/>|}
          ; source "parts/refs.chatmd" inherited
          ]
      |> assert_floor);
    assert (
      Result.is_ok
        (restore_fixture ~suffix:"v3_inherited" ~version:3 ~root:inherited ~sources:[]));
    let future_id =
      Agent_protocol.Id.Prompt_revision.of_string "prv_future_extension" |> protocol_ok
    in
    let future =
      Agent_store.Prompt_artifact_store.Artifact.create
        ~revision_id:future_id
        ~root_relative_path:artifact.root_relative_path
        ~root_chatmd:artifact.root_chatmd
        ~sources:artifact.sources
        ~parser_schema_version:99
        ~runtime_schema_version:1
        ~created_at:timestamp
        ()
      |> store_ok
    in
    Agent_store.Prompt_artifact_store.install artifact_store ~transaction_id future
    |> store_ok;
    assert (
      Result.is_error
        (Agent_session.Prompt_revision_builder.restore
           ~artifact_store
           definition
           future_id));
    let legacy_id =
      Agent_protocol.Id.Prompt_revision.of_string "prv_legacy_extension" |> protocol_ok
    in
    let legacy =
      Agent_store.Prompt_artifact_store.Artifact.create
        ~revision_id:legacy_id
        ~root_relative_path:"root.chatmd"
        ~root_chatmd:"<developer>legacy artifact</developer>"
        ~sources:[]
        ~parser_schema_version:1
        ~runtime_schema_version:1
        ~created_at:timestamp
        ()
      |> store_ok
    in
    Agent_store.Prompt_artifact_store.install artifact_store ~transaction_id legacy
    |> store_ok;
    assert (
      Result.is_ok
        (Agent_session.Prompt_revision_builder.restore
           ~artifact_store
           definition
           legacy_id)));
  print_endline
    "script and schema closure survives deleted live sources without runtime execution";
  [%expect
    {| script and schema closure survives deleted live sources without runtime execution |}]
;;
