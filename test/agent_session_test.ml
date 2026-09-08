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
        { (invocation_fixture ()).context with
          origin = Model
        ; provider_call_id = Some "call"
        ; call_entry_id = Some (id 0)
        }
      |> protocol_ok
    in
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
         assert (
           List.length state.conversation.canonical_history
           = if publish_first then 3 else 2);
         let invocation = List.hd_exn state.invocations in
         assert (Bool.equal (Option.is_some invocation.output_entry_id) publish_first);
         assert (
           match invocation.status with
           | Published (Complete (`String "done")) -> publish_first
           | Resolved (Complete (`String "done")) -> not publish_first
           | _ -> false);
         assert (
           Poly.equal
             state.invocations
             (Agent_session.Memory_backend.state backend).invocations)))
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
      ?(events = "| _ -> Task.pure(state)")
      ?(schema = "true")
      ?(finish = "Task.pure(state)")
      env
  =
  let module EC = Chat_response.Extension_compiler in
  let module M = Chat_response.Moderator_manager in
  let module C = Chat_response.Tool_capability in
  let dir = Eio.Stdenv.cwd env in
  let source =
    {|<script id="handoff" language="chatml" kind="moderator" api="extensibility-v1">
    let initial_state = [0]
    let on_event = fun ctx state event -> match event with
    | `Tool_invoked(p) ->
      let ignored = state[0] <- state[0] + 1 in
      Task.bind(Runtime.emit(`String("committed")), fun ignored ->
      Task.bind(Invocation.resolve(p.context.invocation_id, `Complete(`Null)),
        fun ignored -> |}
    ^ finish
    ^ {|))
    |}
    ^ events
    ^ {|
    </script><tool name="counter" type="moderator" moderator="handoff"
      input_schema="schema.json" output_schema="schema.json"/>|}
  in
  let loader =
    Source_loader.captured_filesystem ~root:dir ~sources:[ "schema.json", schema ]
  in
  let elements =
    Prompt.Chat_markdown.parse_chat_inputs ~dir ~source_loader:loader source
  in
  let capabilities =
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
      ~capabilities:Chat_response.Moderation.Capabilities.default
      ~allocator
      ()
    |> Result.ok_or_failwith
  in
  let tool = List.hd_exn (EC.prepared_tools definition) in
  let invocation () =
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
    ]
    ~f:(fun mode ->
      let request_count = ref 0 in
      let admitted = ref 0 in
      let redirected = Poly.equal mode `Redirect || Poly.equal mode `Redirect_bad in
      with_handoff_actor
        ~reject:(fun next ->
          Poly.equal mode `Publish_rejected
          && List.exists
               next.Agent_session.Session_transition.state.invocations
               ~f:(fun inv -> Option.is_some inv.output_entry_id))
        ~make_worker:(fun env actor_ready ->
          let events =
            if Poly.equal mode `Post_fail
            then
              "| `Post_tool_response(r) -> Task.fail(\"post hook failed\") | _ -> \
               Task.pure(state)"
            else if redirected
            then
              "| `Pre_tool_call(c) -> Task.bind(Tool.redirect(\"counter\", "
              ^ (if Poly.equal mode `Redirect_bad then "`String(\"wrong\")" else "`Null")
              ^ "), fun ignored -> Task.pure(state)) | _ -> Task.pure(state)"
            else "| _ -> Task.pure(state)"
          in
          let manager, _, definition =
            handoff_definition
              ~events
              ~schema:(if redirected then "{\"type\":\"null\"}" else "true")
              ~finish:
                (if Poly.equal mode `End_session
                 then
                   "Task.bind(Runtime.end_session(\"done\"), fun ignored -> \
                    Task.pure(state))"
                 else "Task.pure(state)")
              env
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
              Int.incr request_count;
              if !request_count = 1
              then
                Stdlib.List.to_seq
                  Openai.Responses.Response_stream.
                    [ Output_item_added
                        { item =
                            Function_call
                              { name = (if redirected then "alias" else "counter")
                              ; arguments = ""
                              ; call_id = "counter-call"
                              ; _type = "function_call"
                              ; id = Some "counter-item"
                              ; status = None
                              }
                        ; output_index = 0
                        ; type_ = "response.output_item.added"
                        }
                    ; Function_call_arguments_done
                        { arguments =
                            (if Poly.equal mode `Invalid_json
                             then "[broken"
                             else if redirected
                             then "{}"
                             else "null")
                        ; item_id = "counter-item"
                        ; output_index = 0
                        ; type_ = "response.function_call_arguments.done"
                        }
                    ]
              else (
                assert (
                  List.exists inputs ~f:(function
                    | Openai.Responses.Item.Function_call_output _ -> true
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
            in
            let worker =
              Agent_session.Turn_worker.create
                ~dispatch_tool
                { env
                ; response_dir
                ; tools = []
                ; tool_tbl = String.Table.create ()
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
                ; redact_tool_payload = (fun ~name:_ value -> value)
                }
            in
            Agent_session.Operation_worker.run worker ~sw ~input caps))
        (fun _env actor _writer backend ->
           let rec finished () =
             let state = Agent_session.Session_actor.state actor |> protocol_ok in
             if Option.is_some state.active_operation
             then (
               Eio.Fiber.yield ();
               finished ())
             else state
           in
           let state = finished () in
           assert (List.length state.invocations = 1);
           let invocation = List.hd_exn state.invocations in
           let expected_count =
             if
               Poly.equal mode `Success
               || Poly.equal mode `Post_fail
               || Poly.equal mode `Publish_rejected
               || Poly.equal mode `Redirect
               || Poly.equal mode `End_session
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
               || Poly.equal mode `Post_fail
               || Poly.equal mode `Redirect
               || Poly.equal mode `End_session
             | Published (Fail _) ->
               Poly.equal mode `Deny
               || Poly.equal mode `Disclosure
               || Poly.equal mode `Invalid_json
               || Poly.equal mode `Redirect_bad
               || Poly.equal mode `Revoked
             | Resolved (Complete `Null) -> Poly.equal mode `Publish_rejected
             | _ -> false);
           assert (
             List.length state.conversation.canonical_history
             = if Poly.equal mode `Publish_rejected then 2 else 3);
           let failed = Poly.equal mode `Post_fail || Poly.equal mode `Publish_rejected in
           assert (
             !request_count = if failed || Poly.equal mode `End_session then 1 else 2);
           if Poly.equal mode `End_session then assert saved_snapshot.halted;
           assert (
             !admitted
             =
             if Poly.equal mode `Invalid_json || Poly.equal mode `Redirect_bad
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
           if Poly.equal mode `Post_fail
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

let audit_actor ~sw ~env ~workspace_instance ~reject_archive =
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
        audit_actor ~sw ~env ~workspace_instance ~reject_archive:true
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
        audit_actor ~sw ~env ~workspace_instance ~reject_archive:false
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
        let actor, backend = audit_actor ~sw ~env ~workspace_instance ~reject_archive in
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
