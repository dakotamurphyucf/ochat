open Core
open Fixtures

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
