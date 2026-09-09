open! Core

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : Agent_protocol.Error.t)]
;;

let diagnostics = function
  | Ok value -> value
  | Error errors -> raise_s [%sexp (errors : Agent_server.Config.Diagnostic.t list)]
;;

let key text = Agent_protocol.Idempotency_key.of_string text |> ok
let request connection command = Agent_client.Connection.request connection command |> ok
let save path text = Eio.Path.save ~create:(`Or_truncate 0o600) path text

let moderator =
  {|<script language="chatml" kind="moderator">
type state = int
type event = [ `Session_start | `Session_resume | `Tick ]
let initial_state = 0
let on_event : context -> state -> event -> state task =
  fun ctx st ev -> match ev with
  | `Session_start -> Task.pure(st + 1)
  | _ -> Task.pure(st)
</script>|}
;;

let initial_prompt = "<developer>original administration message</developer>" ^ moderator

let config_text =
  {|(version 1)
(server ((data_dir "./data") (unix_socket "./agent.sock")
 (durability ((journal_flush each) (snapshot_every_events 1)))))
(workspaces (((id project) (source (physical ".")) (access shared_write))))
(prompts (((id agent) (path "./prompt.chatmd") (allowed_workspaces (project))
 (permission_profile unattended) (enabled true))))
(permission_profiles (((id unattended) (tool_default allow)
 (manifest_authorization assume_authorized))))|}
;;

let principal () =
  Agent_protocol.Principal.create
    ~id:(Agent_protocol.Id.Principal.create ())
    ~authentication_kind:"test"
    ~scopes:
      (Agent_protocol.Scope.Set.of_list
         [ List_prompts
         ; List_workspaces
         ; Create_sessions
         ; View_session_transcript
         ; Send_messages
         ; Own_sessions
         ; View_security_state
         ; Manage_grants
         ; Stop_sessions
         ; Delete_sessions
         ; Administer_configuration
         ])
    ~attributes:[]
  |> ok
;;

let connection daemon principal =
  let notifications = Eio.Stream.create 256 in
  let context =
    Agent_server.Connection_context.create
      ~connection_id:Agent_protocol.Id.Transaction.(create () |> to_string)
      ~principal
      ~transport:In_memory
      ~publish_notification:(Eio.Stream.add notifications)
      ~max_attachments:8
  in
  let connection =
    Agent_client.In_memory.create
      ~notifications
      ~request:
        (Agent_server.Dispatcher.dispatch_command
           (Agent_server.Daemon.dispatcher daemon)
           ~context)
      ~close:(fun () -> Agent_server.Daemon.close_connection daemon context)
  in
  ignore
    (Agent_client.Session_handle.initialize
       connection
       ~implementation_name:"admin-test"
       ~implementation_version:"1"
     |> ok
     : Agent_protocol.Initialize.Response.t);
  connection
;;

let with_daemon f =
  Eio_main.run (fun env ->
    Mirage_crypto_rng_unix.use_default ();
    let root =
      Eio.Path.(
        Eio.Stdenv.fs env
        / ("/tmp/ochat-administration-"
           ^ Agent_protocol.Id.Transaction.(create () |> to_string)))
    in
    Eio.Path.mkdir ~perm:0o700 root;
    Exn.protect
      ~finally:(fun () -> Eio.Path.rmtree ~missing_ok:true root)
      ~f:(fun () ->
        save Eio.Path.(root / "prompt.chatmd") initial_prompt;
        save Eio.Path.(root / "server.sexp") config_text;
        let source_file = Eio.Path.native_exn Eio.Path.(root / "server.sexp") in
        let config =
          Agent_server.Config_parser.parse_string ~source_file config_text
          |> diagnostics
          |> Agent_server.Config_validator.validate ~env
          |> diagnostics
        in
        Eio.Switch.run (fun sw ->
          let daemon =
            Agent_server.Daemon.start
              ~sw
              ~env
              ~config
              ~tool_dir:(Eio.Path.native_exn root)
              ~home:(Eio.Path.native_exn root)
              ~process_start_identity:None
              ()
            |> ok
          in
          Exn.protect
            ~finally:(fun () -> Agent_server.Daemon.shutdown daemon |> ok)
            ~f:(fun () -> f sw env root daemon))))
;;

let create_session connection =
  let prompt = Agent_client.Catalog.prompts connection |> ok |> List.hd_exn in
  let workspace = Agent_client.Catalog.workspaces connection |> ok |> List.hd_exn in
  let spec =
    Agent_protocol.Session.Spec.create
      ~execution_host:Daemon
      ~prompt:(Catalog prompt.id)
      ~workspace:(Configured workspace.id)
      ~liveness:Detached
      ~persistence:Durable
      ~start_immediately:false
      ~labels:[]
      ()
    |> ok
  in
  match
    request
      connection
      (Session_create
         { spec
         ; requested_mode = Some Read_write
         ; subscribe = false
         ; idempotency_key = key "create"
         })
  with
  | Session_create created ->
    created.session, (Option.value_exn created.attachment).attachment
  | _ -> failwith "expected session.create"
;;

let state daemon session_id =
  let entry =
    Agent_server.Session_registry.find (Agent_server.Daemon.registry daemon) session_id
    |> Option.value_exn
  in
  entry, Agent_session.Session_actor.state entry.actor |> ok
;;

let reload root daemon text =
  save Eio.Path.(root / "prompt.chatmd") text;
  ignore
    (Agent_server.Daemon.reload_config daemon |> diagnostics : Agent_server.Config_diff.t);
  let prompt =
    Agent_session.Prompt_catalog.find_by_name (Agent_server.Daemon.prompts daemon) "agent"
    |> Option.value_exn
  in
  match prompt.availability with
  | Ready revision -> Agent_session.Prompt_revision.id revision
  | _ -> failwith "replacement catalog not ready"
;;

let command kind session_id attachment_id revision target =
  match kind with
  | `Rebuild ->
    Agent_protocol.Command.Session_rebuild
      { session_id
      ; attachment_id
      ; expected_revision = revision
      ; prompt_choice = Current_catalog
      ; idempotency_key = key "rebuild"
      }
  | `Upgrade ->
    Session_upgrade_prompt
      { session_id
      ; attachment_id
      ; expected_revision = revision
      ; target_revision = target
      ; allow_migration = true
      ; idempotency_key = key "upgrade"
      }
;;

let equal_state left right =
  Sexp.equal
    (Agent_session.Session_state.sexp_of_t left)
    (Agent_session.Session_state.sexp_of_t right)
;;

let failure_prompt =
  {|<developer>must not install failed initialization</developer>
<script language="chatml" kind="moderator">
type state = int
type event = [ `Session_start | `Tick ]
let initial_state = 0
let on_event : context -> state -> event -> state task =
 fun ctx st ev -> match ev with
 | `Session_start -> Task.bind(Schedule.after_ms(60000, `Tick), fun timer -> fail("injected-post-start"))
 | _ -> Task.pure(st)
</script>|}
;;

let check_failed_preparation kind source expected_error =
  with_daemon (fun _sw _env root daemon ->
    let connection = connection daemon (principal ()) in
    Exn.protect
      ~finally:(fun () -> Agent_client.Connection.close connection)
      ~f:(fun () ->
        let session, attachment = create_session connection in
        let target = reload root daemon (source root) in
        let entry, before = state daemon session.id in
        assert (Option.is_some before.moderator);
        let result =
          Agent_client.Connection.request
            connection
            (command kind session.id attachment.id before.counters.revision target)
        in
        (match result with
         | Ok _ -> failwith "failed preparation was accepted"
         | Error failure ->
           assert (String.is_substring failure.message ~substring:expected_error));
        let after = Agent_session.Session_actor.state entry.actor |> ok in
        assert (equal_state before after);
        let directory =
          Agent_store.Session_store.Handle.directory (Option.value_exn entry.store_handle)
        in
        let files = Eio.Path.read_dir Eio.Path.(Eio.Stdenv.fs _env / directory) in
        assert (not (List.exists files ~f:(String.is_prefix ~prefix:"admin-preparation-")))))
;;

let%expect_test "rebuild and upgrade preparation failures preserve exact actor state" =
  List.iter [ `Rebuild; `Upgrade ] ~f:(fun kind ->
    check_failed_preparation kind (fun _ -> failure_prompt) "injected-post-start";
    check_failed_preparation
      kind
      (fun root ->
         "<developer>missing root</developer><tool name=\"read_file\"><read \
          id=\"missing\" path=\""
         ^ Eio.Path.native_exn Eio.Path.(root / "missing-read-root")
         ^ "\"/></tool>")
      "missing-read-root");
  print_endline
    "post-start timer and missing-root failures leave state/reservations unchanged";
  [%expect
    {| post-start timer and missing-root failures leave state/reservations unchanged |}]
;;

let synchronous_model_prompt =
  {|<developer>must not execute a staged model call</developer>
<script language="chatml" kind="moderator">
type state = int
type event = [ `Session_start ]
let initial_state = 0
let on_event : context -> state -> event -> state task =
 fun ctx st ev -> Task.bind(
   Model.call("agent_prompt_v1",
     Json.parse("{\"prompt\":\"missing-model-preparation.chatmd\",\"input\":\"test\",\"is_local\":true}")),
   fun result -> match result with
   | `Error(message) -> fail(message)
   | _ -> fail("unexpected synchronous model outcome"))
</script>|}
;;

let%expect_test "staged synchronous model calls fail before actor claims or execution" =
  List.iter [ `Rebuild; `Upgrade ] ~f:(fun kind ->
    check_failed_preparation
      kind
      (fun _ -> synchronous_model_prompt)
      "synchronous model call requires an initialized session actor");
  print_endline "staged model calls fail closed without changing actor state";
  [%expect {| staged model calls fail closed without changing actor state |}]
;;

let%expect_test "rebuild replaces actual prompt messages while upgrade retains history" =
  List.iter [ `Rebuild; `Upgrade ] ~f:(fun kind ->
    with_daemon (fun _sw _env root daemon ->
      let connection = connection daemon (principal ()) in
      let session, attachment = create_session connection in
      let _, before = state daemon session.id in
      let target =
        reload root daemon "<developer>fresh replacement message</developer>"
      in
      ignore
        (request
           connection
           (command kind session.id attachment.id before.counters.revision target)
         : Agent_protocol.Method_result.t);
      let _, after = state daemon session.id in
      let history = after.conversation.canonical_history in
      let text =
        Jsonaf.to_string
          (`Array (List.map history ~f:Agent_protocol.History.entry_to_json))
      in
      (match kind with
       | `Rebuild ->
         assert (String.is_substring text ~substring:"fresh replacement message");
         assert (
           not (String.is_substring text ~substring:"original administration message"));
         assert (after.identity.generation = before.identity.generation + 1);
         assert (
           not
             (History_entry.Id.equal
                (List.hd_exn history).id
                (List.hd_exn before.conversation.canonical_history).id))
       | `Upgrade -> assert (Poly.equal history before.conversation.canonical_history));
      assert (List.length after.conversation.compaction_archives = 1);
      Agent_client.Connection.close connection));
  print_endline
    "rebuild uses fresh messages and IDs; upgrade keeps canonical history; both archive";
  [%expect
    {| rebuild uses fresh messages and IDs; upgrade keeps canonical history; both archive |}]
;;

let seeded_permission state =
  let session_id = state.Agent_session.Session_state.identity.session_id in
  Agent_protocol.Permission.
    { id = Agent_protocol.Id.Permission.create ()
    ; session_id
    ; generation = state.identity.generation
    ; owner = Operation (Agent_protocol.Id.Operation.create ())
    ; call_id = "seed"
    ; tool_name = "private-tool"
    ; runtime_identity = None
    ; invocation_display = "private invocation"
    ; rationale = None
    ; effects = []
    ; choices = [ Deny ]
    ; created_at = state.identity.updated_at
    ; expires_at = None
    ; state = Pending
    ; resolution = None
    }
;;

let seeded_grant state =
  Agent_protocol.Grant.
    { id = Agent_protocol.Id.Grant.create ()
    ; session_id = state.Agent_session.Session_state.identity.session_id
    ; principal_id = Option.value_exn state.identity.creating_principal
    ; tool_name = "private-tool"
    ; identity_digest = "private-grant"
    ; scope = Exact_session
    ; state = Active
    ; created_at = state.identity.updated_at
    ; expires_at = None
    ; revoked_at = None
    ; revocation_reason = None
    }
;;

let seeded_job state =
  Agent_protocol.Job.
    { id = Agent_protocol.Id.Job.create ()
    ; session_id = state.Agent_session.Session_state.identity.session_id
    ; generation = state.identity.generation
    ; kind = Model_call
    ; payload = `Object []
    ; status = Succeeded
    ; retry_policy = Never
    ; attempt = 1
    ; created_at = state.identity.updated_at
    ; started_at = None
    ; next_run_at = None
    ; completed_at = Some state.identity.updated_at
    ; result = None
    ; delivery = Not_required
    ; launch = None
    }
;;

let seeded_schedule state =
  Agent_protocol.Schedule.
    { id = Agent_protocol.Id.Schedule.create ()
    ; session_id = state.Agent_session.Session_state.identity.session_id
    ; generation = state.identity.generation
    ; payload = `Object []
    ; created_at = state.identity.updated_at
    ; next_due_at = state.identity.updated_at
    ; misfire = Deliver_once_immediately
    ; status = Cancelled
    ; delivery_count = 0
    ; last_delivery_at = None
    }
;;

let seed_children entry attachment_id (before : Agent_session.Session_state.t) =
  let candidate =
    { before with
      Agent_session.Session_state.conversation =
        { before.conversation with
          initial_prompt_entry_count = 0
        ; deferred_user_entries = before.conversation.canonical_history
        }
    ; permissions = [ seeded_permission before ]
    ; grants = [ seeded_grant before ]
    ; jobs = [ seeded_job before ]
    ; schedules = [ seeded_schedule before ]
    ; shell = { before.shell with last_audit_sequence = Some 99L }
    }
  in
  ignore
    (Agent_session.Session_actor.commit_administration
       entry.Agent_server.Session_registry.actor
       ~command_audit:None
       ~attachment_id
       ~expected_revision:before.counters.revision
       ~kind:Upgrade
       candidate
     |> ok
     : Agent_protocol.Session.t)
;;

let read_snapshot connection session_id =
  match request connection (Session_get { session_id; history = None }) with
  | Session_get snapshot -> snapshot
  | _ -> failwith "expected snapshot"
;;

let attach ~sw ~env connection session_id mode =
  Agent_client.Session_handle.attach
    ~sw
    ~clock:(Eio.Stdenv.clock env)
    ~connection
    ~session_id
    ~mode
    ()
  |> ok
;;

let wait_projection env handle sequence =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 3. (fun () ->
    let rec loop () =
      let snapshot =
        Agent_client.Session_handle.projection handle |> Agent_client.Projection.snapshot
      in
      if Int64.(snapshot.latest_event_sequence >= sequence)
      then snapshot
      else (
        Eio.Fiber.yield ();
        loop ())
    in
    loop ())
;;

let assert_empty_children (snapshot : Agent_protocol.Snapshot.t) =
  assert (List.is_empty snapshot.canonical_history.entries);
  assert (List.is_empty snapshot.deferred_entries);
  assert (List.is_empty snapshot.permissions);
  assert (List.is_empty snapshot.grants);
  assert (List.is_empty snapshot.jobs);
  assert (List.is_empty snapshot.schedules)
;;

let%expect_test "reset converges two subscribed clients and filters nested replacements" =
  with_daemon (fun sw env _root daemon ->
    let principal = principal () in
    let writer = connection daemon principal in
    let session, attachment = create_session writer in
    let entry, before = state daemon session.id in
    seed_children entry attachment.id before;
    let writer_handle = attach ~sw ~env writer session.id Read_write in
    let observer_principal =
      { principal with
        scopes = Agent_protocol.Scope.Set.of_list [ View_session_transcript ]
      }
    in
    let observer = connection daemon observer_principal in
    let observer_handle = attach ~sw ~env observer session.id Read_only in
    let before = read_snapshot writer session.id in
    assert (List.length before.permissions = 1 && List.length before.grants = 1);
    let observer_before = read_snapshot observer session.id in
    assert (
      List.is_empty observer_before.permissions && List.is_empty observer_before.grants);
    let reset =
      Agent_client.Session_handle.reset
        writer_handle
        ~expected_revision:before.revision
        ~keep_history:false
        ~keep_tasks:false
        ~keep_cache:true
        ~keep_workspace:true
        ~keep_grants:false
        ~keep_labels:false
      |> ok
    in
    List.iter [ writer_handle; observer_handle ] ~f:(fun handle ->
      let projected = wait_projection env handle reset.latest_event_sequence in
      assert_empty_children projected;
      assert (Int64.equal projected.revision reset.revision));
    assert_empty_children (read_snapshot writer session.id);
    Agent_client.Session_handle.close observer_handle;
    Agent_client.Session_handle.close writer_handle;
    Agent_client.Connection.close observer;
    Agent_client.Connection.close writer);
  print_endline
    "writer and transcript reader clear all reset collections at the committed cursor";
  [%expect
    {| writer and transcript reader clear all reset collections at the committed cursor |}]
;;

let%expect_test "security scope alone never reveals replacement snapshot grants" =
  with_daemon (fun _sw _env _root daemon ->
    let principal = principal () in
    let connection = connection daemon principal in
    let session, attachment = create_session connection in
    let entry, before = state daemon session.id in
    seed_children entry attachment.id before;
    let current = read_snapshot connection session.id in
    let event =
      Agent_protocol.Event.Durable.of_payload
        ~session_id:session.id
        ~sequence:1L
        ~revision:1L
        ~timestamp:session.updated_at
        (Session_updated session)
    in
    let event = Agent_protocol.Event.Durable.with_replacement_snapshot event current in
    let principal =
      { principal with
        scopes =
          Agent_protocol.Scope.Set.of_list
            [ View_session_transcript; View_security_state ]
      }
    in
    let event = Agent_server.Principal_projection.durable principal event in
    let projected =
      Agent_protocol.Event.Durable.replacement_snapshot event |> ok |> Option.value_exn
    in
    assert (List.length projected.permissions = 1);
    assert (
      List.is_empty projected.grants
      && List.is_empty projected.jobs
      && List.is_empty projected.schedules);
    assert (
      Int64.equal projected.latest_event_sequence 1L
      && Int64.equal projected.session.revision 1L);
    Agent_client.Connection.close connection);
  print_endline
    "nested grants/jobs/schedules filtered even with security.read; per-event anchors \
     exact";
  [%expect
    {| nested grants/jobs/schedules filtered even with security.read; per-event anchors exact |}]
;;

let with_blocked_archive env entry f =
  let handle = Option.value_exn entry.Agent_server.Session_registry.store_handle in
  let archive =
    Eio.Path.(
      Eio.Stdenv.fs env / Agent_store.Session_store.Handle.archive_directory handle)
  in
  let held =
    Eio.Path.(
      Eio.Stdenv.fs env
      / (Agent_store.Session_store.Handle.archive_directory handle ^ "-held"))
  in
  Eio.Path.rename archive held;
  Exn.protect
    ~finally:(fun () ->
      Eio.Path.unlink archive;
      Eio.Path.rename held archive)
    ~f:(fun () ->
      save archive "injected non-directory archive root";
      f ())
;;

let%expect_test
    "archive write failure after preparation preserves pinned moderator shell and history"
  =
  List.iter [ `Rebuild; `Upgrade ] ~f:(fun kind ->
    with_daemon (fun _sw env root daemon ->
      let connection = connection daemon (principal ()) in
      let session, attachment = create_session connection in
      let entry, initial = state daemon session.id in
      seed_children entry attachment.id initial;
      let target = reload root daemon "<developer>prepared but uncommitted</developer>" in
      let _, before = state daemon session.id in
      assert (
        Option.is_some before.moderator && Option.is_some before.shell.last_audit_sequence);
      with_blocked_archive env entry (fun () ->
        match
          Agent_client.Connection.request
            connection
            (command kind session.id attachment.id before.counters.revision target)
        with
        | Ok _ -> failwith "archive failure was accepted"
        | Error error ->
          assert (Agent_protocol.Error.equal_code error.code Persistence_error));
      let _, after = state daemon session.id in
      assert (equal_state before after);
      Agent_client.Connection.close connection));
  print_endline
    "prepared rebuild/upgrade rejected by archive IO leave exact prior state installed";
  [%expect
    {| prepared rebuild/upgrade rejected by archive IO leave exact prior state installed |}]
;;

let%expect_test
    "legacy archive references default to compaction without changing identity"
  =
  let encoded =
    Sexp.of_string "((operation_id op_legacy_archive) (revision 7) (sha256 deadbeef))"
  in
  let reference = Agent_session.Session_state.Compaction_archive.t_of_sexp encoded in
  assert (
    Agent_session.Session_state.Compaction_archive.equal_kind reference.kind Compaction);
  assert (Int64.equal reference.revision 7L);
  print_endline "old three-field archive references remain readable";
  [%expect {| old three-field archive references remain readable |}]
;;
