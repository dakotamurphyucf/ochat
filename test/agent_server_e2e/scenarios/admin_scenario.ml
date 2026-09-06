open Core
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Port_reservation = Support.Port_reservation
module Temporary_environment = Support.Temporary_environment
module Unix_driver = Support.Unix_driver

type session =
  { id : Agent_protocol.Id.Session.t
  ; attachment_id : Agent_protocol.Id.Attachment.t
  ; revision : int64
  ; generation : int
  ; prompt_revision : Agent_protocol.Id.Prompt_revision.t
  }

let fail message = raise_s [%sexp "E2E assertion failed", (message : string)]
let require condition message = if not condition then fail message

let protocol_ok = function
  | Ok value -> value
  | Error error ->
    raise_s [%sexp "protocol operation failed", (error : Agent_protocol.Error.t)]
;;

let request connection command =
  Agent_client.Connection.request connection command |> protocol_ok
;;

let idempotency_key value = Agent_protocol.Idempotency_key.of_string value |> protocol_ok

let reserve_port env =
  Eio.Switch.run (fun sw ->
    let reservation = Port_reservation.create ~sw ~env in
    let port = Port_reservation.port reservation in
    Port_reservation.release reservation;
    port)
;;

let fixture env environment name =
  Config_fixture.create environment ~name ~http_port:(reserve_port env)
;;

let path environment native_path = Temporary_environment.path environment native_path

let save environment native_path contents =
  Eio.Path.save ~create:(`Or_truncate 0o600) (path environment native_path) contents
;;

let prompt_source marker =
  sprintf "<developer>administration revision %s</developer>" marker
;;

let write_prompt environment fixture marker =
  save environment (Config_fixture.prompt_path fixture) (prompt_source marker)
;;

let wait_ready daemon env =
  match Daemon_process.wait_ready daemon ~env ~timeout_seconds:15. with
  | Ok _ -> ()
  | Error error ->
    raise_s
      [%sexp
        "daemon did not become ready"
      , (error : Daemon_process.readiness_error)
      , ((Daemon_process.stdout daemon).contents : string)
      , ((Daemon_process.stderr daemon).contents : string)]
;;

let stop_daemon env daemon =
  match Daemon_process.result daemon with
  | Some _ -> ()
  | None -> ignore (Daemon_process.stop daemon ~env ~grace_seconds:1.)
;;

let connect ~sw env fixture =
  let connection =
    Unix_driver.connect ~sw ~env ~socket_path:(Config_fixture.unix_socket fixture)
  in
  ignore
    (Unix_driver.initialize connection |> protocol_ok
     : Agent_protocol.Initialize.Response.t);
  connection
;;

let start_daemon ~sw env fixture environment_overrides =
  let config_path = Config_fixture.config_path fixture in
  if List.is_empty environment_overrides
  then Daemon_process.start ~sw ~env ~fixture ~config_path
  else
    Daemon_process.start_with_environment_overrides
      ~sw
      ~env
      ~fixture
      ~environment_overrides
      ~config_path
;;

let run_connected ~sw env fixture daemon f =
  wait_ready daemon env;
  let connection = connect ~sw env fixture in
  Exn.protect
    ~f:(fun () -> f sw daemon connection)
    ~finally:(fun () -> Agent_client.Connection.close connection)
;;

let daemon_failure daemon exn =
  raise_s
    [%sexp
      "administration daemon interaction failed"
    , (exn : Exn.t)
    , ((Daemon_process.stdout daemon).contents : string)
    , ((Daemon_process.stderr daemon).contents : string)]
;;

let with_daemon ?(environment_overrides = []) env fixture f =
  Eio.Switch.run (fun sw ->
    let daemon = start_daemon ~sw env fixture environment_overrides in
    Exn.protect
      ~f:(fun () ->
        try run_connected ~sw env fixture daemon f with
        | exn -> daemon_failure daemon exn)
      ~finally:(fun () -> stop_daemon env daemon))
;;

let catalog_prompt connection =
  Agent_client.Catalog.prompts connection
  |> protocol_ok
  |> List.find_exn ~f:(fun prompt -> String.equal prompt.name "smoke")
;;

let catalog_workspace connection =
  Agent_client.Catalog.workspaces connection
  |> protocol_ok
  |> List.find_exn ~f:(fun workspace -> String.equal workspace.name "physical")
;;

let current_prompt_revision connection =
  (catalog_prompt connection).current_revision |> Option.value_exn
;;

let session_spec connection ~start_immediately =
  Agent_protocol.Session.Spec.create
    ~execution_host:Daemon
    ~prompt:(Catalog (catalog_prompt connection).id)
    ~workspace:(Configured (catalog_workspace connection).id)
    ~liveness:Detached
    ~persistence:Durable
    ~permission_profile:"unattended"
    ~start_immediately
    ~labels:[ "suite", "administration-idempotency" ]
    ()
  |> protocol_ok
;;

let create_request ?(start_immediately = false) connection ~key =
  Agent_protocol.Session.Create_request.
    { spec = session_spec connection ~start_immediately
    ; requested_mode = Some Read_write
    ; subscribe = false
    ; idempotency_key = idempotency_key key
    }
;;

let session_of_created created =
  let attachment =
    Option.value_exn created.Agent_protocol.Method_result.Create.attachment
  in
  { id = created.session.id
  ; attachment_id = attachment.attachment.id
  ; revision = created.session.revision
  ; generation = created.session.generation
  ; prompt_revision = Option.value_exn created.session.prompt_revision
  }
;;

let create_session connection ~key =
  match request connection (Session_create (create_request connection ~key)) with
  | Session_create created -> session_of_created created
  | _ -> fail "session.create returned the wrong result variant"
;;

let get_session connection session_id =
  match request connection (Session_get { session_id; history = None }) with
  | Session_get snapshot -> snapshot.session
  | _ -> fail "session.get returned the wrong result variant"
;;

let mutation_session = function
  | Agent_protocol.Method_result.Session_start result
  | Session_stop result
  | Session_cancel_operation result
  | Session_compact result
  | Session_delete_history result
  | Session_reset result
  | Session_rebuild result
  | Session_upgrade_prompt result -> result.session
  | _ -> fail "administrative command returned the wrong result variant"
;;

let start_command session key =
  Agent_protocol.Command.Session_start
    { session_id = session.id
    ; attachment_id = session.attachment_id
    ; queue_if_limited = true
    ; idempotency_key = idempotency_key key
    }
;;

let stop_command session key =
  Agent_protocol.Command.Session_stop
    { session_id = session.id
    ; attachment_id = session.attachment_id
    ; mode = Graceful
    ; idempotency_key = idempotency_key key
    }
;;

let compact_command session ~revision key =
  Agent_protocol.Command.Session_compact
    { session_id = session.id
    ; attachment_id = session.attachment_id
    ; expected_revision = Some revision
    ; idempotency_key = idempotency_key key
    }
;;

let reset_command session ~revision ~key ~keep_labels =
  Agent_protocol.Command.Session_reset
    { session_id = session.id
    ; attachment_id = session.attachment_id
    ; expected_revision = revision
    ; keep_history = true
    ; keep_tasks = true
    ; keep_cache = true
    ; keep_workspace = true
    ; keep_grants = true
    ; keep_labels
    ; idempotency_key = idempotency_key key
    }
;;

let rebuild_command session ~revision key =
  Agent_protocol.Command.Session_rebuild
    { session_id = session.id
    ; attachment_id = session.attachment_id
    ; expected_revision = revision
    ; prompt_choice = Current_catalog
    ; idempotency_key = idempotency_key key
    }
;;

let upgrade_command session ~revision ~target_revision key =
  Agent_protocol.Command.Session_upgrade_prompt
    { session_id = session.id
    ; attachment_id = session.attachment_id
    ; expected_revision = revision
    ; target_revision
    ; allow_migration = true
    ; idempotency_key = idempotency_key key
    }
;;

let delete_command session ~revision key =
  Agent_protocol.Command.Session_delete
    { session_id = session.id
    ; attachment_id = session.attachment_id
    ; expected_revision = revision
    ; policy = Remove
    ; confirmation = Agent_protocol.Id.Session.to_string session.id
    ; idempotency_key = idempotency_key key
    }
;;

let observed_is
      (state : Agent_protocol.Session.t)
      (expected : Agent_protocol.Session.observed_state)
  =
  let open Agent_protocol.Session in
  match state.Agent_protocol.Session.observed_state, expected with
  | Stopped, Stopped | Idle, Idle -> true
  | ( ( Stopped
      | Queued_for_slot
      | Starting
      | Recovering
      | Idle
      | Running_turn _
      | Compacting _
      | Waiting_for_permission _
      | Stopping
      | Failed _ )
    , _ ) -> false
;;

let rec await_observed env connection session_id expected attempts =
  let state = get_session connection session_id in
  if observed_is state expected
  then state
  else if attempts = 0
  then fail "session did not reach the expected administrative state"
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_observed env connection session_id expected (attempts - 1))
;;

let rec await_revision env connection previous attempts =
  let current = current_prompt_revision connection in
  if Agent_protocol.Id.Prompt_revision.compare current previous <> 0
  then current
  else if attempts = 0
  then fail "prompt reload did not publish a new revision"
  else (
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_revision env connection previous (attempts - 1))
;;

let reload_prompt env environment fixture daemon connection =
  let previous = current_prompt_revision connection in
  write_prompt environment fixture "B";
  Daemon_process.signal daemon Stdlib.Sys.sighup;
  await_revision env connection previous 250
;;

let test_start_stop env environment =
  let fixture = fixture env environment "admin-start-stop" in
  with_daemon env fixture (fun _sw _daemon connection ->
    let session = create_session connection ~key:"admin:start-stop:create" in
    let started = request connection (start_command session "admin:start-stop:start") in
    let started = mutation_session started in
    require (observed_is started Idle) "session.start did not reach idle";
    let stopped = request connection (stop_command session "admin:start-stop:stop") in
    let stopped = mutation_session stopped in
    require (observed_is stopped Stopped) "session.stop did not reach stopped";
    require
      Int64.(stopped.revision > started.revision)
      "session.stop did not advance revision")
;;

let operation_of_compaction state =
  match state.Agent_protocol.Session.observed_state with
  | Compacting operation_id -> operation_id
  | _ -> fail "session.compact did not return a compacting operation"
;;

let cancellable_compaction_prompt =
  "<developer>cancellable compaction fixture</developer><user>history to \
   compact</user><assistant>prior response</assistant>"
;;

let blackhole_api_url ~sw env =
  let listener =
    Eio.Net.listen
      ~sw
      ~reuse_addr:false
      ~reuse_port:false
      ~backlog:1
      (Eio.Stdenv.net env)
      (`Tcp (Eio.Net.Ipaddr.V4.loopback, 0))
  in
  match Eio.Net.listening_addr listener with
  | `Tcp (_, port) -> sprintf "http://127.0.0.1:%d" port
  | `Unix path -> raise_s [%sexp "loopback listener returned Unix path", (path : string)]
;;

let cancel_active_compaction env connection =
  let session = create_session connection ~key:"admin:cancel:create" in
  let compacted =
    request
      connection
      (compact_command session ~revision:session.revision "admin:cancel:compact")
    |> mutation_session
  in
  let operation_id = operation_of_compaction compacted in
  let command =
    Agent_protocol.Command.Session_cancel_operation
      { session_id = session.id
      ; attachment_id = session.attachment_id
      ; operation_id
      ; idempotency_key = idempotency_key "admin:cancel:operation"
      }
  in
  let cancelled = request connection command |> mutation_session in
  require
    Int64.(cancelled.revision > compacted.revision)
    "session.cancel_operation did not advance revision";
  ignore (await_observed env connection session.id Stopped 250 : Agent_protocol.Session.t)
;;

let test_cancel_operation env environment =
  let fixture = fixture env environment "admin-cancel-operation" in
  save environment (Config_fixture.prompt_path fixture) cancellable_compaction_prompt;
  Eio.Switch.run (fun blocker_sw ->
    let api_url = blackhole_api_url ~sw:blocker_sw env in
    let environment_overrides =
      [ "OPENAI_API_KEY", "e2e-cancellation-fixture"; "API_URL", api_url ]
    in
    with_daemon ~environment_overrides env fixture (fun _sw _daemon connection ->
      cancel_active_compaction env connection))
;;

let get_snapshot connection session_id =
  match request connection (Session_get { session_id; history = None }) with
  | Session_get snapshot -> snapshot
  | _ -> fail "expected session snapshot"
;;

let export_archive connection session revision =
  let export =
    match
      request
        connection
        (Session_export
           { session_id = session.id
           ; attachment_id = session.attachment_id
           ; format = Json
           ; revision = Some revision
           ; history = None
           })
    with
    | Session_export export -> export
    | _ -> fail "expected archived export"
  in
  require (Int64.equal export.session_revision revision) "archive export revision changed";
  match
    request
      connection
      (Blob_read
         { session_id = session.id
         ; attachment_id = session.attachment_id
         ; blob_id = export.blob.id
         ; offset = 0L
         ; max_bytes = Agent_protocol.Blob.Read_request.max_chunk_bytes
         })
  with
  | Blob_read chunk ->
    require chunk.eof "small archive fixture unexpectedly requires multiple chunks";
    Base64.decode_exn chunk.data_base64 |> Jsonaf.of_string
  | _ -> fail "expected archived blob chunk"
;;

let check_corrupt_archive
      ?(prefix = "compaction-")
      environment
      fixture
      session
      revision
      connection
  =
  let directory =
    Filename.concat
      (Config_fixture.data_dir fixture)
      ("sessions/" ^ Agent_protocol.Id.Session.to_string session.id ^ "/archive")
    |> path environment
  in
  let name = Eio.Path.read_dir directory |> List.find_exn ~f:(String.is_prefix ~prefix) in
  let file = Eio.Path.(directory / name) in
  let original = Eio.Path.load file in
  Exn.protect
    ~finally:(fun () -> Eio.Path.save ~create:(`Or_truncate 0o600) file original)
    ~f:(fun () ->
      Eio.Path.save ~create:(`Or_truncate 0o600) file "corrupted fixture archive";
      let result =
        Agent_client.Connection.request
          connection
          (Session_export
             { session_id = session.id
             ; attachment_id = session.attachment_id
             ; format = Json
             ; revision = Some revision
             ; history = None
             })
      in
      match result with
      | Error error ->
        require
          (Agent_protocol.Error.equal_code error.code Persistence_error)
          "corrupt archive returned the wrong error"
      | Ok _ -> fail "corrupt archive was exported")
;;

let test_compact env environment =
  let fixture = fixture env environment "admin-compact" in
  let session, revision, original =
    with_daemon env fixture (fun _sw _daemon connection ->
      let session = create_session connection ~key:"admin:compact:create" in
      let original =
        (get_snapshot connection session.id).canonical_history.entries
        |> List.map ~f:Agent_protocol.History.entry_to_json
        |> fun entries -> `Array entries
      in
      let compacted =
        request
          connection
          (compact_command session ~revision:session.revision "admin:compact")
        |> mutation_session
      in
      ignore (operation_of_compaction compacted : Agent_protocol.Id.Operation.t);
      let terminal = await_observed env connection session.id Stopped 250 in
      require
        Int64.(terminal.revision > compacted.revision)
        "compaction did not terminate";
      let snapshot = get_snapshot connection session.id in
      let revision = List.hd_exn snapshot.archived_revisions in
      let exported = export_archive connection session revision in
      require
        (Poly.equal (Jsonaf.member "history" exported) (Some original))
        "archive lost original history";
      session, revision, original)
  in
  let deleted_id =
    with_daemon env fixture (fun sw _daemon connection ->
      let handle =
        Agent_client.Session_handle.attach
          ~sw
          ~clock:(Eio.Stdenv.clock env)
          ~connection
          ~session_id:session.id
          ~mode:Read_write
          ~subscribe:false
          ()
        |> protocol_ok
      in
      let session =
        { session with
          attachment_id = (Agent_client.Session_handle.attachment handle).id
        }
      in
      let exported = export_archive connection session revision in
      require
        (Poly.equal (Jsonaf.member "history" exported) (Some original))
        "archive did not survive restart";
      check_corrupt_archive environment fixture session revision connection;
      let snapshot = get_snapshot connection session.id in
      let history_id = (List.last_exn snapshot.canonical_history.entries).id in
      let command =
        Agent_protocol.Command.Session_delete_history
          { session_id = session.id
          ; attachment_id = session.attachment_id
          ; history_id
          ; expected_revision = snapshot.revision
          ; idempotency_key = idempotency_key "admin:history:delete"
          }
      in
      let first = request connection command in
      let replay = request connection command in
      require
        (Poly.equal
           (Agent_protocol.Method_result.to_json first)
           (Agent_protocol.Method_result.to_json replay))
        "history deletion replay was not idempotent";
      let after = get_snapshot connection session.id in
      require
        (not
           (List.exists after.canonical_history.entries ~f:(fun entry ->
              Agent_protocol.History.Id.compare entry.id history_id = 0)))
        "history deletion was not authoritative";
      require
        (Poly.equal
           (Jsonaf.member "history" (export_archive connection session revision))
           (Some original))
        "later history mutation changed an archive";
      history_id)
  in
  with_daemon env fixture (fun _sw _daemon connection ->
    let snapshot = get_snapshot connection session.id in
    require
      (not
         (List.exists snapshot.canonical_history.entries ~f:(fun entry ->
            Agent_protocol.History.Id.compare entry.id deleted_id = 0)))
      "deleted history reappeared after restart";
    require
      (List.mem snapshot.archived_revisions revision ~equal:Int64.equal)
      "archive reference disappeared after second restart")
;;

let test_export env environment =
  let fixture = fixture env environment "admin-export" in
  with_daemon env fixture (fun _sw _daemon connection ->
    let session = create_session connection ~key:"admin:export:create" in
    let command =
      Agent_protocol.Command.Session_export
        { session_id = session.id
        ; attachment_id = session.attachment_id
        ; format = Json
        ; revision = Some session.revision
        ; history = None
        }
    in
    match request connection command with
    | Session_export export ->
      require
        (Int64.equal export.session_revision session.revision)
        "session.export returned the wrong revision";
      require Int64.(export.blob.byte_length > 0L) "session.export returned an empty blob"
    | _ -> fail "session.export returned the wrong result variant")
;;

let history_json connection session =
  (get_snapshot connection session.id).canonical_history.entries
  |> List.map ~f:Agent_protocol.History.entry_to_json
  |> fun entries -> `Array entries
;;

let require_archive connection session revision history =
  let snapshot = get_snapshot connection session.id in
  require
    (List.mem snapshot.archived_revisions revision ~equal:Int64.equal)
    "administrative archive missing from snapshot";
  require
    (Poly.equal
       (Jsonaf.member "history" (export_archive connection session revision))
       (Some history))
    "administrative archive lost previous messages"
;;

let snapshot_files environment fixture session =
  Filename.concat
    (Config_fixture.data_dir fixture)
    ("sessions/" ^ Agent_protocol.Id.Session.to_string session.id ^ "/snapshot")
  |> path environment
  |> Eio.Path.read_dir
  |> List.filter ~f:(String.is_prefix ~prefix:"snapshot-")
;;

let advance_snapshots environment fixture connection session =
  let before = snapshot_files environment fixture session in
  require (not (List.is_empty before)) "archive fixture has no ordinary snapshot";
  for index = 1 to 12 do
    let revision = (get_session connection session.id).revision in
    ignore
      (request
         connection
         (reset_command
            session
            ~revision
            ~key:(sprintf "admin:archive:prune:%d" index)
            ~keep_labels:true)
       |> mutation_session
       : Agent_protocol.Session.t)
  done;
  let after = snapshot_files environment fixture session in
  require (List.length after <= 2) "ordinary snapshot pruning did not run";
  require
    (not (List.exists before ~f:(fun name -> List.mem after name ~equal:String.equal)))
    "ordinary snapshots from before administration were not pruned"
;;

let check_admin_archive_restart env environment fixture session history prefix =
  with_daemon env fixture (fun sw _daemon connection ->
    let handle =
      Agent_client.Session_handle.attach
        ~sw
        ~clock:(Eio.Stdenv.clock env)
        ~connection
        ~session_id:session.id
        ~mode:Read_write
        ~subscribe:false
        ()
      |> protocol_ok
    in
    let session =
      { session with attachment_id = (Agent_client.Session_handle.attachment handle).id }
    in
    require_archive connection session session.revision history;
    check_corrupt_archive ~prefix environment fixture session session.revision connection)
;;

let test_reset env environment =
  let fixture = fixture env environment "admin-reset" in
  let session, history =
    with_daemon env fixture (fun _sw _daemon connection ->
      let session = create_session connection ~key:"admin:reset:create" in
      let history = history_json connection session in
      let reset =
        request
          connection
          (reset_command
             session
             ~revision:session.revision
             ~key:"admin:reset"
             ~keep_labels:true)
        |> mutation_session
      in
      require
        Int.(reset.generation = session.generation + 1)
        "reset did not advance generation";
      require Int64.(reset.revision > session.revision) "reset did not advance revision";
      require (observed_is reset Stopped) "reset did not remain stopped";
      require_archive connection session session.revision history;
      check_corrupt_archive
        ~prefix:"reset-"
        environment
        fixture
        session
        session.revision
        connection;
      advance_snapshots environment fixture connection session;
      require_archive connection session session.revision history;
      session, history)
  in
  with_daemon env fixture (fun sw _daemon connection ->
    let handle =
      Agent_client.Session_handle.attach
        ~sw
        ~clock:(Eio.Stdenv.clock env)
        ~connection
        ~session_id:session.id
        ~mode:Read_write
        ~subscribe:false
        ()
      |> protocol_ok
    in
    let session =
      { session with attachment_id = (Agent_client.Session_handle.attachment handle).id }
    in
    require_archive connection session session.revision history)
;;

let test_rebuild env environment =
  let fixture = fixture env environment "admin-rebuild" in
  write_prompt environment fixture "A";
  let session, history =
    with_daemon env fixture (fun _sw daemon connection ->
      let session = create_session connection ~key:"admin:rebuild:create" in
      let history = history_json connection session in
      let revision_b = reload_prompt env environment fixture daemon connection in
      let rebuilt =
        request
          connection
          (rebuild_command session ~revision:session.revision "admin:rebuild")
        |> mutation_session
      in
      let installed = Option.value_exn rebuilt.prompt_revision in
      require
        (Agent_protocol.Id.Prompt_revision.compare installed revision_b = 0)
        "session.rebuild did not install the current catalog revision";
      let actual = history_json connection session |> Jsonaf.to_string in
      require
        (String.is_substring actual ~substring:"administration revision B")
        "rebuild did not install actual new prompt messages";
      require
        (not (String.is_substring actual ~substring:"administration revision A"))
        "rebuild kept obsolete prompt messages";
      require_archive connection session session.revision history;
      advance_snapshots environment fixture connection session;
      require_archive connection session session.revision history;
      session, history)
  in
  check_admin_archive_restart env environment fixture session history "rebuild-"
;;

let test_upgrade_prompt env environment =
  let fixture = fixture env environment "admin-upgrade" in
  write_prompt environment fixture "A";
  let session, history =
    with_daemon env fixture (fun _sw daemon connection ->
      let session = create_session connection ~key:"admin:upgrade:create" in
      let history = history_json connection session in
      let revision_b = reload_prompt env environment fixture daemon connection in
      let upgraded =
        request
          connection
          (upgrade_command
             session
             ~revision:session.revision
             ~target_revision:revision_b
             "admin:upgrade")
        |> mutation_session
      in
      let installed = Option.value_exn upgraded.prompt_revision in
      require
        (Agent_protocol.Id.Prompt_revision.compare installed revision_b = 0)
        "session.upgrade_prompt did not install the target revision";
      require
        (Poly.equal history (history_json connection session))
        "upgrade changed retained canonical history";
      require
        Int.(upgraded.generation = session.generation)
        "upgrade unexpectedly changed generation";
      require_archive connection session session.revision history;
      session, history)
  in
  check_admin_archive_restart env environment fixture session history "upgrade-"
;;

let require_delete_receipt session = function
  | Agent_protocol.Method_result.Session_delete receipt ->
    require
      (Agent_protocol.Id.Session.compare receipt.session_id session.id = 0)
      "session.delete returned another session ID"
  | _ -> fail "session.delete returned the wrong result variant"
;;

let require_session_absent connection session_id =
  match
    Agent_client.Connection.request
      connection
      (Session_get { session_id; history = None })
  with
  | Error error ->
    require
      (Agent_protocol.Error.equal_code error.code Session_not_found)
      "deleted session returned the wrong lookup error"
  | Ok _ -> fail "deleted session remained visible"
;;

let test_delete env environment =
  let fixture = fixture env environment "admin-delete" in
  with_daemon env fixture (fun _sw _daemon connection ->
    let session = create_session connection ~key:"admin:delete:create" in
    delete_command session ~revision:session.revision "admin:delete"
    |> request connection
    |> require_delete_receipt session;
    require_session_absent connection session.id)
;;

let test_stale_revision env environment =
  let fixture = fixture env environment "admin-stale" in
  with_daemon env fixture (fun _sw _daemon connection ->
    let session = create_session connection ~key:"admin:stale:create" in
    let command =
      reset_command
        session
        ~revision:Int64.(session.revision - 1L)
        ~key:"admin:stale"
        ~keep_labels:true
    in
    (match Agent_client.Connection.request connection command with
     | Error error ->
       require
         (Agent_protocol.Error.equal_code error.code Conflict)
         "stale administrative request returned the wrong error"
     | Ok _ -> fail "stale administrative request was accepted");
    let current = get_session connection session.id in
    require
      (Int64.equal current.revision session.revision)
      "stale administrative request changed session revision")
;;

let canonical_result result =
  Agent_protocol.Method_result.to_json result
  |> Agent_protocol.Json_codec.canonical_string
  |> protocol_ok
;;

let test_exact_replay env environment =
  let fixture = fixture env environment "idempotency-replay" in
  with_daemon env fixture (fun _sw _daemon connection ->
    let session = create_session connection ~key:"idempotency:replay:create" in
    let command =
      reset_command
        session
        ~revision:session.revision
        ~key:"idempotency:replay"
        ~keep_labels:true
    in
    let first = request connection command in
    let second = request connection command in
    require
      (String.equal (canonical_result first) (canonical_result second))
      "exact idempotency retry did not replay the stored result";
    let replayed = mutation_session second in
    let current = get_session connection session.id in
    require
      (Int64.equal replayed.revision current.revision)
      "exact idempotency retry executed the mutation twice")
;;

let require_idempotency_conflict = function
  | Error (error : Agent_protocol.Error.t) ->
    require
      (Agent_protocol.Error.equal_code error.code Idempotency_conflict)
      "changed idempotency retry returned the wrong error"
  | Ok _ -> fail "changed idempotency retry was accepted"
;;

let test_changed_payload_conflict env environment =
  let fixture = fixture env environment "idempotency-conflict" in
  with_daemon env fixture (fun _sw _daemon connection ->
    let session = create_session connection ~key:"idempotency:conflict:create" in
    let reset keep_labels =
      reset_command
        session
        ~revision:session.revision
        ~key:"idempotency:conflict"
        ~keep_labels
    in
    ignore (request connection (reset true) : Agent_protocol.Method_result.t);
    Agent_client.Connection.request connection (reset false)
    |> require_idempotency_conflict)
;;

let rec await_daemon_exit env daemon attempts =
  match Daemon_process.result daemon with
  | Some _ -> ()
  | None when attempts > 0 ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
    await_daemon_exit env daemon (attempts - 1)
  | None -> fail "killed daemon did not exit"
;;

let pending_create connection =
  let command =
    Agent_protocol.Command.Session_create
      (create_request connection ~key:"idempotency:pending")
  in
  match request connection command with
  | Session_create created -> command, created.session.id
  | _ -> fail "pending fixture create returned the wrong result variant"
;;

let kill_connected_daemon env daemon connection =
  Daemon_process.signal daemon Stdlib.Sys.sigkill;
  await_daemon_exit env daemon 250;
  Agent_client.Connection.close connection
;;

let create_then_crash env fixture =
  Eio.Switch.run (fun sw ->
    let config_path = Config_fixture.config_path fixture in
    let daemon = Daemon_process.start ~sw ~env ~fixture ~config_path in
    wait_ready daemon env;
    let connection = connect ~sw env fixture in
    let created = pending_create connection in
    kill_connected_daemon env daemon connection;
    created)
;;

let rec replace_success replaced = function
  | Sexp.List [ Atom "outcome"; List (Atom "Success" :: _) ] ->
    Int.incr replaced;
    Sexp.List [ Atom "outcome"; Atom "Pending" ]
  | Sexp.List values -> Sexp.List (List.map values ~f:(replace_success replaced))
  | Atom _ as atom -> atom
;;

let force_pending_receipt environment fixture =
  let receipt_path =
    Filename.concat (Config_fixture.data_dir fixture) "indexes/idempotency.sexp"
  in
  let file = path environment receipt_path in
  let sexp = Eio.Path.load file |> Sexp.of_string in
  let replaced = ref 0 in
  let pending = replace_success replaced sexp in
  require (Int.equal !replaced 1) "pending fixture did not find one successful receipt";
  Eio.Path.save ~create:(`Or_truncate 0o600) file (Sexp.to_string_mach pending)
;;

let session_count connection session_id =
  Agent_client.Admin.list_sessions connection
  |> protocol_ok
  |> List.count ~f:(fun session ->
    Agent_protocol.Id.Session.compare session.Agent_protocol.Session.id session_id = 0)
;;

let test_pending_unknown_outcome env environment =
  let fixture = fixture env environment "idempotency-pending" in
  let command, session_id = create_then_crash env fixture in
  force_pending_receipt environment fixture;
  with_daemon env fixture (fun _sw _daemon connection ->
    match Agent_client.Connection.request connection command with
    | Error error ->
      require
        (Agent_protocol.Error.equal_code error.code Interrupted)
        "pending idempotency retry returned the wrong error";
      require (not error.retryable) "pending idempotency retry was marked retryable";
      require
        (Int.equal (session_count connection session_id) 1)
        "pending idempotency retry duplicated the committed session"
    | Ok _ -> fail "pending idempotency retry executed automatically")
;;

let cases =
  [ "admin.start-stop", test_start_stop
  ; "admin.cancel-operation", test_cancel_operation
  ; "admin.compact", test_compact
  ; "admin.export", test_export
  ; "admin.reset", test_reset
  ; "admin.rebuild", test_rebuild
  ; "admin.upgrade-prompt", test_upgrade_prompt
  ; "admin.delete", test_delete
  ; "admin.stale-revision", test_stale_revision
  ; "idempotency.exact-replay", test_exact_replay
  ; "idempotency.changed-payload-conflict", test_changed_payload_conflict
  ; "idempotency.pending-unknown-outcome", test_pending_unknown_outcome
  ]
;;

let select case =
  match case with
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> raise_s [%sexp "unknown administration case", (name : string)])
;;

let run env ~case =
  Temporary_environment.with_
    ~scenario:"administration-idempotency"
    ~env
    (fun environment ->
       let selected = select case in
       List.iter selected ~f:(fun (name, test) ->
         try test env environment with
         | exn ->
           raise_s
             [%sexp "administration E2E case failed", (name : string), (exn : Exn.t)]);
       print_s
         [%sexp
           { scenario = ("administration-idempotency" : string)
           ; selected_case = (case : string option)
           ; passed_cases = (List.map selected ~f:fst : string list)
           }])
;;
