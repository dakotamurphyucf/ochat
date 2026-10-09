open Core
module F = Crash_recovery_fixture
module Config_fixture = Support.Config_fixture
module Daemon_process = Support.Daemon_process
module Process_manager = Support.Process_manager

let flip contents offset =
  let bytes = Bytes.of_string contents in
  Bytes.set bytes offset (Char.of_int_exn (Char.to_int (Bytes.get bytes offset) lxor 1));
  Bytes.to_string bytes
;;

let assert_restart env fixture expected =
  F.with_daemon env fixture (fun client ->
    F.assert_snapshot
      expected
      (F.get client expected.Agent_protocol.Public.Snapshot.Fields.session.id))
;;

let assert_session_corrupt env fixture session_id =
  F.with_daemon env fixture (fun client ->
    match
      Support.Http_driver.request client (Session_get { session_id; history = None })
    with
    | Error error ->
      F.require
        (Agent_protocol.Error.equal_code error.code Persistence_error
         && not error.retryable)
        ("corrupt session returned wrong error: " ^ error.message)
    | Ok _ -> F.fail "complete authoritative corruption was accepted on restart")
;;

let require_complete_recovery_suffix contents ~offset =
  let max_payload_length =
    Agent_server.Daemon.default_options.factory_limits.max_journal_payload
  in
  let rec scan offset count =
    if Int.equal offset (String.length contents)
    then F.require (count > 0) "recovery appended no complete journal frame"
    else (
      match Agent_store.Frame.decode ~max_payload_length ~contents ~offset with
      | Ok (Complete { next_offset; _ }) ->
        F.require (next_offset > offset) "recovery frame did not advance";
        scan next_offset (count + 1)
      | Ok (Incomplete_tail { offset }) ->
        raise_s [%sexp "recovery left an incomplete journal suffix", (offset : int)]
      | Error error ->
        raise_s
          [%sexp
            "recovery suffix failed frame validation", (error : Agent_store.Frame.error)])
  in
  scan offset 0
;;

let test_journal_tail env environment =
  let fixture = F.fixture env environment "recovery-real-journal-tail" in
  let expected = F.seed env fixture in
  let journal = F.current_journal env fixture expected.session.id in
  let committed = F.read env journal in
  let directory = Filename.dirname journal in
  let complete =
    Eio.Path.read_dir (F.path env directory)
    |> List.filter ~f:(String.is_suffix ~suffix:".log")
    |> List.find_map_exn ~f:(fun name ->
      let contents = F.read env (Filename.concat directory name) in
      Option.some_if (String.length contents > 23) contents)
  in
  let tail = String.prefix complete 23 in
  F.require
    (String.length complete > String.length tail)
    "journal tail fixture lacks a complete frame";
  F.write env journal (committed ^ tail);
  F.with_daemon env fixture (fun client ->
    F.assert_snapshot expected (F.get client expected.session.id);
    F.require
      (String.equal (F.read env journal) (committed ^ tail))
      "cold inspection mutated the incomplete journal";
    let attached =
      Support.Http_driver.request
        client
        (Session_attach
           { session_id = expected.session.id
           ; requested_mode = Read_only
           ; subscribe = false
           ; after_sequence = None
           ; reclaim_token = None
           ; idempotency_key = F.key "tail-recovery:owned-attach"
           })
      |> F.protocol_ok
    in
    let attachment =
      match attached.result with
      | Session_attach value -> value.attachment
      | _ -> F.fail "owned recovery attachment returned the wrong result"
    in
    (match
       F.request
         client
         (Session_detach
            { session_id = expected.session.id
            ; attachment_id = attachment.id
            ; idempotency_key = F.key "tail-recovery:owned-detach"
            })
     with
     | Session_detach _ -> ()
     | _ -> F.fail "owned recovery detach returned the wrong result");
    let recovered = F.get client expected.session.id in
    F.require
      (Agent_protocol.Session.equal_desired_state recovered.session.desired_state Stopped
       && Option.is_none recovered.session.active_operation)
      "owned tail repair activated the stopped session";
    F.assert_snapshot expected recovered);
  let recovered = F.read env journal in
  F.require
    (String.is_prefix recovered ~prefix:committed)
    "tail repair rewrote committed journal bytes";
  require_complete_recovery_suffix recovered ~offset:(String.length committed);
  assert_restart env fixture expected
;;

let test_journal_corruption env environment =
  let fixture = F.fixture env environment "recovery-real-journal-corrupt" in
  let expected = F.seed env fixture in
  let directory = F.current_journal env fixture expected.session.id |> Filename.dirname in
  let journal =
    Eio.Path.read_dir (F.path env directory)
    |> List.filter ~f:(String.is_suffix ~suffix:".log")
    |> List.find_map_exn ~f:(fun name ->
      let path = Filename.concat directory name in
      Option.some_if (String.length (F.read env path) > 20) path)
  in
  let committed = F.read env journal in
  let damaged = flip committed 20 in
  F.write env journal damaged;
  assert_session_corrupt env fixture expected.session.id;
  F.require
    (String.equal (F.read env journal) damaged)
    "failed recovery rewrote authoritative corruption";
  F.write env journal committed;
  assert_restart env fixture expected
;;

let test_journal env environment =
  test_journal_tail env environment;
  test_journal_corruption env environment
;;

let current_snapshot env fixture session_id =
  let directory = F.snapshot_directory fixture session_id in
  ( directory
  , Filename.concat
      directory
      (String.strip (F.read env (Filename.concat directory "CURRENT"))) )
;;

let snapshot_seed env environment name =
  let fixture = F.fixture env environment name in
  let expected = F.seed env fixture in
  assert_restart env fixture expected;
  let directory, current = current_snapshot env fixture expected.session.id in
  let snapshots =
    Eio.Path.read_dir (F.path env directory)
    |> List.filter ~f:(String.is_suffix ~suffix:".bin")
  in
  F.require (List.length snapshots >= 2) "fallback fixture lacks a prior real checkpoint";
  fixture, expected, directory, current
;;

let test_snapshot_fallback env environment =
  let fixture, expected, _directory, current =
    snapshot_seed env environment "recovery-real-snapshot-tail"
  in
  F.write env current "short";
  assert_restart env fixture expected;
  assert_restart env fixture expected
;;

let test_snapshot_corruption env environment =
  let fixture, expected, _directory, current =
    snapshot_seed env environment "recovery-real-snapshot-checksum"
  in
  let saved = F.read env current in
  let damaged = flip saved (String.length saved - 1) in
  F.write env current damaged;
  assert_session_corrupt env fixture expected.session.id;
  F.require
    (String.equal (F.read env current) damaged)
    "failed snapshot recovery modified the damaged checkpoint";
  F.write env current saved;
  assert_restart env fixture expected
;;

let index_path fixture =
  Filename.concat
    (Filename.concat (Config_fixture.data_dir fixture) "indexes")
    "sessions.snapshot"
;;

let test_index_missing env environment =
  let fixture = F.fixture env environment "recovery-real-index-missing" in
  let expected = F.seed env fixture in
  Eio.Path.unlink (F.path env (index_path fixture));
  assert_restart env fixture expected
;;

let rec await_exit env daemon =
  match Daemon_process.result daemon with
  | Some result -> result
  | None ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
    await_exit env daemon
;;

let assert_startup_rejected env fixture ~code =
  Eio.Switch.run (fun sw ->
    let daemon = F.start ~sw env fixture in
    Exn.protect
      ~finally:(fun () -> F.stop env daemon)
      ~f:(fun () ->
        let result =
          Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
            await_exit env daemon)
        in
        F.require
          (Process_manager.equal_exit result.exit (Exited 1))
          "invalid store did not reject startup with exit 1";
        let error =
          Sexp.of_string result.stderr.contents |> Agent_protocol.Error.t_of_sexp
        in
        if not (Agent_protocol.Error.equal_code error.code code && not error.retryable)
        then
          raise_s
            [%sexp
              "current document returned the wrong typed startup error"
            , { expected_code = (code : Agent_protocol.Error.code)
              ; actual_code = (error.code : Agent_protocol.Error.code)
              ; retryable = (error.retryable : bool)
              }]))
;;

let test_index_corruption env environment =
  let fixture = F.fixture env environment "recovery-real-index-corrupt" in
  let expected = F.seed env fixture in
  let index = index_path fixture in
  let saved = F.read env index in
  let damaged =
    match Jsonaf.of_string saved with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, value) ->
           if String.equal name "payload"
           then name, `Object [ "entries", `String "invalid current index entries" ]
           else name, value))
      |> Jsonaf.to_string
    | _ -> F.fail "current index fixture lacks a document envelope"
  in
  F.require (not (String.equal damaged saved)) "index corruption fixture changed no bytes";
  F.write env index damaged;
  assert_startup_rejected env fixture ~code:Persistence_error;
  F.require
    (String.equal (F.read env index) damaged)
    "invalid index was silently overwritten";
  F.write env index saved;
  assert_restart env fixture expected
;;

let rec store_files env directory =
  Eio.Path.read_dir (F.path env directory)
  |> List.concat_map ~f:(fun name ->
    let filename = Filename.concat directory name in
    match Eio.Path.kind ~follow:false (F.path env filename) with
    | `Directory -> store_files env filename
    | `Regular_file when not (String.is_suffix name ~suffix:".lock") ->
      [ filename, F.read env filename ]
    | _ -> [])
  |> List.sort ~compare:(fun (left, _) (right, _) -> String.compare left right)
;;

let run_cli env fixture arguments =
  Eio.Switch.run (fun sw ->
    Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
      Daemon_process.run_cli ~sw ~env ~fixture ~arguments))
;;

let assert_plan result ~version ~status ~mode =
  F.require
    (Process_manager.equal_exit result.Process_manager.exit (Exited 0))
    ("migration CLI failed: " ^ result.stderr.contents);
  let plan = Agent_store.Migration.plan_of_sexp (Sexp.of_string result.stdout.contents) in
  F.require
    (plan.source_version = version
     && plan.target_version = Agent_store.Session_store.current_schema_version
     && plan.session_count = 1)
    "migration CLI returned incorrect versions or session count";
  F.require
    (Agent_store.Migration.equal_status plan.status status)
    "migration CLI returned incorrect status";
  F.require
    (Agent_store.Migration.equal_mode plan.mode mode)
    "migration CLI returned incorrect mode"
;;

let assert_readonly env fixture expected =
  F.require_equal
    "migration store bytes excluding ephemeral locks"
    [%sexp_of: (string * string) list]
    expected
    (store_files env (Config_fixture.data_dir fixture))
;;

let replace_schema_version env fixture schema version =
  let root = Config_fixture.data_dir fixture in
  let schema_path = Filename.concat root "schema.sexp" in
  let altered =
    match Jsonaf.of_string schema with
    | `Object fields ->
      `Object
        (List.map fields ~f:(fun (name, value) ->
           if String.equal name "schema_version"
           then name, `Number (Int.to_string version)
           else name, value))
      |> Jsonaf.to_string
    | _ -> F.fail "store schema fixture lacks a current document envelope"
  in
  F.require
    (not (String.equal schema altered))
    "schema mutation did not change the version";
  F.write env schema_path altered
;;

let migration_version env fixture schema version status =
  let root = Config_fixture.data_dir fixture in
  replace_schema_version env fixture schema version;
  let before = store_files env root in
  List.iter
    [ [ "-inspect-store"; root ], Agent_store.Migration.Validate_only
    ; [ "-migrate-store"; root; "-dry-run" ], Dry_run
    ]
    ~f:(fun (arguments, mode) ->
      assert_plan (run_cli env fixture arguments) ~version ~status ~mode;
      assert_readonly env fixture before);
  let applied = run_cli env fixture [ "-migrate-store"; root ] in
  match status with
  | Agent_store.Migration.Current ->
    assert_plan applied ~version ~status:Current ~mode:Apply;
    assert_readonly env fixture before
  | Migration_required ->
    (* Version one has a supported authority-root compatibility migration. *)
    assert_plan applied ~version ~status:Current ~mode:Apply
  | Schema_too_new ->
    F.require
      (Process_manager.equal_exit applied.exit (Exited 1))
      "future migration apply did not fail closed";
    (match Agent_store.Store_error.t_of_sexp (Sexp.of_string applied.stderr.contents) with
     | Schema_too_new actual ->
       F.require (Int.equal actual version) "future version changed"
     | ( Locked _
       | Missing _
       | Migration_required _
       | Document _
       | Framing _
       | Corrupt _
       | Io _ ) as error ->
       raise_s
         [%sexp
           "unexpected future migration rejection", (error : Agent_store.Store_error.t)]);
    assert_readonly env fixture before;
    assert_startup_rejected env fixture ~code:Persistence_error;
    assert_readonly env fixture before
;;

let malformed_migration env fixture =
  let root = Config_fixture.data_dir fixture in
  List.iter
    [ "{"; ""; "not-a-current-document"; "{\"schema_version\":\"invalid\"}" ]
    ~f:(fun malformed ->
      F.write env (Filename.concat root "schema.sexp") malformed;
      let before = store_files env root in
      List.iter
        [ [ "-inspect-store"; root ]
        ; [ "-migrate-store"; root; "-dry-run" ]
        ; [ "-migrate-store"; root ]
        ]
        ~f:(fun arguments ->
          let result = run_cli env fixture arguments in
          F.require
            (Process_manager.equal_exit result.exit (Exited 1))
            "malformed migration must fail closed";
          (match
             Agent_store.Store_error.t_of_sexp (Sexp.of_string result.stderr.contents)
           with
           | Document _ | Corrupt _ -> ()
           | ( Locked _
             | Missing _
             | Schema_too_new _
             | Migration_required _
             | Framing _
             | Io _ ) as error ->
             raise_s
               [%sexp
                 "unexpected malformed schema rejection"
               , (error : Agent_store.Store_error.t)]);
          assert_readonly env fixture before))
;;

let test_migration env environment =
  let fixture = F.fixture env environment "recovery-real-migration" in
  let expected = F.seed env fixture in
  let root = Config_fixture.data_dir fixture in
  let schema_path = Filename.concat root "schema.sexp" in
  let schema = F.read env schema_path in
  let before = store_files env root in
  assert_plan
    (run_cli env fixture [ "-migrate-store"; root ])
    ~version:Agent_store.Session_store.current_schema_version
    ~status:Current
    ~mode:Apply;
  assert_readonly env fixture before;
  migration_version env fixture schema 1 Migration_required;
  migration_version
    env
    fixture
    schema
    (Agent_store.Session_store.current_schema_version + 1)
    Schema_too_new;
  malformed_migration env fixture;
  F.write env schema_path schema;
  assert_restart env fixture expected
;;
