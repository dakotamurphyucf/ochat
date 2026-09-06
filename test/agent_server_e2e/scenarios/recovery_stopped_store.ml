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
    F.assert_snapshot expected (F.get client expected.Agent_protocol.Snapshot.session.id))
;;

let assert_session_corrupt env fixture session_id =
  F.with_daemon env fixture (fun client ->
    match
      Support.Http_driver.request client (Session_get { session_id; history = None })
    with
    | Error error ->
      F.require
        (Agent_protocol.Error.equal_code error.code Persistence_error
         && String.is_substring error.message ~substring:"Corrupt")
        ("corrupt session returned wrong error: " ^ error.message)
    | Ok _ -> F.fail "complete authoritative corruption was accepted on restart")
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
  assert_restart env fixture expected;
  let recovered = F.read env journal in
  F.require
    (String.is_prefix recovered ~prefix:committed)
    "tail repair rewrote committed journal bytes";
  F.require
    (not (String.is_prefix recovered ~prefix:(committed ^ tail)))
    "tail survived daemon recovery";
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

let assert_startup_rejected env fixture diagnostic =
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
        F.require
          (String.is_substring result.stderr.contents ~substring:diagnostic)
          ("startup omitted expected diagnostic: "
           ^ diagnostic
           ^ "\n"
           ^ result.stderr.contents)))
;;

let test_index_corruption env environment =
  let fixture = F.fixture env environment "recovery-real-index-corrupt" in
  let expected = F.seed env fixture in
  let index = index_path fixture in
  let saved = F.read env index in
  F.write env index "(invalid-session-index";
  assert_startup_rejected env fixture "session index decode failed";
  F.require
    (String.equal (F.read env index) "(invalid-session-index")
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
    (plan.source_version = version && plan.target_version = 1 && plan.session_count = 1)
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
    String.substr_replace_first
      schema
      ~pattern:"(version 1)"
      ~with_:(sprintf "(version %d)" version)
  in
  F.require
    (not (String.equal schema altered))
    "schema mutation did not change the version";
  F.write env schema_path altered
;;

let reject_migration env fixture before version diagnostic =
  let root = Config_fixture.data_dir fixture in
  let applied = run_cli env fixture [ "-migrate-store"; root ] in
  F.require
    (Process_manager.equal_exit applied.exit (Exited 1))
    "unsupported migration apply did not fail closed";
  F.require
    (String.is_substring applied.stderr.contents ~substring:diagnostic)
    "migration apply returned wrong failure";
  assert_readonly env fixture before;
  assert_startup_rejected
    env
    fixture
    (if version > 1 then "Store_schema_too_new" else diagnostic);
  assert_readonly env fixture before
;;

let migration_version env fixture schema version status diagnostic =
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
  reject_migration env fixture before version diagnostic
;;

let malformed_migration env fixture =
  let root = Config_fixture.data_dir fixture in
  List.iter [ "("; ""; "not-a-record"; "((version invalid))" ] ~f:(fun malformed ->
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
        F.require
          (String.is_substring result.stderr.contents ~substring:"Corrupt")
          "malformed migration omitted typed corruption diagnostic";
        F.require
          (not
             (String.is_substring result.stderr.contents ~substring:"Uncaught exception"))
          "malformed migration raised an uncaught exception";
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
    ~version:1
    ~status:Current
    ~mode:Apply;
  assert_readonly env fixture before;
  migration_version env fixture schema 0 Migration_required "Migration_required";
  migration_version env fixture schema 2 Schema_too_new "Schema_too_new";
  malformed_migration env fixture;
  F.write env schema_path schema;
  assert_restart env fixture expected
;;
