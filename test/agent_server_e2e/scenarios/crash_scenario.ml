open Core
module F = Crash_recovery_fixture
module Fault = Support.Crash_fault_io
module Temporary_environment = Support.Temporary_environment
module Daemon_process = Support.Daemon_process

let boundary = function
  | "after-create" -> Fault.After_create
  | "after-bytes" -> Fault.After_bytes 3
  | "before-sync" -> Before_sync
  | "after-sync" -> After_sync
  | "before-rename" -> Before_rename
  | "after-rename" -> After_rename
  | "before-directory-sync" | "after-directory-sync" -> Before_directory_sync
  | name -> F.fail ("unknown persistence boundary: " ^ name)
;;

let reached env path =
  Eio.Flow.copy_string ("crash-boundary " ^ path ^ "\n") (Eio.Stdenv.stdout env);
  Eio.Fiber.await_cancel ()
;;

let run_replace_child env name target =
  let matches path =
    String.is_prefix path ~prefix:(target ^ ".tmp-")
    || String.equal path (Filename.dirname target)
  in
  let directory_opened = ref false in
  let on_boundary path =
    if String.equal name "after-directory-sync"
    then directory_opened := true
    else reached env path
  in
  let wrapped = Fault.wrap env ~matches ~boundary:(boundary name) ~reached:on_boundary in
  Agent_store.Durable_file.replace
    ~env:wrapped
    ~durability:Flush_file_and_directory
    ~path:target
    "new-committed-value"
  |> F.store_ok;
  if String.equal name "after-directory-sync" && !directory_opened then reached env target;
  F.fail "replacement returned without reaching the requested crash boundary"
;;

let frame payload =
  match Agent_store.Frame.encode ~max_payload_length:4096 ~flags:0 payload with
  | Ok encoded -> encoded
  | Error error ->
    raise_s [%sexp "frame encode failed", (error : Agent_store.Frame.error)]
;;

let run_journal_child env count directory =
  let target = Filename.concat directory "0000000000000001.log" in
  let wrapped =
    Fault.wrap
      env
      ~matches:(String.equal target)
      ~boundary:(After_bytes (Int.of_string count))
      ~reached:(reached env)
  in
  let segment =
    Agent_store.Journal_segment.open_existing
      ~env:wrapped
      ~directory
      ~id:Agent_store.Journal_segment.Id.first
    |> F.store_ok
  in
  ignore
    (Agent_store.Journal_segment.append
       ~env:wrapped
       ~durability:Flush
       segment
       ~frame:(frame "new-journal-payload")
     |> F.store_ok
     : int64 * int64);
  F.fail "journal append returned without reaching the requested crash boundary"
;;

let run_child env arguments =
  match arguments with
  | [ "replace"; name; target ] -> run_replace_child env name target
  | [ "journal"; count; directory ] -> run_journal_child env count directory
  | [ "generated-create"; root; boundary ] ->
    Crash_generated_creation.run_child env ~root ~boundary ~recover:false
  | [ "generated-recover"; root; boundary ] ->
    Crash_generated_creation.run_child env ~root ~boundary ~recover:true
  | [ "side-effect"; config_path; marker ] ->
    Crash_side_effect_host.run env ~config_path ~marker
  | [ "notification"; config_path; boundary ] ->
    Crash_notification_host.run env ~config_path ~boundary
  | [ "standalone-notification"; config_path; boundary ] ->
    Crash_notification_host.run ~standalone:true env ~config_path ~boundary
  | [ "ingress"; config_path; recover ] ->
    Crash_ingress_host.run env ~config_path ~recover:(Bool.of_string recover)
  | _ -> F.fail "invalid crash child arguments"
;;

let with_killed_child env environment ~case ~arguments check =
  Eio.Switch.run (fun sw ->
    let child = F.child ~sw env environment ~case ~arguments in
    Exn.protect
      ~f:(fun () ->
        F.await_marker env child "crash-boundary ";
        F.kill env child;
        check ())
      ~finally:(fun () -> F.terminate env child))
;;

let assert_replacement env directory target name =
  let after_rename =
    List.mem
      [ "after-rename"; "before-directory-sync"; "after-directory-sync" ]
      name
      ~equal:String.equal
  in
  F.require
    (String.equal
       (F.read env target)
       (if after_rename then "new-committed-value" else "old"))
    "killed replacement violated the exact old-or-new boundary";
  let staged =
    Eio.Path.read_dir (F.path env directory)
    |> List.filter ~f:(String.is_prefix ~prefix:"CURRENT.tmp-")
  in
  if after_rename
  then F.require (List.is_empty staged) "renamed staging file still exists"
  else (
    F.require (List.length staged = 1) "real replacement staging file is missing";
    let expected =
      if String.equal name "after-create"
      then ""
      else if String.equal name "after-bytes"
      then "new"
      else "new-committed-value"
    in
    F.require
      (String.equal
         (F.read env (Filename.concat directory (List.hd_exn staged)))
         expected)
      "staging bytes do not prove the requested persistence boundary")
;;

let test_replace_boundary env environment name =
  let roots = Temporary_environment.roots environment in
  let directory = Filename.concat roots.temporary ("crash-replace-" ^ name) in
  Eio.Path.mkdir ~perm:0o700 (F.path env directory);
  let target = Filename.concat directory "CURRENT" in
  Agent_store.Durable_file.replace
    ~env
    ~durability:Flush_file_and_directory
    ~path:target
    "old"
  |> F.store_ok;
  with_killed_child
    env
    environment
    ~case:"replace"
    ~arguments:[ "replace"; name; target ]
    (fun () -> assert_replacement env directory target name)
;;

let test_atomic_file_boundaries env environment =
  List.iter
    [ "after-create"
    ; "after-bytes"
    ; "before-sync"
    ; "after-sync"
    ; "before-rename"
    ; "after-rename"
    ; "before-directory-sync"
    ; "after-directory-sync"
    ]
    ~f:(test_replace_boundary env environment)
;;

let assert_journal_tail env segment old count =
  F.require
    (String.equal
       (F.read env (Agent_store.Journal_segment.path segment))
       (old ^ String.prefix (frame "new-journal-payload") count))
    "child did not leave the precise journal write prefix";
  let scan =
    Agent_store.Journal_segment.scan ~env ~max_payload_length:4096 segment |> F.store_ok
  in
  F.require scan.crash_tail "killed partial append was not a crash tail";
  F.require
    (List.length scan.entries = 1)
    "partial append invented or lost a complete record";
  Agent_store.Journal_segment.truncate_crash_tail ~env segment scan |> F.store_ok;
  F.require
    (String.equal (F.read env (Agent_store.Journal_segment.path segment)) old)
    "crash-tail repair did not preserve the exact acknowledged bytes"
;;

let test_journal_boundary env environment count =
  let roots = Temporary_environment.roots environment in
  let directory = Filename.concat roots.temporary (sprintf "crash-journal-%d" count) in
  Eio.Path.mkdir ~perm:0o700 (F.path env directory);
  let segment =
    Agent_store.Journal_segment.create_exclusive
      ~env
      ~directory
      ~id:Agent_store.Journal_segment.Id.first
    |> F.store_ok
  in
  let old = frame "acknowledged-old-payload" in
  ignore
    (Agent_store.Journal_segment.append ~env ~durability:Flush segment ~frame:old
     |> F.store_ok
     : int64 * int64);
  with_killed_child
    env
    environment
    ~case:"journal"
    ~arguments:[ "journal"; Int.to_string count; directory ]
    (fun () -> assert_journal_tail env segment old count)
;;

let test_journal_boundaries env environment =
  let length = String.length (frame "new-journal-payload") in
  List.iter [ 3; 20; length - 1 ] ~f:(test_journal_boundary env environment)
;;

let rec await_daemon_kill env daemon =
  match Daemon_process.result daemon with
  | Some result ->
    F.require
      (Support.Process_manager.equal_exit result.exit (Signaled Stdlib.Sys.sigkill))
      "daemon did not die by SIGKILL"
  | None ->
    Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
    await_daemon_kill env daemon
;;

let test_sigkill_committed_session env environment =
  let fixture = F.fixture env environment "crash-sigkill-session" in
  Eio.Switch.run (fun sw ->
    let first = F.start ~sw env fixture in
    Exn.protect
      ~finally:(fun () -> F.stop env first)
      ~f:(fun () ->
        F.wait_ready env first;
        let expected =
          F.with_client ~sw env fixture (fun client ->
            let created = F.create_session client in
            F.get client created.session.id)
        in
        Daemon_process.signal first Stdlib.Sys.sigkill;
        Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 10. (fun () ->
          await_daemon_kill env first);
        F.with_daemon env fixture (fun client ->
          F.assert_snapshot expected (F.get client expected.session.id));
        F.with_daemon env fixture (fun client ->
          F.assert_snapshot expected (F.get client expected.session.id))))
;;

let test_unknown_outcome env _environment =
  Admin_scenario.run env ~case:(Some "idempotency.pending-unknown-outcome")
;;

let test_forced_process env _environment =
  Process_harness_scenario.run env ~case:(Some "process.forced-termination")
;;

let cases =
  [ "durable-file.old-or-new", test_atomic_file_boundaries
  ; "io-failure.atomic-replacement", Persistence_faults.replacements
  ; "io-failure.snapshot-activation", Persistence_faults.snapshots
  ; "io-failure.journal-rotation", Persistence_faults.rotations
  ; "io-failure.commit-writer-fail-closed", Persistence_faults.writers
  ; "journal.partial-write-sigkill", test_journal_boundaries
  ; "sigkill.acknowledged-session", test_sigkill_committed_session
  ; "generated.creation-stage-recovery", Crash_generated_creation.test
  ; "side-effect.unknown-no-replay", Crash_unknown_effect.test
  ; "invocation.admission-publication-no-replay", Crash_invocation_publication.test
  ; "job.committed-intent-launch-once", Crash_queued_launch.test
  ; "notification.wake-no-replay", Crash_notification_wake.test
  ; "notification.standalone-no-replay", Crash_standalone_notification.test
  ; "ingress.lost-ack-no-replay", Crash_ingress_delivery.test
  ; "idempotency.unknown-outcome", test_unknown_outcome
  ; "sigkill.process-supervision", test_forced_process
  ]
;;

let select = function
  | None -> cases
  | Some name ->
    (match List.Assoc.find cases name ~equal:String.equal with
     | Some test -> [ name, test ]
     | None -> F.fail ("unknown crash case: " ^ name))
;;

let run_matrix env case =
  Temporary_environment.with_ ~scenario:"crash-matrix" ~env (fun environment ->
    let selected = select case in
    List.iter selected ~f:(fun (name, test) ->
      try test env environment with
      | exn ->
        let backtrace = Stdlib.Printexc.get_raw_backtrace () in
        let message =
          [%sexp "crash E2E case failed", (name : string), (exn : Exn.t)]
          |> Sexp.to_string_hum
        in
        Exn.raise_with_original_backtrace (Failure message) backtrace);
    Eio.Flow.copy_string
      (Sexp.to_string_hum
         [%sexp
           { scenario = ("crash-matrix" : string)
           ; passed_cases = (List.map selected ~f:fst : string list)
           }]
       ^ "\n")
      (Eio.Stdenv.stdout env))
;;

let run env ~case =
  match case with
  | Some name when String.is_prefix name ~prefix:"child." ->
    let arguments =
      Sys.getenv_exn "OCHAT_E2E_CRASH_ARGUMENTS"
      |> Sexp.of_string
      |> [%of_sexp: string list]
    in
    (* The selected fault is test control, not a change to the host environment
       delegated to tools. Keep captured authority identical on recovery. *)
    Core_unix.unsetenv "OCHAT_E2E_CRASH_ARGUMENTS";
    run_child env arguments
  | _ -> run_matrix env case
;;
