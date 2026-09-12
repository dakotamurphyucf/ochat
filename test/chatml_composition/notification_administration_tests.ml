open Core
open Fixtures
open Agent_server_test_support
module P = Agent_protocol

type replacement =
  | Reset_keep
  | Reset_drop
  | Rebuild
[@@deriving sexp_of]

let frames state =
  List.filter
    state.Agent_session.Session_state.conversation.canonical_history
    ~f:(fun entry ->
      match entry.P.History.provenance with
      | Runtime_notification _ -> true
      | _ -> false)
;;

let export_snapshot handle revision =
  let exported = H.export handle ~format:Json ~revision:(Some revision) |> protocol_ok in
  [%test_eq: int64] revision exported.session_revision;
  let bytes = Buffer.create 4096 in
  H.download_blob handle ~blob:exported.blob ~output:(Eio.Flow.buffer_sink bytes)
  |> protocol_ok;
  Buffer.contents bytes
  |> Jsonaf.of_string
  |> Jsonaf.member_exn "snapshot"
  |> P.Snapshot.of_json
  |> protocol_ok
;;

let%expect_test
    "reset and rebuild retain historical result references without reviving old work"
  =
  List.iter
    [ Reset_keep, false; Reset_keep, true; Reset_drop, true; Rebuild, true ]
    ~f:(fun (replacement, artifact) ->
      let client = ref None in
      let limits = Agent_server.Daemon.default_options.factory_limits in
      with_daemon
        ~sources:
          (Standalone_result_reference_tests.sources
             ~reject:false
             ~failed:false
             ~repetitions:(if artifact then 2048 else 64))
        ~factory_limits:
          { limits with
            notifications = { limits.notifications with max_payload_bytes = 2048 }
          }
        ~runtime_policy:
          { Chat_response.Runtime_semantics.default_policy with
            honor_request_turn = false
          }
        ~connect:(fun ~sw:_ ~env:_ ~root:_ daemon ->
          let value = connection daemon (principal ()) in
          client := Some value;
          value)
        ~calls:[ "begin", "begin_work", `Object [] ]
        ~after_turn:(fun env handle entry ->
          let state () = A.state entry.actor |> protocol_ok in
          let file name =
            Eio.Path.(
              Eio.Stdenv.fs env
              / (state ()).spec.workspace_instance.canonical_root.native_path
              / name)
          in
          let wait = Background_shell_tests.wait env in
          wait (fun () -> Eio.Path.is_file (file "fixture-work.started"));
          Eio.Path.save ~create:(`Exclusive 0o600) (file "fixture-work.release") "finish";
          wait (fun () ->
            let current = state () in
            Option.is_none current.active_operation
            &&
            match current.deliveries with
            | [ { status = Committed _; wake_disposition = Some (Discarded_wake _); _ } ]
              -> true
            | _ -> false);
          H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
          let before = state () in
          let job = List.hd_exn before.jobs in
          let delivery = List.hd_exn before.deliveries in
          let reference =
            Option.value_exn
              (Option.value_exn delivery.completion_projection).result_reference
          in
          [%test_eq: bool] artifact (Option.is_some reference.artifact);
          let original =
            P.Job.terminal_completion
              job
              ~load_artifact:
                (Background_artifact_tests.read_artifact (Option.value_exn !client))
            |> protocol_ok
            |> Option.value_exn
          in
          let current = state () in
          (match replacement with
           | Reset_keep | Reset_drop ->
             H.reset
               handle
               ~expected_revision:current.counters.revision
               ~keep_history:
                 (match replacement with
                  | Reset_keep -> true
                  | _ -> false)
               ~keep_tasks:false
               ~keep_cache:true
               ~keep_workspace:true
               ~keep_grants:true
               ~keep_labels:true
           | Rebuild ->
             H.rebuild
               handle
               ~expected_revision:current.counters.revision
               ~prompt_choice:Pinned)
          |> protocol_ok
          |> ignore;
          let replaced = state () in
          [%test_eq: int] (before.identity.generation + 1) replaced.identity.generation;
          assert (
            List.is_empty replaced.jobs
            && List.is_empty replaced.invocations
            && List.is_empty replaced.deliveries
            && List.is_empty replaced.subscriptions);
          (match replacement with
           | Reset_keep ->
             assert (List.equal P.History.equal_entry (frames before) (frames replaced))
           | Reset_drop | Rebuild -> assert (List.is_empty (frames replaced)));
          let archive = List.hd_exn replaced.conversation.compaction_archives in
          (match replacement, archive.kind with
           | (Reset_keep | Reset_drop), Reset | Rebuild, Rebuild -> ()
           | _ -> failwith "wrong administrative archive kind");
          let archived =
            Agent_session.Compaction_archive.read
              ~env
              ~handle:(Option.value_exn entry.store_handle)
              ~max_payload_length:(16 * 1024 * 1024)
              archive
            |> protocol_ok
          in
          [%test_eq: int] before.identity.generation archived.identity.generation;
          assert (
            Jsonaf.exactly_equal
              (P.Job.to_json job)
              (P.Job.to_json (List.hd_exn archived.jobs)));
          assert (P.Delivery.equal delivery (List.hd_exn archived.deliveries));
          let export = export_snapshot handle archive.revision in
          [%test_eq: int] 1 (List.length export.jobs);
          let historical_job = List.hd_exn export.jobs in
          assert (Jsonaf.exactly_equal (P.Job.to_json job) (P.Job.to_json historical_job));
          P.Job_result_reference.validate_job reference historical_job |> protocol_ok;
          let exported_completion =
            P.Job.terminal_completion
              historical_job
              ~load_artifact:
                (Background_artifact_tests.read_artifact (Option.value_exn !client))
            |> protocol_ok
            |> Option.value_exn
          in
          assert (P.Completion.equal original exported_completion);
          unload_idle_runtime env entry.runtime;
          assert (Option.is_some (entry.collect_results () |> protocol_ok));
          Option.iter reference.artifact ~f:(fun reference ->
            let reread =
              Background_artifact_tests.read_artifact (Option.value_exn !client) reference
              |> protocol_ok
            in
            assert (P.Completion.equal original reread));
          H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
          Agent_server.Runtime_owner.drain_idle_moderator entry.runtime
          |> protocol_ok
          |> ignore;
          let restarted = state () in
          assert (
            List.is_empty restarted.jobs
            && List.is_empty restarted.deliveries
            && Option.is_none restarted.active_operation);
          assert (List.equal P.History.equal_entry (frames replaced) (frames restarted));
          let restored =
            Agent_session.Session_state.sexp_of_t restarted
            |> Sexp.to_string_mach
            |> Agent_session.Session_persistence.restore_snapshot
            |> Result.map_error ~f:Agent_store.Store_error.to_protocol_error
            |> protocol_ok
          in
          assert (not (Agent_session.Notification_delivery.has_idle_work restored));
          [%test_eq: string] "started\n" (Eio.Path.load (file "fixture-work.started"));
          print_s
            [%sexp
              (replacement : replacement)
            , (artifact : bool)
            , "historical result readable; no new-generation replay"])
        ~settle:Job_launch_tests.settle
        (fun _ -> ()));
  [%expect
    {|
    (Reset_keep false "historical result readable; no new-generation replay")
    (Reset_keep true "historical result readable; no new-generation replay")
    (Reset_drop true "historical result readable; no new-generation replay")
    (Rebuild true "historical result readable; no new-generation replay")
    |}]
;;
