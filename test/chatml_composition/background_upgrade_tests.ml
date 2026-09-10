open Core
open Fixtures
open Agent_server_test_support
module P = Agent_protocol

let%expect_test
    "changed standalone publisher retires undeliverable completion without changing its \
     result"
  =
  let host = ref None in
  with_daemon
    ~sources:(Standalone_notification_tests.sources ~reject:false)
    ~calls:[ "begin", "begin_work", `Object [] ]
    ~connect:(fun ~sw:_ ~env:_ ~root daemon ->
      let client = connection daemon (principal ()) in
      host := Some (root, daemon, client);
      client)
    ~after_turn:(fun env handle entry ->
      let state () = A.state entry.actor |> protocol_ok in
      let workspace = (state ()).spec.workspace_instance.canonical_root.native_path in
      let file name = Eio.Path.(Eio.Stdenv.fs env / workspace / name) in
      Background_shell_tests.wait env (fun () ->
        Eio.Path.is_file (file "fixture-work.started"));
      H.stop handle ~mode:Cancel |> protocol_ok |> ignore;
      let before = state () in
      assert (List.is_empty before.deliveries);
      let job = List.hd_exn before.jobs in
      (match job.status, job.delivery with
       | Cancelled, Pending -> ()
       | _ -> failwith "fixture missed terminal-before-intent boundary");
      Retired_delivery_checks.check_storage before job;
      Retired_delivery_checks.check_actor
        env
        before
        (Completion_contract_tests.capabilities entry);
      let root, daemon, client = Option.value_exn !host in
      let source_path = Eio.Path.(Eio.Stdenv.fs env / root / "begin.chatml") in
      let source = Eio.Path.load source_path in
      Prompt_upgrade_fixtures.replace
        env
        ~root
        ~daemon
        ~client
        ~handle
        ~entry
        ~filename:"begin.chatml"
        ~source:(source ^ "\nlet replacement_revision = 2\n")
        ~allow_migration:false;
      let upgraded = state () in
      assert (List.is_empty upgraded.deliveries);
      (match
         Agent_server.Runtime_owner.deliver_background_job_completion entry.runtime job
       with
       | Ok () -> ()
       | Error error ->
         raise_s
           [%sexp
             "delivery failure has no durable disposition"
           , (error : P.Error.t)
           , ((List.hd_exn (state ()).jobs).delivery : P.Job.delivery)]);
      let retired = state () in
      let retained = List.hd_exn retired.jobs in
      (match retained.delivery with
       | Discarded { reason = Authority_changed; _ } -> ()
       | _ -> failwith "undeliverable job has no durable authority-change disposition");
      assert (
        Jsonaf.exactly_equal
          (P.Job.to_json { job with delivery = retained.delivery })
          (P.Job.to_json retained));
      assert (List.is_empty retired.deliveries);
      Background_shell_tests.wait env (fun () ->
        let snapshot = H.projection handle |> Agent_client.Projection.snapshot in
        List.exists snapshot.jobs ~f:(fun visible ->
          P.Id.Job.equal visible.id retained.id
          && P.Job.equal_delivery visible.delivery retained.delivery));
      let exported = H.export handle ~format:Json ~revision:None |> protocol_ok in
      let bytes = Buffer.create 1024 in
      H.download_blob handle ~blob:exported.blob ~output:(Eio.Flow.buffer_sink bytes)
      |> protocol_ok;
      let snapshot =
        Buffer.contents bytes
        |> Jsonaf.of_string
        |> Jsonaf.member_exn "snapshot"
        |> P.Snapshot.of_json
        |> protocol_ok
      in
      assert (
        Jsonaf.exactly_equal
          (P.Job.to_json retained)
          (P.Job.to_json (List.hd_exn snapshot.jobs)));
      Agent_server.Runtime_owner.unload entry.runtime |> protocol_ok;
      H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
      let restored = state () in
      assert (
        Jsonaf.exactly_equal
          (P.Job.to_json retained)
          (P.Job.to_json (List.hd_exn restored.jobs)));
      [%test_eq: string] "started\n" (Eio.Path.load (file "fixture-work.started"));
      print_endline "result retained; unavailable delivery retired; no replay")
    ~settle:Job_launch_tests.settle
    (fun _ -> ());
  [%expect
    {|
    retirement survives storage; replay cannot revive or rewrite the job
    publisher/dependency revocation: stale and failed saves preserve pending work
    result retained; unavailable delivery retired; no replay
    |}]
;;
