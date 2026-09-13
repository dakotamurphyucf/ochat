open Core
open Fixtures
open Agent_server_test_support
module P = Agent_protocol

let%expect_test "moderator replacement retires completion owned by the previous source" =
  List.iter [ false; true ] ~f:(fun remove ->
    let host = ref None in
    with_daemon
      ~sources:Background_shell_tests.sources
      ~expect_moderator:true
      ~calls:[ "begin", "begin_work", `Object [] ]
      ~connect:(fun ~sw:_ ~env:_ ~root daemon ->
        let client = connection daemon (principal ()) in
        host := Some (root, daemon, client);
        client)
      ~after_turn:(fun env handle entry ->
        let state () = A.state entry.actor |> protocol_ok in
        let workspace = (state ()).spec.workspace_instance.canonical_root.native_path in
        let marker = Eio.Path.(Eio.Stdenv.fs env / workspace / "fixture-work.started") in
        Background_shell_tests.wait env (fun () -> Eio.Path.is_file marker);
        H.stop handle ~mode:Cancel |> protocol_ok |> ignore;
        let before = state () in
        let job = List.hd_exn before.jobs in
        (match job.status, job.delivery with
         | Cancelled, Pending -> ()
         | _ -> failwith "missed terminal-before-moderator-delivery boundary");
        let root, daemon, client = Option.value_exn !host in
        let filename, source =
          match remove with
          | false ->
            ( "coordinator.chatml"
            , List.Assoc.find_exn
                Background_shell_tests.sources
                ~equal:String.equal
                "coordinator.chatml"
              ^ "\nlet replacement_revision = 2\n" )
          | true ->
            ( "agent.chatmd"
            , "<developer>The background coordinator was removed.</developer>" )
        in
        Prompt_upgrade_fixtures.replace
          env
          ~root
          ~daemon
          ~client
          ~handle
          ~entry
          ~filename
          ~source
          ~allow_migration:true;
        Moderator_retirement_checks.check env ~before ~upgraded:(state ());
        (match
           Agent_server.Runtime_owner.deliver_background_job_completion entry.runtime job
         with
         | Ok () -> ()
         | Error error ->
           raise_s
             [%sexp "obsolete moderator delivery has no disposition", (error : P.Error.t)]);
        let retired = state () in
        let retained = List.hd_exn retired.jobs in
        (match retained.delivery with
         | Discarded { reason = Authority_changed; _ } -> ()
         | _ -> failwith "obsolete moderator completion remains pending");
        assert (
          Jsonaf.exactly_equal
            (P.Job.to_json { job with delivery = retained.delivery })
            (P.Job.to_json retained));
        assert (List.is_empty retired.deliveries);
        unload_idle_runtime env entry.runtime;
        H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
        Agent_server.Runtime_owner.drain_idle_moderator entry.runtime
        |> protocol_ok
        |> ignore;
        let restored = state () in
        assert (
          Jsonaf.exactly_equal
            (P.Job.to_json retained)
            (P.Job.to_json (List.hd_exn restored.jobs)));
        assert (List.is_empty restored.deliveries);
        [%test_eq: string] "started\n" (Eio.Path.load marker);
        print_s
          [%sexp (remove : bool), "old result preserved; no delivery to replacement"])
      ~settle:Job_launch_tests.settle
      (fun _ -> ()));
  [%expect
    {|
    (false "old result preserved; no delivery to replacement")
    (true "old result preserved; no delivery to replacement")
    |}]
;;
