open Core
open Agent_server_test_support
open Fixtures
module P = Agent_protocol

let%expect_test
    "failed background completion handler retains its result and never repeats a shell \
     effect after reload"
  =
  let sources =
    List.map Background_shell_tests.sources ~f:(fun (name, source) ->
      match String.equal name "coordinator.chatml" with
      | false -> name, source
      | true ->
        ( name
        , String.substr_replace_all
            source
            ~pattern:"let* delivery = Notification.publish(reference, terminal, wake) in"
            ~with_:
              "let* effect = Tool.call(\"fixture_work\", `Object([])) in\n\
              \      let* delivery = Task.fail(\"completion handler failed after shell \
               effect\") in" ))
  in
  let failed state =
    List.filter state.Agent_session.Session_state.moderator_executions ~f:(fun event ->
      match event.context.phase, event.status with
      | Internal_event, Failed _ -> true
      | _ -> false)
  in
  let wait = Background_shell_tests.wait in
  with_daemon
    ~sources
    ~expect_moderator:true
    ~expected_requests:2
    ~calls:[ "begin", "begin_work", `Object [] ]
    ~after_turn:(fun env handle entry ->
      let state () = A.state entry.actor |> protocol_ok in
      let workspace = (state ()).spec.workspace_instance.canonical_root.native_path in
      let file name = Eio.Path.(Eio.Stdenv.fs env / workspace / name) in
      wait env (fun () -> Eio.Path.is_file (file "fixture-work.started"));
      Eio.Path.save ~create:(`Exclusive 0o600) (file "fixture-work.release") "finish";
      wait env (fun () -> List.length (failed (state ())) = 1);
      let failure_id = (List.hd_exn (failed (state ()))).context.id in
      [%test_eq: string]
        "started\nstarted\n"
        (Eio.Path.load (file "fixture-work.started"));
      let before_sequence = (state ()).counters.event_sequence in
      let submission =
        H.send_message
          handle
          { kind = Plain_text
          ; text = "Continue after the failed handler."
          ; attachments = []
          }
        |> protocol_ok
      in
      wait env (fun () ->
        let current = state () in
        Option.is_none current.active_operation
        && List.is_empty current.conversation.deferred_user_entries);
      let events =
        match
          Agent_session.Durable_event_log.replay
            entry.durable_events
            ~after_sequence:before_sequence
            ~through_sequence:Int64.max_value
        with
        | Available events -> events
        | Snapshot_required -> failwith "lost failure audit"
      in
      let failures =
        List.filter_map events ~f:(fun event ->
          match
            P.Event.Durable.Payload.of_json ~kind:event.kind event.payload |> protocol_ok
          with
          | Operation_failed ({ kind = Turn User_submit; _ } as operation) ->
            Some operation
          | _ -> None)
      in
      (match failures with
       | [ operation ] ->
         Option.iter submission.operation_id ~f:(fun id ->
           assert (P.Id.Operation.equal id operation.id))
       | _ -> failwith "expected one failed user turn after the new submission");
      assert (
        List.exists (state ()).conversation.canonical_history ~f:(fun entry ->
          P.History.Id.equal entry.id submission.history_id));
      H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
      unload_idle_runtime env entry.runtime;
      H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
      for _ = 1 to 10 do
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.02;
        let failures = failed (state ()) in
        [%test_eq: int] 1 (List.length failures);
        assert (
          P.Id.Moderator_execution.equal failure_id (List.hd_exn failures).context.id);
        [%test_eq: string]
          "started\nstarted\n"
          (Eio.Path.load (file "fixture-work.started"))
      done)
    ~settle:Job_launch_tests.settle
    (fun state ->
       [%test_eq: int] 1 (List.length state.jobs);
       let job = List.hd_exn state.jobs in
       [%test_eq: int] 1 job.attempt;
       (match job.delivery, P.Job.terminal_completion job |> protocol_ok with
        | Delivered _, Some (Succeeded _) -> ()
        | _ -> failwith "handler failure lost the original shell result");
       assert (List.is_empty state.deliveries);
       [%test_eq: int] 1 (List.length (failed state));
       print_endline
         "result retained; one failed handler; shell effect did not replay after new \
          input or reload");
  [%expect
    {| result retained; one failed handler; shell effect did not replay after new input or reload |}]
;;
