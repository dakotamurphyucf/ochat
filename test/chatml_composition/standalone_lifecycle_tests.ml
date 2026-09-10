open Core
open Fixtures
open Agent_server_test_support
module P = Agent_protocol

let%expect_test
    "stopped native session reaps its shell and delivers retained cancellation once \
     after runtime reload"
  =
  with_daemon
    ~sources:(Standalone_notification_tests.sources ~reject:false)
    ~calls:[ "begin", "begin_work", `Object [] ]
    ~expected_requests:3
    ~inspect_request:(fun request inputs ->
      match request with
      | 3 ->
        let notifications =
          List.filter_map inputs ~f:(function
            | Openai.Responses.Item.Input_message
                { role = User; content = Text { text; _ } :: _; _ }
              when String.is_prefix text ~prefix:"Ochat runtime notification." ->
              Some text
            | _ -> None)
        in
        [%test_eq: int] 1 (List.length notifications);
        assert (String.is_substring (List.hd_exn notifications) ~substring:"cancelled")
      | _ -> ())
    ~after_turn:(fun env handle entry ->
      let state () = A.state entry.actor |> protocol_ok in
      let workspace = (state ()).spec.workspace_instance.canonical_root.native_path in
      let file name = Eio.Path.(Eio.Stdenv.fs env / workspace / name) in
      Background_shell_tests.wait env (fun () ->
        Eio.Path.is_file (file "fixture-work.pid")
        && Eio.Path.is_file (file "fixture-work.started"));
      let pid =
        Eio.Path.load (file "fixture-work.pid") |> String.strip |> Pid.of_string
      in
      H.stop handle ~mode:Cancel |> protocol_ok |> ignore;
      Background_shell_tests.wait env (fun () ->
        match
          Eio_unix.run_in_systhread (fun () -> Signal_unix.send Signal.zero (`Pid pid))
        with
        | `No_such_process -> true
        | `Ok -> false);
      let stopped = state () in
      assert (Option.is_none stopped.moderator && Option.is_none stopped.active_operation);
      (match stopped.lifecycle.desired with
       | Stopped -> ()
       | _ -> failwith "cancel did not stop native session");
      let job = List.hd_exn stopped.jobs in
      (match P.Job.terminal_completion job |> protocol_ok with
       | Some (Cancelled _) -> ()
       | _ -> failwith "stopped shell did not retain its cancellation");
      [%test_eq: int]
        0
        (List.count stopped.conversation.canonical_history ~f:(fun entry ->
           match entry.P.History.provenance with
           | Runtime_notification _ -> true
           | _ -> false));
      Agent_server.Runtime_owner.drain_idle_moderator entry.runtime
      |> protocol_ok
      |> ignore;
      [%test_eq: Sexp.t]
        (Agent_session.Session_state.sexp_of_t stopped)
        (Agent_session.Session_state.sexp_of_t (state ()));
      Agent_server.Runtime_owner.unload entry.runtime |> protocol_ok;
      H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
      Background_shell_tests.wait env (fun () ->
        let current = state () in
        Option.is_none current.active_operation
        &&
        match current.deliveries with
        | [ { status = Committed _; wake_disposition = Some (Accepted_wake _); _ } ] ->
          true
        | _ -> false);
      let resumed = state () in
      let retained = List.hd_exn resumed.jobs in
      assert (P.Id.Job.equal job.id retained.id);
      [%test_eq: Sexp.t]
        (P.Job.sexp_of_status job.status)
        (P.Job.sexp_of_status retained.status);
      [%test_eq: int] job.attempt retained.attempt;
      let delivery = List.hd_exn resumed.deliveries in
      (match delivery.context.completion, delivery.context.wake with
       | Cancelled _, Request_turn -> ()
       | _ -> failwith "native cancellation did not retain its requested notification");
      H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
      Agent_server.Runtime_owner.unload entry.runtime |> protocol_ok;
      H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
      Agent_server.Runtime_owner.drain_idle_moderator entry.runtime
      |> protocol_ok
      |> ignore;
      let restored = state () in
      [%test_eq: int] 1 (List.length restored.deliveries);
      assert (P.Delivery.equal delivery (List.hd_exn restored.deliveries));
      [%test_eq: string] "started\n" (Eio.Path.load (file "fixture-work.started")))
    ~settle:Job_launch_tests.settle
    (fun state ->
       [%test_eq: int]
         1
         (List.count state.conversation.canonical_history ~f:(fun entry ->
            match entry.P.History.provenance with
            | Runtime_notification _ -> true
            | _ -> false));
       print_endline
         "shell reaped; stopped data held; cancellation delivered once; no replay");
  [%expect {| shell reaped; stopped data held; cancellation delivered once; no replay |}]
;;
