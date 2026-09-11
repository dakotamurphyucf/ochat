open Core
open Fixtures
open Agent_server_test_support
module P = Agent_protocol

type finish =
  | Success
  | Cancel_job
  | Stop_session
[@@deriving sexp_of]

let sources =
  [ ( "agent.chatmd"
    , [%blob "../chatml_extensibility_fixtures/x03-background-shell/agent.chatmd"] )
  ; ( "coordinator.chatml"
    , [%blob "../chatml_extensibility_fixtures/x03-background-shell/coordinator.chatml"] )
  ; "work.sh", [%blob "../chatml_extensibility_fixtures/x03-background-shell/work.sh"]
  ; ( "input.json"
    , [%blob "../chatml_extensibility_fixtures/x03-background-shell/input.json"] )
  ; ( "accepted.json"
    , [%blob "../chatml_extensibility_fixtures/x03-background-shell/accepted.json"] )
  ]
;;

let wait env condition =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 5. (fun () ->
    let rec loop () =
      match condition () with
      | true -> ()
      | false ->
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
        loop ()
    in
    loop ())
;;

let%expect_test
    "X03 shell jobs acknowledge promptly, allow another turn and cancel the actual \
     process"
  =
  List.iter [ Success; Cancel_job; Stop_session ] ~f:(fun finish ->
    let worker_pid = ref None in
    let received = ref false in
    with_daemon
      ~sources
      ~expect_moderator:true
      ~expected_requests:
        (match finish with
         | Success -> 4
         | Cancel_job | Stop_session -> 3)
      ~inspect_request:(fun request inputs ->
        match request with
        | 4 ->
          [%test_eq: int]
            1
            (List.count inputs ~f:(function
               | Openai.Responses.Item.Input_message
                   { role = User; content = Text { text; _ } :: _; _ } ->
                 String.is_prefix text ~prefix:"Ochat runtime notification."
               | _ -> false));
          received := true
        | _ -> ())
      ~calls:[ "begin", "begin_work", `Object [] ]
      ~after_turn:(fun env handle entry ->
        let state = A.state entry.actor |> protocol_ok in
        let job_id =
          match result state "begin" with
          | Pending (Job id, _) -> id
          | other -> raise_s [%sexp (other : I.outcome)]
        in
        let workspace = state.spec.workspace_instance.canonical_root.native_path in
        let file name = Eio.Path.(Eio.Stdenv.fs env / workspace / name) in
        wait env (fun () ->
          let state = A.state entry.actor |> protocol_ok in
          let job =
            List.find_exn state.jobs ~f:(fun job -> P.Id.Job.equal job.id job_id)
          in
          (match P.Job.terminal_completion job |> protocol_ok with
           | None -> ()
           | Some completion ->
             raise_s
               [%sexp
                 "shell completed before fixture startup", (completion : P.Completion.t)]);
          Eio.Path.is_file (file "fixture-work.started"));
        worker_pid
        := Some (Eio.Path.load (file "fixture-work.pid") |> String.strip |> Pid.of_string);
        let assert_running () =
          let state = A.state entry.actor |> protocol_ok in
          let job =
            List.find_exn state.jobs ~f:(fun job -> P.Id.Job.equal job.id job_id)
          in
          match job.status with
          | Running -> ()
          | other -> raise_s [%sexp "shell was not running", (other : P.Job.status)]
        in
        assert_running ();
        H.send_message
          handle
          { kind = Plain_text
          ; text = "Answer another input while the work runs."
          ; attachments = []
          }
        |> protocol_ok
        |> ignore;
        wait env (fun () ->
          let state = A.state entry.actor |> protocol_ok in
          Option.is_none state.active_operation
          && List.is_empty state.conversation.deferred_user_entries);
        assert_running ();
        (match finish with
         | Success ->
           Eio.Path.save ~create:(`Exclusive 0o600) (file "fixture-work.release") "finish"
         | Cancel_job ->
           A.cancel_job_internal entry.actor ~job_id |> protocol_ok |> ignore
         | Stop_session ->
           H.stop handle ~mode:Cancel |> protocol_ok |> ignore;
           let restarted = H.start handle ~queue_if_limited:false |> protocol_ok in
           assert (observed_idle restarted.observed_state));
        wait env (fun () ->
          match
            Eio_unix.run_in_systhread (fun () ->
              Signal_unix.send Signal.zero (`Pid (Option.value_exn !worker_pid)))
          with
          | `No_such_process -> true
          | `Ok -> false);
        [%test_eq: string] "started\n" (Eio.Path.load (file "fixture-work.started")))
      ~settle:(fun env entry ->
        Job_launch_tests.settle env entry;
        let await_delivery () =
          wait env (fun () ->
            let state =
              A.state entry.Agent_server.Session_registry.actor |> protocol_ok
            in
            let model_ready =
              match finish with
              | Success -> !received
              | Cancel_job | Stop_session -> true
            in
            model_ready
            && Option.is_none state.active_operation
            && List.length state.deliveries = 1
            && List.for_all state.deliveries ~f:(fun delivery ->
              match delivery.status with
              | Committed _ -> true
              | _ -> false))
        in
        match await_delivery () with
        | () -> ()
        | exception Eio.Time.Timeout ->
          let state = A.state entry.actor |> protocol_ok in
          raise_s
            [%sexp
              "background notification did not settle"
            , (finish : finish)
            , (List.map state.jobs ~f:(fun job ->
                 ( job.status
                 , job.delivery
                 , Agent_session.Background_job_event.source ~state job ))
               : (P.Job.status
                 * P.Job.delivery
                 * (P.Invocation.observer option, P.Error.t) result)
                   list)
            , (state.deliveries : P.Delivery.t list)
            , (List.map state.moderator_executions ~f:(fun event ->
                 event.context.phase, event.status, event.retirement)
               : (P.Moderator_execution.phase
                 * P.Moderator_execution.status
                 * P.Moderator_execution.retirement option)
                   list)])
      (fun state ->
         let invocation = model_invocation state "begin" in
         let id =
           match invocation.status with
           | Published (Pending (Job id, _)) -> id
           | other -> raise_s [%sexp (other : I.status)]
         in
         [%test_eq: int] 1 (List.length state.jobs);
         let job = List.hd_exn state.jobs in
         assert (P.Id.Job.equal job.id id);
         [%test_eq: int] 1 job.attempt;
         let delivery = List.hd_exn state.deliveries in
         assert (
           Option.equal P.Invocation.equal_work delivery.context.work (Some (Job id)));
         let acknowledgement = Option.value_exn invocation.output_entry_id in
         let history = state.conversation.canonical_history in
         let ack_index, _ =
           List.findi_exn history ~f:(fun _ entry ->
             P.History.Id.equal entry.id acknowledgement)
         in
         let notification_index, _ =
           List.findi_exn history ~f:(fun _ entry ->
             match entry.P.History.provenance with
             | Runtime_notification _ -> true
             | _ -> false)
         in
         assert (ack_index < notification_index);
         [%test_eq: int]
           1
           (List.count history ~f:(fun entry ->
              match
                Agent_session.History_codec.of_protocol entry
                |> protocol_ok
                |> History_entry.item
              with
              | Openai.Responses.Item.Function_call_output { call_id = "begin"; _ } ->
                true
              | _ -> false));
         let completion = P.Job.terminal_completion job |> protocol_ok in
         (match finish, completion with
          | Success, Some (Succeeded (`String text)) ->
            let result = Shell_runtime.Result.t_of_jsonaf (Jsonaf.of_string text) in
            assert (Shell_runtime.Result.equal_status result.status (Exited 0));
            [%test_eq: string] "{\"fixture\":\"complete\"}\n" result.stdout;
            [%test_eq: string] "fixture diagnostic\n" result.stderr;
            assert ((not result.stdout_truncated) && not result.stderr_truncated)
          | (Cancel_job | Stop_session), Some (Cancelled _) -> ()
          | _, other -> raise_s [%sexp (other : P.Completion.t option)]);
         print_s
           [%sexp
             (finish : finish)
           , "one owned attempt; responsive agent; process reaped; external marker \
              retained"]));
  [%expect
    {|
    (Success
     "one owned attempt; responsive agent; process reaped; external marker retained")
    (Cancel_job
     "one owned attempt; responsive agent; process reaped; external marker retained")
    (Stop_session
     "one owned attempt; responsive agent; process reaped; external marker retained")
    |}]
;;

let%expect_test "X03 restart preserves an interrupted shell mutation without reexecution" =
  let module Background = Background_fixtures in
  let original_pid = ref None in
  Background.with_background_daemon
    ~agent:(List.Assoc.find_exn sources "agent.chatmd" ~equal:String.equal)
    ~sources:
      (List.filter sources ~f:(fun (name, _) -> not (String.equal name "agent.chatmd")))
    ~check_restored:(fun before after ->
      assert (P.Id.Job.equal before.id after.id);
      [%test_eq: int] before.attempt after.attempt)
    ~after_recovery:(fun env client entry before ->
      let parent =
        List.find_exn before.jobs ~f:(fun job ->
          match job.status with
          | Waiting_completion _ -> true
          | _ -> false)
      in
      let _, completion = Background.await env client parent in
      (match completion with
       | Failed { code = "background.interrupted"; retryable = false; _ } -> ()
       | other -> raise_s [%sexp (other : P.Completion.t)]);
      let state = A.state entry.actor |> protocol_ok in
      [%test_eq: int] 2 (List.length state.jobs);
      [%test_eq: int] (List.length before.invocations) (List.length state.invocations);
      let workspace = state.spec.workspace_instance.canonical_root.native_path in
      let file name = Eio.Path.(Eio.Stdenv.fs env / workspace / name) in
      [%test_eq: string] "started\n" (Eio.Path.load (file "fixture-work.started"));
      let pid =
        Eio.Path.load (file "fixture-work.pid") |> String.strip |> Pid.of_string
      in
      assert (Pid.equal pid (Option.value_exn !original_pid));
      (match
         Eio_unix.run_in_systhread (fun () -> Signal_unix.send Signal.zero (`Pid pid))
       with
       | `No_such_process -> ()
       | `Ok -> failwith "interrupted shell process survived daemon shutdown");
      print_endline
        "restart retained the mutation and original attempt, reported interruption, and \
         did not replay work")
    (fun env _client entry capabilities ->
       let request = Background.tool capabilities "begin_work" (`Object []) in
       let parent =
         Background.submit entry (Chat_response.Background_request.to_json request)
       in
       let dependency =
         Background_pending_restart_tests.waiting env entry.actor parent.id
       in
       let state = A.state entry.actor |> protocol_ok in
       let workspace = state.spec.workspace_instance.canonical_root.native_path in
       let file name = Eio.Path.(Eio.Stdenv.fs env / workspace / name) in
       wait env (fun () -> Eio.Path.is_file (file "fixture-work.started"));
       original_pid
       := Some (Eio.Path.load (file "fixture-work.pid") |> String.strip |> Pid.of_string);
       let child =
         List.find_exn (A.state entry.actor |> protocol_ok).jobs ~f:(fun job ->
           P.Invocation.equal_work (Job job.id) dependency.work)
       in
       match child.status with
       | Running -> ()
       | other -> raise_s [%sexp (other : P.Job.status)]);
  [%expect
    {| restart retained the mutation and original attempt, reported interruption, and did not replay work |}]
;;
