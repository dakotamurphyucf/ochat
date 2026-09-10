open Core
open Fixtures
open Agent_server_test_support
module P = Agent_protocol

let revoked_projection
      entry
      (state : Agent_session.Session_state.t)
      (delivery : P.Delivery.t)
  =
  let module C = Chat_response.Tool_capability in
  let current = Completion_contract_tests.capabilities entry in
  let pending =
    P.Delivery.create
      ~disclosure_pins:(Option.value_exn delivery.disclosure_pins)
      ~completion_projection:(Option.value_exn delivery.completion_projection)
      delivery.context
    |> protocol_ok
  in
  List.iter [ "begin_work"; "fixture_work" ] ~f:(fun removed ->
    let names =
      C.references current
      |> List.filter_map ~f:(fun reference ->
        Option.some_if (not (String.equal reference.C.name removed)) reference.name)
    in
    let narrowed =
      C.select current ~names
      |> Result.map_error ~f:(fun error -> error.C.message)
      |> Result.ok_or_failwith
    in
    let proposal =
      Agent_session.Notification_delivery.prepare_for_runtime
        ~state:{ state with deliveries = [ pending ] }
        ~source:None
        ~current_capabilities:narrowed
        ~policy:Chat_response.One_off_request.default_policy
        ~max_count:16
      |> protocol_ok
    in
    (match proposal.actions with
     | [ Fail { status = Failed error; _ } ] ->
       [%test_eq: string] "notification.disclosure_denied" error.code
     | _ -> failwith "revoked standalone data was eligible for publication");
    let history_id, at =
      match delivery.status with
      | Committed { history_id; at } -> history_id, at
      | _ -> assert false
    in
    let recovered =
      P.Delivery.commit ~track_wake:true pending ~history_id ~now:at |> protocol_ok
    in
    let proposal =
      Agent_session.Notification_delivery.prepare_idle_for_runtime
        ~state:{ state with deliveries = [ recovered ] }
        ~source:None
        ~current_capabilities:narrowed
        ~policy:Chat_response.One_off_request.default_policy
        ~max_count:16
      |> protocol_ok
    in
    assert (List.is_empty proposal.wakes);
    [%test_eq: int] 1 (List.length proposal.discarded_wakes))
;;

let sources ~reject =
  let original =
    List.Assoc.find_exn Background_shell_tests.sources ~equal:String.equal "agent.chatmd"
  in
  let prefix =
    String.substr_index_exn original ~pattern:{|<script id="coordinator"|}
    |> String.prefix original
  in
  let agent =
    prefix
    ^ {|<script id="begin" language="chatml" kind="tool" src="begin.chatml"/>
<tool name="begin_work" type="chatml" script="begin" entrypoint="run" input_schema="input.json" output_schema="accepted.json" completion_schema="completion.json">
<uses tool="fixture_work"/>
</tool>|}
  in
  [ "agent.chatmd", agent
  ; ( "begin.chatml"
    , {|let run ctx input =
  let* job = Job.start_tool("fixture_work", input) in
  Task.pure(`Pending(`Job(job), `Object([
    {key = "job_id"; value = `String(job)},
    {key = "status"; value = `String("accepted")}
  ])))|}
    )
  ; ("completion.json", if reject then "false" else "true")
  ]
  @ List.filter Background_shell_tests.sources ~f:(fun (name, _) ->
    not (List.mem [ "agent.chatmd"; "coordinator.chatml" ] name ~equal:String.equal))
;;

let%expect_test
    "source-free standalone completion publishes once, wakes under policy and survives \
     reload"
  =
  List.iter
    [ false, true; true, true; false, false ]
    ~f:(fun (reject, wake) ->
      let received = ref false in
      with_daemon
        ~sources:(sources ~reject)
        ~runtime_policy:
          { Chat_response.Runtime_semantics.default_policy with
            honor_request_turn = wake
          }
        ~calls:[ "begin", "begin_work", `Object [] ]
        ~expected_requests:(if wake then 3 else 2)
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
            let text = List.hd_exn notifications in
            [%test_eq: bool]
              reject
              (String.is_substring text ~substring:"background.invalid_completion");
            assert (
              not (reject && String.is_substring text ~substring:"fixture diagnostic"));
            received := true
          | _ -> ())
        ~after_turn:(fun env handle entry ->
          let state = A.state entry.actor |> protocol_ok in
          assert (Option.is_none state.moderator);
          let workspace = state.spec.workspace_instance.canonical_root.native_path in
          let file name = Eio.Path.(Eio.Stdenv.fs env / workspace / name) in
          Background_shell_tests.wait env (fun () ->
            Eio.Path.is_file (file "fixture-work.started"));
          assert (List.is_empty (A.state entry.actor |> protocol_ok).deliveries);
          Eio.Path.save ~create:(`Exclusive 0o600) (file "fixture-work.release") "finish";
          Background_shell_tests.wait env (fun () ->
            let state = A.state entry.actor |> protocol_ok in
            Option.is_none state.active_operation
            && ((not wake) || !received)
            &&
            match state.deliveries with
            | [ { status = Committed _
                ; wake_disposition = Some (Accepted_wake _ | Discarded_wake _)
                ; _
                }
              ] -> true
            | _ -> false);
          let saved = A.state entry.actor |> protocol_ok in
          let delivery = List.hd_exn saved.deliveries in
          (match reject, wake with
           | false, true -> revoked_projection entry saved delivery
           | _ -> ());
          [%test_eq: bool]
            reject
            (Option.value_exn delivery.completion_projection).rejected;
          (match wake, delivery.wake_disposition with
           | true, Some (Accepted_wake _) | false, Some (Discarded_wake _) -> ()
           | _ -> failwith "wrong wake disposition");
          let job = List.hd_exn saved.jobs in
          (match job.delivery, P.Job.terminal_completion job |> protocol_ok with
           | Delivered _, Some (Succeeded _) -> ()
           | _ -> failwith "original job completion was changed");
          let invocation = model_invocation saved "begin" in
          let history = saved.conversation.canonical_history in
          let ack, _ =
            List.findi_exn history ~f:(fun _ item ->
              P.History.Id.equal item.id (Option.value_exn invocation.output_entry_id))
          in
          let notification, _ =
            List.findi_exn history ~f:(fun _ item ->
              match item.P.History.provenance with
              | Runtime_notification _ -> true
              | _ -> false)
          in
          assert (ack < notification);
          [%test_eq: int]
            1
            (List.count history ~f:(fun item ->
               match item.P.History.provenance with
               | Runtime_notification _ -> true
               | _ -> false));
          H.stop handle ~mode:Graceful |> protocol_ok |> ignore;
          Agent_server.Runtime_owner.unload entry.runtime |> protocol_ok;
          H.start handle ~queue_if_limited:false |> protocol_ok |> ignore;
          Agent_server.Runtime_owner.drain_idle_moderator entry.runtime
          |> protocol_ok
          |> ignore;
          let restored = A.state entry.actor |> protocol_ok in
          assert (P.Delivery.equal delivery (List.hd_exn restored.deliveries));
          [%test_eq: string] "started\n" (Eio.Path.load (file "fixture-work.started"));
          print_s
            [%sexp
              (reject : bool)
            , (wake : bool)
            , "one data message; original result retained; no replay"])
        ~settle:Job_launch_tests.settle
        (fun _ -> ()));
  [%expect
    {|
    (false true "one data message; original result retained; no replay")
    (true true "one data message; original result retained; no replay")
    (false false "one data message; original result retained; no replay")
  |}]
;;
