open Core
open Agent_server_test_support
module P = Agent_protocol
module A = Agent_session.Session_actor
module Background = Background_fixtures
module Lab = Documentation_lab_tests

let approve env actor tool_name =
  let permission = Background_moderator_tests.pending_permission env actor in
  [%test_eq: string] tool_name permission.tool_name;
  A.resolve_permission_as_system
    actor
    ~permission_id:permission.id
    ~permission_generation:permission.generation
    ~choice:Approve_once
    ~reason:(Some "Controlled interruption scenario; the file read remains pending")
  |> protocol_ok
  |> ignore
;;

let%expect_test "lab restart retains an interrupted check and does not replay its read" =
  Background.with_background_daemon
    ~agent:(List.Assoc.find_exn Lab.sources "agent.chatmd" ~equal:String.equal)
    ~sources:
      (List.filter Lab.sources ~f:(fun (name, _) ->
         not (String.equal name "agent.chatmd"))
       @ List.map Lab.workspace_files ~f:(fun (name, source) ->
         "workspace/" ^ name, source))
    ~profile:{ permission_profile with tool_default = Ask }
    ~runtime_policy:
      { Agent_server.Daemon.default_options.chatml_runtime_policy with
        honor_request_turn = false
      }
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
      let _, result = Background.await env client parent in
      (match result with
       | Failed { code = "background.interrupted"; retryable = false; _ } -> ()
       | other -> raise_s [%sexp (other : P.Completion.t)]);
      Background_shell_tests.wait env (fun () ->
        let state = A.state entry.actor |> protocol_ok in
        List.exists state.deliveries ~f:(fun delivery ->
          match delivery.status with
          | Committed _ -> true
          | _ -> false));
      let after = A.state entry.actor |> protocol_ok in
      [%test_eq: int] (List.length before.jobs) (List.length after.jobs);
      [%test_eq: int] (List.length before.invocations) (List.length after.invocations);
      [%test_eq: int]
        1
        (List.count after.conversation.canonical_history ~f:(fun entry ->
           match entry.P.History.provenance with
           | Runtime_notification _ -> true
           | _ -> false));
      let capabilities =
        Agent_server.Runtime_owner.with_background_runtime entry.runtime (fun runtime ->
          Ok
            (Agent_session.Script_tool_calls.current_capabilities
               (Option.value_exn runtime.moderator_script_tools)))
        |> protocol_ok
      in
      let report =
        Agent_client.Connection.request
          client
          (Session_attach
             { session_id = before.identity.session_id
             ; requested_mode = Read_write
             ; subscribe = false
             ; after_sequence = None
             ; reclaim_token = None
             ; idempotency_key =
                 P.Idempotency_key.of_string "recovered-lab-reader" |> protocol_ok
             })
        |> protocol_ok
        |> ignore;
        Background.submit
          entry
          (Background.tool capabilities "lab_report" (`Object [])
           |> Chat_response.Background_request.to_json)
      in
      approve env entry.actor "lab_report";
      let _, report = Background.await env client report in
      let report =
        match report with
        | Succeeded (`String text) -> Jsonaf.of_string text
        | Succeeded value -> value
        | other -> raise_s [%sexp (other : P.Completion.t)]
      in
      let run = Lab.field report "runs" |> Jsonaf.list_exn |> List.hd_exn in
      assert (Jsonaf.bool_exn (Lab.field run "finished"));
      [%test_eq: string]
        "background.interrupted"
        (Lab.text (Lab.field run "result") "code");
      print_endline
        "same job attempts and invocations survive restart; one interrupted result is \
         notified and retained in the lab report")
    (fun env _client entry capabilities ->
       let parent =
         Background.submit
           entry
           (Background.tool
              capabilities
              "begin_lab_checks"
              (`Object [ "phase", `String "original" ])
            |> Chat_response.Background_request.to_json)
       in
       approve env entry.actor "begin_lab_checks";
       let _ = Background_pending_restart_tests.waiting env entry.actor parent.id in
       approve env entry.actor "check_tutorials";
       let permission = Background_moderator_tests.pending_permission env entry.actor in
       [%test_eq: string] "read_file" permission.tool_name;
       print_endline
         "exact lab coordinator acknowledges a check; operator-controlled file \
          permission holds its admitted job before shutdown");
  [%expect
    {|
    exact lab coordinator acknowledges a check; operator-controlled file permission holds its admitted job before shutdown
    same job attempts and invocations survive restart; one interrupted result is notified and retained in the lab report
    |}]
;;
