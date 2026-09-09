open Core
open Agent_server_test_support
open Background_fixtures

let%expect_test
    "daemon restart records interrupted background work instead of rerunning it"
  =
  with_background_daemon
    ~profile:{ permission_profile with tool_default = Ask }
    ~check_restored:(fun before restored ->
      [%test_eq: int] before.attempt restored.attempt;
      (match restored.status with
       | Interrupted _ -> print_endline "interrupted; original attempt retained"
       | other -> raise_s [%sexp (other : J.status)]);
      let completion =
        Completion.of_json (Option.value_exn restored.result) |> protocol_ok
      in
      print_s [%sexp (completion : Completion.t)])
    (fun env _client entry capabilities ->
       ignore (submit entry (B.to_json (native capabilities "report.txt")) : J.t);
       let rec wait () =
         let state = A.state entry.actor |> protocol_ok in
         match
           List.exists state.permissions ~f:(fun permission ->
             Agent_protocol.Permission.equal_state permission.state Pending)
         with
         | true -> ()
         | false ->
           Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
           wait ()
       in
       wait ());
  [%expect
    {|
    interrupted; original attempt retained
    (Failed
     ((code background.interrupted)
      (message "daemon restarted while the job was running") (retryable false)
      (details Null)))
    |}]
;;

let%expect_test "daemon schedules pinned native and script jobs without model execution" =
  with_background_daemon (fun env client entry capabilities ->
    let source =
      {|let count = [0.0]
let main input =
  let ignored = count[0] <- count[0] +. 1.0 in
  Task.bind(Tool.call("read_file", input), fun result ->
    match result with
    | `Ok(value) -> Task.pure(`Object([
        { key = "count"; value = `Number(count[0]) },
        { key = "read"; value = value }
      ]))
    | `Error(message) -> Task.fail(message))|}
    in
    let request =
      script
        env
        capabilities
        source
        (`Object [ "root", `String "reports"; "file", `String "report.txt" ])
    in
    List.iter
      [ native capabilities "report.txt"; request; request ]
      ~f:(fun request ->
        let job = submit entry (B.to_json request) in
        let finished, completion = await env client job in
        [%test_eq: int] 1 finished.attempt;
        (match completion with
         | Succeeded value -> print_endline (Jsonaf.to_string value)
         | other -> raise_s [%sexp (other : Completion.t)]);
        let state = A.state entry.actor |> protocol_ok in
        let roots =
          List.filter state.invocations ~f:(fun invocation ->
            Option.exists
              invocation.context.parent_job
              ~f:(Agent_protocol.Id.Job.equal job.id))
        in
        [%test_eq: int] 1 (List.length roots);
        let root = List.hd_exn roots in
        assert (Option.is_none root.context.parent_invocation);
        assert (
          List.exists state.invocations ~f:(fun invocation ->
            Option.exists
              invocation.context.parent_invocation
              ~f:(Agent_protocol.Id.Invocation.equal root.context.id))));
    print_endline
      "three jobs; fresh script state; persisted owned invocations; no model history");
  [%expect
    {|
    "report.txt:1-1:\n[total_lines=1]\nbackground report"
    {"count":1,"read":"report.txt:1-1:\n[total_lines=1]\nbackground report"}
    {"count":1,"read":"report.txt:1-1:\n[total_lines=1]\nbackground report"}
    three jobs; fresh script state; persisted owned invocations; no model history
    |}]
;;

let%expect_test "daemon preserves denied outcomes and expires queue budget before effects"
  =
  with_background_daemon (fun env client entry capabilities ->
    let denied = submit entry (B.to_json (native capabilities "../secret.txt")) in
    let _, completion = await env client denied in
    (match completion with
     | Succeeded (`String text) ->
       assert (not (String.is_substring text ~substring:"PRIVATE-BACKGROUND-SENTINEL"));
       print_endline text
     | other -> raise_s [%sexp (other : Completion.t)]);
    let before = A.state entry.actor |> protocol_ok in
    let expired =
      submit
        entry
        ~created_at:
          (Agent_protocol.Timestamp.of_string "2000-01-01T00:00:00Z" |> protocol_ok)
        (B.to_json (native capabilities "report.txt"))
    in
    let _, completion = await env client expired in
    print_s [%sexp (completion : Completion.t)];
    let malformed = submit entry (`Object []) in
    let _, completion = await env client malformed in
    (match completion with
     | Failed error -> print_s [%sexp (error.code : string), (error.retryable : bool)]
     | other -> raise_s [%sexp (other : Completion.t)]);
    let after = A.state entry.actor |> protocol_ok in
    [%test_eq: int] (List.length before.invocations) (List.length after.invocations));
  [%expect
    {|
    error running read_file: requested file is outside the configured read roots
    Expired
    (background.invalid_request false)
    |}]
;;

let%expect_test "background permission denial is a structured terminal failure" =
  with_background_daemon
    ~profile:{ permission_profile with tool_default = Deny }
    (fun env client entry capabilities ->
       let job = submit entry (B.to_json (native capabilities "report.txt")) in
       let _, completion = await env client job in
       print_s [%sexp (completion : Completion.t)];
       let state = A.state entry.actor |> protocol_ok in
       [%test_eq: int] 2 (List.length state.invocations);
       assert (
         List.for_all state.invocations ~f:(fun invocation ->
           Option.is_none invocation.observation)));
  [%expect
    {|
    (Failed
     ((code invocation.permission_denied)
      (message "Tool execution was not authorized.") (retryable false)
      (details Null)))
    |}]
;;

let%expect_test "cancelling a waiting background job cleans up its owned permission" =
  with_background_daemon
    ~profile:{ permission_profile with tool_default = Ask }
    (fun env _client entry capabilities ->
       let job = submit entry (B.to_json (native capabilities "report.txt")) in
       let rec wait_pending () =
         let state = A.state entry.actor |> protocol_ok in
         match
           List.find state.permissions ~f:(fun permission ->
             Agent_protocol.Permission.equal_state permission.state Pending)
         with
         | Some permission -> permission
         | None ->
           Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
           wait_pending ()
       in
       let permission = wait_pending () in
       A.cancel_job_internal entry.actor ~job_id:job.id |> protocol_ok |> ignore;
       let rec wait_cleanup () =
         let state = A.state entry.actor |> protocol_ok in
         match
           List.for_all state.invocations ~f:(fun invocation ->
             match invocation.status with
             | Resolved _ | Published _ -> true
             | _ -> false)
         with
         | false ->
           Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
           wait_cleanup ()
         | true -> state
       in
       let state = wait_cleanup () in
       let permission =
         List.find_exn state.permissions ~f:(fun current ->
           Agent_protocol.Id.Permission.equal current.id permission.id)
       in
       print_s [%sexp (permission.state : Agent_protocol.Permission.state)];
       let job =
         List.find_exn state.jobs ~f:(fun current ->
           Agent_protocol.Id.Job.equal current.id job.id)
       in
       print_s [%sexp (job.status : J.status)];
       print_s
         [%sexp
           (Completion.of_json (Option.value_exn job.result) |> protocol_ok
            : Completion.t)];
       [%test_eq: int] 2 (List.length state.invocations);
       print_endline "owned calls resolved before daemon shutdown");
  [%expect
    {|
    Cancelled
    Cancelled
    (Cancelled "job cancelled")
    owned calls resolved before daemon shutdown
    |}]
;;

let%expect_test
    "background standalone jobs reconstruct private dependencies and typed errors"
  =
  with_background_daemon
    ~agent:[%blob "../chatml_extensibility_fixtures/x04-standalone/agent.chatmd"]
    ~sources:
      [ ( "compare.chatml"
        , [%blob "../chatml_extensibility_fixtures/x04-standalone/compare.chatml"] )
      ; "input.json", [%blob "../chatml_extensibility_fixtures/x04-standalone/input.json"]
      ; ( "output.json"
        , [%blob "../chatml_extensibility_fixtures/x04-standalone/output.json"] )
      ]
    (fun env client entry capabilities ->
       List.iter [ "second.txt"; "second.txt"; "report.txt" ] ~f:(fun right ->
         let request =
           tool
             capabilities
             "compare_reports"
             (`Object [ "left", `String "report.txt"; "right", `String right ])
         in
         let job = submit entry (B.to_json request) in
         let _, completion = await env client job in
         match completion with
         | Succeeded value ->
           [%test_eq: float]
             1.
             (Jsonaf.member_exn "invocation_count" value |> Jsonaf.float_exn);
           print_endline (Jsonaf.to_string value)
         | Failed error -> print_s [%sexp (error : Agent_protocol.Invocation.tool_error)]
         | other -> raise_s [%sexp (other : Completion.t)]));
  [%expect
    {|
    {"invocation_count":1,"left":"report.txt:1-1:\n[total_lines=1]\nbackground report","right":"second.txt:1-1:\n[total_lines=1]\nsecond report"}
    {"invocation_count":1,"left":"report.txt:1-1:\n[total_lines=1]\nbackground report","right":"second.txt:1-1:\n[total_lines=1]\nsecond report"}
    ((code reports.same_file) (message "Choose two different reports.")
     (retryable false) (details Null))
    |}]
;;

let%expect_test "configured moderator cannot be bypassed by background dispatch" =
  let agent =
    native_agent
    ^ {|
<script id="owner" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event = fun ctx state event -> Task.pure(state)
</script>|}
  in
  with_background_daemon ~agent (fun env client entry capabilities ->
    let before = A.state entry.actor |> protocol_ok in
    let job = submit entry (B.to_json (native capabilities "report.txt")) in
    let _, completion = await env client job in
    print_s [%sexp (completion : Completion.t)];
    let after = A.state entry.actor |> protocol_ok in
    [%test_eq: int] (List.length before.invocations) (List.length after.invocations));
  [%expect
    {|
    (Failed
     ((code background.invalid_state)
      (message "job-owned moderator handoff is not installed") (retryable false)
      (details (Object ()))))
    |}]
;;
