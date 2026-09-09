open Core
open Fixtures
open Agent_server_test_support
module J = Agent_protocol.Job
module Completion = Agent_protocol.Completion

let with_probe source =
  let declaration =
    {|<script id="probe" language="chatml" kind="tool" src="probe.chatml"/>
<tool name="probe" type="chatml" script="probe" entrypoint="run" input_schema="any.json" output_schema="any.json"><uses tool="read_file"/></tool>|}
  in
  fun sources ->
    ("probe.chatml", source)
    :: List.map sources ~f:(fun (name, content) ->
      match String.equal name "agent.chatmd" with
      | true -> name, content ^ "\n" ^ declaration
      | false -> name, content)
;;

let read = `Object [ "root", `String "reports"; "file", `String "report-a.json" ]

let run start =
  "let run ctx input = Task.bind("
  ^ start
  ^ ", fun job -> Task.pure(`Pending(`Job(job), `String(\"accepted\"))))"
;;

let sources =
  let tools =
    [ "direct", run "Job.start_tool(\"read_file\", input)"
    ; "alias", run "Tool.spawn(\"read_file\", input)"
    ; "script", run "Job.start_script(input)"
    ; ( "cancelled"
      , {|let run ctx input =
        Task.bind(Job.start_tool("read_file", input), fun job ->
          Task.bind(Job.cancel(job), fun ignored ->
            Task.bind(Job.get(job), fun status ->
              Task.pure(`Pending(`Job(job), status)))))|}
      )
    ; ( "caught"
      , {|let run ctx input =
        Task.bind(Task.catch(
          Task.bind(Job.start_tool("read_file", input), fun discarded -> Task.fail("discard")),
          fun ignored -> Task.pure("caught")), fun ignored ->
          Task.bind(Job.start_tool("read_file", input), fun job ->
            Task.pure(`Pending(`Job(job), `String("accepted")))))|}
      )
    ; ( "invalid"
      , {|let run ctx input =
        Task.bind(Job.start_tool("read_file", input), fun job ->
          Task.pure(`Pending(`Job(job), `Null)))|}
      )
    ]
  in
  let declarations =
    List.map tools ~f:(fun (name, _) ->
      "<script id=\""
      ^ name
      ^ "\" language=\"chatml\" kind=\"tool\" src=\""
      ^ name
      ^ ".chatml\"/>\n"
      ^ "<tool name=\""
      ^ name
      ^ "\" type=\"chatml\" script=\""
      ^ name
      ^ "\" entrypoint=\"run\" input_schema=\"any.json\" output_schema=\""
      ^ (if String.equal name "invalid" then "string.json" else "any.json")
      ^ "\"><uses tool=\"read_file\"/></tool>")
  in
  [ ( "agent.chatmd"
    , "<developer>Inspect reports through the declared tools.</developer>\n"
      ^ "<tool name=\"read_file\"><read id=\"reports\" \
         path=\"${workspace}/reports\"/></tool>\n"
      ^ String.concat ~sep:"\n" declarations )
  ; "any.json", "true"
  ; "string.json", {|{"type":"string"}|}
  ]
  @ List.map tools ~f:(fun (name, source) -> name ^ ".chatml", source)
;;

let settle env (entry : Agent_server.Session_registry.entry) =
  Eio.Time.with_timeout_exn (Eio.Stdenv.clock env) 15. (fun () ->
    let rec wait () =
      let state = A.state entry.actor |> protocol_ok in
      let finished =
        List.for_all state.jobs ~f:(fun job ->
          match job.status with
          | Queued | Running | Waiting_permission _ | Waiting_completion _ -> false
          | Succeeded | Failed _ | Cancelled | Interrupted _ -> true)
      in
      match finished with
      | true -> ()
      | false ->
        Eio.Time.sleep (Eio.Stdenv.clock env) 0.01;
        wait ()
    in
    wait ())
;;

let%expect_test
    "standalone Job operations launch through the qualified daemon and preserve rollback"
  =
  let script_input =
    `Object
      [ ( "source"
        , `String
            {|let main input = Task.bind(Tool.call("read_file", input), fun result ->
        match result with | `Ok(value) -> Task.pure(value) | `Error(message) -> Task.fail(message))|}
        )
      ; "tools", `Array [ `String "read_file" ]
      ; "input", read
      ]
  in
  with_daemon
    ~sources
    ~settle
    ~calls:
      [ "direct", "direct", read
      ; "alias", "alias", read
      ; "script", "script", script_input
      ; "cancelled", "cancelled", read
      ; "caught", "caught", read
      ]
    (fun state ->
       [%test_eq: int] 5 (List.length state.jobs);
       List.iter [ "direct"; "alias"; "script"; "caught" ] ~f:(fun name ->
         match result state name with
         | Pending (Job id, `String "accepted") ->
           let job =
             List.find_exn state.jobs ~f:(fun job ->
               Agent_protocol.Id.Job.equal job.id id)
           in
           [%test_eq: int] 1 job.attempt;
           (match J.terminal_completion job |> protocol_ok with
            | Some (Succeeded (`String text)) ->
              assert (String.is_substring text ~substring:(String.strip report_a))
            | other -> raise_s [%sexp (other : Completion.t option)]);
           print_endline (name ^ ": accepted once, executed once")
         | other -> raise_s [%sexp (other : I.outcome)]);
       match result state "cancelled" with
       | Pending (Job id, status) ->
         [%test_eq: string]
           "cancelled"
           (Jsonaf.member_exn "status" status |> Jsonaf.string_exn);
         let job =
           List.find_exn state.jobs ~f:(fun job -> Agent_protocol.Id.Job.equal job.id id)
         in
         [%test_eq: int] 0 job.attempt;
         (match J.terminal_completion job |> protocol_ok with
          | Some (Cancelled _) ->
            print_endline "cancelled ticket acknowledged without execution"
          | _ -> failwith "cancelled ticket lost its terminal completion")
       | other -> raise_s [%sexp (other : I.outcome)]);
  [%expect
    {|
    direct: accepted once, executed once
    alias: accepted once, executed once
    script: accepted once, executed once
    caught: accepted once, executed once
    cancelled ticket acknowledged without execution
    |}]
;;

let%expect_test "invalid acknowledgement aborts the standalone launch before publication" =
  with_daemon
    ~sources
    ~calls:[ "invalid", "invalid", read ]
    (fun state ->
       assert (List.is_empty state.jobs);
       assert (List.is_empty (native_reads state));
       match result state "invalid" with
       | Fail error -> print_endline error.code
       | other -> raise_s [%sexp (other : I.outcome)]);
  [%expect {| invocation.invalid_output |}]
;;

let%expect_test
    "job IDs cannot restore read or cancellation authority removed from a script"
  =
  List.iter [ `Read; `Cancel ] ~f:(fun action ->
    List.iter [ false; true ] ~f:(fun allowed ->
      let operation =
        match action with
        | `Read -> "Task.bind(Job.get(id), fun value -> Task.pure(value))"
        | `Cancel -> "Task.bind(Job.cancel(id), fun ignored -> Task.pure(`Null))"
      in
      let child_source =
        "let main input = match input with | `String(id) -> "
        ^ operation
        ^ " | _ -> Task.fail(\"expected id\")"
      in
      let selected =
        match allowed with
        | true -> "[`String(\"read_file\")]"
        | false -> "[]"
      in
      let source =
        {|let run ctx input =
        Task.bind(Job.start_tool("read_file", input), fun target ->
          Task.bind(Job.start_script(`Object([
            {key="source"; value=`String(|}
        ^ Jsonaf.to_string (`String child_source)
        ^ {|)},
            {key="tools"; value=`Array(|}
        ^ selected
        ^ {|)},
            {key="input"; value=`String(target)}
          ])), fun probe -> Task.pure(`Pending(`Job(probe), `String("accepted")))))|}
      in
      with_daemon
        ~sources:(with_probe source sources)
        ~settle
        ~calls:[ "probe", "probe", read ]
        (fun state ->
           let id =
             match result state "probe" with
             | Pending (Job id, _) -> id
             | other -> raise_s [%sexp (other : I.outcome)]
           in
           let job =
             List.find_exn state.jobs ~f:(fun job ->
               Agent_protocol.Id.Job.equal job.id id)
           in
           [%test_eq: int] 2 (List.length state.jobs);
           (match allowed, J.terminal_completion job |> protocol_ok with
            | true, Some (Succeeded _) | false, Some (Failed _) -> ()
            | _, completion -> raise_s [%sexp (completion : Completion.t option)]);
           print_s [%sexp (action : [ `Read | `Cancel ]), (allowed : bool)])));
  [%expect
    {|
    (Read false)
    (Read true)
    (Cancel false)
    (Cancel true)
    |}]
;;

let%expect_test "background one-off scripts retain their selected tools and job ancestry" =
  List.iter [ `Allowed; `Unselected; `No_job_budget ] ~f:(fun mode ->
    let tool =
      match mode with
      | `Unselected -> "direct"
      | _ -> "read_file"
    in
    let source =
      "let main input = Task.bind(Job.start_tool(\""
      ^ tool
      ^ "\", input), fun id -> Task.pure(`String(id)))"
    in
    let limits =
      match mode with
      | `No_job_budget -> [ "limits", `Object [ "max_tasks", `Number "0" ] ]
      | _ -> []
    in
    let input =
      `Object
        ([ "source", `String source
         ; "tools", `Array [ `String "read_file" ]
         ; "input", read
         ]
         @ limits)
    in
    with_daemon
      ~sources
      ~settle
      ~calls:[ "script", "script", input ]
      (fun state ->
         let parent_id =
           match result state "script" with
           | Pending (Job id, _) -> id
           | other -> raise_s [%sexp (other : I.outcome)]
         in
         let parent =
           List.find_exn state.jobs ~f:(fun job ->
             Agent_protocol.Id.Job.equal job.id parent_id)
         in
         match mode, J.terminal_completion parent |> protocol_ok with
         | `Allowed, Some (Succeeded (`String id)) ->
           let id = Agent_protocol.Id.Job.of_string id |> protocol_ok in
           let child =
             List.find_exn state.jobs ~f:(fun job ->
               Agent_protocol.Id.Job.equal job.id id)
           in
           let launch = Option.value_exn child.launch in
           assert (
             Option.equal
               (fun (a, attempt) (b, expected) ->
                  Agent_protocol.Id.Job.equal a b && Int.equal attempt expected)
               launch.parent_job
               (Some (parent_id, parent.attempt)));
           [%test_eq: int] 1 launch.nested_depth;
           [%test_eq: int] 2 (List.length state.jobs);
           (match J.terminal_completion child |> protocol_ok with
            | Some (Succeeded (`String text)) ->
              assert (String.is_substring text ~substring:(String.strip report_a))
            | _ -> failwith "nested job did not run its selected tool");
           print_endline "nested launch retained parent attempt and depth"
         | (`Unselected | `No_job_budget), Some (Failed failure) ->
           [%test_eq: int] 1 (List.length state.jobs);
           assert (List.is_empty (native_reads state));
           (match mode with
            | `No_job_budget ->
              [%test_eq: string] "background.resource_limit" failure.code;
              print_endline "transactional starts retain the spawned-task ceiling"
            | _ -> print_endline "script could not regain an unselected registered tool")
         | _, completion -> raise_s [%sexp (completion : Completion.t option)]));
  [%expect
    {|
    nested launch retained parent attempt and depth
    script could not regain an unselected registered tool
    transactional starts retain the spawned-task ceiling
    |}]
;;
