open Core
open Fixtures

let source =
  [%blob "../../docs-src/examples/learning/check-reports/scripts/aggregate.chatml"]
;;

let one_off_source =
  [%blob
    "../../docs-src/examples/learning/check-reports/scripts/aggregate-one-off.chatml"]
;;

let decoder_sources =
  [ ( "libraries/checked-json/chatml.project.json"
    , [%blob "../../docs-src/examples/libraries/checked-json/chatml.project.json"] )
  ; ( "libraries/checked-json/src/checked_json.chatml"
    , [%blob "../../docs-src/examples/libraries/checked-json/src/checked_json.chatml"] )
  ; ( "libraries/checked-json/src/checked_json.chatmli"
    , [%blob "../../docs-src/examples/libraries/checked-json/src/checked_json.chatmli"] )
  ]
;;

let project = [%blob "../../docs-src/examples/learning/check-reports/chatml.project.json"]

let program_agent =
  [%blob "../../docs-src/examples/learning/check-reports/program.chatmd"]
;;

let tool_agent = [%blob "../../docs-src/examples/learning/check-reports/tool.chatmd"]

let input_schema =
  [%blob "../../docs-src/examples/learning/check-reports/schemas/input.json"]
;;

let output_schema =
  [%blob "../../docs-src/examples/learning/check-reports/schemas/output.json"]
;;

let workspace_files =
  [ ( "reports/report-a.json"
    , [%blob "../../docs-src/examples/learning/check-reports/reports/report-a.json"] )
  ; ( "reports/report-b.json"
    , [%blob "../../docs-src/examples/learning/check-reports/reports/report-b.json"] )
  ; "scripts/aggregate-one-off.chatml", one_off_source
  ]
;;

let input = `Array [ `String "report-a.json"; `String "report-b.json" ]

let request input =
  `Object
    [ "source", `String one_off_source
    ; "input", input
    ; "tools", `Array [ `String "read_file" ]
    ]
;;

let tool_sources =
  [ "agent.chatmd", tool_agent
  ; "scripts/aggregate.chatml", source
  ; "schemas/input.json", input_schema
  ; "schemas/output.json", output_schema
  ; "chatml.project.json", project
  ]
  @ decoder_sources
;;

let summary state call_id =
  match result state call_id with
  | Complete value -> print_endline (Jsonaf.to_string value)
  | other -> raise_s [%sexp (other : I.outcome)]
;;

let%test_unit "each invalid filename position rejects the entire request before reads" =
  let invalid =
    List.init 3 ~f:(fun bad_index ->
      ( Int.to_string bad_index
      , `Array
          (List.init 3 ~f:(fun index ->
             if Int.equal index bad_index then `Null else `String "report-a.json")) ))
  in
  let check state =
    [%test_eq: int] 0 (List.length (native_reads state));
    List.iter invalid ~f:(fun (id, _) ->
      match result state id with
      | Fail error when String.equal error.code "chatml.execution_failed" -> ()
      | other -> raise_s [%sexp (other : I.outcome)])
  in
  with_daemon
    ~workspace_files
    ~sources:[ "agent.chatmd", program_agent ]
    ~calls:(List.map invalid ~f:(fun (id, input) -> id, "run_chatml", request input))
    check;
  (* Relax only the test's input schema, so the actual authored decoder rather
     than schema admission proves this invariant. The shipped schema stays strict. *)
  let sources =
    List.map tool_sources ~f:(fun (name, bytes) ->
      name, if String.equal name "schemas/input.json" then "{}" else bytes)
  in
  with_daemon
    ~workspace_files
    ~sources
    ~calls:
      (List.map invalid ~f:(fun (id, files) ->
         id, "summarize_checks", `Object [ "files", files ]))
    check
;;

let%test_unit "report snapshot order and counts are independent for each invocation" =
  let workspace_files =
    ( "reports/first.json"
    , {|[{"check":"earliest failure","status":"failed"},{"check":"setup instructions","status":"failed"}]|}
    )
    :: workspace_files
  in
  let input =
    `Array [ `String "first.json"; `String "report-a.json"; `String "first.json" ]
  in
  let expected =
    Jsonaf.of_string
      {|[{"check":"earliest failure","failures":2},{"check":"setup instructions","failures":3},{"check":"verification steps","failures":1}]|}
  in
  let check state =
    [%test_eq: int] 6 (List.length (native_reads state));
    List.iter [ "first"; "again" ] ~f:(fun id ->
      match result state id with
      | Complete actual -> assert (Jsonaf.exactly_equal actual expected)
      | other -> raise_s [%sexp (other : I.outcome)])
  in
  with_daemon
    ~workspace_files
    ~sources:[ "agent.chatmd", program_agent ]
    ~calls:(List.map [ "first"; "again" ] ~f:(fun id -> id, "run_chatml", request input))
    check;
  with_daemon
    ~workspace_files
    ~sources:tool_sources
    ~calls:
      (List.map [ "first"; "again" ] ~f:(fun id ->
         id, "summarize_checks", `Object [ "files", input ]))
    check
;;

let%expect_test
    "public raw program and configured tool return the same ordered report calculation"
  =
  with_daemon
    ~workspace_files
    ~sources:[ "agent.chatmd", program_agent ]
    ~calls:[ "aggregate", "run_chatml", request input ]
    (fun state ->
       [%test_eq: int] 2 (List.length (native_reads state));
       summary state "aggregate");
  with_daemon
    ~workspace_files
    ~sources:tool_sources
    ~calls:[ "aggregate", "summarize_checks", `Object [ "files", input ] ]
    (fun state ->
       [%test_eq: int] 2 (List.length (native_reads state));
       summary state "aggregate");
  [%expect
    {|
    [{"check":"setup instructions","failures":2},{"check":"verification steps","failures":1}]
    [{"check":"setup instructions","failures":2},{"check":"verification steps","failures":1}]
    |}]
;;

let%expect_test
    "public configured tool rejects invalid input before reading and retains file \
     boundaries"
  =
  with_daemon
    ~workspace_files
    ~sources:tool_sources
    ~calls:
      [ "invalid", "summarize_checks", `Object []
      ; ( "denied"
        , "summarize_checks"
        , `Object [ "files", `Array [ `String "../secret.json" ] ] )
      ]
    (fun state ->
       List.iter [ "invalid"; "denied" ] ~f:(fun id ->
         match result state id with
         | Fail error -> print_endline (id ^ ": " ^ error.code)
         | other -> raise_s [%sexp (other : I.outcome)]);
       [%test_eq: int] 0 (List.length (children state (model_invocation state "invalid")));
       [%test_eq: int] 1 (List.length (native_reads state));
       assert (
         not
           (String.is_substring
              (Sexp.to_string (Agent_session.Session_state.sexp_of_t state))
              ~substring:"PRIVATE-REPORT-SENTINEL"));
       print_endline
         "invalid input performs no reads; denied file contents never enter the session");
  [%expect
    {|
    invalid: invocation.invalid_input
    denied: chatml.execution_failed
    invalid input performs no reads; denied file contents never enter the session
    |}]
;;

let%expect_test
    "public report program fails instead of returning partial totals for malformed data"
  =
  let workspace_files =
    ("reports/broken.json", {|[{"check":"setup instructions","status":"unknown"}]|})
    :: workspace_files
  in
  with_daemon
    ~workspace_files
    ~sources:[ "agent.chatmd", program_agent ]
    ~calls:
      [ ( "aggregate"
        , "run_chatml"
        , request (`Array [ `String "report-a.json"; `String "broken.json" ]) )
      ]
    (fun state ->
       [%test_eq: int] 2 (List.length (native_reads state));
       match result state "aggregate" with
       | Fail error ->
         assert (
           String.is_substring
             error.message
             ~substring:"A check status must be passed or failed.");
         print_endline
           "invalid report status fails the invocation; no partial summary is returned"
       | other -> raise_s [%sexp (other : I.outcome)]);
  [%expect
    {| invalid report status fails the invocation; no partial summary is returned |}]
;;
