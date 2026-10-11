open Core
open Fixtures

let sources =
  [ ( "agent.chatmd"
    , {|<developer>Read only fully checked filename arrays.</developer>
<tool name="read_file"><read id="reports" path="${workspace}/reports"/></tool>
<script id="checked" language="chatml" kind="tool" src="checked.chatml" project="chatml.project.json"/>
<tool name="checked_reads" type="chatml" script="checked" entrypoint="run"
 input_schema="any.json" output_schema="any.json"><uses tool="read_file"/></tool>|}
    )
  ; "any.json", "{}"
  ; ( "chatml.project.json"
    , {|{"schema_version":1,"namespace":"checked-host",
"modules":{"Entry":{"implementation":"checked.chatml"}},
"dependencies":{"Decoder":{"source":{"kind":"local","path":"library"},"project":"chatml.project.json"}},
"environments":{"default":{"modules":{"Checked_json":"Decoder.Checked_json"}}}}|}
    )
  ; ( "library/chatml.project.json"
    , [%blob "../../docs-src/examples/libraries/checked-json/chatml.project.json"] )
  ; ( "library/src/checked_json.chatml"
    , [%blob "../../docs-src/examples/libraries/checked-json/src/checked_json.chatml"] )
  ; ( "library/src/checked_json.chatmli"
    , [%blob "../../docs-src/examples/libraries/checked-json/src/checked_json.chatmli"] )
  ; ( "checked.chatml"
    , {|let run ctx input =
  match Checked_json.array(input, Checked_json.string) with
  | `Error(error) -> Task.pure(`Complete(`String(Checked_json.describe(error))))
  | `Ok(files) ->
      let* results = Task.map_array(files, fun file ->
        let* result = Tool.call("read_file", `Object([
          {key = "root"; value = `String("reports")},
          {key = "file"; value = `String(file)}
        ])) in
        match result with
        | `Ok(value) -> Task.pure(value)
        | `Error(message) -> Task.fail(message)) in
      Task.pure(`Complete(`Array(results)))|}
    )
  ]
;;

let%expect_test
    "configured decoder companion checks the entire input before real native admission"
  =
  with_daemon
    ~sources
    ~calls:
      [ "invalid", "checked_reads", `Array [ `String "report-a.json"; `Null ]
      ; ( "valid"
        , "checked_reads"
        , `Array [ `String "report-a.json"; `String "report-b.json" ] )
      ]
    (fun state ->
       (match result state "invalid" with
        | Complete (`String message) -> print_endline message
        | outcome -> raise_s [%sexp (outcome : I.outcome)]);
       [%test_eq: int] 0 (List.length (children state (model_invocation state "invalid")));
       let reads = children state (model_invocation state "valid") in
       [%test_eq: int] 2 (List.length reads);
       [%test_eq: int] 2 (List.length (native_reads state));
       List.iter reads ~f:(fun invocation ->
         [%test_eq: string] "read_file" invocation.context.tool_name);
       (match result state "valid" with
        | Complete (`Array [ `String first; `String second ]) ->
          assert (String.is_suffix first ~suffix:(String.strip report_a));
          assert (String.is_suffix second ~suffix:(String.strip report_b))
        | outcome -> raise_s [%sexp (outcome : I.outcome)]);
       print_endline "invalid later element: no reads; valid input: two ordered reads");
  [%expect
    {|
    $[1]: Unexpected null.
    invalid later element: no reads; valid input: two ordered reads
    |}]
;;
