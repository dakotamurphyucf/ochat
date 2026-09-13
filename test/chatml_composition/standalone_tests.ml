open Core
open Fixtures

let input left right = `Object [ "left", `String left; "right", `String right ]

let sources ~bad_output =
  [ "agent.chatmd", [%blob "../chatml_extensibility_fixtures/x04-standalone/agent.chatmd"]
  ; ( "compare.chatml"
    , [%blob "../chatml_extensibility_fixtures/x04-standalone/compare.chatml"] )
  ; "input.json", [%blob "../chatml_extensibility_fixtures/x04-standalone/input.json"]
  ; ( "output.json"
    , match bad_output with
      | false -> [%blob "../chatml_extensibility_fixtures/x04-standalone/output.json"]
      | true -> {|{"type":"string"}|} )
  ]
;;

let%expect_test
    "X04 standalone source validates inputs and gives parallel invocations fresh globals"
  =
  with_daemon
    ~sources:(sources ~bad_output:false)
    ~calls:
      [ "first", "compare_reports", input "report-a.json" "report-b.json"
      ; "second", "compare_reports", input "report-b.json" "report-a.json"
      ; "invalid", "compare_reports", `Object [ "left", `True ]
      ; "same", "compare_reports", input "report-a.json" "report-a.json"
      ]
    (fun state ->
       List.iter [ "first"; "second" ] ~f:(fun id ->
         match result state id with
         | Complete value ->
           [%test_eq: float]
             1.
             (Jsonaf.member_exn "invocation_count" value |> Jsonaf.float_exn);
           let left = Jsonaf.member_exn "left" value |> Jsonaf.string_exn in
           let right = Jsonaf.member_exn "right" value |> Jsonaf.string_exn in
           let expected_left, expected_right =
             match id with
             | "first" -> report_a, report_b
             | _ -> report_b, report_a
           in
           assert (String.is_substring left ~substring:(String.strip expected_left));
           assert (String.is_substring right ~substring:(String.strip expected_right));
           [%test_eq: int] 2 (List.length (children state (model_invocation state id)))
         | other -> raise_s [%sexp (other : I.outcome)]);
       List.iter [ "invalid"; "same" ] ~f:(fun id ->
         [%test_eq: int] 0 (List.length (children state (model_invocation state id)));
         match result state id with
         | Fail error -> print_s [%sexp (id : string), (error.code : string)]
         | other -> raise_s [%sexp (other : I.outcome)]);
       [%test_eq: int] 4 (List.length (native_reads state));
       print_endline
         "two independent results, each with count 1 and two selected native reads; no \
          moderator or new session");
  [%expect
    {|
    (invalid invocation.invalid_input)
    (same reports.same_file)
    two independent results, each with count 1 and two selected native reads; no moderator or new session
    |}]
;;

let%expect_test "X04 rejects a result that violates the declared output schema" =
  with_daemon
    ~sources:(sources ~bad_output:true)
    ~calls:[ "invalid-output", "compare_reports", input "report-a.json" "report-b.json" ]
    (fun state ->
       [%test_eq: int] 2 (List.length (native_reads state));
       match result state "invalid-output" with
       | Fail error -> print_endline error.code
       | other -> raise_s [%sexp (other : I.outcome)]);
  [%expect {| invocation.invalid_output |}]
;;

let%expect_test
    "context reference preserves caller identities and implementation dependencies"
  =
  let module P = Agent_protocol in
  let sources =
    [ ( "agent.chatmd"
      , {|
<developer>Inspect tool context through the declared tools.</developer>
<tool name="read_file"><read id="reports" path="${workspace}/reports"/></tool>
<script id="inspect" language="chatml" kind="tool" src="inspect.chatml"/>
<tool name="inspect" type="chatml" script="inspect" entrypoint="run"
 input_schema="any.json" output_schema="any.json"><uses tool="read_file"/></tool>
<script id="wrapper" language="chatml" kind="tool" src="wrapper.chatml"/>
<tool name="wrapper" type="chatml" script="wrapper" entrypoint="run"
 input_schema="any.json" output_schema="any.json"><uses tool="inspect"/></tool>
|}
      )
    ; ( "inspect.chatml"
      , [%blob "../chatml_extensibility_fixtures/authoring-invocation/inspect.chatml"] )
    ; ( "wrapper.chatml"
      , {|
let run ctx input =
  let* result = Tool.call("inspect", input) in
  match result with
  | `Ok(value) -> Task.pure(`Complete(value))
  | `Error(message) -> Task.fail(message)
|}
      )
    ; "any.json", "true"
    ]
  in
  with_daemon
    ~sources
    ~calls:
      [ "direct", "inspect", `Null
      ; "nested", "wrapper", `Null
      ; "declined", "inspect", `String "decline"
      ]
    (fun state ->
       let direct = model_invocation state "direct" in
       let wrapper = model_invocation state "nested" in
       let nested = List.hd_exn (children state wrapper) in
       let inspect invocation =
         match outcome invocation with
         | Complete value ->
           let string name = Jsonaf.member_exn name value |> Jsonaf.string_exn in
           [%test_eq: string]
             (P.Id.Invocation.to_string invocation.context.id)
             (string "invocation_id");
           [%test_eq: string]
             (P.Id.Session.to_string invocation.context.session_id)
             (string "session_id");
           [%test_eq: string]
             invocation.context.implementation_revision
             (string "implementation_revision");
           [%test_eq: string]
             invocation.context.capability_fingerprint
             (string "capability_fingerprint");
           [%test_eq: string] "inspect" (string "tool_name");
           let optional name expected =
             assert (
               Jsonaf.exactly_equal
                 (Jsonaf.member_exn name value)
                 (Option.value_map expected ~default:`Null ~f:(fun text -> `String text)))
           in
           optional "provider_call_id" invocation.context.provider_call_id;
           optional
             "parent_invocation"
             (Option.map
                invocation.context.parent_invocation
                ~f:P.Id.Invocation.to_string);
           optional "parent_event" None;
           optional "parent_job" None;
           let tools = Jsonaf.member_exn "available_tools" value |> Jsonaf.list_exn in
           [%test_eq: string list]
             [ "read_file" ]
             (List.map tools ~f:(fun tool ->
                Jsonaf.member_exn "name" tool |> Jsonaf.string_exn));
           value
         | other -> raise_s [%sexp (other : I.outcome)]
       in
       let direct_view = inspect direct in
       let nested_view = inspect nested in
       [%test_eq: string]
         "Model"
         (Jsonaf.member_exn "origin" direct_view |> Jsonaf.string_exn);
       [%test_eq: string]
         "Script"
         (Jsonaf.member_exn "origin" nested_view |> Jsonaf.string_exn);
       assert (
         not
           (String.equal
              direct.context.capability_fingerprint
              nested.context.capability_fingerprint));
       assert (
         Option.exists
           nested.context.parent_invocation
           ~f:(P.Id.Invocation.equal wrapper.context.id));
       [%test_eq: int] 1 (List.length (children state wrapper));
       [%test_eq: int] 0 (List.length (native_reads state));
       (match result state "declined" with
        | Fail { code = "inspection.declined"; retryable = false; details = `Null; _ } ->
          ()
        | other -> raise_s [%sexp (other : I.outcome)]);
       print_endline
         "direct/nested identities retained; only declared read_file metadata; no file \
          effects; structured failure");
  [%expect
    {| direct/nested identities retained; only declared read_file metadata; no file effects; structured failure |}]
;;
