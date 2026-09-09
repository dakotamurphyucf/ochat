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
