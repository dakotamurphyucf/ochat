open Core
open Fixtures

let source = [%blob "../chatml_extensibility_fixtures/x01-report/aggregate.chatml"]
let agent = [%blob "../chatml_extensibility_fixtures/x01-report/agent.chatmd"]

let request files =
  `Object
    [ "source", `String source
    ; "input", `Array (List.map files ~f:(fun file -> `String file))
    ; "tools", `Array [ `String "read_file" ]
    ]
;;

let%expect_test
    "X01 aggregates real reports through registered run_chatml without a new conversation"
  =
  with_daemon
    ~sources:[ "agent.chatmd", agent ]
    ~calls:[ "aggregate", "run_chatml", request [ "report-a.json"; "report-b.json" ] ]
    (fun state ->
       let expected =
         [%blob "../chatml_extensibility_fixtures/x01-report/expected.json"]
         |> Jsonaf.of_string
       in
       let actual =
         match result state "aggregate" with
         | Complete value -> value
         | other -> raise_s [%sexp (other : I.outcome)]
       in
       if not (Jsonaf.exactly_equal actual expected)
       then raise_s [%sexp { actual : Jsonaf.t; expected : Jsonaf.t }];
       let outer = model_invocation state "aggregate" in
       let script = children state outer |> List.hd_exn in
       [%test_eq: int] 1 (List.length (children state outer));
       [%test_eq: int] 2 (List.length (children state script));
       [%test_eq: int] 2 (List.length (native_reads state));
       print_endline (Jsonaf.to_string actual);
       print_endline
         "one persisted session; two ordinary model-loop requests; no moderator, job or \
          extra model request");
  [%expect
    {|
    [{"check":"lint","failures":2},{"check":"test","failures":1}]
    one persisted session; two ordinary model-loop requests; no moderator, job or extra model request
    |}]
;;

let%expect_test "X01 out-of-root reads use the same denial boundary as direct model calls"
  =
  with_daemon
    ~sources:[ "agent.chatmd", agent ]
    ~calls:
      [ "nested-denied", "run_chatml", request [ "../secret.json" ]
      ; ( "direct-denied"
        , "read_file"
        , `Object [ "root", `String "reports"; "file", `String "../secret.json" ] )
      ]
    (fun state ->
       let direct = result state "direct-denied" in
       let nested =
         native_reads state
         |> List.find_exn ~f:(fun invocation ->
           I.equal_origin invocation.context.origin Script)
         |> outcome
       in
       assert (I.equal_outcome direct nested);
       let failed =
         match result state "nested-denied" with
         | Fail _ -> true
         | _ -> false
       in
       assert failed;
       assert (
         not
           (String.is_substring
              (Sexp.to_string (Agent_session.Session_state.sexp_of_t state))
              ~substring:"PRIVATE-REPORT-SENTINEL"));
       print_endline
         "direct and nested read denial match; aggregation fails; private content is \
          absent");
  [%expect
    {| direct and nested read denial match; aggregation fails; private content is absent |}]
;;
