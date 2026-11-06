open! Core
module P = Agent_protocol
module Fixture = Goal_resume_request_tests
module Create = P.Goal_native_create_request
module Control = P.Goal_native_control_request

let ok = Fixture.ok
let replace = Goal_native_intent_tests.replace
let key text = P.Idempotency_key.of_string text |> ok
let goal_id = P.Id.Goal.of_string "goal_native_create" |> ok

let%expect_test "native create/control freeze generation and CAS without execution fields"
  =
  let source = Fixture.make_source () in
  let objective = P.Goal.Objective.of_string "Native held objective" |> ok in
  let request =
    Create.create
      ~generation:0
      ~source
      ~goal_id
      ~expected_catalog_revision:0L
      ~objective
      ~budget:P.Goal.Budget.unlimited
      ~key:(key "native-create")
    |> ok
  in
  let decoded = Create.of_json (Create.to_json request) |> ok in
  assert (Jsonaf.exactly_equal (Create.to_json request) (Create.to_json decoded));
  assert (Int.equal 0 (Create.generation decoded));
  assert (P.Run_source.equal source (Create.source decoded));
  assert (
    Result.is_error
      (Create.create
         ~generation:1
         ~source
         ~goal_id
         ~expected_catalog_revision:0L
         ~objective
         ~budget:P.Goal.Budget.unlimited
         ~key:(key "different-generation")));
  List.iter
    [ replace (Create.to_json request) "generation" (`String "00")
    ; replace (Create.to_json request) "expected_catalog_revision" (`String "-1")
    ; replace (Create.to_json request) "expected_goal_revision" (`String "0")
    ; replace (Create.to_json request) "principal_id" (`String "pri_forged")
    ; replace (Create.to_json request) "session_id" (`String "ses_forged")
    ]
    ~f:(fun wire -> assert (Result.is_error (Create.of_json wire)));
  let reason = P.Goal.Reason.of_string "Native task needs input" |> ok in
  List.iter [ Control.Mutation.Hold reason; Abandon reason ] ~f:(fun mutation ->
    let request =
      Control.create
        ~generation:0
        ~goal_id
        ~expected_catalog_revision:7L
        ~expected_goal_revision:4L
        ~key:(key "native-control")
        ~mutation
      |> ok
    in
    let decoded = Control.of_json (Control.to_json request) |> ok in
    assert (Jsonaf.exactly_equal (Control.to_json request) (Control.to_json decoded));
    List.iter
      [ replace (Control.to_json request) "expected_goal_revision" (`String "+4")
      ; replace (Control.to_json request) "kind" (`String "enable")
      ; replace (Control.to_json request) "kind" (`String "operator_pause")
      ; replace (Control.to_json request) "reason" `Null
      ; replace (Control.to_json request) "source" (P.Run_source.to_json source)
      ]
      ~f:(fun wire -> assert (Result.is_error (Control.of_json wire))));
  print_endline "generation zero; exact source and CAS; no supplied identity or enable";
  [%expect {| generation zero; exact source and CAS; no supplied identity or enable |}]
;;
