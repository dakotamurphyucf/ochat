open Core
open Authoring_evaluation
open Runner

let candidate = Authoring_evaluation_fixtures.Solutions.background

let source_with pattern replacement =
  let source = Jsonaf.member_exn "source" candidate |> Jsonaf.string_exn in
  assert (String.is_substring source ~substring:pattern);
  Execution_tests.replace
    candidate
    "source"
    (`String (String.substr_replace_all source ~pattern ~with_:replacement))
;;

let publish = "let* delivery = Notification.publish(reference, terminal, wake) in"

let%expect_test
    "completion replay is ignored and conflicting publication attempts fail the oracle"
  =
  Eio_main.run (fun env ->
    let original = Jsonaf.member_exn "source" candidate |> Jsonaf.string_exn in
    let replay =
      String.substr_replace_all original ~pattern:"let on_event" ~with_:"let handle_event"
      ^ {|
let on_event ctx state event =
  let* updated = handle_event(ctx, state, event) in
  match event with
  | `Internal_event(payload) ->
    (match Json.get_field(payload, "kind") with
      | `Some(`String("background_job_completed")) -> handle_event(ctx, updated, event)
      | _ -> Task.pure(updated))
  | _ -> Task.pure(updated)
|}
    in
    let replay = Execution_tests.replace candidate "source" (`String replay) in
    print_s [%sexp (Background_cases.execute ~env ~finish:Release replay : execution)];
    let duplicate =
      source_with
        publish
        (publish
         ^ "\n\
            let another = {key = \"duplicate\"; invocation_id = `Some(invocation); work \
            = `Some(`Job(job))} in\n\
            let* repeated = Notification.publish(another, terminal, wake) in")
    in
    (match Background_cases.execute ~env ~finish:Release duplicate with
     | Failed (Semantics, _) -> print_endline "conflicting publication: rejected"
     | result -> raise_s [%sexp (result : execution)]);
    let wrong =
      Execution_tests.replace
        candidate
        "source"
        (`String "let initial_state = 0\nlet on_event ctx state event = Task.pure(state)")
    in
    (match Background_cases.execute ~env ~finish:Release wrong with
     | Failed (Semantics, _) -> print_endline "missing acknowledgement: rejected"
     | result -> raise_s [%sexp (result : execution)]);
    let changed_result =
      source_with
        "`Succeeded(field(result, \"value\"))"
        "`Succeeded(`String(\"changed\"))"
    in
    (match Background_cases.execute ~env ~finish:Release changed_result with
     | Failed (Semantics, _) -> print_endline "changed completion: rejected"
     | result -> raise_s [%sexp (result : execution)]);
    let wrong_ack =
      source_with "value = `String(job)" "value = `String(\"another-job\")"
    in
    (match Background_cases.execute ~env ~finish:Release wrong_ack with
     | Failed (Semantics, _) -> print_endline "wrong acknowledgement ID: rejected"
     | result -> raise_s [%sexp (result : execution)]);
    let widening =
      Execution_tests.replace
        candidate
        "binding"
        (`String
            ([%blob "fixtures/observe-binding.chatmd"] ^ "\n<tool name=\"read_file\"/>"))
    in
    match Background_cases.execute ~env ~finish:Release widening with
    | Failed (Capability, _) ->
      print_endline "additional authority: rejected before runtime"
    | result -> raise_s [%sexp (result : execution)]);
  [%expect
    {|
    Passed
    conflicting publication: rejected
    missing acknowledgement: rejected
    changed completion: rejected
    wrong acknowledgement ID: rejected
    additional authority: rejected before runtime
    |}]
;;

let%expect_test
    "background authoring acknowledges before release and supports public cancellation"
  =
  Eio_main.run (fun env ->
    List.iter [ Background_cases.Release; Cancel ] ~f:(fun finish ->
      let result = Background_cases.execute ~env ~finish candidate in
      print_s [%sexp (finish : Background_cases.finish), (result : execution)]));
  [%expect
    {|
    (Release Passed)
    (Cancel Passed)
    |}]
;;
