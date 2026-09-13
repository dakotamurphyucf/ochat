open Core
open Authoring_evaluation
open Runner

let delta = Authoring_evaluation_fixtures.Solutions.delta

let%expect_test "held-out standalone delta executes through the public session path" =
  Eio_main.run (fun env ->
    let result = Execution_cases.execute_standalone ~env delta in
    print_s [%sexp (result : execution)]);
  [%expect {| Passed |}]
;;

let reconciliation = Authoring_evaluation_fixtures.Solutions.reconciliation

let%expect_test "held-out ledger reconciliation uses confined native file calls" =
  Eio_main.run (fun env ->
    let result = Execution_cases.execute_one_off ~env reconciliation in
    print_s [%sexp (result : execution)]);
  [%expect {| Passed |}]
;;

let replace candidate name value =
  match candidate with
  | `Object fields -> `Object (List.Assoc.add fields name value ~equal:String.equal)
  | _ -> failwith "candidate must be an object"
;;

let%expect_test "task oracles reject plausible wrong outputs and weaker schemas" =
  Eio_main.run (fun env ->
    let empty =
      replace
        delta
        "source"
        (`String
            {|let run ctx input = Task.pure(`Complete(`Object([
          {key = "added"; value = `Array([])}, {key = "removed"; value = `Array([])}
        ])))|})
    in
    let cases =
      [ "wrong answer", Semantics, Execution_cases.execute_standalone ~env, empty
      ; ( "weak input schema"
        , Semantics
        , Execution_cases.execute_standalone ~env
        , replace delta "input_schema" `True )
      ; ( "narrow output schema"
        , Semantics
        , Execution_cases.execute_standalone ~env
        , replace delta "output_schema" (`Object [ "type", `String "string" ]) )
      ; ( "missing read binding"
        , Capability
        , Execution_cases.execute_one_off ~env
        , replace reconciliation "tools" (`Array []) )
      ]
    in
    List.iter cases ~f:(fun (name, expected, execute, candidate) ->
      match execute candidate with
      | Failed (actual, _) when equal_failure actual expected ->
        print_endline (name ^ ": rejected by runtime oracle")
      | result -> raise_s [%sexp (name : string), (result : execution)]));
  [%expect
    {|
    wrong answer: rejected by runtime oracle
    weak input schema: rejected by runtime oracle
    narrow output schema: rejected by runtime oracle
    missing read binding: rejected by runtime oracle
    |}]
;;

let%expect_test
    "three guidance conditions score the standalone task with actual execution"
  =
  Eio_main.run (fun env ->
    let module V = Chat_response.Authoring_validation in
    let module Q = Chat_response.Authoring_context in
    let module C = Chat_response.Tool_capability in
    let host =
      V.create_host
        ~runtime_identity:"evaluation-standalone-v1"
        ~targets:[ Standalone_tool ]
        ~moderator_surface:Ordinary
        ~compilation:Chatml_compilation.default_limits
      |> Result.ok_or_failwith
    in
    let context =
      Q.create ~secret:"offline-standalone-evaluation-secret" () |> Result.ok_or_failwith
    in
    let capabilities =
      C.create
        ~owner:"evaluation-standalone"
        ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "no tools")
        []
      |> Result.map_error ~f:(fun e -> e.C.message)
      |> Result.ok_or_failwith
    in
    let task =
      List.find_exn Tasks.all ~f:(fun task -> String.equal task.id "standalone-delta")
    in
    let backend =
      Reference_backend.create
        ~env
        ~context
        ~host
        ~capabilities
        ~scope:"evaluation-standalone-generation-1"
        ~primer:"Submit a standalone validation request containing source and schemas."
        ~tool_descriptions:(Jsonaf.to_string Q.parameters)
        ~execute:(Execution_cases.execute_standalone ~env)
    in
    let provider ~step ~messages:_ =
      { action =
          (match step with
           | 1 ->
             Retrieve
               (Reference_backend.request
                  ~task:"standalone_tool"
                  ~topic_id:"runtime.invocations.standalone"
                  "topic")
           | 2 -> Submit delta
           | _ -> failwith "unexpected scripted provider request")
      ; provider_input_tokens = None
      }
    in
    let rows =
      List.map policies ~f:(fun policy ->
        run
          ~now:(fun () -> 0.)
          ~provenance:Offline_transcript
          ~model:"fixture-delta-v1"
          ~suite_revision:Tasks.fingerprint
          ~runtime_revision:(V.host_fingerprint host)
          ~limits:Tasks.limits
          ~policy
          ~backend
          ~provider
          task)
    in
    let scores = compare ~task_ids:[ task.id ] rows |> Result.ok_or_failwith in
    List.iter scores ~f:(fun score ->
      print_s
        [%sexp
          (score.policy : policy)
        , (score.first_pass_compile_rate : float)
        , (score.runtime_success_rate : float)]));
  [%expect
    {|
    (Minimal 1 1)
    (Automatic_retrieval 1 1)
    (Selected_preload 1 1)
    |}]
;;
