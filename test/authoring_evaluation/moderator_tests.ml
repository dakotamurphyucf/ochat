open Core
open Authoring_evaluation
open Runner

let candidate =
  `Object
    [ "source", `String [%blob "fixtures/quota.chatml"]
    ; "binding", `String [%blob "fixtures/quota-binding.chatmd"]
    ; "input_schema", Jsonaf.of_string [%blob "fixtures/quota-input.json"]
    ; "output_schema", Jsonaf.of_string [%blob "fixtures/quota-output.json"]
    ]
;;

let%expect_test "quota moderator preserves state through sequential calls and rejections" =
  Eio_main.run (fun env ->
    print_s [%sexp (Moderator_cases.execute ~env candidate : execution)]);
  [%expect {| Passed |}]
;;

let changed_source pattern replacement =
  let original = Jsonaf.member_exn "source" candidate |> Jsonaf.string_exn in
  assert (String.is_substring original ~substring:pattern);
  Execution_tests.replace
    candidate
    "source"
    (`String (String.substr_replace_all original ~pattern ~with_:replacement))
;;

let%expect_test
    "quota oracle rejects lost state, rejected-call mutation and unresolved calls"
  =
  Eio_main.run (fun env ->
    let mutations =
      [ "reset after success", changed_source "Task.pure(remaining)" "Task.pure(11.0)"
      ; ( "change state on rejection"
        , changed_source "Task.pure(state)\n    else" "Task.pure(state -. 1.0)\n    else"
        )
      ; ( "unresolved call"
        , Execution_tests.replace
            candidate
            "source"
            (`String
                "let initial_state = 11.0\n\
                 let on_event ctx state event = Task.pure(state)") )
      ; ( "resolve twice"
        , changed_source
            "Task.pure(remaining)"
            "let* () = Invocation.resolve(p.context.invocation_id, \
             `Complete(`Object([{key = \"remaining\"; value = `Number(remaining)}]))) in \
             Task.pure(remaining)" )
      ]
    in
    List.iter mutations ~f:(fun (name, candidate) ->
      match Moderator_cases.execute ~env candidate with
      | Failed (Semantics, _) -> print_endline (name ^ ": rejected")
      | result -> raise_s [%sexp (name : string), (result : execution)]);
    let added =
      Execution_tests.replace
        candidate
        "binding"
        (`String ([%blob "fixtures/quota-binding.chatmd"] ^ "\n<tool name=\"read_file\"/>"))
    in
    match Moderator_cases.execute ~env added with
    | Failed (Capability, _) ->
      print_endline "additional authority: rejected before runtime"
    | result -> raise_s [%sexp (result : execution)]);
  [%expect
    {|
    reset after success: rejected
    change state on rejection: rejected
    unresolved call: rejected
    resolve twice: rejected
    additional authority: rejected before runtime
    |}]
;;

let%expect_test
    "quota authoring envelope is validated and scored in each guidance condition"
  =
  Eio_main.run (fun env ->
    let module V = Chat_response.Authoring_validation in
    let module Q = Chat_response.Authoring_context in
    let module C = Chat_response.Tool_capability in
    let host =
      V.create_host
        ~runtime_identity:"evaluation-quota-v1"
        ~targets:[ Moderator ]
        ~moderator_surface:Ordinary
        ~compilation:Chatml_compilation.default_limits
      |> Result.ok_or_failwith
    in
    let context =
      Q.create ~secret:"offline-quota-evaluation-secret" () |> Result.ok_or_failwith
    in
    let capabilities =
      C.create
        ~owner:"evaluation-quota"
        ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "no tools")
        []
      |> Result.map_error ~f:(fun e -> e.C.message)
      |> Result.ok_or_failwith
    in
    let task =
      List.find_exn Tasks.all ~f:(fun task -> String.equal task.id "moderator-quota")
    in
    let backend =
      Reference_backend.create
        ~env
        ~context
        ~host
        ~capabilities
        ~scope:"evaluation-quota-generation-1"
        ~primer:"Submit the quota evaluation envelope described in the task."
        ~tool_descriptions:(Jsonaf.to_string Q.parameters)
        ~execute:(Moderator_cases.execute ~env)
    in
    let backend =
      { backend with validate = Moderator_cases.validate ~env ~host ~capabilities }
    in
    let provider ~step ~messages:_ =
      { action =
          (match step with
           | 1 ->
             Retrieve
               (Reference_backend.request
                  ~task:"moderator_tool"
                  ~topic_id:"runtime.invocations.moderator"
                  "topic")
           | 2 -> Submit candidate
           | _ -> failwith "unexpected quota provider request")
      ; provider_input_tokens = None
      }
    in
    let rows =
      List.map policies ~f:(fun policy ->
        run
          ~now:(fun () -> 0.)
          ~provenance:Offline_transcript
          ~model:"fixture-quota-v1"
          ~suite_revision:Tasks.fingerprint
          ~runtime_revision:(V.host_fingerprint host)
          ~limits:Tasks.limits
          ~policy
          ~backend
          ~provider
          task)
    in
    compare ~task_ids:[ task.id ] rows
    |> Result.ok_or_failwith
    |> List.iter ~f:(fun score ->
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
