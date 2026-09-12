open Core
open Authoring_evaluation
open Runner
module Q = Chat_response.Authoring_context
module V = Chat_response.Authoring_validation
module C = Chat_response.Tool_capability
module R = Chatml_host_runtime
module Surface = Chatml.Chatml_extension_surface
module Codec = Chatml.Chatml_value_codec

let candidate source =
  `Object
    [ "version", `Number "1"
    ; "target", `String "one_off_script"
    ; "source", `String source
    ; "tools", `Array []
    ]
;;

let%expect_test
    "evaluation flow uses installed references, actual validation and execution"
  =
  Eio_main.run (fun env ->
    let context =
      Q.create ~secret:"offline-evaluation-test-cursors" () |> Result.ok_or_failwith
    in
    let host =
      V.create_host
        ~runtime_identity:"offline-evaluation-runtime-v1"
        ~targets:[ One_off_script; Standalone_tool; Moderator; Generated_chatmd ]
        ~moderator_surface:Ordinary
        ~compilation:Chatml_compilation.default_limits
      |> Result.ok_or_failwith
    in
    let capabilities =
      C.create
        ~owner:"evaluation-test"
        ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "no tools")
        []
      |> Result.map_error ~f:(fun e -> e.C.message)
      |> Result.ok_or_failwith
    in
    let executions = ref 0 in
    let execute request =
      incr executions;
      let source =
        Jsonaf.member "source" request |> Option.value_exn |> Jsonaf.string_exn
      in
      match
        R.compile_script_detailed
          ~surface:Surface.one_off_v1
          ~required_bindings:Surface.one_off_entrypoints
          ~source
          ()
      with
      | Error error -> Failed (Semantics, error.formatted)
      | Ok compiled ->
        let input = `Object [ "evidence", `Array [ `String "alpha"; `True ] ] in
        (match
           R.run_entrypoint
             ~limits:{ fuel = 10000; max_tasks = 1000 }
             { surface = Surface.one_off_v1; operations = [] }
             compiled
             ~entrypoint:"main"
             ~arguments:[ Codec.jsonaf_to_value input ]
             ()
           |> Result.bind ~f:Codec.value_to_jsonaf_result
         with
         | Ok actual when Jsonaf.exactly_equal actual input -> Passed
         | Ok _ -> Failed (Semantics, "identity output changed")
         | Error message -> Failed (Semantics, message))
    in
    let backend =
      Reference_backend.create
        ~env
        ~context
        ~host
        ~capabilities
        ~scope:"offline-evaluation-generation-1"
        ~primer:"Submit a validation request for a one-off ChatML program."
        ~tool_descriptions:(Jsonaf.to_string Q.parameters)
        ~execute
    in
    (* Check every manifest selector against installed source without exposing
       private evaluator answers. This is a source/compatibility check only. *)
    List.iter Tasks.all ~f:(fun task ->
      assert (not (List.is_empty (backend.preload task))));
    let test_task =
      { id = "integration-identity"
      ; family = "one_off_script"
      ; prompt = "Return the input unchanged."
      ; preload_topics = [ "chatml.syntax.calls" ]
      ; compaction_after_step = None
      }
    in
    let provider ~step ~messages =
      let action =
        match step with
        | 1 -> Submit (candidate "let main input = Task.pure input")
        | 2 ->
          Retrieve
            (Reference_backend.request
               ~task:"one_off_script"
               ~topic_id:"chatml.syntax.calls"
               "topic")
        | 3 ->
          assert (
            List.exists messages ~f:(fun message ->
              equal_category message.category Documentation
              && String.is_substring message.text ~substring:"Task.pure("));
          Submit (candidate "let main input = Task.pure(`Null)")
        | 4 -> Submit (candidate "let main input = Task.pure(input)")
        | _ -> failwith "unexpected evaluation provider request"
      in
      { action; provider_input_tokens = None }
    in
    let rows =
      List.map policies ~f:(fun policy ->
        run
          ~now:(fun () -> 0.)
          ~provenance:Offline_transcript
          ~model:"scripted-v1"
          ~suite_revision:Tasks.fingerprint
          ~runtime_revision:(V.host_fingerprint host)
          ~limits:{ max_steps = 4; max_attempts = 3 }
          ~policy
          ~backend
          ~provider
          test_task)
    in
    assert (!executions = 6);
    List.iter rows ~f:(fun row ->
      assert row.runtime_success;
      assert (not row.first_pass_compile);
      match row.attempts with
      | [ { validation = Invalid (Semantics, _); execution = None }
        ; { validation = Valid; execution = Some (Failed (Semantics, _)) }
        ; { validation = Valid; execution = Some Passed }
        ] -> ()
      | _ -> raise_s [%sexp (row : result)]);
    let scores = compare ~task_ids:[ test_task.id ] rows |> Result.ok_or_failwith in
    (match
       backend.validate
         (`Object
             [ "version", `Number "1"
             ; "target", `String "one_off_script"
             ; "source", `String "let main input = Task.pure(input)"
             ; "tools", `Array [ `String "not-selected" ]
             ])
     with
     | Invalid (Capability, _) -> ()
     | result -> raise_s [%sexp (result : validation)]);
    printf
      "%d manifest topic selections resolved; %d policies repaired and executed through \
       actual services\n"
      (List.length Tasks.all)
      (List.length scores);
    print_endline
      "type errors, wrong runtime answers and missing capabilities remain distinct";
    print_endline
      "offline integration only; held-out model quality and other execution families not \
       evaluated");
  [%expect
    {|
    8 manifest topic selections resolved; 3 policies repaired and executed through actual services
    type errors, wrong runtime answers and missing capabilities remain distinct
    offline integration only; held-out model quality and other execution families not evaluated
    |}]
;;
