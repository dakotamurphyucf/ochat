open Core
open Authoring_evaluation
open Runner
module V = Chat_response.Authoring_validation
module Q = Chat_response.Authoring_context
module C = Chat_response.Tool_capability

let count source =
  `Object
    [ "version", `Number "1"
    ; "target", `String "one_off_script"
    ; "source", `String source
    ; "tools", `Array []
    ]
;;

let moderator ~id ~name ~source ~input ~output =
  `Object
    [ "source", `String source
    ; ( "binding"
      , `String
          (sprintf
             {|<tool name="%s" type="moderator" moderator="%s" input_schema="input.json" output_schema="output.json"/>|}
             name
             id) )
    ; "input_schema", input
    ; "output_schema", output
    ]
;;

let tally =
  moderator
    ~id:"tally"
    ~name:"tally"
    ~source:[%blob "fixtures/tally.chatml"]
    ~input:
      (Jsonaf.of_string
         {|{"type":"object","required":["amount"],"properties":{"amount":{"type":"integer"}},"additionalProperties":false}|})
    ~output:(Jsonaf.of_string {|{"type":"integer"}|})
;;

let digest =
  moderator
    ~id:"digest_owner"
    ~name:"hash"
    ~source:[%blob "fixtures/digest.chatml"]
    ~input:Digest_cases.input_schema
    ~output:(Jsonaf.of_string {|{"type":"string"}|})
;;

let empty_capabilities () =
  Mirage_crypto_rng_unix.use_default ();
  C.create
    ~owner:"repair-evaluation"
    ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "empty")
    []
  |> Result.map_error ~f:(fun e -> e.C.message)
  |> Result.ok_or_failwith
;;

let run_task ~env ~id ~surface ~target ~capabilities ~validate ~execute ~provider =
  let task = List.find_exn Tasks.all ~f:(fun task -> String.equal task.id id) in
  let host =
    V.create_host
      ~runtime_identity:("evaluation-" ^ id)
      ~targets:[ target ]
      ~moderator_surface:surface
      ~compilation:Chatml_compilation.default_limits
    |> Result.ok_or_failwith
  in
  let context =
    Q.create ~secret:"offline-repair-evaluation" () |> Result.ok_or_failwith
  in
  let selected =
    C.references capabilities
    |> List.map ~f:(fun reference ->
      C.find capabilities ~name:reference.name
      |> Result.map_error ~f:(fun e -> e.C.message)
      |> Result.ok_or_failwith
      |> C.descriptor
      |> Openai.Completions.jsonaf_of_tool)
  in
  let backend =
    Reference_backend.create
      ~env
      ~context
      ~host
      ~capabilities
      ~scope:("repair-" ^ id)
      ~primer:"Use the installed references and return the task's candidate envelope."
      ~tool_descriptions:
        (Jsonaf.to_string
           (`Object
               [ "authoring_context_parameters", Q.parameters
               ; "selected_tools", `Array selected
               ]))
      ~execute
  in
  let backend = { backend with validate = validate ~host ~capabilities } in
  let rows =
    List.map policies ~f:(fun policy ->
      run
        ~now:(fun () -> 0.)
        ~provenance:Offline_transcript
        ~model:("fixture-" ^ id)
        ~suite_revision:Tasks.fingerprint
        ~runtime_revision:(V.host_fingerprint host)
        ~limits:Tasks.limits
        ~policy
        ~backend
        ~provider
        task)
  in
  List.iter rows ~f:(fun row ->
    let expected_attempts =
      match id, row.attempts with
      | ( "ocaml-transfer-repair"
        , [ { validation = Invalid (Semantics, _); execution = None }
          ; { validation = Valid; execution = Some (Failed (Semantics, _)) }
          ; { validation = Valid; execution = Some Passed }
          ] ) -> true
      | ( "compacted-event-repair"
        , [ { validation = Valid; execution = Some (Failed (Semantics, _)) }
          ; { validation = Valid; execution = Some Passed }
          ] ) -> true
      | ( "missing-process-capability"
        , [ { validation = Invalid (Semantics, _); execution = None }
          ; { validation = Valid; execution = Some Passed }
          ] ) -> true
      | _ -> false
    in
    match row.runtime_success && expected_attempts with
    | true -> ()
    | false -> raise_s [%sexp (row : result)]);
  ignore (compare ~task_ids:[ id ] rows |> Result.ok_or_failwith : score list);
  List.iter rows ~f:(fun row ->
    print_s
      [%sexp
        (row.policy : policy)
      , (row.first_pass_compile : bool)
      , (row.repairs : int)
      , (row.retrieval_calls : int)
      , (row.compactions : int)])
;;

let reference topic =
  Retrieve (Reference_backend.request ~task:"moderator_tool" ~topic_id:topic "topic")
;;

let answer action = { action; provider_input_tokens = None }
let decline = Decline "scripted repair failed; inspect recorded attempts"

let%expect_test
    "OCaml transfer repairs both call syntax and a type-correct wrong array count"
  =
  Eio_main.run (fun env ->
    run_task
      ~env
      ~id:"ocaml-transfer-repair"
      ~surface:Ordinary
      ~target:One_off_script
      ~capabilities:(empty_capabilities ())
      ~validate:(fun ~host ~capabilities candidate ->
        V.validate ~env ~host ~capabilities candidate |> Reference_backend.classification)
      ~execute:(Repair_cases.execute_count ~env)
      ~provider:(fun ~step ~messages:_ ->
        answer
          (match step with
           | 1 -> Submit (count "let main input = Task.pure input")
           | 2 ->
             Retrieve
               (Reference_backend.request
                  ~task:"one_off_script"
                  ~topic_id:"chatml.syntax.calls"
                  "topic")
           | 3 -> Submit (count "let main input = Task.pure(`Number(0.0))")
           | 4 -> Submit (count [%blob "fixtures/count.chatml"])
           | _ -> decline)));
  [%expect
    {|
    (Minimal false 2 1 0)
    (Automatic_retrieval false 2 1 0)
    (Selected_preload false 2 1 0)
    |}]
;;

let%expect_test
    "tally authoring refreshes lost event contracts and preserves state across calls"
  =
  Eio_main.run (fun env ->
    run_task
      ~env
      ~id:"compacted-event-repair"
      ~surface:Ordinary
      ~target:Moderator
      ~capabilities:(empty_capabilities ())
      ~validate:(Moderator_cases.validate ~id:"tally" ~name:"tally" ~env)
      ~execute:(Repair_cases.execute_tally ~env)
      ~provider:(fun ~step ~messages ->
        answer
          (match step with
           | 1 -> reference "runtime.invocations.moderator"
           | 2 ->
             Submit
               (Execution_tests.replace
                  tally
                  "source"
                  (`String
                      "let initial_state = 0\n\
                       let on_event ctx state event = Task.pure(state)"))
           | 3 ->
             assert (
               not
                 (List.exists messages ~f:(fun m ->
                    equal_category m.category Documentation)));
             assert (
               List.exists messages ~f:(fun m ->
                 String.is_substring
                   m.text
                   ~substring:"Earlier reference text was compacted"));
             reference "runtime.invocations.moderator"
           | 4 ->
             assert (
               List.exists messages ~f:(fun m ->
                 equal_category m.category Documentation
                 && String.is_substring m.text ~substring:"Tool_invoked"));
             Submit tally
           | _ -> decline)));
  [%expect
    {|
    (Minimal true 1 2 1)
    (Automatic_retrieval true 1 2 1)
    (Selected_preload true 1 2 1)
    |}]
;;

let%expect_test "moderator discovers the selected digest instead of unavailable Process" =
  Eio_main.run (fun env ->
    run_task
      ~env
      ~id:"missing-process-capability"
      ~surface:Delegated
      ~target:Moderator
      ~capabilities:
        (Digest_cases.capabilities ~on_call:(fun _ ->
           failwith "validation executed digest"))
      ~validate:(Digest_cases.validate ~env)
      ~execute:(Digest_cases.execute ~env)
      ~provider:(fun ~step ~messages:_ ->
        answer
          (match step with
           | 1 ->
             Submit
               (Execution_tests.replace
                  digest
                  "source"
                  (`String
                      "let initial_state = 0\n\
                       let on_event ctx state event = let* value = Process.run(\"echo\", \
                       `Null) in Task.pure(state)"))
           | 2 -> reference "runtime.invocations.moderator"
           | 3 -> Submit digest
           | _ -> decline)));
  [%expect
    {|
    (Minimal false 1 1 0)
    (Automatic_retrieval false 1 1 0)
    (Selected_preload false 1 1 0)
    |}]
;;

let%expect_test "digest oracle rejects bypassed calls and modified native output" =
  Eio_main.run (fun env ->
    List.iter
      [ ( "bypass"
        , "let initial_state = 0\n\
           let on_event ctx state event = match event with | `Tool_invoked(p) -> let* () \
           = Invocation.resolve(p.context.invocation_id, `Complete(`String(\"fake\"))) \
           in Task.pure(state) | _ -> Task.pure(state)" )
      ; ( "modified output"
        , String.substr_replace_all
            [%blob "fixtures/digest.chatml"]
            ~pattern:"`Complete(value)"
            ~with_:"`Complete(`String(\"changed\"))" )
      ]
      ~f:(fun (name, source) ->
        match
          Digest_cases.execute
            ~env
            (Execution_tests.replace digest "source" (`String source))
        with
        | Failed (Semantics, message)
          when String.is_substring message ~substring:"digest must be called once" ->
          print_endline (name ^ ": rejected")
        | result -> raise_s [%sexp (name : string), (result : execution)]));
  [%expect
    {|
    bypass: rejected
    modified output: rejected
    |}]
;;
