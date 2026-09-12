open Core
open Authoring_evaluation
open Runner
module V = Chat_response.Authoring_validation
module Q = Chat_response.Authoring_context
module C = Chat_response.Tool_capability

let evaluate ~env ~capabilities ~id ~target ~candidate ~validate ~execute =
  let host =
    V.create_host
      ~runtime_identity:("evaluation-" ^ id ^ "-v1")
      ~targets:[ target ]
      ~moderator_surface:Ordinary
      ~compilation:Chatml_compilation.default_limits
    |> Result.ok_or_failwith
  in
  let context =
    Q.create ~secret:"offline-native-metadata-evaluation" () |> Result.ok_or_failwith
  in
  let task = List.find_exn Tasks.all ~f:(fun task -> String.equal task.id id) in
  let descriptors =
    C.references capabilities
    |> List.map ~f:(fun reference ->
      let binding =
        C.find capabilities ~name:reference.name
        |> Result.map_error ~f:(fun error -> error.C.message)
        |> Result.ok_or_failwith
      in
      Openai.Completions.jsonaf_of_tool (C.descriptor binding))
  in
  let backend =
    Reference_backend.create
      ~env
      ~context
      ~host
      ~capabilities
      ~scope:("evaluation-" ^ id)
      ~primer:"Submit the candidate described in the task; retrieve references as needed."
      ~tool_descriptions:
        (Jsonaf.to_string
           (`Object
               [ "authoring_context_parameters", Q.parameters
               ; "selected_tools", `Array descriptors
               ]))
      ~execute
  in
  let backend = { backend with validate = validate ~host ~capabilities } in
  let provider ~step ~messages:_ =
    { action =
        (match step with
         | 1 -> Retrieve (Reference_backend.request ~task:task.family "prepare")
         | 2 -> Submit candidate
         | _ -> failwith "unexpected native-metadata evaluation request")
    ; provider_input_tokens = None
    }
  in
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
    match row.runtime_success with
    | true -> ()
    | false -> raise_s [%sexp (row : result)]);
  compare ~task_ids:[ task.id ] rows
  |> Result.ok_or_failwith
  |> List.iter ~f:(fun score ->
    print_s
      [%sexp
        (score.policy : policy)
      , (score.first_pass_compile_rate : float)
      , (score.runtime_success_rate : float)])
;;

let%expect_test "ledger guidance uses the confined reader's actual native registration" =
  Eio_main.run (fun env ->
    Execution_host.with_capabilities
      ~env
      ~declarations:Execution_cases.read_declaration
      ~files:[]
      (fun capabilities ->
         evaluate
           ~env
           ~capabilities
           ~id:"one-off-reconcile"
           ~target:One_off_script
           ~candidate:Execution_tests.reconciliation
           ~validate:(fun ~host ~capabilities candidate ->
             V.validate ~env ~host ~capabilities candidate
             |> Reference_backend.classification)
           ~execute:(Execution_cases.execute_one_off ~env)));
  [%expect
    {|
    (Minimal 1 1)
    (Automatic_retrieval 1 1)
    (Selected_preload 1 1)
    |}]
;;

let%expect_test
    "background guidance validates against the configured probe and scores both endings"
  =
  Eio_main.run (fun env ->
    Execution_host.with_capabilities
      ~env
      ~declarations:Background_cases.probe
      ~files:[ "probe.sh", Background_cases.probe_script ]
      (fun capabilities ->
         evaluate
           ~env
           ~capabilities
           ~id:"async-observe-once"
           ~target:Moderator
           ~candidate:Background_tests.candidate
           ~validate:(Background_cases.validate ~env)
           ~execute:(fun candidate ->
             match Background_cases.execute ~env ~finish:Release candidate with
             | Passed -> Background_cases.execute ~env ~finish:Cancel candidate
             | failure -> failure)));
  [%expect
    {|
    (Minimal 1 1)
    (Automatic_retrieval 1 1)
    (Selected_preload 1 1)
    |}]
;;
