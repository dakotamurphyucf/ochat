open Core
open Runner
module V = Chat_response.Authoring_validation
module Q = Chat_response.Authoring_context
module C = Chat_response.Tool_capability

(* The task selects trusted evaluator code, never a candidate-supplied host. *)
let with_backend ~env ~runtime_revision (task : task) f =
  let audit = Execution_audit.create () in
  let prepare ~target ~surface ~validate ~execute capabilities =
    let host =
      V.create_host
        ~runtime_identity:runtime_revision
        ~targets:[ target ]
        ~moderator_surface:surface
        ~compilation:Chatml_compilation.default_limits
      |> Result.ok_or_failwith
    in
    let context =
      Q.create ~secret:"private-evaluation-reference-cursors" () |> Result.ok_or_failwith
    in
    let selected =
      C.references capabilities
      |> List.map ~f:(fun reference ->
        C.find capabilities ~name:reference.name
        |> Result.map_error ~f:(fun error -> error.C.message)
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
        ~scope:("evaluation-" ^ task.id)
        ~primer:
          "Use the installed references and return the candidate envelope requested by \
           the task. Retrieve missing contracts before authoring."
        ~tool_descriptions:
          (Jsonaf.to_string
             (`Object
                 [ "authoring_context_parameters", Q.parameters
                 ; "selected_tools", `Array selected
                 ]))
        ~execute
    in
    let backend = { backend with validate = validate ~host ~capabilities } in
    f
      Driver.
        { backend
        ; target_identity = V.host_fingerprint host
        ; capability_identity = C.fingerprint capabilities
        ; (* Passing output oracles alone is not a comprehensive authority audit. *)
          audit = (fun () -> Execution_audit.result audit)
        }
  in
  let validate ~host ~capabilities candidate =
    V.validate ~env ~host ~capabilities candidate |> Reference_backend.classification
  in
  let empty f =
    Mirage_crypto_rng_unix.use_default ();
    C.create
      ~owner:"evaluation-empty"
      ~resource_fingerprint:(Chatmd_shell_spec.Source_ref.digest "empty")
      []
    |> Result.map_error ~f:(fun error -> error.C.message)
    |> Result.ok_or_failwith
    |> f
  in
  let reader f =
    Execution_host.with_capabilities
      ~env
      ~declarations:Execution_cases.read_declaration
      ~files:[]
      f
  in
  match task.id with
  | "one-off-reconcile" ->
    reader
      (prepare
         ~target:One_off_script
         ~surface:Ordinary
         ~validate
         ~execute:(Execution_cases.execute_one_off ~audit ~env))
  | "standalone-delta" ->
    empty
      (prepare
         ~target:Standalone_tool
         ~surface:Ordinary
         ~validate
         ~execute:(Execution_cases.execute_standalone ~audit ~env))
  | "moderator-quota" ->
    empty
      (prepare
         ~target:Moderator
         ~surface:Ordinary
         ~validate:(Moderator_cases.validate ~id:"quota" ~name:"reserve" ~env)
         ~execute:(Moderator_cases.execute ~audit ~env))
  | "async-observe-once" ->
    Execution_host.with_capabilities
      ~env
      ~declarations:Background_cases.probe
      ~files:[ "probe.sh", Background_cases.probe_script ]
      (prepare
         ~target:Moderator
         ~surface:Ordinary
         ~validate:(Background_cases.validate ~env)
         ~execute:(fun candidate ->
           match
             Background_cases.execute
               ~audit
               ~replay_job_delivery:true
               ~env
               ~finish:Release
               candidate
           with
           | Passed ->
             Background_cases.execute
               ~audit
               ~replay_job_delivery:true
               ~env
               ~finish:Cancel
               candidate
           | failed -> failed))
  | "child-evidence-review" ->
    reader
      (prepare
         ~target:Generated_chatmd
         ~surface:Ordinary
         ~validate:(Child_cases.validate ~env)
         ~execute:(Child_cases.execute ~audit ~env))
  | "ocaml-transfer-repair" ->
    empty
      (prepare
         ~target:One_off_script
         ~surface:Ordinary
         ~validate
         ~execute:(Repair_cases.execute_count ~audit ~env))
  | "compacted-event-repair" ->
    empty
      (prepare
         ~target:Moderator
         ~surface:Ordinary
         ~validate:(Moderator_cases.validate ~id:"tally" ~name:"tally" ~env)
         ~execute:(Repair_cases.execute_tally ~audit ~env))
  | "missing-process-capability" ->
    Digest_cases.capabilities ~on_call:(fun _ -> failwith "readonly host executed digest")
    |> prepare
         ~target:Moderator
         ~surface:Delegated
         ~validate:(Digest_cases.validate ~env)
         ~execute:(Digest_cases.execute ~audit ~env)
  | id -> invalid_arg ("unknown evaluation task: " ^ id)
;;
