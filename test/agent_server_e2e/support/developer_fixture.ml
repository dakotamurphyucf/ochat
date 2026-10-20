open! Core
module P = Agent_protocol
module S = Agent_server

let ok = function
  | Ok value -> value
  | Error error -> raise_s [%sexp (error : P.Error.t)]
;;

let checked = function
  | Ok value -> value
  | Error _ -> raise_s [%sexp "developer E2E fixture admission failed"]
;;

let require condition message =
  if not condition
  then raise_s [%sexp "developer E2E fixture assertion failed", (message : string)]
;;

let developer_configuration ?capture_policy ?(max_runs = 3) () =
  let target = Chatml.Chatml_compiler_contract.Target.One_off_v1 in
  let contracts =
    S.Authoring_standard_contracts.create
      ~targets:[ target ]
      ~max_identity_bytes:(16 * 1024 * 1024)
      ~max_work_steps:1_000_000
      ~checkpoint:ignore
    |> checked
  in
  let selected =
    S.Authoring_standard_contracts.find contracts target
    |> Option.value_exn
    |> S.Authoring_context_service.Developer.Selected_contract.of_standard_entry
    |> ok
  in
  let default_capture_policy =
    S.Authoring_context_service.Developer.Target_policy.create
      ~local_project:(fun _ ~root:_ ~project:_ ~selection:_ -> None)
      ~local_source:(fun _ ~root:_ ~member:_ -> None)
      ~authoring_project:(fun _ ~origin:_ ~context:_ ~current:_ -> None)
      ~document_project:
        (fun
          _
          ~origin:_
          ~context:_
          ~current:_
          ~parsed:_
          ~declaration:_
          ~build:_
          ~metadata:_ -> None)
      ~document_source:
        (fun
          _ ~origin:_ ~context:_ ~current:_ ~parsed:_ ~declaration:_ ~script:_ ->
        Some selected)
  in
  let capture_policy = Option.value capture_policy ~default:default_capture_policy in
  let document =
    Document_schema.Limits.create
      ~max_bytes:(1024 * 1024)
      ~max_depth:16
      ~max_fields:1024
      ~max_nodes:2048
    |> checked
  in
  let capture_limits =
    S.Authoring_context_service.Developer.Limits.create
      ~max_prepared_input_bytes:4096
      ~max_document_origin_bytes:16384
      ~max_editor_versions:8
      ~build:
        (Chatml_project_build.Limits.create
           ~max_work_steps:100_000
           ~max_input_bytes:(Some 4096)
           ~document
         |> Or_error.ok_exn)
    |> Or_error.ok_exn
  in
  let commands = Agent_store.Developer_command_limits.default ~document in
  let store =
    Agent_store.Developer_run_store.Limits.create
      ~max_runs
      ~max_epochs_per_run:3
      ~max_jobs_per_run:4
      ~max_invocations_per_run:5
      ~max_inference_attempts_per_run:16
      ~commands
      ~max_inference_bytes_per_run:8192
      ~max_selected_per_commit:2
      ~max_job_payload_bytes:1024
      ~max_document_bytes:(1024 * 1024)
      ~max_recovery_entries:16
      ~max_recovery_bytes:(1024 * 1024)
      ~max_work_steps:100_000
      ~document
    |> Or_error.ok_exn
  in
  let roots =
    S.Developer_root_owner.Limits.create
      ~max_active_runs:2
      ~max_staged_jobs:2
      ~max_native_occupancy:2
      ~max_active_workers:1
      ~max_quarantined_workers:2
      ~max_active_inference_attempts:8
      ~max_admitted_call_bindings:32
    |> Or_error.ok_exn
  in
  let disclosure =
    S.Developer_result_disclosure.Limits.create ~max_result_bytes:65536 ~json:document
    |> ok
  in
  let workspace_cleanup =
    Agent_session.Workspace_cleanup.Limits.create
      ~max_entries:128
      ~max_bytes:65536
      ~max_work_steps:100_000
    |> Or_error.ok_exn
  in
  let limits =
    S.Daemon.Developer.Configuration.Limits.create
      ~store
      ~roots
      ~max_active_commands:2
      ~disclosure
      ~compilation:(Bounded Chatml_compilation.default_limits)
      ~max_dependency_depth:8
      ~execution:Chatml_execution.default_limits
      ~max_output_bytes:65536
      ~receipt:document
      ~max_agent_sources:4
      ~max_agent_source_work_steps:100_000
      ~max_native_bindings:8
      ~workspace_cleanup
      ~max_agent_cache_entries:8
    |> ok
  in
  S.Daemon.Developer.Configuration.create
    ~capture_policy
    ~capture_limits
    ~limits
    ~result_disclosure:
      (S.Developer_result_disclosure.Policy.create ~authorize:(fun _ _ -> true))
    ~receipt_read:(fun _ _ -> true)
    ~command_control:(fun _ _ -> true)
    ~publisher_limits:
      (Agent_store.Developer_result_publisher.Limits.create
         ~inline_bytes:1024
         ~max_result_bytes:65536
         ~max_pending:2
       |> ok)
    ~publisher_storage:Agent_store.Developer_result_publisher.Storage.Automatic
    ~model_target:None
    ~authorize_native:(fun invocation binding ->
      let name = (Chat_response.Tool_capability.reference binding).name in
      require (String.equal name "append_to_file") "unexpected developer native binding";
      require
        (P.Invocation.equal_origin
           (P.Host_invocation.context invocation).origin
           P.Invocation.Script)
        "developer native origin changed";
      Ok ())
    ~prepare_output:(function
      | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
      | _ -> Error (P.Error.invalid_request "unexpected developer native output"))
  |> ok
;;

let semantic_authoring_policy env =
  let module Service = S.Authoring_context_service in
  let module Policy = S.Authoring_installation.Host_policy in
  let base =
    Policy.basic_supplied ~initialize_limits:S.Daemon.default_options.protocol_limits
    |> ok
  in
  let type_display =
    Chatml_type_display.Limits.create
      ~max_nodes:512
      ~max_edges:1024
      ~max_depth:16
      ~max_work_steps:100_000
      ~max_text_bytes:1024
      ~max_output_bytes:4096
    |> Or_error.ok_exn
  in
  let query =
    Chatml_authoring_query.Limits.create
      ~max_items:8
      ~max_member_depth:4
      ~max_member_edges:128
      ~max_label_bytes:4096
      ~max_work_steps:100_000
      ~type_display
    |> Or_error.ok_exn
  in
  let semantic =
    Policy.Semantic.create
      ~script_selection:None
      ~project_metadata:None
      ~project_build_limits:
        (Chatml_project_build.Limits.create
           ~max_work_steps:100_000
           ~max_input_bytes:(Some 4096)
           ~document:Document_schema.Limits.default
         |> Or_error.ok_exn)
      ~limits:
        (Service.Semantic.Limits.create
           ~max_document_bundle_files:4
           ~max_document_bundle_bytes:2048
           ~max_parser_work_steps:100_000
           ~max_diagnostic_work_steps:100_000
           ~max_selection_rows:4
           ~max_retained_selection_source_bytes:(4 * 1024 * 1024)
           ~max_retained_selection_input_bytes:(4 * 1024 * 1024)
           ~max_selection_work_steps:1_000_000
         |> Or_error.ok_exn)
      ~prefix_limits:
        (Service.Semantic.Prefix_limits.create
           ~max_document_bundle_files:4
           ~max_document_bundle_bytes:2048
           ~max_parser_work_steps:100_000
           ~max_host_work_steps:100_000
         |> Or_error.ok_exn)
      ~query_limits:
        (Service.Semantic.Query_limits.create
           ~max_host_work_steps:100_000
           ~projection:query
         |> Or_error.ok_exn)
      ~document_dir:(Eio.Stdenv.cwd env)
  in
  Policy.with_semantic base ~semantic |> ok
;;
