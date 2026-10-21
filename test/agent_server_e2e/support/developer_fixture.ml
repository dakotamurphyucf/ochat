open! Core
module P = Agent_protocol
module S = Agent_server
module R = P.Developer_repl_wire

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

let developer_configuration ?capture_policy ?(max_runs = 3) ?authorize_native () =
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
  let authorize_native =
    Option.value authorize_native ~default:(fun invocation binding ->
      let name = (Chat_response.Tool_capability.reference binding).name in
      require (String.equal name "append_to_file") "unexpected developer native binding";
      require
        (P.Invocation.equal_origin
           (P.Host_invocation.context invocation).origin
           P.Invocation.Script)
        "developer native origin changed";
      Ok ())
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
    ~authorize_native
    ~prepare_output:(function
      | Openai.Responses.Tool_output.Output.Text value -> Ok (`String value)
      | _ -> Error (P.Error.invalid_request "unexpected developer native output"))
  |> ok
;;

let document ~bytes ~depth ~fields ~nodes =
  Document_schema.Limits.create
    ~max_bytes:bytes
    ~max_depth:depth
    ~max_fields:fields
    ~max_nodes:nodes
  |> checked
;;

let repl_wire_limits () =
  R.Limits.create
    ~request:(document ~bytes:512000 ~depth:24 ~fields:2048 ~nodes:4096)
    ~result:(document ~bytes:(2 * 1024 * 1024) ~depth:20 ~fields:2048 ~nodes:4096)
    ~page:(document ~bytes:8192 ~depth:8 ~fields:128 ~nodes:128)
    ~response:(document ~bytes:(3 * 1024 * 1024) ~depth:24 ~fields:4096 ~nodes:8192)
    ~max_source_bytes:4096
    ~max_reference_bytes:384
    ~max_output_bytes:32768
    ~max_page_items:8
    ~max_event_bytes:4096
    ~max_registry_entries:18
  |> ok
;;

let repl_configuration () =
  let parse =
    Chatml.Chatml_parse.Submission.Limits.create
      ~max_source_bytes:4096
      ~max_tokens:4096
      ~max_work_steps:100_000
      ~sites:
        (Chatml.Chatml_syntax_sites.Limits.create ~max_sites:4096 ~max_name_bytes:16384
         |> checked)
    |> checked
  in
  let display =
    Chatml_typechecker.Type_snapshot.Limits.create
      ~max_nodes:4096
      ~max_edges:4096
      ~max_label_bytes:16384
    |> checked
  in
  let root_copy =
    Chatml_typechecker.Repl_type_roots.Limits.create
      ~max_bindings:128
      ~max_type_roots:256
      ~max_type_nodes:4096
      ~max_variable_cells:1024
      ~max_type_edges:4096
      ~max_copy_work:100_000
      ~max_copy_depth:128
      ~max_name_work:100_000
      ~max_name_bytes:16384
    |> checked
  in
  let typing_limits =
    Chatml_repl_typing.Limits.create
      ~parse
      ~display
      ~max_display_name_bytes:4096
      ~max_statements:16
      ~root_copy
    |> checked
  in
  S.Daemon.Developer_configuration.Repl.create
    ~scope_limits:
      (Chatml_configured_lookup.Repl_scope.Limits.create
         ~max_snippets:16
         ~max_identity_bytes:4096
       |> checked)
    ~root_limits:
      (Chatml_dependency_session.Roots.Limits.create
         ~max_checked_modules:16
         ~max_edges:64
         ~max_dependency_depth:16
       |> checked)
    ~typing_limits
    ~evaluator_limits:
      (Chatml_repl_execution.Limits.create
         ~max_preview_bytes:4096
         ~max_preview_nodes:1024
         ~max_preview_depth:16
         ~max_diagnostic_bytes:1024
         ~max_retained_tasks:8
       |> checked)
    ~execution_policy:(Bounded Chatml_execution.default_limits)
    ~type_display_limits:
      (Chatml_type_display.Limits.create
         ~max_nodes:4096
         ~max_edges:8192
         ~max_depth:32
         ~max_work_steps:131072
         ~max_text_bytes:4096
         ~max_output_bytes:32768
       |> checked)
    ~max_instances:2
    ~max_keys_per_instance:4
    ~max_submissions_per_epoch:8
    ~max_source_bytes:4096
    ~max_receipt_bytes:4096
    ~max_total_receipt_bytes:(1024 * 1024)
    ~max_total_type_detail_bytes:(1024 * 1024)
    ~max_task_origins:8
    ~max_reference_page_bytes:4096
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
