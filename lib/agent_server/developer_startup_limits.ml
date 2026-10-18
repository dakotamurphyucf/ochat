open! Core
module P = Agent_protocol
module Config = Developer_startup_configuration
module D = Document_schema
module Store = Agent_store.Developer_run_store
module Dev = Developer_daemon_configuration

type t =
  { capture : Authoring_context_service.Developer.Limits.t
  ; host : Dev.Limits.t
  ; publisher : Agent_store.Developer_result_publisher.Limits.t
  ; approvals : Developer_approval_owner.Limits.t option
  ; repl : Dev.Repl.t option
  ; portable : Dev.Portable.t option
  }

let capture t = t.capture
let host t = t.host
let publisher t = t.publisher
let approvals t = t.approvals
let repl t = t.repl
let portable t = t.portable
let invalid () = P.Error.invalid_request "invalid developer startup capacity"
let admitted result = Result.map_error result ~f:(fun _ -> invalid ())

let json ~bytes ~depth ~fields ~nodes =
  D.Limits.create ~max_bytes:bytes ~max_depth:depth ~max_fields:fields ~max_nodes:nodes
  |> admitted
;;

let create_base (capacity : Config.Capacity.t) =
  let open Result.Let_syntax in
  let mib = 1024 * 1024 in
  let%bind document = json ~bytes:mib ~depth:32 ~fields:8192 ~nodes:16384 in
  let%bind receipt = json ~bytes:65536 ~depth:16 ~fields:1024 ~nodes:2048 in
  let%bind commands =
    Agent_store.Developer_command_limits.create
      ~max_commands:1024
      ~max_bytes:mib
      ~max_terminal_bytes:65536
    |> admitted
  in
  let%bind store =
    Store.Limits.create
      ~max_runs:capacity.max_runs
      ~max_epochs_per_run:128
      ~max_jobs_per_run:256
      ~max_invocations_per_run:1024
      ~max_inference_attempts_per_run:256
      ~max_inference_bytes_per_run:65536
      ~commands
      ~max_selected_per_commit:32
      ~max_job_payload_bytes:65536
      ~max_document_bytes:mib
      ~max_recovery_entries:((2 * capacity.max_runs) + 1)
      ~max_recovery_bytes:(capacity.max_runs * mib)
      ~max_work_steps:1_000_000
      ~document
    |> admitted
  in
  let%bind roots =
    Developer_root_owner.Limits.create
      ~max_active_runs:(Int.min capacity.max_runs capacity.max_active_commands)
      ~max_staged_jobs:256
      ~max_native_occupancy:capacity.max_native_bindings
      ~max_active_workers:capacity.max_active_agent_calls
      ~max_quarantined_workers:capacity.max_active_agent_calls
      ~max_active_inference_attempts:64
      ~max_admitted_call_bindings:capacity.max_native_bindings
    |> admitted
  in
  let%bind disclosure =
    Developer_result_disclosure.Limits.create ~max_result_bytes:65536 ~json:receipt
  in
  let%bind workspace_cleanup =
    Agent_session.Workspace_cleanup.Limits.create
      ~max_entries:256
      ~max_bytes:mib
      ~max_work_steps:1_000_000
    |> admitted
  in
  let%bind host =
    Dev.Limits.create
      ~store
      ~roots
      ~max_active_commands:capacity.max_active_commands
      ~disclosure
      ~compilation:(Bounded Chatml_compilation.default_limits)
      ~max_dependency_depth:32
      ~execution:Chatml_execution.default_limits
      ~max_output_bytes:65536
      ~receipt
      ~max_agent_sources:capacity.max_agent_sources
      ~max_agent_source_work_steps:1_000_000
      ~max_native_bindings:capacity.max_native_bindings
      ~workspace_cleanup
      ~max_agent_cache_entries:32
  in
  let%bind host =
    Dev.Limits.with_max_active_agent_calls
      host
      ~max_active_calls:capacity.max_active_agent_calls
  in
  let%bind build =
    Chatml_project_build.Limits.create
      ~max_work_steps:1_000_000
      ~max_input_bytes:(Some mib)
      ~document
    |> admitted
  in
  let%bind capture =
    Authoring_context_service.Developer.Limits.create
      ~max_prepared_input_bytes:Chatmd_source_bundle.default_limits.max_source_bytes
      ~max_document_origin_bytes:65536
      ~max_editor_versions:32
      ~build
    |> admitted
  in
  let%map publisher =
    Agent_store.Developer_result_publisher.Limits.create
      ~inline_bytes:1024
      ~max_result_bytes:65536
      ~max_pending:(Int.min capacity.max_active_agent_calls 64)
  in
  capture, host, publisher, receipt
;;

let repl_preset (capacity : Config.Capacity.t) =
  let open Result.Let_syntax in
  let retained = capacity.max_retained_tasks in
  if retained > 1024
  then Error (invalid ())
  else (
    let%bind sites =
      Chatml.Chatml_syntax_sites.Limits.create ~max_sites:4096 ~max_name_bytes:16384
      |> admitted
    in
    let%bind parse =
      Chatml.Chatml_parse.Submission.Limits.create
        ~max_source_bytes:capacity.max_repl_source_bytes
        ~max_tokens:65536
        ~max_work_steps:100_000
        ~sites
      |> admitted
    in
    let%bind display =
      Chatml_typechecker.Type_snapshot.Limits.create
        ~max_nodes:4096
        ~max_edges:4096
        ~max_label_bytes:16384
      |> admitted
    in
    let%bind root_copy =
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
      |> admitted
    in
    let%bind typing_limits =
      Chatml_repl_typing.Limits.create
        ~parse
        ~display
        ~max_display_name_bytes:4096
        ~max_statements:16
        ~root_copy
      |> admitted
    in
    let%bind scope_limits =
      Chatml_configured_lookup.Repl_scope.Limits.create
        ~max_snippets:capacity.max_repl_submissions_per_epoch
        ~max_identity_bytes:4096
      |> admitted
    in
    let%bind root_limits =
      Chatml_dependency_session.Roots.Limits.create
        ~max_checked_modules:16
        ~max_edges:64
        ~max_dependency_depth:16
      |> admitted
    in
    let%bind evaluator_limits =
      Chatml_repl_execution.Limits.create
        ~max_preview_bytes:4096
        ~max_preview_nodes:1024
        ~max_preview_depth:16
        ~max_diagnostic_bytes:1024
        ~max_retained_tasks:retained
      |> admitted
    in
    let%bind type_display_limits =
      Chatml_type_display.Limits.create
        ~max_nodes:4096
        ~max_edges:8192
        ~max_depth:32
        ~max_work_steps:131072
        ~max_text_bytes:4096
        ~max_output_bytes:32768
      |> admitted
    in
    Dev.Repl.create
      ~scope_limits
      ~root_limits
      ~typing_limits
      ~evaluator_limits
      ~execution_policy:(Bounded Chatml_execution.default_limits)
      ~type_display_limits
      ~max_instances:capacity.max_repl_instances
      ~max_keys_per_instance:capacity.max_repl_keys_per_instance
      ~max_submissions_per_epoch:capacity.max_repl_submissions_per_epoch
      ~max_source_bytes:capacity.max_repl_source_bytes
      ~max_receipt_bytes:4096
      ~max_total_receipt_bytes:(1024 * 1024)
      ~max_total_type_detail_bytes:(1024 * 1024)
      ~max_task_origins:retained
      ~max_reference_page_bytes:(Int.max 4096 (retained * 512)))
;;

let bounded_product value factor extra =
  if value <= 0 || factor <= 0 || extra < 0 || value > (Int.max_value - extra) / factor
  then Error (invalid ())
  else Ok ((value * factor) + extra)
;;

let portable_preset (capacity : Config.Capacity.t) initialize_limits ~pending =
  let open Result.Let_syntax in
  let request_bytes =
    Int.min initialize_limits.P.Initialize.Limits.max_request_bytes (1024 * 1024)
  in
  (* Mirrors the actual portable admission's conservative per-pending
     display/framing reserve; excess configured capacity rejects before an
     Endpoint or broker is constructed. *)
  let%bind page_bytes = bounded_product pending ((6 * 256) + (12 * 1024)) 256 in
  let%bind page_nodes = bounded_product pending 32 16 in
  let%bind page_fields = bounded_product pending 24 8 in
  let%bind response_page_bytes = bounded_product 1 page_bytes 512 in
  let%bind response_page_nodes = bounded_product 1 page_nodes 8 in
  let%bind response_page_fields = bounded_product 1 page_fields 8 in
  let%bind request = json ~bytes:request_bytes ~depth:24 ~fields:2048 ~nodes:4096 in
  let%bind receipt = json ~bytes:65536 ~depth:16 ~fields:1024 ~nodes:2048 in
  let%bind page = json ~bytes:page_bytes ~depth:8 ~fields:page_fields ~nodes:page_nodes in
  let%bind response =
    json
      ~bytes:(Int.max (65536 + 4096) response_page_bytes)
      ~depth:24
      ~fields:(Int.max (1024 + 32) response_page_fields)
      ~nodes:(Int.max (2048 + 32) response_page_nodes)
  in
  let%bind wire_limits =
    P.Developer_common.Limits.create
      ~request
      ~receipt
      ~response
      ~approval_page:page
      ~max_output_bytes:65536
      ~max_fixture_steps:64
      ~max_fixture_bytes:65536
      ~max_approval_display_bytes:256
      ~max_approval_decision_reason_bytes:256
      ~max_approval_page:pending
      ~max_event_bytes:4096
  in
  let%bind policy =
    Dev.Portable.create
      ~wire_limits
      ~max_connections:128
      ~max_watches_per_connection:capacity.max_watches_per_connection
      ~max_pending_hints_per_connection:64
  in
  Ok policy
;;

let create
      ~capacity
      ~(approvals : Config.approvals)
      ~(repl : Config.repl)
      ~portable
      ~initialize_limits
  =
  let open Result.Let_syntax in
  let%bind () = Config.Capacity.validate capacity in
  let%bind capture, host, publisher, _receipt = create_base capacity in
  let%bind approvals =
    match approvals with
    | Config.Disabled -> Ok None
    | Once ->
      Developer_approval_owner.Limits.create
        ~max_pending:capacity.max_pending_approvals
        ~max_request_bytes:4096
        ~max_display_bytes:256
        ~max_decision_reason_bytes:256
        ~wait_seconds:30.
      |> Result.map ~f:Option.some
  in
  let%bind repl =
    match repl with
    | Config.Disabled -> Ok None
    | Standard_v1 -> repl_preset capacity |> Result.map ~f:Option.some
  in
  let%map portable =
    if portable
    then
      portable_preset
        capacity
        initialize_limits
        ~pending:(if Option.is_some approvals then capacity.max_pending_approvals else 1)
      |> Result.map ~f:Option.some
    else Ok None
  in
  { capture; host; publisher; approvals; repl; portable }
;;
