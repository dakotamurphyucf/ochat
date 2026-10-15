open! Core
module P = Agent_protocol
module Service = Authoring_context_service

module Source = struct
  type t =
    { read : Authoring_source_host.Read_policy.t
    ; document : Authoring_source_host.Document_policy.t
    ; limits : Authoring_source_host.Limits.t
    }

  let create ~read ~document ~limits = { read; document; limits }
end

module Upload = struct
  type t =
    { limits : Agent_store.Authoring_upload_store.Limits.t
    ; grant : Authoring_upload_host.Upload_policy.t
    ; publication : Authoring_upload_host.Publication.t option
    ; rebind : Authoring_upload_host.Rebind.t option
    }

  let create ~limits ~grant ~publication ~rebind = { limits; grant; publication; rebind }
end

module Semantic = struct
  type t =
    { script_selection : Service.Semantic.Script_selection.Policy.t option
    ; project_metadata : Service.Semantic.Project_metadata.Policy.t option
    ; project_build_limits : Chatml_project_build.Limits.t
    ; limits : Service.Semantic.Limits.t
    ; prefix_limits : Service.Semantic.Prefix_limits.t
    ; query_limits : Service.Semantic.Query_limits.t
    ; document_dir : Eio.Fs.dir_ty Eio.Path.t
    }

  let create
        ~script_selection
        ~project_metadata
        ~project_build_limits
        ~limits
        ~prefix_limits
        ~query_limits
        ~document_dir
    =
    { script_selection
    ; project_metadata
    ; project_build_limits
    ; limits
    ; prefix_limits
    ; query_limits
    ; document_dir
    }
  ;;
end

module Assistance = struct
  type t =
    { lexical : bool
    ; assistance : bool
    ; selected_tools : Service.Assistance.Selected_tool_policy.t option
    ; project_build_limits : Chatml_project_build.Limits.t
    ; limits : Service.Assistance.Limits.t
    }

  let create ~lexical ~assistance ~selected_tools ~project_build_limits ~limits =
    if not (lexical || assistance)
    then Or_error.error_string "no authoring assistance family is installed"
    else Ok { lexical; assistance; selected_tools; project_build_limits; limits }
  ;;
end

type t =
  { core : bool
  ; source : Source.t option
  ; upload : Upload.t option
  ; supplied_document : Service.Supplied_document_policy.t option
  ; selection : Service.Selection_policy.t
  ; context_limits : Chatml_authoring_context.Limits.t
  ; analysis_policy : Chatml_authoring_analysis.Policy.t
  ; protocol_limits : P.Authoring.Limits.t
  ; service_limits : Service.Limits.t
  ; semantic : Semantic.t option
  ; assistance : Assistance.t option
  }

let invalid message = Error (P.Error.invalid_request message)

let validate_initialize t limits =
  if
    limits.P.Initialize.Limits.max_request_bytes <= 0
    || t.protocol_limits.max_request_bytes > limits.max_request_bytes
  then invalid "authoring request ceiling exceeds initialize ceiling"
  else Ok ()
;;

let create
      ~core
      ~source
      ~upload
      ~supplied_document
      ~selection
      ~context_limits
      ~analysis_policy
      ~protocol_limits
      ~service_limits
      ~semantic
      ~assistance
      ~initialize_limits
  =
  let open Result.Let_syntax in
  if (not core) && Option.is_none upload
  then invalid "authoring installation has no enabled family"
  else if
    (not core)
    && (Option.is_some source
        || Option.is_some supplied_document
        || Option.is_some semantic
        || Option.is_some assistance)
  then invalid "authoring context families require core"
  else if
    core
    && Option.is_none source
    && Option.is_none upload
    && Option.is_none supplied_document
  then invalid "authoring core has no source origin"
  else (
    let t =
      { core
      ; source
      ; upload
      ; supplied_document
      ; selection
      ; context_limits
      ; analysis_policy
      ; protocol_limits
      ; service_limits
      ; semantic
      ; assistance
      }
    in
    let%map () = validate_initialize t initialize_limits in
    t)
;;

let profile_error _ = P.Error.invalid_request "invalid built-in authoring profile"
let admitted result = Result.map_error result ~f:profile_error

let basic_supplied ~(initialize_limits : P.Initialize.Limits.t) =
  let open Result.Let_syntax in
  let request_bytes = Int.min initialize_limits.max_request_bytes (2 * 1024 * 1024) in
  if request_bytes <= 0
  then invalid "initialize request ceiling is not positive"
  else (
    let source_bytes = Int.min request_bytes (1024 * 1024) in
    let%bind protocol_limits =
      P.Authoring.Limits.create
        ~max_sources:64
        ~max_source_bytes:source_bytes
        ~max_context_source_bytes:(4 * 1024 * 1024)
        ~max_request_bytes:request_bytes
        ~max_result_bytes:(1024 * 1024)
        ~max_path_candidates:64
    in
    let%bind context_limits =
      Chatml_authoring_context.Limits.create
        ~max_contexts:16
        ~max_sources:64
        ~max_open_documents:16
        ~max_document_bytes:source_bytes
        ~max_effective_input_bytes:(4 * 1024 * 1024)
        ~max_context_source_bytes:(4 * 1024 * 1024)
        ~max_total_source_bytes:(16 * 1024 * 1024)
      |> admitted
    in
    let%bind recording =
      Chatml_typechecker.Semantic.Limits.create
        ~max_facts:20_000
        ~max_edges:40_000
        ~max_label_bytes:400_000
        ~max_work_steps:400_000
      |> admitted
    in
    let%bind type_graph =
      Chatml_semantics.Frozen.Limits.create
        ~max_nodes:20_000
        ~max_edges:40_000
        ~max_label_bytes:400_000
      |> admitted
    in
    let%bind semantic =
      Chatml_semantics.Limits.create
        ~max_input_bytes:(2 * 1024 * 1024)
        ~recording
        ~type_graph
        ~max_published_facts:20_000
        ~max_published_edges:40_000
        ~max_published_label_bytes:400_000
        ~max_work_steps:400_000
      |> admitted
    in
    let%bind analysis_policy =
      Chatml_authoring_analysis.Policy.create
        ~compilation:{ wall_seconds = 5.; max_source_bytes = 512 * 1024 }
        ~semantic
        ~max_dependency_depth:16
        ~max_results:16
        ~max_projected_diagnostics:64
        ~max_projected_sites:256
        ~max_projection_sources:64
        ~max_position_index_entries:4096
        ~max_projection_source_bytes:(4 * 1024 * 1024)
      |> admitted
    in
    let%bind service_limits =
      Service.Limits.create
        ~max_connections:128
        ~max_active_requests_per_connection:4
        ~max_roots_per_connection:8
        ~max_response_bytes:(1024 * 1024)
        ~max_provisional_results:16
      |> admitted
    in
    let%bind assistance_limits =
      Service.Assistance.Limits.create
        ~protocol_limits
        ~max_scan_work_steps:400_000
        ~max_assistance_work_steps:400_000
        ~max_contract_identity_bytes:(16 * 1024 * 1024)
        ~max_contract_work_steps:1_000_000
        ~max_projection_work_steps:400_000
        ~max_parser_input_bytes:source_bytes
        ~max_imported_inputs:64
        ~max_selected_tools:64
        ~max_tool_metadata_bytes:(64 * 1024)
      |> admitted
    in
    let%bind assistance =
      let%bind project_build_limits =
        Chatml_project_build.Limits.create
          ~max_work_steps:400_000
          ~max_input_bytes:(Some (4 * 1024 * 1024))
          ~document:Document_schema.Limits.default
        |> admitted
      in
      Assistance.create
        ~lexical:true
        ~assistance:true
        ~selected_tools:None
        ~project_build_limits
        ~limits:assistance_limits
      |> admitted
    in
    create
      ~core:true
      ~source:None
      ~upload:None
      ~supplied_document:
        (Some
           (Service.Supplied_document_policy.create
              ~authorize:Operator_authorization.is_current))
      ~selection:
        (Service.Selection_policy.create
           ~project_contract:(fun _ ~root:_ ~project:_ ~selection:_ -> None)
           ~document_static:(fun _ ~requested_environment:_ -> None))
      ~context_limits
      ~analysis_policy
      ~protocol_limits
      ~service_limits
      ~semantic:None
      ~assistance:(Some assistance)
      ~initialize_limits)
;;

let upload_only ~initialize_limits ~upload =
  let open Result.Let_syntax in
  let%map policy = basic_supplied ~initialize_limits in
  { policy with
    core = false
  ; source = None
  ; upload = Some upload
  ; supplied_document = None
  ; semantic = None
  ; assistance = None
  }
;;
