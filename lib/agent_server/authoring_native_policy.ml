open! Core
module P = Agent_protocol
module Service = Authoring_context_service
module Host_policy = Authoring_installation.Host_policy
module Bound = Authoring_configured_project_policy

type t =
  { host_policy : Host_policy.t option
  ; configured_project : Bound.t option
  }

module Limits = struct
  type t =
    { max_contract_identity_bytes : int
    ; max_contract_work_steps : int
    ; source : Authoring_source_host.Limits.t
    ; analysis : Chatml_authoring_analysis.Policy.t
    ; project_build : Chatml_project_build.Limits.t
    ; semantic : Service.Semantic.Limits.t
    ; prefix : Service.Semantic.Prefix_limits.t
    ; query : Service.Semantic.Query_limits.t
    }

  let invalid () = P.Error.invalid_request "invalid configured authoring limits"

  let create
        ~max_contract_identity_bytes
        ~max_contract_work_steps
        ~source
        ~analysis
        ~project_build
        ~semantic
        ~prefix
        ~query
    =
    if
      max_contract_identity_bytes <= 0
      || max_contract_identity_bytes > 64 * 1024 * 1024
      || max_contract_work_steps <= 0
      || max_contract_work_steps > 1_000_000
    then Error (invalid ())
    else
      Ok
        { max_contract_identity_bytes
        ; max_contract_work_steps
        ; source
        ; analysis
        ; project_build
        ; semantic
        ; prefix
        ; query
        }
  ;;

  let standard =
    let open Result.Let_syntax in
    let admitted result = Result.map_error result ~f:(fun _ -> invalid ()) in
    let%bind source =
      Authoring_source_host.Limits.create
        ~max_contexts:16
        ~max_context_requests:64
        ~max_manifest_entries:128
        ~max_browse_entries:1024
        ~max_read_bytes:(256 * 1024)
        ~max_context_read_bytes:(4 * 1024 * 1024)
      |> admitted
    in
    let%bind analysis = Authoring_host_policy.standard_analysis_policy () in
    let%bind project_build =
      Chatml_project_build.Limits.create
        ~max_work_steps:400_000
        ~max_input_bytes:(Some (16 * 1024 * 1024))
        ~document:Document_schema.Limits.default
      |> admitted
    in
    let%bind semantic =
      Service.Semantic.Limits.create
        ~max_document_bundle_files:128
        ~max_document_bundle_bytes:(2 * 1024 * 1024)
        ~max_parser_work_steps:1_000_000
        ~max_diagnostic_work_steps:1_000_000
        ~max_selection_rows:128
        ~max_retained_selection_source_bytes:(16 * 1024 * 1024)
        ~max_retained_selection_input_bytes:(16 * 1024 * 1024)
        ~max_selection_work_steps:1_000_000
      |> admitted
    in
    let%bind prefix =
      Service.Semantic.Prefix_limits.create
        ~max_document_bundle_files:128
        ~max_document_bundle_bytes:(2 * 1024 * 1024)
        ~max_parser_work_steps:1_000_000
        ~max_host_work_steps:1_000_000
      |> admitted
    in
    let%bind type_display =
      Chatml_type_display.Limits.create
        ~max_nodes:512
        ~max_edges:1024
        ~max_depth:16
        ~max_work_steps:100_000
        ~max_text_bytes:1024
        ~max_output_bytes:4096
      |> admitted
    in
    let%bind projection =
      Chatml_authoring_query.Limits.create
        ~max_items:32
        ~max_member_depth:4
        ~max_member_edges:128
        ~max_label_bytes:4096
        ~max_work_steps:100_000
        ~type_display
      |> admitted
    in
    let%bind query =
      Service.Semantic.Query_limits.create ~max_host_work_steps:100_000 ~projection
      |> admitted
    in
    create
      ~max_contract_identity_bytes:(40 * 1024 * 1024)
      ~max_contract_work_steps:1_000_000
      ~source
      ~analysis
      ~project_build
      ~semantic
      ~prefix
      ~query
  ;;

  let max_contract_identity_bytes t = t.max_contract_identity_bytes
  let max_contract_work_steps t = t.max_contract_work_steps
end

let bind_error = function
  | Bound.Error.Invalid_configuration ->
    P.Error.invalid_request "invalid configured authoring access"
  | Source_host_closed -> P.Error.invalid_request "configured authoring source is closed"
  | Foreign_domain ->
    P.Error.invalid_request "configured authoring source domain mismatch"
  | Root_binding_mismatch ->
    P.Error.invalid_request "configured authoring root binding mismatch"
  | Contract_limit -> P.Error.invalid_request "configured authoring contract limit"
;;

let create
      ~base
      ~configured
      ~contracts
      ~source_configuration
      ~source_host
      ~initialize_limits
      ~document_dir
      ~(limits : Limits.t)
      ~checkpoint
  =
  match configured with
  | None -> Ok { host_policy = base; configured_project = None }
  | Some configuration ->
    let open Result.Let_syntax in
    let%bind base =
      Option.value_map
        base
        ~default:(Error (P.Error.invalid_request "configured authoring is disabled"))
        ~f:Result.return
    in
    let%bind () = Host_policy.validate_initialize base initialize_limits in
    let%bind bound =
      Bound.bind
        source_host
        ~configuration
        ~project_sources:source_configuration
        ~contracts
        ~max_contract_identity_bytes:limits.max_contract_identity_bytes
        ~max_work_steps:limits.max_contract_work_steps
        ~checkpoint
      |> Result.map_error ~f:bind_error
    in
    let%bind policy =
      Host_policy.with_configured_projects
        base
        ~bound
        ~source_limits:limits.source
        ~analysis_policy:limits.analysis
    in
    let project_metadata =
      Service.Semantic.Project_metadata.Policy.create
        ~authorize_now:
          (fun
            ~original_actor ~actor ~current:_ ~parsed:_ ~declaration:_ ~capture ->
          Bound.project_capture_authorized bound ~original_actor ~actor capture)
    in
    let semantic =
      Host_policy.Semantic.create
        ~script_selection:None
        ~project_metadata:(Some project_metadata)
        ~project_build_limits:limits.project_build
        ~limits:limits.semantic
        ~prefix_limits:limits.prefix
        ~query_limits:limits.query
        ~document_dir
    in
    let%map policy = Host_policy.with_semantic policy ~semantic in
    { host_policy = Some policy; configured_project = Some bound }
;;

let host_policy t = t.host_policy
let configured_project t = t.configured_project
