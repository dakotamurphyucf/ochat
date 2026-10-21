open! Core
module Owner = Authoring_context_service_core
module Publication = Authoring_analysis_publication
module Index = Authoring_analysis_index
module Wire = Authoring_analysis_wire
module Ledger = Chatml_authoring_context
module Sources = Authoring_source_host
module Project = Chatml_authoring_analysis
module Document = Chatmd_authoring_analysis
module Analysis = Agent_protocol.Authoring.Analysis
module Template = Authoring_semantic_template

let response_within_owner owner response =
  Wire.within_host_limit response ~max_bytes:owner.Owner.limits.max_response_bytes
  |> Result.map_error ~f:(fun _ -> Owner.Error.Resource_limit)
;;

let prepare_parsed_only owner current ~inputs ~retained ~target ~reason ~checkpoint =
  let open Result.Let_syntax in
  let%bind _, document_before, index_before = Owner.Current.analysis_heads current in
  checkpoint ();
  let identity = Ledger.Document.Inputs.identity inputs in
  let%bind templates_after = Template.retire current ~identity in
  let document_after = Document.Document_cache.retire document_before ~identity in
  let index_after = Index.retire index_before ~identity in
  let%bind response =
    Analysis.Analyze.unavailable ~context:(Owner.Current.expected current) ~target ~reason
    |> Result.map_error ~f:(fun _ -> Owner.Error.Invalid_request)
  in
  let%map () = response_within_owner owner response in
  Publication.Prepared.parsed_only
    ~retained
    ~document_after
    ~index_after
    ~templates_after
    ~response
;;

let prepare_checked
      owner
      current
      ~inputs
      ~retained
      ~script
      ~selected_build
      ~env
      ~contract
      ~checked_template
      ~template_limits
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind project_before, document_before, index_before =
    Owner.Current.analysis_heads current
  in
  let%bind templates_after =
    Template.document
      current
      ~inputs
      ~script
      ~selected:selected_build
      ~contract
      ~requested:checked_template
      ~limits:template_limits
      ~env
      ~checkpoint
  in
  let%bind candidate =
    Document.Candidate.analyze_with_contract
      script
      ~contract
      ~policy:owner.Owner.analysis_policy
      ~env
      ~checkpoint
    |> Result.map_error ~f:(function
      | Document.Error.Compiler error -> Authoring_semantic_error.project error
      | Membership_limit | Retention_limit -> Owner.Error.Resource_limit
      | Stale_document | Selection_mismatch | Policy_mismatch -> Owner.Error.Stale_context
      | Invalid_selection
      | Foreign_declaration
      | Ambiguous_declaration
      | Script_body_mismatch
      | Projection _
      | Invalid_occupancy -> Owner.Error.Invalid_request)
  in
  let%bind document_after =
    Document.Document_cache.publish
      document_before
      ~current_inputs:inputs
      ~occupied_elsewhere:(Project.Project_cache.retained_count project_before)
      candidate
      ~checkpoint
    |> Result.map_error ~f:(function
      | Document.Error.Retention_limit -> Owner.Error.Resource_limit
      | _ -> Owner.Error.Stale_context)
  in
  let source = Ledger.Document.Inputs.source inputs in
  let contexts = Project.Project_cache.contexts project_before in
  let capture_receipt = Owner.Retained_document_capture.capture retained in
  let%bind capture =
    Wire.document_capture
      ~contexts
      ~expected:(Owner.Current.expected current)
      ~source
      ~script
      ~candidate
    |> Result.map_error ~f:Authoring_semantic_error.wire
  in
  let identity = Ledger.Document.Inputs.identity inputs in
  let checked = Document.Candidate.checked candidate in
  let%bind index_after, snapshot =
    match Project.Checked.status checked with
    | Project.Checked.Failed _ -> Ok (Index.retire index_before ~identity, None)
    | Project.Checked.Complete _ ->
      Index.prepare_complete
        index_before
        ~identity
        ~capture
        ~validated_graph:(Project.Checked.input_fingerprint checked)
      |> Result.map ~f:(fun (index, snapshot) -> index, Some snapshot)
      |> Result.map_error ~f:(fun _ -> Owner.Error.Resource_limit)
  in
  let%bind response =
    Wire.document
      ~contexts
      ~expected:(Owner.Current.expected current)
      ~source
      ~script
      ~candidate
      ~input_sources:
        (Sources.Document_context.Parsed_capture.input_sources capture_receipt)
      ~snapshot
      ~policy:owner.Owner.analysis_policy
      ~checkpoint
    |> Result.map_error ~f:Authoring_semantic_error.wire
  in
  let%map () = response_within_owner owner response in
  Publication.Prepared.checked_document
    ~retained
    ~document_after
    ~index_after
    ~templates_after
    ~response
;;
