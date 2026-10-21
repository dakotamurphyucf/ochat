open! Core
module Owner = Authoring_context_service_core
module Publication = Authoring_analysis_publication
module Index = Authoring_analysis_index
module Wire = Authoring_analysis_wire
module Overlay = Authoring_project_overlay
module Project = Chatml_authoring_analysis
module Document = Chatmd_authoring_analysis
module Ledger = Chatml_authoring_context
module Analysis = Agent_protocol.Authoring.Analysis
module Template = Authoring_semantic_template

let check_response_size owner response =
  match
    Wire.within_host_limit response ~max_bytes:owner.Owner.limits.max_response_bytes
  with
  | Ok () -> Ok ()
  | Error _ -> Error Owner.Error.Resource_limit
;;

let prepare
      owner
      current
      ~env
      ~build_limits
      ~checked_template
      ~template_limits
      ~checkpoint
  =
  let open Result.Let_syntax in
  let%bind project_before, document_before, index_before =
    Owner.Current.analysis_heads current
  in
  let%bind inputs = Owner.Current.project_inputs current ~checkpoint in
  let%bind prepared =
    match Overlay.prepare current ~limits:build_limits ~checkpoint with
    | Ok prepared -> Ok prepared
    | Error (Overlay.Error.Context error) -> Error error
    | Error Overlay.Error.No_selected_contract -> Error Owner.Error.Selection_unavailable
    | Error Overlay.Error.Work_limit -> Error Owner.Error.Resource_limit
    | Error Overlay.Error.Lock_identity_unavailable -> Error Owner.Error.Resource_limit
    | Error (Overlay.Error.Invalid_overlay _) -> Error Owner.Error.Invalid_request
  in
  let contract = Overlay.Prepared.contract prepared in
  let%bind templates_after =
    Template.project
      current
      ~inputs
      ~receipt:(Overlay.Prepared.overlay prepared)
      ~contract
      ~requested:checked_template
      ~limits:template_limits
      ~env
      ~checkpoint
  in
  let%bind candidate =
    Project.Project_candidate.analyze_validated_with_contract
      inputs
      ~receipt:(Overlay.Prepared.overlay prepared)
      ~contract
      ~policy:owner.Owner.analysis_policy
      ~env
      ~checkpoint
    |> Result.map_error ~f:Authoring_semantic_error.project
  in
  let%bind project_after =
    Project.Project_cache.publish
      project_before
      candidate
      ~occupied_elsewhere:(Document.Document_cache.retained_count document_before)
      ~checkpoint
    |> Result.map_error ~f:(function
      | Project.Error.Retention_limit -> Owner.Error.Resource_limit
      | _ -> Owner.Error.Stale_context)
  in
  let document_after =
    Document.Document_cache.transition
      document_before
      ~contexts:(Project.Project_cache.contexts project_after)
  in
  let payload = Project.Project_candidate.payload candidate in
  let stamp = Project.Project_payload.stamp payload in
  let checked = Project.Project_payload.checked payload in
  let%bind capture =
    Wire.project_capture ~expected:(Owner.Current.expected current) ~stamp
    |> Result.map_error ~f:Authoring_semantic_error.wire
  in
  let%bind index_after, snapshot =
    match Project.Checked.status checked with
    | Project.Checked.Failed _ ->
      Ok (Index.retire index_before ~identity:(Ledger.Inputs.identity inputs), None)
    | Project.Checked.Complete _ ->
      Index.prepare_complete
        index_before
        ~identity:(Ledger.Inputs.identity inputs)
        ~capture
        ~validated_graph:(Project.Checked.input_fingerprint checked)
      |> Result.map ~f:(fun (index, snapshot) -> index, Some snapshot)
      |> Result.map_error ~f:(fun _ -> Owner.Error.Resource_limit)
  in
  let%bind response =
    Wire.project
      ~contexts:(Project.Project_cache.contexts project_after)
      ~expected:(Owner.Current.expected current)
      ~stamp
      ~checked
      ~snapshot
      ~policy:owner.Owner.analysis_policy
      ~checkpoint
    |> Result.map_error ~f:Authoring_semantic_error.wire
  in
  let%map () = check_response_size owner response in
  Publication.Prepared.checked_project
    ~project_after
    ~document_after
    ~index_after
    ~templates_after
    ~response
;;
