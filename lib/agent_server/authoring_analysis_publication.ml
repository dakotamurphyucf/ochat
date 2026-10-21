open! Core
module P = Agent_protocol
module Core_owner = Authoring_context_service_core
module Project = Chatml_authoring_analysis
module Document = Chatmd_authoring_analysis
module Ledger = Chatml_authoring_context

module Prepared = struct
  type outcome =
    | No_acquisition
    | Parsed_only
    | Checked_project
    | Checked_document

  type t =
    | Unavailable_result of P.Authoring.Analysis.Analyze.t
    | Parsed_result of
        { retained : Core_owner.Retained_document_capture.t
        ; document_after : Document.Document_cache.t
        ; index_after : Authoring_analysis_index.t
        ; templates_after : Authoring_checked_template_cache.t
        ; response : P.Authoring.Analysis.Analyze.t
        }
    | Project_result of
        { project_after : Project.Project_cache.t
        ; document_after : Document.Document_cache.t
        ; index_after : Authoring_analysis_index.t
        ; templates_after : Authoring_checked_template_cache.t
        ; response : P.Authoring.Analysis.Analyze.t
        }
    | Document_result of
        { retained : Core_owner.Retained_document_capture.t
        ; document_after : Document.Document_cache.t
        ; index_after : Authoring_analysis_index.t
        ; templates_after : Authoring_checked_template_cache.t
        ; response : P.Authoring.Analysis.Analyze.t
        }

  let outcome = function
    | Unavailable_result _ -> No_acquisition
    | Parsed_result _ -> Parsed_only
    | Project_result _ -> Checked_project
    | Document_result _ -> Checked_document
  ;;

  let response = function
    | Unavailable_result response
    | Parsed_result { response; _ }
    | Project_result { response; _ }
    | Document_result { response; _ } -> response
  ;;

  let unavailable ~response = Unavailable_result response

  let parsed_only ~retained ~document_after ~index_after ~templates_after ~response =
    Parsed_result { retained; document_after; index_after; templates_after; response }
  ;;

  let checked_project
        ~project_after
        ~document_after
        ~index_after
        ~templates_after
        ~response
    =
    Project_result
      { project_after; document_after; index_after; templates_after; response }
  ;;

  let checked_document ~retained ~document_after ~index_after ~templates_after ~response =
    Document_result { retained; document_after; index_after; templates_after; response }
  ;;
end

let publish_now owner ~borrowed_index prepared =
  if not (phys_equal borrowed_index owner.Core_owner.analysis_index)
  then Error Core_owner.Error.Stale_context
  else (
    (match prepared with
     | Prepared.Unavailable_result _ -> ()
     | Parsed_result { retained; document_after; index_after; templates_after; _ } ->
       owner.document_cache <- document_after;
       owner.document_capture <- Some retained;
       owner.analysis_index <- index_after;
       owner.checked_templates <- templates_after
     | Project_result { project_after; document_after; index_after; templates_after; _ }
       ->
       owner.project_cache <- project_after;
       owner.document_cache <- document_after;
       owner.analysis_index <- index_after;
       owner.checked_templates <- templates_after
     | Document_result { retained; document_after; index_after; templates_after; _ } ->
       owner.document_cache <- document_after;
       owner.document_capture <- Some retained;
       owner.analysis_index <- index_after;
       owner.checked_templates <- templates_after);
    Ok (Prepared.response prepared))
;;
