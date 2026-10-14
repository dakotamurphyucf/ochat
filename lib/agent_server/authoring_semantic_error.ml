open! Core
module Owner = Authoring_context_service_core
module Compiler = Chatml_compilation
module Semantics = Chatml_semantics
module Sites = Chatml.Chatml_syntax_sites
module Recorder = Chatml_typechecker.Semantic
module Graph = Chatml_typechecker.Type_snapshot
module Build = Chatml_project_build

let compilation = function
  | Compiler.Analysis_error.Policy error ->
    (match error.code with
     | "chatml.source_limit"
     | "chatml.metadata_allocation_limit"
     | "chatml.compile_timeout"
     | "chatml.compiler_limit" -> Owner.Error.Resource_limit
     | "chatml.entry_mismatch" -> Owner.Error.Selection_unavailable
     | "chatml.invalid_limits" -> Owner.Error.Invalid_request
     | _ -> Owner.Error.Analysis_unavailable)
  | Analysis error ->
    (match error with
     | Semantics.Error.Input_limit
     | Work_limit
     | Publication_fact_limit
     | Publication_edge_limit
     | Publication_label_byte_limit
     | Accounting_overflow -> Owner.Error.Resource_limit
     | Incompatible_contract -> Owner.Error.Selection_unavailable
     | Invalid_dependency_depth | Invalid_cursor | Invalid_input _ ->
       Owner.Error.Invalid_request
     | Sites (Sites.Error.Site_limit | Name_byte_limit | Accounting_overflow) ->
       Owner.Error.Resource_limit
     | Sites Invalid_location -> Owner.Error.Analysis_unavailable
     | Recording
         ( Recorder.Error.Fact_limit
         | Edge_limit
         | Label_byte_limit
         | Work_limit
         | Accounting_overflow ) -> Owner.Error.Resource_limit
     | Recording
         ( Recorder.Error.Foreign_binding
         | Foreign_context
         | Foreign_site
         | Missing_sites
         | Ambiguous_site
         | Failed_invocation
         | Already_settled
         | Invalid_location
         | Foreign_unit
         | Foreign_signature_stage
         | Signature_already_committed
         | Foreign_group_stage
         | Group_already_committed
         | Unfinished_group ) -> Owner.Error.Analysis_unavailable
     | Type_graph
         (Graph.Error.Node_limit | Edge_limit | Label_byte_limit | Accounting_overflow) ->
       Owner.Error.Resource_limit
     | Type_graph (Unbound_recursive_variable _) -> Owner.Error.Analysis_unavailable)
;;

let project = function
  | Chatml_authoring_analysis.Error.Compiler error -> compilation error
  | Receipt_mismatch | Result_identity_mismatch | Stale_generation | Policy_mismatch ->
    Owner.Error.Stale_context
  | Context _ -> Owner.Error.Stale_context
  | Invalid_occupancy -> Owner.Error.Invalid_request
  | Retention_limit -> Owner.Error.Resource_limit
;;

let wire = function
  | Authoring_analysis_wire.Error.Wire_limit | Projection_limit ->
    Owner.Error.Resource_limit
  | Invalid_stamp | Invalid_site -> Owner.Error.Analysis_unavailable
;;

let selected_entry_overlay = function
  | Authoring_selected_script_overlay.Error.Body_limit | Lock_identity_unavailable ->
    Owner.Error.Resource_limit
  | Invalid_selected_entry -> Owner.Error.Selection_unavailable
  | Recomposition error ->
    (match Build.Error.problem error with
     | Limit_exceeded | Accounting_overflow -> Owner.Error.Resource_limit
     | Missing_catalog _
     | Unexpected_catalog _
     | Namespace_conflict _
     | Invalid_repository_member _
     | Invalid_acquisition_key
     | Invalid_dependency _
     | Missing_module _
     | Compiler_requirement
     | Invalid_entry_selection
     | Invalid_lock_identity
     | Lock_mismatch
     | Missing_capture _
     | Duplicate_capture _
     | Needs_update
     | Declaration _
     | Catalog _
     | Prepared _ -> Owner.Error.Selection_unavailable)
;;
