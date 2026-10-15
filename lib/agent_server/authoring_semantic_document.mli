open! Core
module Owner = Authoring_context_service_core
module Publication = Authoring_analysis_publication
module Ledger = Chatml_authoring_context
module Sources = Authoring_source_host
module Document = Chatmd_authoring_analysis

(** Prepare one already selected genuine Document script result outside the
    final Owner gate. The caller has just acquired [capture] from its exact
    current root owner or supplied bytes, and [script] was admitted against
    that receipt by the existing Script.assess. A separate host policy proves
    the actual Build/contract and any decoded project route; this module only
    compiles, projects and prepares the sole cache/index transition. *)
val prepare_checked
  :  Owner.t
  -> Owner.Current.t
  -> inputs:Ledger.Document.Inputs.t
  -> retained:Owner.Retained_document_capture.t
  -> script:Document.Script.t
  -> selected_build:Chatml_project_build.t
  -> env:Eio_unix.Stdenv.base
  -> contract:Chatml.Chatml_compiler_contract.t
  -> checked_template:bool
  -> template_limits:Authoring_checked_template.Limits.t
  -> checkpoint:(unit -> unit)
  -> (Publication.Prepared.t, Owner.Error.t) result

(** Successful explicit capture with no authorized selected Build still
    replaces stale checked evidence for this context, retaining only genuine
    parser/import and optional independently admitted decoded project facts. *)
val prepare_parsed_only
  :  Owner.t
  -> Owner.Current.t
  -> inputs:Ledger.Document.Inputs.t
  -> retained:Owner.Retained_document_capture.t
  -> target:Agent_protocol.Authoring.Analysis.Target.selector
  -> reason:Agent_protocol.Authoring.Analysis.Analyze.unavailable
  -> checkpoint:(unit -> unit)
  -> (Publication.Prepared.t, Owner.Error.t) result
