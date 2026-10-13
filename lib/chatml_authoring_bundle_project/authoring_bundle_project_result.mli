(** Private immutable Project-result reconciliation. The caller owns the one
    attempt budget and passes its charged checkpoint; this module retains none. *)

type error =
  | Declaration of Chatml_project.Error.t
  | Mismatch

val selection_request
  :  Agent_protocol.Authoring_context.Context.Selection.t
  -> Chatml_project.Selection.request

val reconcile
  :  Agent_protocol.Authoring_bundle.Bundle.Manifest.Project.t
  -> Chatml_project_build.t
  -> root_source:Chatml_source.t
  -> expected_sources:Chatml_source.t list
  -> document:Document_schema.Limits.t
  -> checkpoint:(unit -> unit)
  -> (unit, error) result
