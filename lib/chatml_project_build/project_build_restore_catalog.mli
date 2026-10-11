open Core
module Model = Project_build_model

(** Rebuild one own catalog from its exact declaration and selected captures.
    Derive prospective observations only from captured direct children of authored
    source directories; existing Discovery/Plan owns convention and override rules.
    No historical enumeration or optional-Missing proof is synthesized. *)
val create
  :  Project_build_budget.t
  -> project:Chatml_project.t
  -> request:Chatml_project.Selection.request
  -> sources:Chatml_source.t list
  -> (Chatml_catalog.t, Model.Error.t) Result.t
