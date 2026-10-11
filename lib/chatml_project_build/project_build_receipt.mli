open Core
module Model = Project_build_model
module Prepared = Chatml_prepared_program

(** Materialized own-catalog receipt and compiler scopes. Contains no acquisition
    key, checkpoint, native handle or inferred type. The caller admits the whole
    source closure through Prepared.create after the finished lock is captured. *)
type t

val create
  :  Project_build_budget.t
  -> Model.Catalog_input.t
  -> catalogs:Model.Catalog_input.t list
  -> contract:Chatml.Chatml_compiler_contract.t
  -> fingerprint:Chatml_source.Digest.t
  -> (t, Model.Error.t) Result.t

val catalog : t -> Chatml_project.Lock.Catalog.t
val scopes : t -> Prepared.Scope.t list

(** Exact source metadata; this does not authenticate acquisition provenance. *)
val source_entry : Chatml_source.t -> Chatml_project.Lock.Source_entry.t
