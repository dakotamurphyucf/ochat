open Core
module Model = Project_build_model
module Project = Chatml_project
module Catalog = Chatml_catalog
module Prepared = Chatml_prepared_program
module Contract = Chatml.Chatml_compiler_contract

(** All selected declaration environments: the catalog's own selected environment
    plus scopes of every actually captured member, including companions and
    nonexecuted modules. Unique by own-package environment name. *)
val environments : Catalog.t -> checkpoint:(unit -> unit) -> Project.Scope_spec.t list

(** Referenced declared dependencies only, canonical by alias name. Syntactically
    declared aliases unused in every selected environment are not acquired. This
    is declaration selection, never executable module-edge analysis. *)
val dependencies : Catalog.t -> checkpoint:(unit -> unit) -> Project.Dependency.t list

val check_requirement
  :  Project.Scope_spec.t
  -> contract:Contract.t
  -> fingerprint:Chatml_source.Digest.t
  -> checkpoint:(unit -> unit)
  -> (unit, Model.Error.t) Result.t

(** Resolve immutable configured bindings against the selected catalog closure.
    No source parser, inference, acquisition or ambient name environment. *)
val bindings
  :  Project_build_budget.t
  -> Model.Catalog_input.t
  -> catalogs:Model.Catalog_input.t list
  -> Project.Scope_spec.t
  -> (Prepared.Scope.binding list, Model.Error.t) Result.t
