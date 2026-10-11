open Core
module Model = Project_build_model

(** Actual entry capture, source-owned configured scope and selected receipt.
    Captured host entries use the root's own environment; they never acquire a
    filename override or invented file origin. Final assembly must reconcile any
    repeated source ID against the full catalog inventory before admission. *)
type t

val create
  :  Project_build_budget.t
  -> Model.Catalog_input.t
  -> catalogs:Model.Catalog_input.t list
  -> entry:Model.Entry.t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> (t, Model.Error.t) Result.t

val source : t -> Chatml_source.t
val scope : t -> Chatml_prepared_program.Scope.t
val receipt : t -> Chatml_project.Lock.Entry.t
val environment : t -> string
