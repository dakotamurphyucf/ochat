open Core

(** Private captured-route planner. It owns the single work budget covering
    route admission, exact captured catalog planning, prior selection checks
    and assembly; the public Build facade retains only the completed wrapper. *)

module Route : sig
  type t

  val create
    :  declaration:Chatml_source.Id.t
    -> origin:Chatml_project.Lock.Catalog.origin
    -> dependencies:Chatml_project.Lock.Dependency_selection.t list
    -> t

  val declaration : t -> Chatml_source.Id.t
end

val compose
  :  root_project:Chatml_source.Id.t
  -> selection:Chatml_project.Selection.request
  -> entry:Project_build_model.Entry.t
  -> sources:Chatml_source.t list
  -> routes:Route.t list
  -> previous:Project_build_assembly.t option
  -> lock_id:Chatml_source.Id.t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> limits:Project_build_model.Limits.t
  -> checkpoint:(unit -> unit)
  -> ( Project_build_assembly.t * Project_build_model.Accounting.t
       , Project_build_model.Error.t )
       result

val recompose
  :  Project_build_assembly.t
  -> sources:Chatml_source.t list
  -> lock_id:Chatml_source.Id.t
  -> contract:Chatml.Chatml_compiler_contract.t
  -> limits:Project_build_model.Limits.t
  -> checkpoint:(unit -> unit)
  -> ( Project_build_assembly.t * Project_build_model.Accounting.t
       , Project_build_model.Error.t )
       result
