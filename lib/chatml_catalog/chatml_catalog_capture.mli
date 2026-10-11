open Core
module Plan = Chatml_catalog_plan
module Error = Chatml_catalog_error

module Binding : sig
  type t

  val name : t -> string
  val module_id : t -> Chatml_prepared_program.Module_id.t
  val declared_at : t -> Chatml_project.Declared_at.t
end

module Accounting : sig
  (** Only validated immutable source facts. Enumeration/work/transport budget
      receipts stay with each acquisition owner, outside this pure artifact. *)
  type t

  val source_count : t -> int
  val source_bytes : t -> int
end

type t

val create
  :  Plan.t
  -> sources:Chatml_source.t list
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val project : t -> Chatml_project.t
val selection : t -> Chatml_project.Selection.t
val sources : t -> Chatml_source.t list
val entry : t -> Chatml_source.Id.t option
val definitions : t -> Chatml_prepared_program.Definition.t list
val bindings : t -> Binding.t list
val accounting : t -> Accounting.t
val find_source : t -> Chatml_source.Id.t -> Chatml_source.t option

(** Resolve a captured planned member's whole environment. Poll the member
    search and long namespace/path comparisons when [checkpoint] is supplied;
    callback exceptions propagate unchanged and the callback is not retained.
    Foreign, absent or unplanned sources return [None]. *)
val scope_for
  :  ?checkpoint:(unit -> unit)
  -> t
  -> source:Chatml_source.Id.t
  -> Chatml_project.Scope_spec.t option
