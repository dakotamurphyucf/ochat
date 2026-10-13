open Core
module Lang = Chatml.Chatml_lang
module Id = Chatml_prepared_program.Module_id

module Unit : sig
  type t

  val create
    :  id:Id.t
    -> source:Chatml_source.t
    -> program:Lang.resolved_program
    -> visibility:Chatml.Chatml_module_visibility.t
    -> t

  val id : t -> Id.t
  val source : t -> Chatml_source.t
end

module Failure : sig
  type t =
    | Invalid_linkage of Chatml_diagnostic.t
    | Runtime of Chatml_diagnostic.t

  val diagnostic : t -> Chatml_diagnostic.t
end

(** Shared unit evaluator for ordinary linked runs and incremental REPL
    initialization. The caller supplies only authentic direct value-dependency
    IDs and completed public instances. Source, visibility, builtin control,
    trace phase and error classification follow the same path. No registry or
    graph traversal is hidden here; cancellation propagates unchanged. *)
val evaluate
  :  Unit.t
  -> surface:Chatml.Chatml_builtin_surface.surface
  -> completed:Lang.Configured_modules.t
  -> direct_dependencies:Id.t list
  -> phase:Chatml_trace.Event.Phase.t
  -> ?before_evaluation:(unit -> unit)
  -> ?control:Lang.execution_control
  -> unit
  -> (Lang.env * Lang.value, Failure.t) Result.t
