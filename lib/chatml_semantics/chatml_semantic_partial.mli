open Core
module Identity = Chatml_semantic_identity
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Sites = Chatml.Chatml_syntax_sites

(** Same-run successfully parsed syntax and owned diagnostics ONLY. No graph,
    type, context, export or certified-reference accessors exist. *)
module Invalidity = Chatml_checked_graph.Invalidity

module Syntax : sig
  type t

  val source : t -> Captured.t
  val owner : t -> Prepared.Module_id.t
  val phase : t -> Chatml_configured_lookup.Phase.t
  val sites : t -> Sites.t
end

type t

val identity : t -> Identity.t
val invalidity : t -> Invalidity.t
val sources : t -> Captured.t list
val parsed : t -> Syntax.t list
val diagnostics : t -> Chatml_diagnostic.t list

(** Private construction; charge retained sites/labels/journal/diagnostic edges
    before publication. All source/phase rows came from this exact driver run. *)
val create
  :  Prepared.t
  -> identity:Identity.t
  -> invalidity:Invalidity.t
  -> parsed:Chatml_checked_graph.Parsed.t list
  -> diagnostic:Chatml_diagnostic.t
  -> budget:Chatml_semantic_budget.t
  -> checkpoint:(unit -> unit)
  -> t
