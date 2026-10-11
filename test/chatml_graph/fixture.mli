open Core
module Captured = Chatml_source
module Prepared = Chatml_prepared_program
module Lookup = Chatml_configured_lookup

(** Explicit own-file scopes; first tuple is entry. Every implementation,
    including entry, is catalogued to exercise canonical overlap. *)
val input
  :  ?interface:string * string
  -> (string * string * (string * string) list) list
  -> Prepared.t

val source : Prepared.t -> string -> Captured.t
val context : Prepared.t -> string -> Lookup.t
val span : Captured.t -> Source.span

val checked
  :  ?checkpoint:(unit -> unit)
  -> ?max_dependency_depth:int
  -> Prepared.t
  -> ( Chatml_configured_check.Checked_unit.t Chatml_dependency_session.t
       , Chatml_diagnostic.t )
       Result.t

val ok : ('a, Chatml_diagnostic.t) Result.t -> 'a
val paths : Prepared.Module_id.t list -> string list
