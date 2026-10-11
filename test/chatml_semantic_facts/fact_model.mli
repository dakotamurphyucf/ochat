open Core
module R = Chatml_typechecker.Semantic
module B = Chatml_typechecker.Semantic_binding

(** Small independent parent/delta interpreter for the emitted context protocol.
    It does not infer types or inspect checker environments. *)
val resolve
  :  R.Settled.t
  -> B.Context_id.t
  -> namespace:B.Namespace.t
  -> name:string
  -> B.Symbol_id.t option

val binding : R.Settled.t -> B.Symbol_id.t -> B.t
val authored : R.Settled.t -> file:string -> name:string -> kind:B.Kind.t -> B.t list
val reference_at : R.Settled.t -> file:string -> offset:int -> R.Fact.reference
val target_binding : R.Fact.reference -> B.Symbol_id.t
val offset : Chatml_source.Location.t -> int
