open Core
module Origin = Chatml_configured_lookup.Origin

(** Existing checker lexical value environment with explicit optional configured
    binding provenance. Type rows remain typ Env.t; only lexical tenv uses this
    module. No second name resolver, global/mutable origin table or new inference. *)
module Binding : sig
  type t

  val semantic : t -> Chatml_semantic_binding.t option
  val scheme : t -> Chatml_inferred_type.typ
  val origin : t -> Origin.t option

  (** Original implementation introduction range; validated on diagnostic/host use. *)
  val span : t -> Source.span option
end

type t

val empty : t

(** Authentic builtins/ordinary local bindings have no configured provenance. *)
val of_schemes : Chatml_inferred_type.typ Chatml_inferred_type.Env.t -> t

val find : t -> name:string -> Binding.t option

(** Existing generalization/value-restriction decisions happen before insertion.
    Local/parameter/recursive/pattern/module bindings shadow earlier imports and
    reset origin. Persistent scope snapshots preserve prior declaration targets. *)
val add_local : t -> name:string -> scheme:Chatml_inferred_type.typ -> t

(** Caller has checked existing lexical open collisions and actual public member.
    Preserve the original raw scheme/weak cells and exact canonical member path. *)
val add_imported
  :  t
  -> name:string
  -> scheme:Chatml_inferred_type.typ
  -> origin:Origin.t
  -> t

(** Functional update for already inserted bindings, preserving raw schemes/origin. *)
val with_span : t -> name:string -> span:Source.span -> t

(** Optional compiler-owned fact metadata only. Ordinary environments carry None;
    no recorder, source inventory, callback or inference machine is retained. *)
val semantic_context : t -> Chatml_semantic_recorder.Context.t option

val with_semantic_exn
  :  t
  -> name:string
  -> binding:Chatml_semantic_binding.t
  -> context:Chatml_semantic_recorder.Context.t
  -> t
