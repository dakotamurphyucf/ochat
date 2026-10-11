open Core
open Chatml_inferred_type

module Engine : sig
  (** Operations borrowed from one compilation's inference engine. They operate
      only on the supplied probe types; name allocation is compilation-wide.
      [unify] returns expected type failures and preserves cancellation. Compiler
    signature entrypoints protect checkpoint exception origins before lending
    these operations; their immutable invocation callbacks never enter returned
    aliases, visibility or diagnostics. *)
  type t =
    { instantiate : typ -> typ
    ; unify : typ -> typ -> (unit, string) result
    ; fresh_name : unit -> string
    ; checkpoint : unit -> unit
    }
end

module Visibility = Chatml.Chatml_module_visibility

module Error : sig
  type t = Chatml_diagnostic.t
end

(** A checked transparent interface. Its schemes and aliases belong to the
    calling compilation. The runtime must apply [visibility] before exposing
    the module, while keeping private implementation bindings in closures. *)
type t

val values : t -> typ Env.t
val types : t -> Chatml_type_alias.t
val visibility : t -> Visibility.t

(** Match one implementation against a signature, including nested modules.
    [values] and [types] contain explicit implementation exports; [outer_types]
    is the lexical scope in which the signature is declared. All value claims
    share one probe, so aliases of mutable exports cannot specialize separately.
    On failure or cancellation, implementation inference cells are unchanged.
    Successful matching commits validated weak-variable constraints, selects
    public members and preserves the specification's universal requirements.
    An optional source is the explicit owner of the supplied raw signature;
    captured callers should use [check_captured] to preserve source/syntax pairing.
    Existing raw-span/string callers remain unowned compatibility adapters.
    No initializer or host capability is invoked. *)
val check
  :  ?source:Chatml_source.t
  -> Engine.t
  -> values:typ Env.t
  -> types:Chatml_type_alias.t
  -> outer_types:Chatml_type_alias.t
  -> signature:Chatml.Chatml_module_signature.t
  -> span:Source.span
  -> (t, Error.t) result

(** Check an exact captured companion interface. Primary signature claims own
    interface bytes; already owned alias failures retain their defining source.
    Metadata validation/allocation precedes the probe's non-cancellable commit.
    No checkpoint or captured inventory is retained by the result. *)
val check_captured
  :  Chatml.Chatml_parse.Interface.t
  -> Engine.t
  -> values:typ Env.t
  -> types:Chatml_type_alias.t
  -> outer_types:Chatml_type_alias.t
  -> (t, Error.t) result
