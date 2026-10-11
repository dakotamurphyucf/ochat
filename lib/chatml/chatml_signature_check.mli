open Core
open Chatml_inferred_type

module Engine : sig
  (** Operations borrowed from one compilation's inference engine. They operate
      only on the supplied probe types; name allocation is compilation-wide.
      [unify] returns expected type failures and preserves cancellation. *)
  type t =
    { instantiate : typ -> typ
    ; unify : typ -> typ -> (unit, string) result
    ; fresh_name : unit -> string
    ; checkpoint : unit -> unit
    }
end

module Visibility = Chatml.Chatml_module_visibility

module Error : sig
  type t =
    { message : string
    ; span : Source.span
    }
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
    No initializer or host capability is invoked. *)
val check
  :  Engine.t
  -> values:typ Env.t
  -> types:Chatml_type_alias.t
  -> outer_types:Chatml_type_alias.t
  -> signature:Chatml.Chatml_module_signature.t
  -> span:Source.span
  -> (t, Error.t) result
