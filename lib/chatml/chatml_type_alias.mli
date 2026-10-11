open Core
open Chatml_inferred_type
module Syntax = Chatml.Chatml_type_expr

(** Persistent lexical type namespace. Declarations validate once with abstract
    parameters. Parameterized aliases retain their immutable syntax and defining
    namespace; application elaborates with actual arguments in that namespace,
    never in the caller's. This preserves legacy arrow normalization after
    substitution. Closed zero-parameter aliases reuse their compiled body.
    No initializer, tool or provider is ever executed. *)
type t

module Error : sig
  type t =
    { message : string
    ; span : Source.span
    }
end

val empty : t

(** Install an already compiled host alias. The body must be closed and contain
    no mutable inference variables. Existing names and primitive names reject.
    The constructor also validates recursive binders and internal type kinds. *)
val add_host
  :  t
  -> name:string
  -> body:typ
  -> checkpoint:(unit -> unit)
  -> (t, string) result

(** Declare a transparent alias. Duplicate/free parameters, wrong arity and
    nonregular or unguarded recursion reject before installation.
    All traversals poll [checkpoint];
    its exceptions (including cancellation) propagate unchanged. *)
val declare
  :  t
  -> Syntax.Declaration.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

(** Elaborate an annotation with no implicitly quantified parameters. *)
val elaborate : t -> Syntax.t -> checkpoint:(unit -> unit) -> (typ, Error.t) result
