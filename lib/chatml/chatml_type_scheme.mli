open Core
module Inferred = Chatml_inferred_type
module Equality = Chatml.Chatml_equality_requirement

module Error : sig
  type t =
    | Representation_equality
    | Conflicting_equality of Equality.Conflict.t
  [@@deriving sexp_of]
end

(** Compiler-owned scheme transforms. The caller exclusively owns input cells
    during an invocation. Original Bound and Free cells are never mutated.
    Bound copies are memoized before descent, including cyclic backedges.
    Lower-level/non-quantified Free cells deliberately retain original identity.
    Lexical Mu/Rec_var names, constructor arities and raw row layers survive.

    A first Bound visit returns transformed contents, preserving the existing
    dereferenced root view; a revisit returns the copied Bound reference.
    Physical identity memo searches are linear in copied cell count and poll
    checkpoint. Traversal uses an explicit worklist, not input-sized recursion.
    Exceptions propagate unchanged; no callback or partial result is retained. *)

(** Quantify only Free levels strictly greater than [level], retaining their
    globally unique names and exact equality requirements. The checker owns
    value restriction, contractiveness and recursive-function rules. *)
val generalise : Inferred.typ -> level:int -> checkpoint:(unit -> unit) -> Inferred.typ

(** Each invocation owns separate value/row and representation-kind generic
    tables. Fresh Free cells use the engine's globally unique [fresh_name] at
    [level]. Repeated value equality requirements are combined on new cells.
    Bound memoization distinguishes these roles. Non-quantified Free cells are
    not copied. Expected equality conflicts return Error; fresh-name/checkpoint
    exceptions retain original identity and backtrace. *)
val instantiate
  :  Inferred.typ
  -> level:int
  -> fresh_name:(unit -> Inferred.name)
  -> checkpoint:(unit -> unit)
  -> (Inferred.typ, Error.t) Result.t
