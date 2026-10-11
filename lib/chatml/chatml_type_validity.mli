open Core
open Chatml_inferred_type

(** Inspect compiler-owned types, including cyclic inference links. Traversals
    poll [checkpoint] and propagate cancellation unchanged. No mutation. *)
val has_recursive_type : ?checkpoint:(unit -> unit) -> typ -> bool

(** Validate lexical recursive binders, contractive recursion and internal
    row/payload/representation kinds. Generalized and weak variables remain
    admissible; this is not a check that a type is closed. *)
val check_contractive : ?checkpoint:(unit -> unit) -> typ -> (unit, string) result
