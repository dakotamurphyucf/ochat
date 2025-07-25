(** ChatML resolver — lexical-address resolution and slot allocation.

    The module exposes two high-level helpers.  Everything else found in
    the corresponding [.ml] file is implementation detail and is *not*
    meant for external use. *)

open Chatml.Chatml_lang

(** [resolve_program prog] rewrites [prog] so that every variable is
    given an explicit lexical address ([EVarLoc]) and every binding site
    carries a pre-computed list of frame slots.  The program must have
    been type-checked beforehand so that the resolver can access the
    principal type for each node.  Idempotent. *)
val resolve_program : program -> program

(** [eval_program env prog] is a convenience wrapper that calls
    {!resolve_program} and then delegates to {!Chatml_lang.eval_program}
    to execute the result. *)
val eval_program : env -> program -> unit
