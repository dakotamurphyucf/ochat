(** ChatML resolver — lexical-address resolution and slot allocation.

    The module exposes two high-level helpers.  Everything else found in
    the corresponding [.ml] file is implementation detail and is *not*
    meant for external use. *)

open Chatml.Chatml_lang

(** [resolve_checked_program checked prog] resolves a program that has
    already been accepted by {!Chatml_typechecker.check_program}. *)
val resolve_checked_program
  : Chatml_typechecker.checked_program
  -> program
  -> program

(** [resolve_program prog] rewrites [prog] so that every variable is
    given an explicit lexical address ([EVarLoc]) and every binding site
    carries a pre-computed list of frame slots.  This convenience helper
    first type-checks the program strictly and raises on failure.
    Idempotent for already-resolved, well-typed programs. *)
val resolve_program : program -> program

(** [eval_program env prog] type-checks [prog].  If successful, it
    resolves and evaluates it in [env]; otherwise it returns the typing
    diagnostic and performs no evaluation. *)
val eval_program
  : env
  -> program
  -> (unit, Chatml_typechecker.diagnostic) result
