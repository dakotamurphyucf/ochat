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

type eval_error =
  | Type_diagnostic of Chatml_typechecker.diagnostic
  | Runtime_diagnostic of runtime_error

(** [run_program env prog] is the canonical public runner
    for ChatML programs.  It strictly type-checks [prog], resolves local
    variables to lexical addresses, and evaluates the resolved program in
    [env].  On failure it returns either a type diagnostic or a
    structured run-time diagnostic. *)
val run_program
  : env
  -> program
  -> (unit, eval_error) result

(** [typecheck_resolve_and_eval env prog] preserves the historical API
    shape used by older callers: it returns type-checking failures in the
    [Error] branch and may still raise run-time exceptions during
    evaluation.  New callers that want structured run-time diagnostics
    should prefer {!run_program}. *)
val typecheck_resolve_and_eval
  : env
  -> program
  -> (unit, Chatml_typechecker.diagnostic) result

(** [eval_program env prog] is a backwards-compatible alias for
    {!typecheck_resolve_and_eval}. *)
val eval_program
  : env
  -> program
  -> (unit, Chatml_typechecker.diagnostic) result
