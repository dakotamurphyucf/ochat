(** ChatML resolver — lexical-address resolution and lowering to the
    resolved evaluator AST. *)

open Chatml.Chatml_lang

(** Resolve a program using an existing successful typechecking snapshot. *)
val resolve_checked_program
  :  Chatml_typechecker.checked_program
  -> program
  -> resolved_program

(** Resolve a program using the default typechecker entrypoint. *)
val resolve_program : program -> resolved_program

(** Failure mode for the end-to-end [run_program] helper. *)
type eval_error =
  | Type_diagnostic of Chatml_typechecker.diagnostic
  | Runtime_diagnostic of runtime_error

(** Typecheck, resolve, and evaluate a program into an existing
    environment, returning either a type or runtime diagnostic. *)
val run_program : env -> program -> (unit, eval_error) result

(** Typecheck, resolve, and evaluate a program, reporting only
    typechecking failures. *)
val typecheck_resolve_and_eval
  :  env
  -> program
  -> (unit, Chatml_typechecker.diagnostic) result

(** Typecheck and evaluate a program using the default pipeline expected by
    most tests and embedders. *)
val eval_program : env -> program -> (unit, Chatml_typechecker.diagnostic) result
