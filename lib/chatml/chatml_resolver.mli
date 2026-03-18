(** ChatML resolver — lexical-address resolution and lowering to the
    resolved evaluator AST. *)

open Chatml.Chatml_lang

val resolve_checked_program
  :  Chatml_typechecker.checked_program
  -> program
  -> resolved_program

val resolve_program : program -> resolved_program

type eval_error =
  | Type_diagnostic of Chatml_typechecker.diagnostic
  | Runtime_diagnostic of runtime_error

val run_program : env -> program -> (unit, eval_error) result

val typecheck_resolve_and_eval
  :  env
  -> program
  -> (unit, Chatml_typechecker.diagnostic) result

val eval_program : env -> program -> (unit, Chatml_typechecker.diagnostic) result
