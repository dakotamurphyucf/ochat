(** Public evaluator entrypoints for resolved ChatML programs.

    The evaluator itself operates on the resolved AST emitted by the
    resolver.  Most embedders use {!eval_program}; host runtimes that
    interpret {!Chatml_lang.task} values also use {!apply_value_result}
    to invoke ChatML closures or builtin functions from OCaml. *)

open Chatml_lang

(** Intermediate result produced by the evaluator trampoline. *)
type eval_result =
  | Value of value
  | TailCall of clos * value list

(** Finish evaluation of a possibly tail-calling result in the given frame
    environment. *)
val finish_eval : Frame_env.env -> eval_result -> value

(** Apply a runtime value as a function.

    This supports both ChatML closures and builtin functions and returns a
    structured runtime error instead of raising.  It is the preferred API
    for host-side task interpreters. *)
val apply_value_result : value -> value list -> (value, runtime_error) result

(** Exception-raising wrapper around {!apply_value_result}. *)
val apply_value_exn : value -> value list -> value

(** Evaluate a resolved expression to either a value or a deferred
    tail-call. *)
val eval_expr : env -> Frame_env.env -> resolved_expr node -> eval_result

(** Evaluate an entire resolved program into the supplied top-level
    environment. *)
val eval_program : env -> resolved_program -> unit
