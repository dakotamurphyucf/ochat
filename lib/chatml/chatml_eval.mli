(** Public evaluator entrypoints for resolved ChatML programs.

    The evaluator itself operates on the resolved AST emitted by the
    resolver.  Most embedders use {!eval_program}; host runtimes that
    interpret {!Chatml_lang.task} values also use {!apply_value_result}
    to invoke ChatML closures or builtin functions from OCaml. An environment's
    optional execution control follows copied closure/module environments.
    Its checkpoint runs at each expression; control exceptions propagate rather
    than becoming ordinary catchable script failures. *)

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

(** Evaluate into a copy of the supplied bindings whose immutable source owner
    is [source]. Export cells and captured closures retain that owner. This
    returns the owned environment; the input environment is not relabelled.
    Rejects different captured bytes before evaluation. Runtime failures remain
    structured; execution-control exceptions propagate. No initializer executes
    during compilation. *)
val eval_captured_program
  :  env
  -> source:Chatml_source.t
  -> resolved_program
  -> (env, runtime_error) result

(** Select checked public exports into a fresh table while sharing values and
    private closure environments. Reuses inline signature selection, preserving
    source/control/dependencies and polling the current control. Does not execute
    a body or add a synthetic expression/fuel step. [span] belongs to this unit.
    Runtime failures remain structured; cancellation/control propagate. *)
val select_exports
  :  env
  -> visibility:Chatml_module_visibility.t
  -> span:Source.span
  -> (env, runtime_error) result
