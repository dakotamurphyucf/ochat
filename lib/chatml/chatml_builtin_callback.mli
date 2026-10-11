open Chatml_lang

(** Apply a ChatML callback at a builtin boundary. Evaluator diagnostics become
    [Failure] for the enclosing builtin call to attach its source span. Callback
    arity and non-function errors include [name]. Cancellation and unexpected
    exceptions propagate unchanged; no callback is retried. *)
val apply : value -> name:string -> value list -> value
