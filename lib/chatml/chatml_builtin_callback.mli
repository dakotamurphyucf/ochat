open Chatml_lang

(** Apply a ChatML callback at a builtin boundary. Evaluator diagnostics retain their
    source owner, span and related sites as [Runtime_error]. The enclosing builtin
    adds a caller site only when the diagnostic has no owner. Callback
    arity and non-function errors include [name]. Cancellation and unexpected
    exceptions propagate unchanged; no callback is retried. *)
val apply : value -> name:string -> value list -> value
