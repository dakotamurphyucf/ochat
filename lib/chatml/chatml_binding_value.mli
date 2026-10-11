open! Core

(** A tuple kind or arity mismatch at the original binder range. The caller
    attaches the source owner of the evaluated declaration or closure. *)
type mismatch =
  { span : Source.span
  ; message : string
  }

(** Extract named leaves from an already evaluated RHS or argument in the same
    order as [Chatml_binding_pattern.binders]. Wildcards accept any value and
    produce no leaf. Mutable children remain shared. No environment or frame is
    modified; failure never publishes partial bindings. Resource admission is
    caller-owned. Every visited binder polls [checkpoint]; exceptions propagate
    unchanged, and callbacks are not retained. *)
val extract
  :  ?checkpoint:(unit -> unit)
  -> Chatml_binding_pattern.t
  -> Chatml_lang.value
  -> ((string * Chatml_lang.value) list, mismatch) Result.t
