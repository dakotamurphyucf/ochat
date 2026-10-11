open Core

(** Small source-to-diagnostic adapter shared by compiler and evaluator. It owns
    no mutable origin, inventory, callback or execution authority. *)
type t = Chatml_source.t

(** Validate one actual diagnostic/task site, rather than scanning every AST
    expression. Invalid legacy spans have no owned site. Checkpoint exceptions
    propagate unchanged. *)
val site
  :  t
  -> Source.span
  -> checkpoint:(unit -> unit)
  -> Chatml_diagnostic.Site.t option

(** Attach the captured owner only to an unowned diagnostic's legacy span.
    Already owned failures retain their original source. *)
val attach : t -> Chatml_diagnostic.t -> checkpoint:(unit -> unit) -> Chatml_diagnostic.t
