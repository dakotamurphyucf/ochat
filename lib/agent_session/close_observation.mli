open! Core

type 'a exit =
  | Returned of 'a
  | Raised of exn * Stdlib.Printexc.raw_backtrace

type 'a t

(** Construct an immutable actual return/catch observation. SAME diagnostic groups
    are deduplicated in first occurrence order; an absent primary is allowed, but
    a supplied primary must be one of those actual groups. This public data
    constructor issues no execution/custody authority and makes no success claim. *)
val create
  :  'a exit
  -> diagnostics:Cleanup_diagnostic.t list
  -> ?primary:Cleanup_diagnostic.t
  -> unit
  -> 'a t

val exit : 'a t -> 'a exit
val diagnostics : 'a t -> Cleanup_diagnostic.t list
val primary : 'a t -> Cleanup_diagnostic.t option

(** Resume exactly the observed return/raw exception and backtrace. This does not
    consume diagnostic delivery and does not execute the operation again. *)
val resume : 'a t -> 'a
