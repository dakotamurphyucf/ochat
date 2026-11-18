open! Core
module Failure = Agent_session.Callback_cleanup.Failure

(** Private immutable diagnostic groups. Creating a group records one actual
    observed stage outcome, and grants no execution/disposal authority. *)
module Diagnostic : sig
  type t = Agent_session.Cleanup_diagnostic.t

  val create : Failure.t list -> t
  val failures : t -> Failure.t list
  val equal : t -> t -> bool
end

type disposition =
  | Retained
  | Disposed
[@@deriving equal, sexp_of]

type t

(** Trusted concrete Owner consumers supply disposition only after their actual
    stage proof. Empty diagnostics do not imply completed disposal. *)
val create : ?primary:Diagnostic.t -> disposition:disposition -> Diagnostic.t list -> t

val disposition : t -> disposition
val diagnostics : t -> Diagnostic.t list
val primary : t -> Diagnostic.t option
val failures : t -> Failure.t list
val combine : disposition:disposition -> t list -> t

(** One-shot legacy delivery of actual groups, with the original call primary
    preferred over older secondary diagnostics. Creating-domain checked before
    mutation. Reported getters stay immutable and never consume delivery. *)
val take_legacy_failure_exn : t -> Failure.t option

(** SAME selected carrier for origin-sensitive legacy adapters. Delivery rules are
    identical to [take_legacy_failure_exn], which projects this result. *)
val take_legacy_diagnostic_exn : t -> (Diagnostic.t * Failure.t) option
