open! Core
module Failure = Callback_cleanup.Failure

(** Immutable diagnostic payload of one actual producer/stage. Equality is SAME
    stable carrier identity, not payload equality. This data grants no resource,
    callback, execution, custody or disposal authority. *)
type t

val create : Failure.t list -> t
val equal : t -> t -> bool
val failures : t -> Failure.t list

(** Trusted one-shot legacy presentation. Deduplicates SAME carriers in original
    order; validates primary membership and every creating domain BEFORE any
    delivery mutation. Selects pending primary/first failure and marks all pending
    groups delivered. Immutable reported payloads are unchanged. New actual
    failures need their own carriers; delivery never certifies cleanup success. *)
val take_legacy_diagnostic_exn : ?primary:t -> t list -> (t * Failure.t) option
