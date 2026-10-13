open! Core

(** Pure, same-domain admission order for one developer run's root and native
    terminal intents. This module neither writes the Store nor owns capacity,
    execution, permission, or a scheduler. The physical root owner holds those
    capabilities and maps each opaque slot to its exact Store Prepared/Handoff.
    All operations are non-yielding. *)

type 'intent t
type 'intent callback
type 'intent drain
type witness

module Error : sig
  type t =
    | Closed
    | Limit_exceeded
    | Busy
    | Stale
  [@@deriving equal, sexp_of]
end

type kind =
  | Root
  | Native
[@@deriving equal, sexp_of]

(** [max_native_occupancy] bounds ACTIVE plus completed/retained native
    callbacks together. One root slot is additional and unique. A completed
    callback still consumes its original slot until exact terminal settlement;
    returning an error to its caller never frees capacity or admits a
    replacement. The creating Eio domain owns this pure state. *)
val create : max_native_occupancy:int -> ('intent t, Error.t) result

(** Reserve before the first yielding admission read or native callback. This
    does not grant effect authority: the owner separately checks its actor,
    capture, selected registry and live parent. Root and native callbacks can
    nest because this slot is not a callback lock. No new slot is admitted while
    a terminal intent is queued/draining/retained or close has begun. *)
val reserve : 'intent t -> kind:kind -> ('intent callback, Error.t) result

(** Abandon a callback that never produced a terminal intent, for example a
    failed durable dispatch. Only its exact live slot can be abandoned. *)
val abandon : 'intent t -> 'intent callback -> (unit, Error.t) result

(** Retain this callback's already frozen, issuer-checked terminal intent
    before any terminal I/O yield; the physical owner must supply an immutable
    value, since the polymorphic queue cannot deep-copy or validate it. Exact
    same-domain slot identity and one-shot completion are required. Append in
    completion order without freeing occupancy. This closes effect admission
    and invalidates every earlier [witness]. *)
val complete : 'intent t -> 'intent callback -> intent:'intent -> (unit, Error.t) result

(** A non-yielding current-effect witness exists only while the queue is empty,
    no drain is active, and close has not begun. A known settlement empties the
    queue and issues a NEW witness, so an enclosing live native parent can
    recheck current policy after its child returns. A witness held across any
    intervening terminal intent never becomes valid again. *)
val effect_witness : 'intent t -> witness option

val check_effect_witness : 'intent t -> witness -> bool

(** One physical drain token for the head only. The owner must retain the
    corresponding exact Prepared/Handoff across unknown writes; taking a
    retained head is recovery, not callback replay. A second drainer gets
    [Busy]. [None] means no completed intent is queued. *)
val take_head : 'intent t -> ('intent drain option, Error.t) result

val payload : 'intent drain -> 'intent

(** Physical head identity across a retained unknown write and a later fresh
    recovery drain token. A matching payload value alone is insufficient. *)
val same_head : 'intent drain -> 'intent drain -> bool

(** Keep this same head after attempted/unknown Store settlement. Its slot and
    payload remain retained, later intents cannot bypass it, and a later
    [take_head] returns this same head for exact recovery. *)
val retain : 'intent t -> 'intent drain -> (unit, Error.t) result

(** Retire only this head after the Store and capacity have conclusively
    settled its exact transaction. Other callbacks/intents stay intact. An
    unattempted prewrite conflict does NOT settle the intent: the owner may
    prepare the same immutable payload from a fresh Observation under this
    drain token, with its own finite retry bound. *)
val settle : 'intent t -> 'intent drain -> (unit, Error.t) result

(** Stop future reservations/effects without discarding live callbacks or
    completed intents. Existing callbacks can still complete or abandon and
    the owner can still drain exact terminal bookkeeping. The owner cancels and
    joins physical callbacks separately before releasing its Store Run_claim. *)
val begin_close : 'intent t -> (unit, Error.t) result

val has_native_occupancy : 'intent t -> bool
val is_empty : 'intent t -> bool
