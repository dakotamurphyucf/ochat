open! Core

(** Resource owner for one runtime graph, independent of request switches and
    durable session identities. Single Eio domain. Never shared with children. *)
type t

type error = Closed [@@deriving equal, sexp_of]

(** Automatically closes on the enclosing switch release, including standalone
    owners. Explicit close is idempotent. A closed owner cannot prepare again. *)
val create : sw:Eio.Switch.t -> t

val switch : t -> Eio.Switch.t
val is_closed : t -> bool

(** Trusted adapter registration. No authentication is granted. Release callbacks
    must be bounded and idempotent; they close idle channels and private
    caches. The graph cancels/drains workers before closing this owner. *)
val on_release : t -> (unit -> unit) -> (unit, error) Result.t

(** Idempotent, closes admission before invoking callbacks in reverse order.
    May yield while joining bounded channel teardown under cancellation protection. All callbacks run even if one raises; the first exception then propagates. *)
val close : t -> unit
