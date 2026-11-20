(** Reconstruct a verified neutral projection from one admitted session state.
    The effective order is the exact moderator application order. Private
    provider materialization never participates. *)
open! Core

module P = Agent_protocol

module Row : sig
  type t

  val occurrence : t -> P.History.Id.t
  val origin : t -> P.Recovery_item_version.origin
  val effective_ordinal : t -> int
  val canonical_ordinal : t -> int option
  val version : t -> remaining_bytes:int -> P.Recovery_item_version.t option * int

  (** Only an exact supported, non-redacted neutral projection is addressable. *)
  val entry : t -> P.History.entry option
end

type t

(** Build bounded, ordered coordinates without decoding raw canonical payloads.
    An oversized canonical or overlay coordinate view yields [None]. *)
val create
  :  source_revision:int64
  -> overlay_incarnation:int64
  -> history:P.History.entry list
  -> moderator:Session.Moderator_state.Identity_snapshot.t option
  -> (t option, P.Error.t) Result.t

val find : t -> P.History.Id.t -> Row.t option
val at : t -> ordinal:int -> Row.t option
