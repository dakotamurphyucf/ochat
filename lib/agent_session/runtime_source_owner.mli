open! Core

(** Process-local issuer retained privately by the actual Factory. It grants no
    execution, source selection or Store authority. Fixed source completion uses
    it only after actually acquired target and parent resources successfully join.
    Factual child accounting also uses the private issuer to authenticate the
    actual Factory/Actor/Store association; that use supplies no cleanup evidence
    or NoEffects permission. No decoder or restoration exists.
    Operations using the issuer belong to its creating Eio domain. *)
type t

module Identity : sig
  (** Read-only identity captured by an Actor at creation. It cannot be converted
      back to an issuer or replaced by an identity from a newly created Factory. *)
  type t

  (** Deliberate physical issuer identity, not equality of durable IDs. *)
  val equal : t -> t -> bool
end

(** Creates a fresh issuer. Creating another issuer cannot complete an attempt
    belonging to an existing Actor's different captured identity. *)
val create : unit -> t

val identity : t -> Identity.t

(** Association check only; it grants no permission or cleanup evidence. *)
val matches : t -> Identity.t -> bool
