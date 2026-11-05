open! Core

(** Process-local issuer retained privately by the actual Factory. It grants no
    execution, source selection or Store authority. The Factory uses it only
    inside its fixed source lifetime, after all actually acquired target and
    parent resources have successfully joined. No decoder or restoration exists.
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
