open! Core

(** Immutable parent-side locator for an actually linked captured child. Pure
    constructors and decoders validate association, not child existence, store
    ownership, lifecycle, installation or authority. Complete original JSON and
    compatible nested fields remain exact within the reserved 4 KiB profile. *)
type t

val create
  :  reservation:Recurrence_child_reservation.t
  -> reference:Agent_store.Delegation_store.Reference.t
  -> (t, Agent_protocol.Error.t) result

val occurrence : t -> Recurrence_identity.Occurrence.t
val reference : t -> Agent_store.Delegation_store.Reference.t
val equal : t -> t -> bool

val validate
  :  t
  -> reservation:Recurrence_child_reservation.t
  -> (unit, Agent_protocol.Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
