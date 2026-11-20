open! Core

(** Private, same-host, source-owned grants to one exact plain-text note head.
    These values route a policy check. None convey session or artifact rights. *)

module Incarnation : sig
  type t [@@deriving equal]

  (** Checked durable identity for a session and its authority generation.
      The host looks up the nonce from the actual canonical actor. *)
  val create
    :  session:Agent_protocol.Session_ref.t
    -> nonce:Agent_protocol.Id.Session_incarnation.t
    -> generation:int
    -> (t, Agent_protocol.Error.t) result

  val session : t -> Agent_protocol.Session_ref.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
end

module Revision : sig
  type t [@@deriving compare, equal]

  val initial : t
  val of_int64 : int64 -> (t, Agent_protocol.Error.t) result
  val to_int64 : t -> int64
end

module Reference : sig
  type t [@@deriving equal]

  val source : t -> Agent_protocol.Session_ref.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
end

module Receipt : sig
  type t

  val reference : t -> Reference.t
  val revision : t -> Revision.t
end

type create =
  { expected_revision : Revision.t
  ; expected_catalog_revision : Recovery_notes.Catalog.Catalog_revision.t
  ; selection : Recovery_notes.Catalog.selection
  ; expires_at : Agent_protocol.Timestamp.t
  }

type revoke =
  { expected_revision : Revision.t
  ; reference : Reference.t
  }

type t

val owner : t -> Agent_protocol.Session_ref.t
val revision : t -> Revision.t
val incarnation : t -> generation:int -> (Incarnation.t, Agent_protocol.Error.t) result

(** A uniform response for invalid, stale, unowned or inaccessible selectors. *)
val unavailable : unit -> Agent_protocol.Error.t

(** [resolve] evaluates the private grant only. The actor independently checks
    recipient authority and the current catalog head at the point of disclosure. *)
val resolve
  :  t
  -> source_generation:int
  -> recipient:Incarnation.t
  -> reference:Reference.t
  -> now:Agent_protocol.Timestamp.t
  -> (Recovery_notes.Catalog.selection, Agent_protocol.Error.t) result

(** Validates and preserves the private carrier, including bounded unknown data. *)
val to_json : t -> (Jsonaf.t, Agent_protocol.Error.t) result

val of_json
  :  Jsonaf.t
  -> session_id:Agent_protocol.Id.Session.t
  -> (t, Agent_protocol.Error.t) result

val check_unchanged
  :  t option
  -> incoming:t option
  -> (unit, Agent_protocol.Error.t) result

(** Pure validated reducer for the journal. Only checked actor transitions may
    commit these results. Reuse the same writer key and exact arguments to
    resolve an ambiguous ACK. A fresh operation with an old basis conflicts. *)
module Journal : sig
  type ledger = t

  type mutation =
    | Create of Incarnation.t * create
    | Revoke of revoke

  val mutation_to_json : mutation -> Jsonaf.t
  val mutation_of_json : Jsonaf.t -> (mutation, Agent_protocol.Error.t) result

  val establish
    :  owner:Agent_protocol.Session_ref.t
    -> nonce:Agent_protocol.Id.Session_incarnation.t
    -> (ledger, Agent_protocol.Error.t) result

  val mutate
    :  ledger
    -> source:Incarnation.t
    -> catalog:Recovery_notes.Catalog.t option
    -> principal:Agent_protocol.Id.Principal.t
    -> key:Agent_protocol.Idempotency_key.t
    -> now:Agent_protocol.Timestamp.t
    -> mutation
    -> ( [ `Applied of ledger * Receipt.t | `Replayed of Receipt.t ]
         , Agent_protocol.Error.t )
         result
end

(** Check limits on the raw ledger, including extensions, before splitting the
    outer state document and after merging it. *)
val validate_encoded : Jsonaf.t -> (unit, Document_schema.Error.t) result
