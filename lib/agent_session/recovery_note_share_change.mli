open! Core

(** Compact source-owned journal operations. A recorded timestamp admits finite
    retention deterministically on replay. Live writer guards recheck expiry and
    ownership at the actual commit cutpoint. *)

type t

val establish
  :  owner:Agent_protocol.Session_ref.t
  -> nonce:Agent_protocol.Id.Session_incarnation.t
  -> t

val create
  :  source:Recovery_note_share.Incarnation.t
  -> recipient:Recovery_note_share.Incarnation.t
  -> principal:Agent_protocol.Id.Principal.t
  -> key:Agent_protocol.Idempotency_key.t
  -> now:Agent_protocol.Timestamp.t
  -> Recovery_note_share.create
  -> t

val revoke
  :  source:Recovery_note_share.Incarnation.t
  -> principal:Agent_protocol.Id.Principal.t
  -> key:Agent_protocol.Idempotency_key.t
  -> now:Agent_protocol.Timestamp.t
  -> Recovery_note_share.revoke
  -> t

type outcome =
  | Established of Recovery_note_share.t
  | Applied of Recovery_note_share.t * Recovery_note_share.Receipt.t
  | Replayed of Recovery_note_share.Receipt.t

val evaluate
  :  t
  -> previous:Recovery_note_share.t option
  -> catalog:Recovery_notes.Catalog.t option
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> (outcome, Agent_protocol.Error.t) result

val apply
  :  t
  -> previous:Recovery_note_share.t option
  -> catalog:Recovery_notes.Catalog.t option
  -> session_id:Agent_protocol.Id.Session.t
  -> generation:int
  -> (Recovery_note_share.t, Agent_protocol.Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
