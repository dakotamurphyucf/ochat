open! Core

(** Compact V1 journal evidence. Replaying the admitted mutation reconstructs
    the exact durable receipt and advances the catalog once. This value conveys
    no live write authority. *)
type t

val create
  :  owner:Agent_protocol.Session_ref.t
  -> identity:Recovery_notes.Catalog.Mutation_identity.t
  -> Recovery_notes.Catalog.mutation
  -> t

val evaluate
  :  t
  -> previous:Recovery_note_session.t option
  -> session_id:Agent_protocol.Id.Session.t
  -> (Recovery_notes.Catalog.outcome, Agent_protocol.Error.t) result

(** A journal entry represents one fresh committed operation. A retained retry
    returns through the actor without creating another journal entry. *)
val apply
  :  t
  -> previous:Recovery_note_session.t option
  -> session_id:Agent_protocol.Id.Session.t
  -> (Recovery_note_session.t, Agent_protocol.Error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
