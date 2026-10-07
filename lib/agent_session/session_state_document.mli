open! Core

(** Owner-held projection and immutable document preservation context. Updating
    a state preserves unknown paths; conflicting deletions fail before commit. *)
type t = Session_state.t Document_schema.Extension_carrier.t

val authored : Session_state.t -> t
val value : t -> Session_state.t
val with_value : t -> Session_state.t -> t
val shape : Document_schema.Shape.t

val decode
  :  limits:Document_schema.Limits.t
  -> Document_schema.Document.t
  -> (t, Document_schema.Error.t) result

val encode
  :  t
  -> limits:Document_schema.Limits.t
  -> (Document_schema.Document.t, Document_schema.Error.t) result

val attachment_to_jsonaf : Agent_protocol.Session.Attachment.t -> Jsonaf.t

val attachment_of_jsonaf
  :  Jsonaf.t
  -> (Agent_protocol.Session.Attachment.t, Agent_protocol.Error.t) result

val attachment_shape : Document_schema.Shape.t
val archive_reference_to_jsonaf : Session_state.Compaction_archive.t -> Jsonaf.t

val archive_reference_of_jsonaf
  :  Jsonaf.t
  -> (Session_state.Compaction_archive.t, Agent_protocol.Error.t) result

val archive_reference_shape : Document_schema.Shape.t
val lifecycle_to_jsonaf : Session_state.Lifecycle.t -> Jsonaf.t

val lifecycle_of_jsonaf
  :  Jsonaf.t
  -> (Session_state.Lifecycle.t, Agent_protocol.Error.t) result

val adopt
  :  t
  -> limits:Document_schema.Limits.t
  -> t
  -> (t, Document_schema.Error.t) result
