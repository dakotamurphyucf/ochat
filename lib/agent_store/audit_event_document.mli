(** Validated current audit event projection with preserved unknown named fields.
    Sequence is a positive canonical decimal string in storage. Optional IDs
    preserve absence and reject null; opaque payload may legitimately be null. *)
open! Core

type t

val create
  :  Agent_protocol.Audit.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val of_document
  :  Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val to_document
  :  t
  -> limits:Document_schema.Limits.t
  -> (Document_schema.Document.t, Document_schema.Error.t) result

val value : t -> Agent_protocol.Audit.t
