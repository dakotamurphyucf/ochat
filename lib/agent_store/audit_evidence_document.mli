(** Immutable chain evidence: hashes cover the exact embedded named event bytes,
    never a converted/reencoded projection. Stored hashes and chain ownership are
    checked before conversion and current event validation. *)
open! Core

type t

val create
  :  Audit_event_document.t
  -> previous_hash:string option
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val of_document
  :  Document_schema.Document.t
  -> previous_hash:string option
  -> next_sequence:int64
  -> limits:Document_schema.Limits.t
  -> (t, Store_error.t) result

val to_document
  :  t
  -> limits:Document_schema.Limits.t
  -> (Document_schema.Document.t, Document_schema.Error.t) result

val event : t -> Audit_event_document.t
val event_bytes : t -> string
val record_hash : t -> string
