(** Pure versioned named-field storage codec. No I/O or runtime restoration.
    Readers retain the returned carrier across edits; admission failures never
    mutate stored bytes. New authored records explicitly use an empty carrier. *)
open! Core

type t = Session_metadata.t

val limits : Document_schema.Limits.t

val of_document
  :  Document_schema.Document.t
  -> (t Document_schema.Extension_carrier.t, Document_schema.Error.t) Result.t

val to_document
  :  t Document_schema.Extension_carrier.t
  -> (Document_schema.Document.t, Document_schema.Error.t) Result.t

val stored_session_id
  :  Document_schema.Document.t
  -> (Agent_protocol.Id.Session.t, Document_schema.Error.t) Result.t
