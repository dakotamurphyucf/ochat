(** Pure versioned named-field storage codec. No I/O or runtime restoration.
    Readers retain the returned carrier across edits; admission failures never
    mutate stored bytes. New authored records explicitly use an empty carrier. *)
open! Core

type t = { created_at : Agent_protocol.Timestamp.t }

val limits : Document_schema.Limits.t

val of_document
  :  Document_schema.Document.t
  -> (t Document_schema.Extension_carrier.t, Document_schema.Error.t) Result.t

val to_document
  :  t Document_schema.Extension_carrier.t
  -> (Document_schema.Document.t, Document_schema.Error.t) Result.t

(** Read-only migration inspection uses the same envelope/kind/semantics
    boundary. Current versions additionally undergo current-domain validation;
    newer positive versions are reported without constructing their payload. *)
val stored_version : Document_schema.Document.t -> (int, Document_schema.Error.t) Result.t
