(** Pure blob metadata owner. Carriers retain unknown fields and optional-field
    presence. Every decoder validates current invariants after JSON conversion. *)
open! Core

type t

val limits : Document_schema.Limits.t
val create : Blob_metadata.t -> (t, Document_schema.Error.t) Result.t
val value : t -> Blob_metadata.t
val of_document : Document_schema.Document.t -> (t, Document_schema.Error.t) Result.t
val to_document : t -> (Document_schema.Document.t, Document_schema.Error.t) Result.t
val of_bytes : string -> (t, Document_schema.Error.t) Result.t
val to_bytes : t -> (string, Document_schema.Error.t) Result.t
val with_value : t -> Blob_metadata.t -> (t, Document_schema.Error.t) Result.t

(** Inspect original named identity before conversion/current-domain decoding. *)
val stored_blob_id
  :  Document_schema.Document.t
  -> (Agent_protocol.Id.Blob.t, Document_schema.Error.t) Result.t

(** Same blob shape used by private artifact references and metadata. *)
val blob_shape : Document_schema.Shape.t

val blob_of_json
  :  Jsonaf.t
  -> (Agent_protocol.Blob.Metadata.t, Document_schema.Error.t) Result.t

val blob_to_json : Agent_protocol.Blob.Metadata.t -> Jsonaf.t
