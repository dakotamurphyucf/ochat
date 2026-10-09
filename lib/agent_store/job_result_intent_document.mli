(** Pure private result preparation document. The IO owner validates original
    frame integrity and filename/session identity before admitting this codec.
    Exact embedded metadata publication strings consume the same 32 KiB bound. *)
open! Core

type t

val limits : Document_schema.Limits.t

val create
  :  reference:Agent_protocol.Job_artifact.t
  -> stage:Blob_stage_documents.t
  -> (t, Document_schema.Error.t) Result.t

val reference : t -> Agent_protocol.Job_artifact.t
val stage : t -> Blob_stage_documents.t
val of_document : Document_schema.Document.t -> (t, Document_schema.Error.t) Result.t
val to_document : t -> (Document_schema.Document.t, Document_schema.Error.t) Result.t

val stored_identity
  :  Document_schema.Document.t
  -> ( Agent_protocol.Id.Session.t * Agent_protocol.Id.Blob.t
       , Document_schema.Error.t )
       Result.t
