(** Versioned private developer selection document. The conversation v1
    Job_result_intent_document and Job_artifact codec remain unchanged. The
    enclosing owner validates the framed file, typed name, real run root and
    Store job before treating decoded fields as a pending comparison. *)
open! Core

type t

(** Maximum canonical Inline Completion bytes admitted to this checksummed
    carrier. Selection above this ceiling uses Artifact without truncation. *)
val max_inline_bytes : int

module Selection : sig
  type t =
    | Inline of Agent_protocol.Completion.t
    | Artifact of
        { reference : Developer_result_reference.t
        ; stage : Blob_stage_documents.t
        }
end

(** Inline serialization is bounded by this document's size ceiling; larger
    selected completions must use Artifact. No source bytes are normalized.
    [selected_at] and all result bytes are frozen before the first write. *)
val create
  :  owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> job_id:Agent_protocol.Id.Job.t
  -> attempt:int
  -> selected_at:Agent_protocol.Timestamp.t
  -> after:Agent_protocol.Developer_job.State.t
  -> selection:Selection.t
  -> (t, Document_schema.Error.t) result

val owner : t -> Agent_protocol.Execution_owner.developer
val capture_fingerprint : t -> Chatml_source.Digest.t
val job_id : t -> Agent_protocol.Id.Job.t
val attempt : t -> int
val selected_at : t -> Agent_protocol.Timestamp.t
val after : t -> Agent_protocol.Developer_job.State.t
val selection : t -> Selection.t
val limits : Document_schema.Limits.t
val to_document : t -> (Document_schema.Document.t, Document_schema.Error.t) result
val of_document : Document_schema.Document.t -> (t, Document_schema.Error.t) result

module Stored_identity : sig
  type t

  val owner : t -> Agent_protocol.Execution_owner.developer
  val job_id : t -> Agent_protocol.Id.Job.t
  val attempt : t -> int
end

(** Read owner (including epoch), job and attempt to compare the canonical
    epoch-bearing filename before full decoding. It never treats a filename or
    document as issuer authority. *)
val stored_identity
  :  Document_schema.Document.t
  -> (Stored_identity.t, Document_schema.Error.t) result
