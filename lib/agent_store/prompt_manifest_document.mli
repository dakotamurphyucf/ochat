(** Current named manifest carrier. The complete authored inventory is bounded to
    262144 bytes, including envelope and unknown fields, before external effects. *)
type t

val max_bytes : int
val limits : Document_schema.Limits.t
val create : Prompt_manifest.t -> (t, Document_schema.Error.t) result
val of_document : Document_schema.Document.t -> (t, Document_schema.Error.t) result
val value : t -> Prompt_manifest.t
val to_document : t -> (Document_schema.Document.t, Document_schema.Error.t) result

val stored_revision_id
  :  Document_schema.Document.t
  -> (Agent_protocol.Id.Prompt_revision.t, Document_schema.Error.t) result

type document = t

(** Exact immutable publication custody. Stored bytes are never reencoded for
    integrity verification; original digest and identity precede conversion. *)
module Publication : sig
  type t

  val create : Prompt_manifest.t -> (t, Document_schema.Error.t) result

  val of_bytes
    :  string
    -> revision_id:Agent_protocol.Id.Prompt_revision.t
    -> expected_sha256:string
    -> (t, Store_error.t) result

  val document : t -> document
  val bytes : t -> string
  val sha256 : t -> string
end
