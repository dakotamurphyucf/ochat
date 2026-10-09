(** Validated current named delegation document. Original immutable admission
    JSON and nested/envelope extensions survive disposition updates. *)
type t

val max_payload_length : int
val limits : Document_schema.Limits.t

(** Author a new known value with no existing preservation carrier. Existing
    records must use [of_record], which checks immutable admission identity. *)
val create : Delegation_record.t -> (t, Document_schema.Error.t) result

val of_record : Delegation_record.t -> (t, Document_schema.Error.t) result
val of_document : Document_schema.Document.t -> (t, Document_schema.Error.t) result
val to_document : t -> (Document_schema.Document.t, Document_schema.Error.t) result
val value : t -> Delegation_record.t
val to_record : t -> Delegation_record.t

(** Original key extraction only, before any conversion/current admission. *)
val stored_key
  :  Document_schema.Document.t
  -> (Delegation_record.Key.t, Document_schema.Error.t) result

(** SHA256 of the original admission subtree serialized with its original JSON
    ordering and lexemes. It is independent of mutable record disposition. *)
val admission_sha256 : t -> string

val with_disposition
  :  t
  -> stage:Delegation_record.stage
  -> revocation:Delegation_record.revocation option
  -> artifact_collection:Delegation_record.artifact_collection option
  -> (t, Document_schema.Error.t) result
