open! Core

(** Owner-held projection and immutable document preservation context. Updating
    a state preserves unknown paths; conflicting deletions fail before commit.
    A decoded original retains its admitted immutable ledger and complete profile.
    Updating the native value never replaces this original preservation basis;
    any different profile re-admits the original child before continuity checks. *)
type t

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

(** Adjacent generic v1 through v6 to v7 conversion. The v5 to v6 step introduces
    empty historical organization references without resolving host objects. The
    v6 to v7 step introduces required content revisions in canonical and deferred
    entries. Missing configuration revision
    is introduced in the v4 to v5 step. Missing metadata revision
    defaults to zero. Missing captured selection and job
    bindings become Unresolved; missing ledger becomes empty with UNKNOWN prior
    tracking coverage. An existing same-name ledger must admit under the durable
    profile and exact identity, or conversion rejects without replacing it.
    Original stored bytes and admitted child unknown fields remain untouched. *)
val upgrade
  :  Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> (Document_schema.Document.t, Document_schema.Error.t) Result.t

(** Generic legacy model-job binding used by the delta conversion. *)
val legacy_model_job_target
  :  Jsonaf.t
  -> (Jsonaf.t option, Document_schema.Error.t) Result.t

(** Internal carrier operation for an independently archive-admitted history edit.
    Retain an exact, nonempty canonical identity prefix, preserving each retained
    envelope and all unrelated extensions. Only suffix preservation paths retire;
    this grants no actor mutation/archival permission and is not a wire API.
    The original immutable inference-ledger validation basis remains unchanged. *)
val retire_canonical_suffix
  :  t
  -> retained_ids:Agent_protocol.History.Id.t list
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

(** Internal custody transfer for a validated ordinary deletion. Rechecks its exact
    basis and preserves the full ordered retained subsequence. Callers must admit
    the exact previous archive before using this carrier. *)
val retire_canonical_deletion
  :  t
  -> deletion:History_deletion.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result
