open! Core

(** Owner-held projection and immutable document preservation context. Updating
    a state preserves unknown paths; conflicting deletions fail before commit.
    A decoded original retains its admitted immutable ledger and complete profile.
    Updating the native value never replaces this original preservation basis;
    any different profile re-admits the original child before continuity checks.
    Historical absent run_state and explicit null remain distinct when its native
    value is None, including adoption and unrelated writes. The optional delivery
    subscription binding has the same policy, keyed by its typed delivery ID.
    A newly admitted Some value introduces its known field; wire presence never
    supplies authority. *)
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

(** Compare complete native storage projections, including opaque inference
    selections/ledger and exact private pending carriers. This performs no state
    admission or validation roundtrip; callers must validate their candidate and
    persist through the ordinary document encoder. Historical outer field-presence
    policy is normalized equally for both values. Recoverable child serialization
    errors are returned unchanged; no custody or publication authority is granted. *)
val equal_values
  :  Session_state.t
  -> limits:Document_schema.Limits.t
  -> Session_state.t
  -> (bool, Document_schema.Error.t) result

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

(** Adjacent generic v1 through v9 conversion. The v8 to v9 step wraps each
    original pending entry, preserving its entire raw document, and introduces
    independent pending revision/disposition fields with unknown legacy owner. The v7 to v8 step is
    structural identity: absent run_state is not filled with an invented value. The v5 to v6 step introduces
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

(** Exact sanctioned queue custody transition. Rechecks the complete pure basis,
    exact prestate archive for destructive changes, and exact bounded expiry
    archive reference. Adoption transfers the original entry object to canonical
    storage and its remaining wrapper to explicit private disposition custody. *)
val transfer_pending
  :  t
  -> plan:Pending_plan.t
  -> archive:Session_state.Compaction_archive.t option
  -> expiry_archive:Pending_archive.Reference.t option
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

(** Immutable result of one ordered admission. Its native carrier and exact final
    encoding were admitted together under the supplied limits, including original
    native validation, exact candidate comparison, unknown-field preservation and
    final document/custody bounds. Construction is private; this is not a cache.
    Re-decode [document] to establish the next durable preservation basis. *)
module Admitted : sig
  type nonrec state_document = t
  type t

  val state_document : t -> state_document
  val document : t -> Document_schema.Document.t
end

(** Opaque ordered transaction prefixes. Only validated checkpoints and [finish] return durable carriers;
    ordinary public encode/decode always retain complete state validation. *)
module Transaction : sig
  type document = t
  type t

  val begin_
    :  document
    -> limits:Document_schema.Limits.t
    -> (t, Document_schema.Error.t) result

  val value : t -> Session_state.t
  val with_value : t -> Session_state.t -> t

  (** Archive/retirement checkpoint validates current prefix completely; it cannot
      archive or expose an inconsistent native prefix. *)
  val checkpoint
    :  t
    -> limits:Document_schema.Limits.t
    -> (document, Document_schema.Error.t) result

  val transfer_pending
    :  t
    -> plan:Pending_plan.t
    -> archive:Session_state.Compaction_archive.t option
    -> expiry_archive:Pending_archive.Reference.t option
    -> limits:Document_schema.Limits.t
    -> (t, Document_schema.Error.t) result

  (** Validate complete candidate and exact ordered semantics; only owner-stamped
      time/counters normalize. Ordinary durable encoding rechecks preservation. *)
  val finish_encoded
    :  t
    -> next:Session_state.t
    -> limits:Document_schema.Limits.t
    -> (Admitted.t, Document_schema.Error.t) result

  (** Same admission and native carrier semantics as [finish_encoded], projecting
      its carrier for callers that subsequently merge further captured fields. *)
  val finish
    :  t
    -> next:Session_state.t
    -> limits:Document_schema.Limits.t
    -> (document, Document_schema.Error.t) result
end
