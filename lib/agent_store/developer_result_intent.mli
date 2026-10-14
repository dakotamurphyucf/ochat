(** One exact private developer-job result selection. This binds the versioned
    document and optional Blob stage to an authentic physical Result_root and
    claimed attempt before any I/O. The common Result_intent_file owns framed
    no-follow save/remove; this module owns developer document identity and
    selection checks. It does not publish a terminal or grant a result read. *)
open! Core

type t

(** Pure construction. The claim must be issued by [runs], the root by the
    same Store, and the document must exactly match its owner, capture, job,
    attempt and proposed terminal. For Artifact, reference and temporary /
    durable stage metadata must share the selected Blob ID, digest, byte length,
    developer target, original principal and exact allowed-use string. Inline
    has no Blob metadata/stage. No path or filesystem access occurs. *)
val make
  :  runs:Developer_run_store.t
  -> root:Developer_run_store.Result_root.t
  -> claim:Developer_run_store.Claim.t
  -> Developer_result_intent_document.t
  -> (t, Store_error.t) result

val document : t -> Developer_result_intent_document.t
val stage : t -> Blob_stage_documents.t option

(** Re-establish the same complete checksummed frame before any Artifact
    stage. An ambiguous save retains the same immutable [t] for exact retry;
    conflicting, linked or corrupt existing bytes reject. *)
val save : t -> (unit, Store_error.t) result

(** Best-effort exact marker retirement after the worker supplies an actual
    Store-issued Installed receipt. The caller must first verify that receipt's
    physical Store, job/attempt and installed result against [document]. A
    typed cleanup error leaves the marker; unexpected exceptions propagate. *)
val remove_exact : t -> (unit, Store_error.t) result
