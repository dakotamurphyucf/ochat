(** Private comparison identity for one developer result artifact. This record
    is selected from an actual physical Claim before result I/O. It is neither
    a Store transition token nor a disclosure grant. *)
open! Core

type t

(** Construct only after deriving validated Blob metadata (ID, canonical result
    byte length/digest and media type) from the frozen Completion bytes. This
    reference then yields the one canonical [allowed_use] for the surrounding
    Blob_store.Metadata; no placeholder Blob metadata or duplicate identity
    string builder is needed. *)
val create
  :  claim:Developer_run_store.Claim.t
  -> outcome:Agent_protocol.Stored_completion.outcome
  -> blob:Agent_protocol.Blob.Metadata.t
  -> (t, Store_error.t) result

(** Read-only reconstruction of the same comparison identity from an actual
    Store-validated terminal job whose Artifact result is already published.
    Validate the whole job State against immutable kind/creation/attempt and
    require a terminal Artifact whose outcome agrees with its status; do not
    infer it merely from a Blob ID.
    Startup inventory uses this when the private intent marker was retired but
    a durable Blob remains. A copied DTO cannot prove Store provenance; the
    caller must obtain [job] through authenticated
    [Result_root.rows_for_inventory].
    This does not permit another write, result disclosure or Blob deletion. *)
val of_terminal_job : Agent_protocol.Developer_job.t -> (t, Store_error.t) result

val owner : t -> Agent_protocol.Execution_owner.developer
val capture_fingerprint : t -> Chatml_source.Digest.t
val job_id : t -> Agent_protocol.Id.Job.t
val attempt : t -> int
val outcome : t -> Agent_protocol.Stored_completion.outcome
val blob : t -> Agent_protocol.Blob.Metadata.t

(** Exact private metadata use string. The Blob target alone identifies a run,
    not the original epoch, capture, job or attempt. *)
val allowed_use : t -> string

(** Verify an actual claimed job or Store-installed job against every retained
    immutable coordinate. Matching IDs alone never authorize publication. *)
val matches_job : t -> Agent_protocol.Developer_job.t -> bool

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Store_error.t) result
