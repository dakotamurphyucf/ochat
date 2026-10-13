(** Daemon-owned developer run checkpoint document. No ID, observation, or
    retained record grants host execution or result-read authority. *)
open! Core

module Limits : sig
  type t

  (** Every bound is positive and finite. The document limits use the existing
      Document_schema JSON byte/depth/field/node accounting. Aggregate recovery
      limits cover directory entries, bytes and work before allocation. *)
  val create
    :  max_runs:int
    -> max_epochs_per_run:int
    -> max_jobs_per_run:int
    -> max_invocations_per_run:int
    -> max_selected_per_commit:int
    -> max_job_payload_bytes:int
    -> max_document_bytes:int
    -> max_recovery_entries:int
    -> max_recovery_bytes:int
    -> max_work_steps:int
    -> document:Document_schema.Limits.t
    -> t Or_error.t
end

module Error : sig
  type t =
    | Store of Store_error.t
    | Conflict
    | Foreign_or_stale
    | Limit_exceeded
    | Closed
  [@@deriving sexp_of]
end

type t
type handle

module Checkpoint : sig
  type phase =
    | Active
    | Interrupted
    | Closed
  [@@deriving equal, sexp_of]

  type t

  val owner : t -> Agent_protocol.Execution_owner.developer
  val capture_fingerprint : t -> Chatml_source.Digest.t
  val phase : t -> phase
  val revision : t -> int64
end

module Observation : sig
  type t

  (** Immutable exact file-revision observation, physically tied to its issuing
      store and run. A matching numeric revision alone is not a CAS proof. *)
  val checkpoint : t -> Checkpoint.t
end

module Prepared : sig
  type t

  (** The complete bounded exact previous/new document bytes, bound to one
      physical issuing handle. This is minted before I/O so an uncertain
      acknowledgement can be reconciled without repeating the effect or
      inventing a commit. *)
end

module Installed : sig
  type t

  val observation : t -> Observation.t
end

type reconciliation =
  | Installed of Installed.t
  | Absent
  | Diverged

(** Open one singleton facade for this physical Session_store under its existing
    daemon lock. The implementation checks the lock remains live and belongs to
    the expected root/host on every operation; Session_store.close invalidates
    all handles. An additive developer-runs subtree is admitted without changing
    the existing root/session schema. Absence on an older root means empty. A
    bounded startup scan rejects links, unknown authoritative files, duplicate
    run IDs and malformed documents. Recognized incomplete atomic-writer
    temporary files are never treated as authority. No separate daemon lock or
    writer exists. Opening syncs the daemon root after finding or creating the
    subtree, including a retry after an uncertain subtree creation.
    One final versioned document layout has checkpoint, invocation and job
    fields from the outset; the latter may be empty. Unknown versions fail
    closed. There is no intermediate milestone schema or migration. The caller
    retains [sw] until [close_and_wait]. *)
val open_or_create
  :  Session_store.t
  -> sw:Eio.Switch.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

(** Trusted host wiring first authenticates the real principal/run and compares
    [capture_fingerprint] with its actual admitted Chatml_developer_capture.t.
    This lower store persists those coordinates and checks exact current epoch;
    it does not authenticate a caller or admit an effect. Repeating an identical
    admission returns the same run; a changed capture/principal conflicts. A new
    epoch requires an exact prior observation and interrupts the previous epoch;
    its older claimed jobs can later finish bookkeeping without new execution.
    The configured epoch/run quota is reported as [Limit_exceeded]; malformed
    over-limit documents found on disk fail recovery. *)
val admit_epoch
  :  t
  -> owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> previous:Observation.t option
  -> (handle * Observation.t, Error.t) result

val owner : handle -> Agent_protocol.Execution_owner.developer
val capture_fingerprint : handle -> Chatml_source.Digest.t
val observe : handle -> (Observation.t, Error.t) result

(** Foundation checkpoint-only CAS. The prepared value makes no durability
    claim. A successful commit atomically replaces the file and syncs the
    parent directory before acknowledgement; an interrupted replacement must
    be reconciled. No job is visible through this operation. A new revision
    must be positive, representable and monotone. *)
val prepare_checkpoint
  :  handle
  -> expected:Observation.t
  -> phase:Checkpoint.phase
  -> (Prepared.t, Error.t) result

(** One serialized writer commits a prepared exact document by atomic replace
    and file/directory sync. An Error or cancellation may occur after durability;
    neither permits capacity publication or blind abort until [reconcile]. The
    original cancellation/exception identity is preserved. A later actor must
    serialize its publication handoff so a later mutation cannot race it. *)
val commit : handle -> Prepared.t -> (Installed.t, Error.t) result

(** Sync the owned parent directory, then compare the bounded authoritative
    file with the prepared exact before/after bytes and revision. An Installed
    response therefore confirms the observed rename is durable even if the
    original replacement was interrupted during directory sync. Diverged means
    another authoritative state exists and requires explicit recovery. *)
val reconcile : handle -> Prepared.t -> (reconciliation, Error.t) result

(** Stop admission, join owned operations and invalidate all handles. No runtime
    callback is replayed by closing. *)
val close_and_wait : t -> (unit, Error.t) result
