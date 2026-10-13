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

module Invocation_observation : sig
  type t

  val revision : t -> int
  val value : t -> Agent_protocol.Host_invocation.t
end

module Invocation_lookup : sig
  type t

  val prior : t -> Invocation_observation.t option
end

(** Exact lookup tokens are tied to an immutable Observation, including an
    observed absence. Neither a numeric revision nor an ID alone is a CAS
    proof. Reads disclose only lower-store data, not service authority. *)
val find_invocation
  :  Observation.t
  -> Agent_protocol.Id.Invocation.t
  -> Invocation_lookup.t

val find_job
  :  Observation.t
  -> Agent_protocol.Id.Job.t
  -> Agent_protocol.Developer_job.t option

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

(** First native CAS may install Dispatching directly from observed absence;
    there is no synthetic persisted Admitted step. If Admitted is already
    present, exact context and entry revision must match. Terminal or already
    Dispatching records reject. The higher actor must confirm durability before
    any callback effect. *)
val prepare_dispatch
  :  handle
  -> expected:Observation.t
  -> lookup:Invocation_lookup.t
  -> dispatched:Agent_protocol.Host_invocation.t
  -> (Prepared.t, Error.t) result

(** Atomically advance this run checkpoint with newly selected Run_root jobs
    from a direct run action, so none has a claimed parent job. A worker may
    later launch from the run scope with a real parent job; that requires the
    separate physical worker-claim path, not this root-action entrypoint. This
    lower layer validates owner/capture/epoch/launch and immutable new queued
    shape; a live root scope and policy belong to the actor. *)
val prepare_root_selected
  :  handle
  -> expected:Observation.t
  -> phase:Checkpoint.phase
  -> selected:Agent_protocol.Developer_job.t list
  -> (Prepared.t, Error.t) result

(** One CAS changes the exact Dispatching invocation to terminal and appends
    only its newly selected child jobs. Native active-parent proof remains an
    actor responsibility. Empty selected preserves the old no-child behavior. *)
val prepare_terminal_selected
  :  handle
  -> expected:Observation.t
  -> lookup:Invocation_lookup.t
  -> terminal:Agent_protocol.Host_invocation.t
  -> selected:Agent_protocol.Developer_job.t list
  -> (Prepared.t, Error.t) result

(** Foundation checkpoint-only commit. A developer dispatch or selected-job
    Prepared is rejected; it must use [commit_and_settle]. One serialized writer
    replaces the exact document and syncs file/directory before acknowledgement.
    An Error or cancellation may occur after durability, so [reconcile] checks
    authority before a caller interprets that outcome. The original
    cancellation/exception identity is preserved. *)
val commit : handle -> Prepared.t -> (Installed.t, Error.t) result

(** Foundation checkpoint-only reconciliation; a dispatch or selected-job
    Prepared is rejected. Sync the owned parent directory, then compare the
    bounded authoritative
    file with the prepared exact before/after bytes and revision. An Installed
    response therefore confirms the observed rename is durable even if the
    original replacement was interrupted during directory sync. Diverged means
    another authoritative state exists and requires explicit recovery. *)
val reconcile : handle -> Prepared.t -> (reconciliation, Error.t) result

module Settlement : sig
  type t = reconciliation =
    | Installed of Installed.t
    | Absent
    | Diverged
end

module Handoff : sig
  type t

  (** Trusted host adapter over the exact provisional shared-capacity
      reservations for one prepared transaction. The callback must be
      idempotent, acquire only the capacity mutex and never re-enter this
      store. First use binds this token physically to one Prepared value. *)
  val create : settle:(Settlement.t -> unit) -> t
end

(** One daemon-root lease covers bounded compare/replace, file+directory
    durability, exact reconciliation and protected capacity settlement. The
    settlement callback may repeat until it succeeds; a successful settlement
    is recorded once and a repeat of the same pair returns that historical
    outcome without writing or calling it again. Native/application effects do
    not repeat. Installed publishes, exact Absent aborts and Diverged quarantines.
    Unknown I/O retains the bounded pending pair; a later mutation/reset for
    the same run is rejected until reconciliation. Unexpected exception or
    cancellation preserves its original identity after protected bookkeeping.
    On process restart the host rebuilds capacity from durable selected jobs. *)
val commit_and_settle
  :  handle
  -> Prepared.t
  -> Handoff.t
  -> (Settlement.t, Error.t) result

(** One bounded read/settlement attempt under the same lease, no replay loop.
    Only the retained physical pair is accepted. A known successful settlement
    releases the pending barrier; divergence remains quarantined. *)
val reconcile_and_settle
  :  handle
  -> Prepared.t
  -> Handoff.t
  -> (Settlement.t, Error.t) result

(** Stop admission, join owned operations and invalidate all handles. No runtime
    callback is replayed by closing. A physically pending handoff belongs to
    this store lifetime and cannot be acknowledged after close; its exact
    durable document remains for fresh recovery. The host actor must retire
    this run's unclaimed reservations and reconcile durable rows in the same
    daemon-wide capacity ledger before admitting work on a reopened store.
    Claimed worker leases remain charged until their actual cleanup. This
    storage operation itself makes no capacity-recovery claim. *)
val close_and_wait : t -> (unit, Error.t) result
