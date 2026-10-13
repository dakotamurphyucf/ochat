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
    (** This Prepared never attempted replacement: its first commit lost the
        before-bytes CAS, or another claim already installed identical
        after-bytes. Only this exact pending token is retired. *)
    | Prewrite_conflict
    | Foreign_or_stale
    | Limit_exceeded
    | Closed
  [@@deriving sexp_of]
end

type t
type handle
type store = t

module Run_claim : sig
  type t

  (** Non-yielding physical claim of one stable run ID in this exact Store.
      [handle] must have been issued by [store]. Registration precedes any
      actor admission read or callback, so a second owner for the same run
      cannot pass an Eio wait and enter; an in-flight same-run [admit_epoch]
      or unresolved Store transaction also excludes acquisition. Other run IDs
      remain independent. Acquire/release stay on the issuing Eio domain and
      perform no Eio operation themselves.
      This checks physical identity, not principal/source authorization or
      latest epoch, which the host verifies after acquisition. The claim is
      distinct from a one-shot worker Claim and is not execution authority. *)
  val acquire : store:store -> handle:handle -> (t, Error.t) result

  (** Non-yielding release of this exact issuer token after host-owned actions,
      workers and uncertain handoffs are joined/reconciled. A pending Store
      transaction for the run rejects release. Repeated release is harmless;
      the shared Store and unrelated runs stay open. *)
  val release : t -> (unit, Error.t) result
end

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

module Job_lookup : sig
  type t

  val value : t -> Agent_protocol.Developer_job.t option
end

(** Exact job presence and bytes tied to a physical Observation. A copied job
    or bare ID is not a claim basis. *)
val find_job_exact : Observation.t -> Agent_protocol.Id.Job.t -> Job_lookup.t

module Claim : sig
  type t

  val owner : t -> Agent_protocol.Execution_owner.developer
  val job : t -> Agent_protocol.Developer_job.t
  val attempt : t -> int

  (** Verify the actual issuing Store and retained handle are live, the job is
      still in the latest Active epoch, and the caller is on the issuing domain
      BEFORE atomically consuming this token's one-shot entry bit. A foreign or
      stale handle rejects without consuming it; [Ok false] means it entered
      once already. A repeated installed reconciliation returns this SAME token.
      Entry alone is not host permission or capacity authority. [admit] is a
      trusted, non-yielding local issuer-liveness check evaluated inside the
      writer after the Store read and immediately before the atomic entry bit;
      it must not call the Store or perform Eio operations. [on_enter] is a
      trusted non-yielding marker executed immediately after this call wins the
      CAS, before any cancellation point, so the physical scope can distinguish
      its own interrupted entry from a replayed token belonging to another
      already-live callback. *)
  val try_enter
    :  t
    -> handle:handle
    -> admit:(unit -> bool)
    -> on_enter:(unit -> unit)
    -> (bool, Error.t) result

  (** Trusted scope cleanup before any callback starts. This closes a token
      consumed by a Store CAS whose acknowledgement/caller was cancelled before
      callback entry. A token already abandoned or never consumed is harmless.
      The scope alone proves its callback has not begun. *)
  val abandon_before_callback : t -> handle:handle -> (unit, Error.t) result

  (** Read actual latest epoch/phase from the issuing Store on every parent
      effect check. This may yield; callers recheck local proof liveness after
      it returns. An actor's missed epoch-advance notice cannot validate an
      old claim. *)
  val current_for_effect : t -> (bool, Error.t) result
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
    over-limit documents found on disk fail recovery. A live Run_claim for this
    run rejects same/new-epoch admission until the physical owner relinquishes
    it. The exclusion is registered before [admit_epoch]'s first yield and
    removed on every exit; a Run_claim cannot slip in while that operation
    waits for I/O. Other runs remain independent. *)
val admit_epoch
  :  t
  -> owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> previous:Observation.t option
  -> (handle * Observation.t, Error.t) result

val owner : handle -> Agent_protocol.Execution_owner.developer
val capture_fingerprint : handle -> Chatml_source.Digest.t
val issued_by : t -> handle -> bool
val observe : handle -> (Observation.t, Error.t) result

(** Read the authoritative latest epoch/phase under the daemon writer. This is
    an epoch-lifecycle check, not permission to execute a job. *)
val is_latest_active : handle -> (bool, Error.t) result

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

(** Bounded claim preparation from an exact queued lookup in the latest
    Active epoch. A trusted host samples [now], proves due/current authority
    and acquires the existing shared capacity lease before this CAS. The
    resulting distinct Prepared changes only this job to Running, increments
    the attempt once, starts it and clears retry wait. No callback can run yet. *)
val prepare_job_claim
  :  handle
  -> expected:Observation.t
  -> lookup:Job_lookup.t
  -> now:Agent_protocol.Timestamp.t
  -> (Prepared.t, Error.t) result

module Claimed : sig
  type t

  val claim : t -> Claim.t
  val observation : t -> Observation.t
end

type claim_reconciliation =
  | Claimed of Claimed.t
  | Claim_absent
  | Claim_diverged

(** The one document writer and per-run pending barrier durably install Running
    before minting a physical Claim. Unknown commit/cancellation enters no
    callback; exact bounded reconciliation can establish Installed. Repeating a
    settled Prepared returns the same Claim even after unrelated document
    mutations. Two separate preparations of the same queued observation and
    timestamp cannot mint two Claims: only the Prepared that attempted the
    replacement may accept identical installed after-bytes. A proven prewrite
    conflict retires only its own pending token; attempted or uncertain writes
    retain the exact Prepared for recovery. *)
val commit_job_claim : handle -> Prepared.t -> (Claimed.t, Error.t) result

val reconcile_job_claim : handle -> Prepared.t -> (claim_reconciliation, Error.t) result

(** Fresh-Observation CAS for the SAME physical Claim/owner/capture/job/attempt
    and immutable launch/payload/retry fields; unrelated document revisions do
    not invalidate claim ownership. Current epoch allows Waiting_permission
    within the live scope, resumption to Running, then Queued retry,
    Waiting_completion or terminal. A stale epoch permits terminal bookkeeping
    only, never another effect/wait/retry. [revoked_after] is selected under the
    same authoritative read, so a concurrent reset cannot publish a proposed
    retry or wait; an already issued Prepared is never rewritten. A
    completed/superseded attempt cannot be overwritten. Waiting_completion later resolves by actor authority, not
    by entering this claim again. Result/artifact custody remains higher-layer. *)
val prepare_claimed_transition
  :  handle
  -> expected:Observation.t
  -> claim:Claim.t
  -> after:Agent_protocol.Developer_job.State.t
  -> revoked_after:Agent_protocol.Developer_job.State.t
  -> (Prepared.t, Error.t) result

(** An issued Claim whose callback never entered can be abandoned exactly once
    by the trusted worker owner. This atomically closes its entry gate before
    preparing only Cancelled or Interrupted bookkeeping for the same Running
    attempt. It cannot create a native effect, retry or result. If I/O is
    uncertain, reconcile the exact Prepared without reopening entry. *)
val prepare_unentered_claim_cleanup
  :  handle
  -> expected:Observation.t
  -> claim:Claim.t
  -> status:[ `Cancelled | `Interrupted of string ]
  -> now:Agent_protocol.Timestamp.t
  -> (Prepared.t, Error.t) result

(** A first-commit CAS mismatch before any write returns [Prewrite_conflict]
    and retires only this pending token. An attempted write never takes that
    path, even if a later observation diverges. *)
val commit_claimed_transition : handle -> Prepared.t -> (Installed.t, Error.t) result

val reconcile_claimed_transition
  :  handle
  -> Prepared.t
  -> (reconciliation, Error.t) result

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
  type t =
    | Installed of Installed.t
    | Absent
    (** This selected transaction never attempted replacement and lost its
        first CAS, including an identical after-image installed by another
        Prepared. Its exact pending token is retired; provisional capacity
        remains with the caller for bounded fresh-observation preparation. *)
    | Prewrite_conflict
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
    A proven prewrite conflict retires only this Prepared's pending token; its
    callback must retain staged capacity so the caller can reprepare the same
    sealed selection from a fresh Observation, with a finite retry bound and
    without replaying the root action. An identical after-image from a
    different never-attempted Prepared is also a prewrite conflict, not an
    adoption of publication authority. An attempted or unknown write remains
    quarantined on divergence. Unknown I/O retains the bounded pending pair;
    a later mutation/reset for the same run is rejected until reconciliation.
    Unexpected exception or
    cancellation preserves its original identity after protected bookkeeping.
    [admit] is a trusted non-yielding host proof checked after temporary-file
    sync and immediately before entering the authoritative rename. False
    prevents publication, settles an exact absent handoff and returns
    [Foreign_or_stale]. The host must bind it to the retained actor, physical
    run scope, current selection and capture; a caller-supplied ID/digest is
    not proof. The check cannot be atomic with an external authorization
    change during the kernel rename. Reconciliation of a write already
    attempted never calls [admit] or replays the action.
    On process restart the host rebuilds capacity from durable selected jobs. *)
val commit_and_settle
  :  handle
  -> Prepared.t
  -> Handoff.t
  -> admit:(unit -> bool)
  -> (Settlement.t, Error.t) result

(** One bounded read/settlement attempt under the same lease, no replay loop.
    Only the retained physical pair is accepted. A known successful settlement
    releases the pending barrier; divergence remains quarantined. *)
val reconcile_and_settle
  :  handle
  -> Prepared.t
  -> Handoff.t
  -> (Settlement.t, Error.t) result

(** Stop admission, join owned operations and invalidate all handles. An active
    Run_claim rejects explicit close before any state change; the daemon must
    close/join/release all per-run owners first. This check never waits for a
    callback while holding the writer lease. No runtime
    callback is replayed by closing. A physically pending handoff belongs to
    this store lifetime and cannot be acknowledged after close; its exact
    durable document remains for fresh recovery. The host actor must retire
    this run's unclaimed reservations and reconcile durable rows in the same
    daemon-wide capacity ledger before admitting work on a reopened store.
    Claimed worker leases remain charged until their actual cleanup. This
    storage operation itself makes no capacity-recovery claim. *)
val close_and_wait : t -> (unit, Error.t) result
