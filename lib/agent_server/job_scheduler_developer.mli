open! Core

(** Private physical attachment and observation lifetime for the ONE
    Job_scheduler chooser. This module reads bounded Root candidates and
    manages scan/offer child fibers; it never chooses owners, holds a Worker
    Claim, writes a result, or owns capacity. The Root Registry is the only
    source of current physical roots. *)
type t

module Availability : sig
  type t =
    | Runnable
    | Malformed_supported
    | Artifact_installer_unavailable
    | Executor_unavailable
  [@@deriving equal, sexp_of]
end

module Batch : sig
  type t

  (** Every candidate came from one exact current Root/Store observation and
      has this same typed owner. Empty reads produce no batch. A Batch is
      comparison data; [take_batch] removes each queued batch once, while
      exact candidate/attempt offer coalescing still governs retained values. *)
  val owner : t -> Agent_protocol.Execution_owner.t

  val candidates : t -> Developer_root_owner.Job_candidate.t list

  (** Classification only for a candidate physically contained in this Batch.
      Dispatch by authentic saved Job.kind to the installed Tool/Script or
      Model package of this SAME Root attachment. Runnable and
      Malformed_supported are offered to that package's physical Worker Claim;
      missing package and uninstalled artifact remain queued. This is a hint,
      never execution authority or a second scan. *)
  val availability
    :  t
    -> Developer_root_owner.Job_candidate.t
    -> (Availability.t, Agent_protocol.Error.t) result
end

type offer_result =
  | Skipped
  | Unavailable
  | Attempted

(** The owning scheduler switch outlives this value and its joined child
    fibers. There is no fiber or Store read until [poll] or a wake hint. The
    attachment count cannot exceed physically active registry roots. The
    per-root offer count is capped by that Registry's configured
    max_active_workers; the actual Worker and shared Job_capacity still decide
    admission. This is a task-lifetime bound, not a second capacity ledger. *)
val create
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> registry:Developer_root_owner.Registry.t
  -> capacity:Job_capacity.t
  -> t

(** Install the Tool/Script package on the Root's sole attachment. The same
    issuer, replacement, idempotency and model-only ordering rules as
    [attach_model] apply. *)
val attach
  :  t
  -> root:Developer_root_owner.run
  -> runner:Developer_background_job_runner.t
  -> (unit, Agent_protocol.Error.t) result

(** Install a Model package on the SAME physical Root attachment and shared
    scan/offer lifetime. Either package may be attached first, including a
    Model-only Root; an exact repeated attachment is idempotent, while a
    different package cannot replace one already exposed. The package must be
    issued by this exact current Root in [registry]. Addition does not claim
    queued jobs or create another fairness cursor, worker loop or capacity
    ledger. Retirement/close joins all offer tasks regardless of package. *)
val attach_model
  :  t
  -> root:Developer_root_owner.run
  -> runner:Developer_model_job_runner.t
  -> (unit, Agent_protocol.Error.t) result

val wake : t -> root:Developer_root_owner.run -> unit

(** Start at most one cooperative candidate read when due, selecting one
    physical attachment at a time in rotating order. One completed scan
    retains at most one finite batch, capped independently of the Store's
    maximum job inventory. Advance past empty/unsupported prefixes across
    passes, so a queued unavailable entry cannot permanently starve a later
    runnable one. A scan error retains no partial batch. No synchronous
    Store read occurs in the conversation chooser. *)
val poll : t -> unit

(** Consume the latest fresh batch once. At most one unavailable retry batch
    may separately remain; there is no unbounded queue. A returned value may
    already be stale; [current] and the physical Worker Claim must recheck it. *)
val take_batch : t -> Batch.t option

(** At most one comparison-only retry batch (64 candidates) may coexist with
    one fresh scan batch (64 candidates). All attached Roots, including the
    retry Root, remain in the bounded round-robin scan: later windows and new
    durable jobs must remain discoverable. A fresh batch from the same owner
    replaces the retry before ordering, so the chooser never sees duplicate
    owner groups; a newer unavailable batch may likewise replace it. Every
    offer rechecks physical Root/package and Worker authority. *)
val retry_batch : t -> Batch.t option

val clear_retry : t -> Batch.t -> unit

(** Test coordination only: an actual [Unavailable] decision retained a finite
    batch for this exact physical Root. This is neither a claim nor authority;
    the test must assert effects and durable state independently. *)
val retry_pending : t -> root:Developer_root_owner.run -> bool

(** Compare exact attachment generation, physical Root/packages, and live
    Registry membership. No job or source bytes are used as authority. A
    stale/foreign batch is rejected before classification or offer. *)
val current : t -> Batch.t -> (bool, Agent_protocol.Error.t) result

(** Recheck [current] and exact candidate/attempt membership, then fork a
    bounded offer task under the owning switch. An already in-flight exact
    candidate/attempt is not offered again. Different jobs of one Root may
    overlap up to the Root's configured active-worker bound. [Attempted]
    means the task was accepted and its capacity decision or pre-Worker exit
    has been observed. [Unavailable] means the actual shared ledger returned
    no lease; the bounded batch is retained with its candidate order rotated.
    [Skipped] covers a stale/coalesced offer. No outcome proves a Store Claim. The
    observer is nonyielding and one-shot; the chooser never awaits Store
    claim preparation, a handler, or terminal persistence. The task alone
    calls the matching physically attached package on the
    exact candidate and never replays its effect after an error or uncertain
    save. A full Script/Tool/Model effect cannot serially block the conversation
    chooser. A malformed supported request is an attempted owner turn even
    if its physical Claim persists Failed and returns Error. *)
val offer
  :  t
  -> Batch.t
  -> Developer_root_owner.Job_candidate.t
  -> (offer_result, Agent_protocol.Error.t) result

(** Cancel and join exact scan/offer borrowers before removing this physical
    attachment. A foreign Root cannot remove one with equal copied IDs.
    Existing Root/Worker custody handles entered claims and unknown saves.
    Reject scanner/offer self-join before state mutation. *)
val retire : t -> root:Developer_root_owner.run -> (unit, Agent_protocol.Error.t) result

(** Idempotently prevent scans/offers and cancel active child tasks. The
    scheduler calls [await_idle] before physical Root/Store close. *)
val close : t -> unit

(** After close, join scanner and offer tasks. Reject pre-close or self-join
    without mutation. Cancellation/exception never acknowledges unjoined
    work; no authority-bearing callback escapes. *)
val await_idle : t -> (unit, Agent_protocol.Error.t) result
