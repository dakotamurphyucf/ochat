(** Live private result publication for a physical developer-job attempt. The
    worker owns the Claim, capacity and sole post-join Store terminal CAS; this
    publisher freezes comparison state, writes an exact intent and optional
    staged Blob, then acknowledges only a real installed terminal. It cannot
    run the handler, grant result reads, restore startup state, or prove global
    collection safety. Conversation result v1 remains on Job_result_store. *)
open! Core

type t

module Limits : sig
  type t

  (** [inline_bytes] may be zero (Automatic always chooses Artifact) and may
      not exceed [max_result_bytes] or the fixed private-document Inline
      ceiling. [max_result_bytes] and [max_pending] are positive/finite;
      [max_result_bytes] bounds canonical Completion JSON, not just its payload,
      and is at most the protocol Completion decoder's 9 MiB ceiling. The
      publisher also rejects a Blob coordinator whose upload ceiling is lower
      than this bound. [max_pending] bounds retained process-local selections,
      including failed/uncertain first attempts; it is at most 1024 and the
      declared canonical result ceilings together may not exceed 64 MiB. *)
  val create
    :  inline_bytes:int
    -> max_result_bytes:int
    -> max_pending:int
    -> (t, Agent_protocol.Error.t) result
end

module Storage : sig
  type t =
    | Automatic
    | Artifact
  [@@deriving equal, sexp]
end

(** Bind this one run's actual Result_root to its physically issuing Store and
    the already-bound shared Blob coordinator. The publisher checks that the
    canonical result ceiling fits the coordinator's upload limit and borrows the
    owner's long-lived switch, never a joined worker child switch. Construction
    performs only issuer/binding/limit validation, not stage I/O or a Store
    transition. It checks that switch is live; selection and staging check it
    again, including after yielding intent/Blob I/O. A closed switch or
    cancellation preserves its original exception and cannot publish a new
    attempt. Exact Installed acknowledgement may still retire a marker after
    the switch has closed. A different physical Store with equal IDs rejects. *)
val create
  :  sw:Eio.Switch.t
  -> runs:Developer_run_store.t
  -> root:Developer_run_store.Result_root.t
  -> blobs:Blob_store.t
  -> limits:Limits.t
  -> (t, Agent_protocol.Error.t) result

module Selected : sig
  type t

  val claim : t -> Developer_run_store.Claim.t
  val proposed : t -> Agent_protocol.Developer_job.State.t
  val after : t -> Agent_protocol.Developer_job.State.t
  val completion : t -> Agent_protocol.Completion.t
  val selected_at : t -> Agent_protocol.Timestamp.t
  val reference : t -> Developer_result_reference.t option

  (** Save/reverify the identical checksummed private intent before any Blob
      stage. Artifact mode then uses Blob_store's single shared staged-write
      engine under its coordinator. Inline has no Blob stage. Once both are
      exact, the reviewed Result_publication_phase enters non-discardable
      Attempted custody BEFORE returning to the worker's one Store CAS. Typed
      error, unexpected exception or cancellation retains this same token and
      cannot authorize selecting a replacement or replaying a handler. *)
  val ensure_attempted : t -> (unit, Agent_protocol.Error.t) result

  (** Verify the actual Store-issued Installed receipt's physical issuer,
      owner/capture/job/attempt, terminal state and selected result before
      settling the phase. Exact marker retirement is best-effort: typed cleanup
      error leaves its root; unexpected exception/cancellation retains original
      identity/backtrace after phase settlement and process-local slot release.
      The worker retires installed Claim/capacity before propagating such an
      unexpected exception. *)
  val acknowledge_installed
    :  t
    -> Developer_run_store.Installed.t
    -> (unit, Agent_protocol.Error.t) result
end

(** Immutable selection with bounded process-local retention, before any
    intent/stage/Store I/O. [claim] must be physically issued by this
    publisher's Store and match the exact root run,
    principal, capture, job and attempt. [proposed] is the worker's post-join
    normalized terminal state with an Inline Completion; the worker separately
    compares it with its OWN callback state and same physical Claim. Selection
    preserves every lifecycle field and outcome; only Inline -> Artifact of
    exact content bytes/digest is allowed. Automatic uses the byte threshold
    only when the entire versioned Inline document also fits its fixed carrier;
    otherwise it chooses Artifact. Artifact forces a retained Blob even below
    the threshold. No result is truncated. The same attempt already
    pending is returned for byte-identical proposed/result even if a retry asks
    for another storage preference: the first selected mode wins. A different
    result fails closed. Pending capacity is checked before allocating a new
    Blob identity. *)
val select
  :  t
  -> storage:Storage.t
  -> claim:Developer_run_store.Claim.t
  -> proposed:Agent_protocol.Developer_job.State.t
  -> selected_at:Agent_protocol.Timestamp.t
  -> (Selected.t, Agent_protocol.Error.t) result

(** Comparison state for the actor's in-process retry decision only. This is
    not an execution Claim, result-read grant or startup-restored evidence. A
    pending selection survives uncertain intent, bytes, metadata, Store write
    and cleanup acknowledgements until exact terminal reconciliation; no
    result replacement or callback replay is permitted. *)
val pending
  :  t
  -> job:Agent_protocol.Developer_job.t
  -> (Selected.t option, Agent_protocol.Error.t) result
