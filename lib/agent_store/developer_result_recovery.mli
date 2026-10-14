(** Read-only startup inventory and exact resumption of previously selected
    developer-job results. The host opens the actual exclusive Store Recovery
    window before exposing a worker. This module preserves that window's
    original historical owner/epoch/capture/attempt and never runs a handler,
    mints a Claim, chooses a replacement Completion, or writes a terminal. *)
open! Core

type t

module Limits : sig
  type t

  (** One finite allowance for the COMPLETE sibling intent namespace plus
      relevant developer Blob stage/metadata/content evidence and real run
      documents. Directory opens, entries (including empty records), framed
      bytes, metadata, content and in-memory comparison work all debit this
      attempt. The work charge is propagated into Store document enumeration,
      the shared Blob scanner, decoding/hash/copy loops and publication, with
      a caller-domain checkpoint between bounded read chunks; no per-run or
      per-stage scan resets the allowance. Result bytes
      must also fit the existing Blob upload and Completion decoder ceilings.
      Zero or overflowing caps reject before any enumeration. *)
  val create
    :  max_entries:int
    -> max_bytes:int
    -> max_work:int
    -> max_result_bytes:int
    -> (t, Agent_protocol.Error.t) result
end

(** Bind to the same physical Store, Result_root and shared Blob coordinator
    as live publication. [sw] is the startup owner's live switch; it is
    checked before and after waiting for the Blob coordinator, Store reads and
    stage I/O. Physical Store/root binding is rechecked after coordinator
    acquisition. A path, target label or equal
    host/run IDs cannot substitute for those opaque owners. Construction does
    not open a Recovery window or stage bytes. *)
val create
  :  sw:Eio.Switch.t
  -> runs:Developer_run_store.t
  -> root:Developer_run_store.Result_root.t
  -> blobs:Blob_store.t
  -> limits:Limits.t
  -> (t, Agent_protocol.Error.t) result

module Restored : sig
  type t

  val candidate : t -> Developer_run_store.Recovery.Candidate.t
  val after : t -> Agent_protocol.Developer_job.State.t
  val selected_at : t -> Agent_protocol.Timestamp.t
  val completion : t -> Agent_protocol.Completion.t
  val reference : t -> Developer_result_reference.t option

  (** Require this original physical window's CURRENT Pending for the exact
      Candidate and frozen [after], then re-establish the same framed intent
      and verify the already complete selected Blob bytes through the existing
      stage engine. Recheck Store affinity and original switch after every wait and
      enter the shared non-discardable Attempted phase before returning to the
      host's one [Recovery.commit_terminal]. This operation is unavailable for
      a missing/partial stage because its lost Completion bytes cannot be
      recreated from the Artifact intent. Error/cancellation preserves this
      immutable token and original exception/backtrace. One action-local Reader
      bounds the pre/post Store authority reads; the shared intent-file path
      separately caps its frame at 128 KiB, and the shared Blob stage engine
      separately caps selected content by [max_result_bytes] and its configured
      upload limit. The Reader does not claim to meter those backend writes or
      their bounded revalidation reads. *)
  val ensure_attempted
    :  t
    -> pending:Developer_run_store.Recovery.Pending.t
    -> (unit, Agent_protocol.Error.t) result

  (** Accept only a real Installed receipt issued by this Store for the exact
      candidate/job/attempt and frozen terminal. The shared phase retires the
      exact marker best-effort after installation; typed cleanup error may
      retain it, but cannot rewrite the installed job. Unexpected cleanup
      exception propagates after releasing the local slot. *)
  val acknowledge_installed
    :  t
    -> Developer_run_store.Installed.t
    -> (unit, Agent_protocol.Error.t) result
end

module Incomplete : sig
  type t

  val candidate : t -> Developer_run_store.Recovery.Candidate.t
  val after : t -> Agent_protocol.Developer_job.State.t
  val selected_at : t -> Agent_protocol.Timestamp.t
  val reference : t -> Developer_result_reference.t option

  (** An exact decoded intent exists, but its selected Artifact bytes are
      absent or partial. The intent retains comparison custody. It carries no
      Completion value and exposes no stage repair or terminal publication:
      replaying the handler or replacing it with Interrupted/Success would
      lose the frozen selection. The startup window remains unresolved until a
      separately authorized exact-byte recovery policy is available. *)
end

module Evidence : sig
  type t = private
    | Selected of Restored.t
    | No_selection of Developer_run_store.Recovery.Candidate.t
    | Incomplete_selection of Incomplete.t

  (** [Selected] has a valid exact intent and complete matching stage/content.
      [Incomplete_selection] has a valid exact intent but its matching stage
      is absent or partial; it retains the frozen selection without inventing
      the missing Completion bytes.
      [No_selection] requires complete proved absence of intent AND stage for
      that candidate after the entire admitted inventory. None of these
      variants itself authorizes an Interrupted terminal or result disclosure. *)
end

(** Validate the entire sibling `developer-result-preparations` tree, not only
    filenames for [view]'s candidates. Each run/epoch/job key must be canonical
    and match a real retained run/epoch/job and decoded intent. Other authentic
    runs and epochs are valid inventory members. A marker already removed for a
    completed Artifact job is distinguished from an orphan by its real Store
    terminal and exact durable Blob metadata/content; verified empty run/epoch
    directories do not imply outstanding selection. Unknown, linked, corrupt,
    foreign, conflicting, uncertain temporary or unpaired stage evidence
    rejects the WHOLE scan, including when a run document is absent. Every
    candidate receives one Evidence only after all checks and aggregate budgets
    finish; a failed scan exposes none. This is custody comparison, not the
    positive cross-owner deletion proof owned by the collector. *)
val inventory
  :  t
  -> window:Developer_run_store.Recovery.t
  -> view:Developer_run_store.Recovery.View.t
  -> (Evidence.t list, Agent_protocol.Error.t) result
