open Core
module Baseline = Prompt_project_baseline

type state =
  | Available
  | Busy
  | Uncertain
  | Closed
[@@deriving equal, sexp_of]

module Untracked : sig
  type t =
    | Missing_index
    | Invalid_index of Baseline.Error.t
end

module Error : sig
  type t =
    | Busy
    | Closed
    | Uncertain
    | Stale_snapshot
    | Foreign_snapshot
    | Untracked of Untracked.t
    | Baseline of Baseline.Error.t
    | Store of Agent_store.Store_error.t
    | Publication_uncertain of Agent_store.Store_error.t
end

type t

val state : t -> state

(** Borrow one opened metadata directory plus existing host-exclusive
    coordination until switch cleanup revokes admission, cancels and joins work.
    Switch-only lifetime; no public manual close/self-join endpoint. Fixed child
    basename, never a metadata-derived path or source authority. All calls occur
    on the opening Eio domain; concurrent/reentrant admission returns Busy.
    Expected/known bytes and snapshots remain immutable.

    Missing and invalid documents produce an owner whose snapshots are Untracked;
    ordinary filesystem/permission/read errors instead return Store failures.
    Nothing initializes or silently treats an unreadable index as new. *)
val open_existing
  :  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  -> sw:Eio.Switch.t
  -> limits:Baseline.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

(** Only after the caller has actually created a new owned host data root, under
    its exclusive coordination. Refuse a present destination; install the bounded
    authoritative empty document using Flush_file_and_directory. Existing-root
    open NEVER initializes/repairs missing or corrupt historical metadata.
    The host startup integration must establish newly-created-root ownership;
    mere absence of this child file is insufficient proof. Writer failure is
    uncertain; no automatic retry or temp ownership guess. *)
val create_new
  :  [ `Close | Eio.Fs.dir_ty ] Eio.Path.t
  -> sw:Eio.Switch.t
  -> limits:Baseline.Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

module Snapshot : sig
  module History : sig
    (** Finite metadata only: no source authority, artifact references or raw
        invalid-document diagnostics. *)
    type t =
      | Tracked of int
      | Missing
      | Invalid
    [@@deriving equal, sexp_of]
  end

  (** Immutable generation/owner-affine state. May be kept across preparation but
      supplies no native authority and becomes stale after another publication.
      This persisted index generation is distinct from the original Catalog
      pending-candidate generation; the host snapshots/seals both together. *)
  type t

  type previous =
    | First_admission
    | Established of Baseline.Entry.t
    | Untracked of Untracked.t

  val generation : t -> int option
  val history : t -> History.t

  val previous
    :  t
    -> receipt:Agent_store.Prompt_source_receipt.t
    -> checkpoint:(unit -> unit)
    -> (previous, Error.t) Result.t

  (** Exact last-successful artifacts join host prune protection independently of
      active/cached/runtime revisions. This is reference evidence, not deletion
      authority. No directory enumeration determines a baseline. *)
  val protected_revisions : t -> Agent_protocol.Id.Prompt_revision.t list
end

(** Bound current expected read before handing a snapshot to preparation. Missing
    or malformed historical index remains Untracked (never First_admission).
    A known index with no definition entry proves first definition admission only
    while this owner is authoritative. Changed source receipt returns a typed
    intent mismatch rather than borrowing another root's previous selection. *)
val snapshot : t -> checkpoint:(unit -> unit) -> (Snapshot.t, Error.t) Result.t

module Candidate : sig
  (** Entire bounded merged document, final bytes, expected generation/byte
      identity and post-publication Snapshot allocated before writer entry. *)
  type t

  (** Actual next index generation allocated by successful preparation, not an
      acknowledgment of publication. Pure and safe to inspect before writing. *)
  val generation : t -> int
end

(** Pure prepare: owner/snapshot affinity, counter overflow, capacity, successful
    entry references, unknown preservation and complete encoded bound all checked
    before retaining a Candidate. Untracked refuses automatic replacement. No
    failed/removed-definition overwrite and no history eviction. *)
val prepare
  :  t
  -> snapshot:Snapshot.t
  -> successes:Baseline.Success.t list
  -> checkpoint:(unit -> unit)
  -> (Candidate.t, Error.t) Result.t

(** Recheck owner generation and bounded exact expected index bytes, then final
    Eio cancellation / borrowed checkpoint / Eio cancellation poll. Caller may
    supply its existing whole-operation deadline through checkpoint; none is reset
    or retained. All result/state allocations precede protected writer entry.
    Set Uncertain BEFORE Durable_file.replace_in Flush_file_and_directory; no
    post-writer checkpoint/deadline. A pending waiter cancellation is checked
    without callback before acknowledgment. Only successful delivered writing
    installs preallocated next state; writer error/exception or undelivered success
    remains Uncertain. Original exception/backtrace propagates. Close revokes,
    cancels/joins active worker before borrowed directory release. No lock held
    across callback/IO, retry, rollback, old-current exposure or unknown-temp removal.
    Host Catalog publication/pointer coordination belongs to its integration owner.
    The host must durably install every finalized successful artifact BEFORE this
    publication, and retain baseline references in pruning protection. Success
    finalization alone proves no durability. *)
val publish
  :  t
  -> Candidate.t
  -> checkpoint:(unit -> unit)
  -> (Snapshot.t, Error.t) Result.t
