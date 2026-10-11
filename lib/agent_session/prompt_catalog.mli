open! Core

type availability =
  | Ready of Prompt_revision.t
  | Unavailable of Prompt_revision_builder.Diagnostic.t list
  | Disabled

type entry =
  { definition : Prompt_definition.t
  ; availability : availability
  }

type prepared
type t

(** Catalogs, source selections and mutable preparation/publication tokens belong
    to the creating Eio domain. Read accessors touching mutable state and legacy raising APIs reject a
    foreign domain with Invalid_argument before inspecting mutable state;
    prepare_owned/install_owned return Error, and Seal operations return
    Foreign_domain. prepared_entries is an immutable projection and inspects no
    token state. Native values inside revisions retain their own lifetime rules. *)

(** [content_limits] is the catalog's fixed host policy for stored document
    restoration. [None] rejects content-bearing stored revisions. Cached revisions
    already own their validated projection and are reused without another replay;
    changing restoration policy requires a new catalog. Fresh preparation retains
    its acquisition callback's original limits through publication. *)
val create
  :  env:Eio_unix.Stdenv.base
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> content_limits:Chatmd_prompt_content.Limits.t option
  -> t

(** [prepare] builds every enabled revision without changing the published
    catalog. Invalid entries are retained as unavailable. *)
val prepare
  :  t
  -> transaction_id:(Prompt_definition.t -> Agent_protocol.Id.Transaction.t)
  -> created_at:Agent_protocol.Timestamp.t
  -> Prompt_definition.t list
  -> prepared

(** [install] atomically publishes a complete prepared catalog. It is the
    raising compatibility adapter for [install_owned]; a sealed or uncertain
    publication rejects without invalidating its candidate. *)
val install : t -> prepared -> unit

val entries : t -> entry list

(** Immutable candidate availability snapshot for host success finalization before
    sealing. This projection supplies no install admission or durable authority. *)
val prepared_entries : prepared -> entry list

val find : t -> Agent_protocol.Id.Prompt_definition.t -> entry option
val find_by_name : t -> string -> entry option

(** Previously built revisions remain addressable after catalog reload. *)
val find_revision : t -> Agent_protocol.Id.Prompt_revision.t -> Prompt_revision.t option

(** [prune_unreferenced_artifacts t ~additional] retains every revision cached
    by the catalog plus [additional] durable-session revisions. *)
val prune_unreferenced_artifacts
  :  t
  -> additional:Agent_protocol.Id.Prompt_revision.t list
  -> (int, Agent_store.Store_error.t) result

(** Restores and caches an immutable revision that is no longer the current
    catalog revision for its prompt definition, using the fixed host content
    policy supplied at construction. Both cache hits and loaded artifacts must
    belong to [definition_id]; present artifact definition/receipt metadata must
    agree. Old artifacts without that metadata retain the explicitly selected
    definition. Conflicting content for a cached revision ID rejects.

    Admit a currently configured definition before uncached restoration, then
    release the catalog mutex during artifact IO/replay. A reload or removal
    after this admission does not revoke the immutable historical revision.
    Already cached revisions remain addressable after removal. Failed restoration
    is never cached. Restoration writes its separate cache, so catalog publication
    cannot discard revisions concurrently restored during preparation. *)
val restore_revision
  :  t
  -> definition_id:Agent_protocol.Id.Prompt_definition.t
  -> revision_id:Agent_protocol.Id.Prompt_revision.t
  -> (Prompt_revision.t, Prompt_revision_builder.Diagnostic.t list) result

module Source_selection : sig
  type t

  val receipt : t -> Agent_store.Prompt_source_receipt.t

  type availability =
    | Live
    | Unavailable of Prompt_source_owner.Error.t
    | Revoked
    | Closed

  val availability : t -> availability

  val check_intent
    :  t
    -> Agent_store.Prompt_source_receipt.t
    -> (unit, Prompt_source_owner.Error.t) Result.t

  val lease
    :  t
    -> expected:Agent_store.Prompt_source_receipt.t
    -> sw:Eio.Switch.t
    -> checkpoint:(unit -> unit)
    -> (Prompt_source_owner.t, Prompt_source_owner.Error.t) Result.t
end

val source_selection
  :  t
  -> Agent_protocol.Id.Prompt_definition.t
  -> Source_selection.t option

(** Actual current operator definitions authorize their original directory opens.
    Bound root count before acquisition. Retain unavailable live-root intent apart
    from captured revision availability. Reuse installed policy cell for exact
    unchanged intent with the SAME immutable ceilings; different supplied limits
    for unchanged installed intent reject before native acquisition. Candidate
    discard never revokes installed policy. Cleanup
    every candidate on failure BEFORE return, without held catalog mutex. [sw]
    owns installed descriptors; explicit discard/retirement releases them sooner.
    No receipt path is used to acquire authority. Legacy [prepare] stays unchanged.
    [prepare_programs] and [prepare_content] borrow the same live source scope and
    its actual capture before artifact publication. They may acquire source graphs
    and document content, never initialize executable scripts. Neither callback is
    retained; original exceptions and cancellation propagate after candidate
    cleanup. Without [prepare_programs], the existing parser-only program custody
    path remains. Captured documents require [prepare_content];
    it is invoked only when the actual root or a queued agent contains captured
    or builtin document occurrences. All-dynamic and document-free captures omit
    this hook and retain absent content custody. Its immutable result is validated
    by the Builder before publication; an invoked hook's limits are not reset. *)
val prepare_owned
  :  t
  -> ?prepare_programs:
       (Prompt_source_owner.Scope.t
        -> Prompt_source_owner.Capture.t
        -> (Chatml_prompt_programs.t, Prompt_revision_builder.Diagnostic.t list) Result.t)
  -> ?prepare_content:
       (Prompt_source_owner.Scope.t
        -> Prompt_source_owner.Capture.t
        -> (Chatmd_content_capture.t, Prompt_revision_builder.Diagnostic.t list) Result.t)
  -> sw:Eio.Switch.t
  -> source_limits:Prompt_source_owner.Limits.t
  -> max_roots:int
  -> checkpoint:(unit -> unit)
  -> transaction_id:(Prompt_definition.t -> Agent_protocol.Id.Transaction.t)
  -> created_at:Agent_protocol.Timestamp.t
  -> Prompt_definition.t list
  -> prepared Or_error.t

(** Idempotently closes unused candidate owners; installed, sealed and uncertain
    candidates are a no-op. Only the affine Seal may dispose its sealed candidate.
    Does not revoke shared installed read policy or immutable captured revisions. *)
val discard_prepared : prepared -> unit

(** Typed ownership/generation admission for an owned candidate. A foreign,
    discarded or stale candidate cannot publish policy. Legacy install invalidates
    an outstanding unsealed owned candidate; subsequent owned install rejects.
    All install paths reject while a Seal is pending or publication is uncertain. *)
val install_owned : t -> prepared -> unit Or_error.t

module Publication_state : sig
  type t =
    | Available
    | Sealed
    | Uncertain
  [@@deriving equal, sexp_of]
end

(** Single opening Eio domain only. Sealing blocks owned preparation and installers,
    not immutable
    revision lookups/restoration. Uncertainty blocks new owned preparation and
    installation until the actual host restarts/reconciles its durable baseline;
    this module performs no durable writing or repair. *)
val publication_state : t -> Publication_state.t

module Seal : sig
  module Error : sig
    type t =
      | Busy
      | Foreign_domain
      | Publication_uncertain
      | Unowned_candidate
      | Foreign_candidate
      | Stale_candidate
      | Discarded_candidate
      | Already_installed
      | Generation_exhausted
      | Conflicting_revision of Agent_protocol.Id.Prompt_revision.t
      | Aborted
      | Committed
      | Pending
    [@@deriving equal, sexp_of]
  end

  (** Affine to its actual Catalog and owned candidate. Contains preallocated
      install maps, revocations, retirements, next generation and result carriers;
      no preparation/checkpoint callback is retained. Existing candidate/retired
      Source_owner capabilities keep their scoped explicit close ownership. *)
  type t

  (** Before durable acknowledgment only. First releases seal/install admission
      without yielding, then closes unused candidate owners outside the catalog
      mutex under cancellation protection. Idempotent after successful abort.
      Never revokes shared installed policies. Committed/uncertain seals reject. *)
  val abort : t -> (unit, Error.t) Result.t

  (** Record an uncertain external durable publication, without choosing old/new
      durable bytes. Idempotent; no callback, poll, IO, allocation or yielding.
      Existing installed pointers and captured revisions remain intact. Blocks
      replacement admission and retains pending revision prune protection.
      Call [retire] separately to close unused candidate owners. *)
  val mark_uncertain : t -> (unit, Error.t) Result.t

  (** Host final pointer phase AFTER acknowledged durable publication. One-shot,
      idempotent after commit; aborted/uncertain seals reject. Nonyielding and
      nonallocating: only precomputed policy revocations and pointer assignments,
      with no mutex acquisition, callback, poll, IO or fresh admission. Work is
      proportional to preadmitted replaced/removed policies. This operation never
      overwrites the separate restored-revision cache. The host coordinates its
      other already-preallocated pointer swaps in the SAME nonyielding protected
      phase; this token alone promises no cross-service/durable transaction. *)
  val commit : t -> (unit, Error.t) Result.t

  (** After commit close retired installed owners; after uncertainty close unused
      candidate owners. Native cancellation/join may suspend, so cleanup must run
      after the complete host pointer phase, outside every catalog mutex.
      Idempotent. A pending seal rejects; cleanup exceptions preserve their original
      identity/backtrace and cannot roll back acknowledged publication. Concurrent
      cleanup returns Busy until the admitted join has completed. *)
  val retire : t -> (unit, Error.t) Result.t
end

(** Allocate and validate before external durable IO. Require SAME pending owned
    candidate and current catalog generation, reject installed/discarded/foreign
    candidates, overflow and revision-ID collisions with different definition or
    original manifest digest. Identical revision identity is reusable. All scans,
    comparisons and allocations happen before sealing, with cooperative checkpoint
    polls outside the catalog mutex. Final callback/Eio checks precede the short
    exact generation/token admission. No IO occurs under that mutex.

    Once admitted, legacy/owned install, new owned preparation and generic discard cannot
    invalidate this seal. Immutable restorations may still populate their separate
    cache while external durable writing proceeds. Original checkpoint exceptions
    and cancellation propagate, with no seal/partial policy publication on failure.
    This module neither constructs Baseline.Success nor writes the baseline. *)
val seal : t -> prepared -> checkpoint:(unit -> unit) -> (Seal.t, Seal.Error.t) Result.t
