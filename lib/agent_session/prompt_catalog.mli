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

(** [install] atomically publishes a complete prepared catalog. *)
val install : t -> prepared -> unit

val entries : t -> entry list
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

(** Idempotently closes unused candidate owners; installed candidate is a no-op.
    Does not revoke shared installed read policy or immutable captured revisions. *)
val discard_prepared : prepared -> unit

(** Typed ownership/generation admission for an owned candidate. A foreign,
    discarded or stale candidate cannot publish policy. Legacy install invalidates
    an outstanding owned candidate; subsequent owned install rejects. *)
val install_owned : t -> prepared -> unit Or_error.t
