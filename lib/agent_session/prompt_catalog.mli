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

val create
  :  env:Eio_unix.Stdenv.base
  -> artifact_store:Agent_store.Prompt_artifact_store.t
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
    catalog revision for its prompt definition. *)
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
    [prepare_programs] borrows the SAME live source scope and its actual capture
    before artifact publication. It may acquire source graphs, never initialize
    executable scripts. The callback is not retained; original exceptions and
    cancellation propagate after candidate cleanup. Without it, the existing
    parser-only custody path remains. *)
val prepare_owned
  :  t
  -> ?prepare_programs:
       (Prompt_source_owner.Scope.t
        -> Prompt_source_owner.Capture.t
        -> (Chatml_prompt_programs.t, Prompt_revision_builder.Diagnostic.t list) Result.t)
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
