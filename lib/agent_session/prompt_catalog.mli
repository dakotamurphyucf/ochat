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
