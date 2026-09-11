open! Core

module Diagnostic : sig
  type t =
    { code : string
    ; message : string
    ; source : string option
    }
  [@@deriving compare, equal, sexp]
end

(** [build] captures, inspects, persists, and reparses one prompt revision.
    Capture imports, scripts, extension schemas and static relative nested-agent sources at their
    declaration directories. Deduplicate cycles; reject closures exceeding 256
    total source files (including the root) or 8 MiB of captured source bytes.
    A dependency returning different bytes during one capture fails closed.
    Explicit absolute agent references remain external dependencies.
    It never authorizes a manifest or starts executable runtime resources. *)
val build
  :  env:Eio_unix.Stdenv.base
  -> artifact_store:Agent_store.Prompt_artifact_store.t
  -> transaction_id:Agent_protocol.Id.Transaction.t
  -> created_at:Agent_protocol.Timestamp.t
  -> Prompt_definition.t
  -> (Prompt_revision.t, Diagnostic.t list) result

(** [restore] verifies and reparses an already installed pinned revision. *)
val restore
  :  artifact_store:Agent_store.Prompt_artifact_store.t
  -> Prompt_definition.t
  -> Agent_protocol.Id.Prompt_revision.t
  -> (Prompt_revision.t, Diagnostic.t list) result

(** Reparse a host-selected artifact against an existing captured tree. Verifies
    the complete tree/inventory and parser/runtime contract before parsing through
    the captured source loader. Does not install an artifact, consult live source
    files, instantiate tools or initialize scripts. Intended for retained authored
    subdocuments whose source root differs from the containing revision's root.
    The host must supply the admitted source and original definition policy; this
    is source preparation, not an execution or catalog authorization. *)
val reparse
  :  definition:Prompt_definition.t
  -> artifact:Agent_store.Prompt_artifact_store.Artifact.t
  -> materialized_tree:Eio.Fs.dir_ty Eio.Path.t
  -> (Prompt_revision.t, Diagnostic.t list) result
