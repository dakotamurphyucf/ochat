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
    Capture imports, scripts and static relative nested-agent sources at their
    declaration directories. Deduplicate cycles; reject closures exceeding 256
    agent sources (including the root) or 8 MiB of captured source bytes.
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
