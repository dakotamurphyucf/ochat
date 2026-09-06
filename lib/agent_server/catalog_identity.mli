open! Core

(** Stable opaque protocol identities derived from versioned configuration
    slugs. *)

val prompt_definition : string -> Agent_protocol.Id.Prompt_definition.t
val workspace_definition : string -> Agent_protocol.Id.Workspace_definition.t
