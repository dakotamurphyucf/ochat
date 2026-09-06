open! Core

(** Prepared immutable prompt revision parsed from its artifact tree. *)

type t

val create
  :  definition:Prompt_definition.t
  -> artifact:Agent_store.Prompt_artifact_store.Artifact.t
  -> materialized_tree:Eio.Fs.dir_ty Eio.Path.t
  -> elements:Prompt.Chat_markdown.top_level_elements list
  -> t

val id : t -> Agent_protocol.Id.Prompt_revision.t
val definition : t -> Prompt_definition.t
val artifact : t -> Agent_store.Prompt_artifact_store.Artifact.t
val materialized_tree : t -> Eio.Fs.dir_ty Eio.Path.t
val elements : t -> Prompt.Chat_markdown.top_level_elements list
val root_relative_path : t -> string
