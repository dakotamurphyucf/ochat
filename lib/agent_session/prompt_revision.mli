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

(** Present only for exact parser/program custody; legacy revisions retain None. *)
val parser_context : t -> Prompt.Chat_markdown.Parsed.t option

val programs : t -> Chatml_prompt_programs.t option

(** Attach the already-admitted parser/program custody after artifact replay.
    Reject a divergent artifact bundle/current Root/raw revision/active mapping or
    elements not owned by this actual Parsed. No compilation/execution authority. *)
val with_custody
  :  t
  -> parser_context:Prompt.Chat_markdown.Parsed.t
  -> programs:Chatml_prompt_programs.t
  -> checkpoint:(unit -> unit)
  -> t Or_error.t

(** Present only after exact content custody and original parser affinity have
    been validated against this artifact. *)
val content : t -> Chatmd_prompt_content.t option

(** The exact validated projection attached by [with_content], retaining this
    revision's original parser identity. No reparse or materialization occurs. *)
val document_projection : t -> Prompt.Chat_markdown.Document_projection.t option

(** Attach a projection created by the SAME admitted content owner and original
    Parsed already held by this revision. Verify exact artifact metadata/private
    payload bytes before replacing elements; parser/script ownership stays on the
    original Parsed. Reject missing/divergent bundle and foreign projection. *)
val with_content
  :  t
  -> content:Chatmd_prompt_content.t
  -> materialized:Chatmd_prompt_content.Materialized.t
  -> checkpoint:(unit -> unit)
  -> t Or_error.t
