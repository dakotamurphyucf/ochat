open! Core

(** Pure comparison of one retained selected declaration with a genuine
    current-root reparse over the SAME frozen import bytes. This grants neither
    native source reads nor a new Build/contract selection. The caller still
    checks the original/request actor and exact retained head before serving. *)
module Error : sig
  type t =
    | Foreign_owner_or_imports
    | Changed_route_or_environment
    | Ambiguous_declaration
    | No_validated_build
end

type t

(** Requires [old] to be a physical row in [rows] for [old_capture], [updated] to own the
    exact current root source, and all nonroot import IDs/digests/bytes to match
    the retained closure. The script ID only finds a candidate; source_ref,
    project selection, environment, external locator, declaring-source owner,
    project route and converted Surface (language/kind/API/limits) must match
    independently. Only an inline body
    may change. This function does not cast an old Project_capture into the new
    parser generation. The caller uses existing selected-entry recomposition
    and Script.assess to bind the derived Build to this returned declaration.
    [checkpoint] covers every scan/byte comparison and preserves cancellation
    identity. *)
val prove
  :  old_capture:Authoring_source_host.Document_context.Parsed_capture.t
  -> rows:Authoring_document_selection_rows.t
  -> old:Authoring_document_selection_rows.Row.t
  -> updated:Authoring_source_host.Document_context.Parsed_capture.t
  -> current_source:Chatml_source.t
  -> script_id:string
  -> max_script_bytes:int
  -> max_locator_bytes:int
  -> max_path_components:int
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

val declaration : t -> Prompt.Chat_markdown.Declaration.t
val selected : t -> Authoring_script_selection.Selected.t
val parsed : t -> Prompt.Chat_markdown.Parsed.t
val updated_capture : t -> Authoring_source_host.Document_context.Parsed_capture.t
val host_source : t -> Authoring_source_host.Document_context.Script_capture.t option

(** Original parsed/declaration/metadata witness of the admitted row. Final
    host-policy rechecks use these exact values with current Inputs; they must
    never replace present metadata with [None] to make a selection pass. *)
val original_parsed : t -> Prompt.Chat_markdown.Parsed.t

val original_capture : t -> Authoring_source_host.Document_context.Parsed_capture.t
val original_declaration : t -> Prompt.Chat_markdown.Declaration.t

val original_metadata
  :  t
  -> Authoring_source_host.Document_context.Project_capture.t option
