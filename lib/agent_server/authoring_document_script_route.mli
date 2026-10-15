open! Core

(** Pure bounded binding of one genuine generated ChatMD script declaration to
    its exact captured compiler entry. This module has no source host, root,
    native path, actor, parser loader or project preparation authority. The
    caller supplies only one already admitted Parsed receipt and its exact
    captured source associations; {!Authoring_source_host.Document_context}
    owns physical root/actor checks and the later joined native preparation. *)

module Error : sig
  type t =
    | Foreign_declaration
    | Invalid_project_route
    | Missing_external_source
    | Conflicting_external_source
    | Limit
  [@@deriving sexp_of]
end

type t

(** First prove actual declaration membership and declaring-input source bytes
    using the existing {!Authoring_project_declaration.admit} route. Its
    resolved project member must equal [project_member] exactly. Inline script
    bytes come from [declaration] and receive a collision-checked bounded
    logical fragment ID derived from [root_source]'s actual captured identity,
    not a caller-chosen namespace. For [script_path], resolve relative to the genuine
    declaring parser input using {!Source_loader.resolve_member}; look up its
    exact bytes in [bundle] and compare with [Declaration.script_body] before
    admitting a captured file. Compute a project-relative [root_member] only
    when the external member is proven inside the same selected project root;
    otherwise pass [None], never an inferred file route. No body, member or
    project path is normalized into a different meaning. Every list visit and
    byte comparison calls [checkpoint], whose finite allowance is owned by the
    joined source-host operation. Reject before copying a body above
    [max_script_bytes]. A generated ID is comparison data only, not authority. *)
val admit
  :  parsed:Prompt.Chat_markdown.Generated.receipt
  -> input_sources:(Chatmd_parser_input.t * Chatml_source.t) list
  -> bundle:Chatmd_source_bundle.t
  -> declaration:Prompt.Chat_markdown.Declaration.t
  -> project_member:Source.Portable_path.t
  -> root_source:Chatml_source.t
  -> max_script_bytes:int
  -> max_locator_bytes:int
  -> max_path_components:int
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) result

val source : t -> Chatml_source.t
val entry : t -> Chatml_project_prepare.Request.Entry.t
val environment : t -> string option
val project_member : t -> Source.Portable_path.t
