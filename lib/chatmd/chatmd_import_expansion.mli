open! Core

(** Recursive ChatMD import expansion with source provenance. *)

type sourced_node =
  { node : Chatmd_ast.node
  ; source : Chatmd_shell_spec.Source_ref.t
  ; source_node : Source_loader.source
  }

(** Legacy attribute accepted when reading previously serialized messages. *)
val source_attribute : string

(** [expand ~parse ~dir ~file ~source document] recursively replaces import
    elements with their parsed contents. Imported declarations retain their
    source file, source directory, digest, and optional namespace. *)
val expand
  :  parse:(string -> Chatmd_ast.document)
  -> loader:Source_loader.t
  -> root_source:Source_loader.source
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> file:string
  -> source:string
  -> Chatmd_ast.document
  -> sourced_node list
