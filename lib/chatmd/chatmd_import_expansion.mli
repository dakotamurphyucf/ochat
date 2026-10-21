open! Core

(** Recursive ChatMD import expansion with source provenance. *)

type sourced_node =
  { node : Chatmd_ast.node
  ; source : Chatmd_shell_spec.Source_ref.t
  ; source_node : Source_loader.source
  ; range : Chatmd_parser_input.Range.t option
  ; text_fragments : Chatmd_parser_builder.Text_fragment.t list
  ; input : Chatmd_parser_input.t option
  ; children : sourced_node list option
  }

(** Legacy attribute accepted when reading previously serialized messages. *)
val source_attribute : string

(** [expand ~parse ~dir ~file ~source document] recursively replaces import
    elements with their parsed contents. Imported declarations retain their
    source file, source directory, digest, and optional namespace. Child
    provenance survives inline imports in [children]; None inherits the parent.
    With [canonical_sources], file names are normalized root-relative paths. *)
val expand
  :  ?canonical_sources:bool
  -> parse:(string -> Chatmd_ast.document)
  -> loader:Source_loader.t
  -> root_source:Source_loader.source
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> file:string
  -> source:string
  -> Chatmd_ast.document
  -> sourced_node list

(** Same expansion/context policy for nodes built from their exact parser input.
    Imported reads and parsing are provided by the invocation's bounded owner;
    parent reconstruction preserves its range, imported nodes their own ranges. *)
val expand_located
  :  checkpoint:(unit -> unit)
  -> check_node:(depth:int -> Chatmd_ast.node -> unit)
  -> canonical_sources:bool
  -> read_import:(Source_loader.t -> Source_loader.source -> (string, string) Result.t)
  -> parse:(Source_loader.source -> string -> Chatmd_located_ast.t list)
  -> loader:Source_loader.t
  -> root_source:Source_loader.source
  -> dir:Eio.Fs.dir_ty Eio.Path.t
  -> file:string
  -> source:string
  -> Chatmd_located_ast.t list
  -> sourced_node list
