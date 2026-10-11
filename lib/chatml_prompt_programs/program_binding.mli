open Core
module Input = Chatmd_parser_input

(** Actual parser-owned declaration plus its exact registration Root. Comparisons
    use canonical role/path/revision/range/body identity, never native paths. *)
type t [@@deriving compare, equal, sexp_of]

val of_declaration
  :  Prompt.Chat_markdown.Parsed.t
  -> Prompt.Chat_markdown.Declaration.t
  -> checkpoint:(unit -> unit)
  -> t Or_error.t

(** Structural reconstruction from admitted custody fields. Validates exact
    nonempty range and Root ownership; actual AST membership is checked again
    against a replayed Parsed before executable restoration. *)
val of_metadata
  :  Input.t
  -> registration_input:Input.t
  -> start_byte:int
  -> end_byte:int
  -> script_id:string
  -> script_digest:Chatml_source.Digest.t
  -> checkpoint:(unit -> unit)
  -> t Or_error.t

val declaration_path : t -> Source.Portable_path.t
val registration_input : t -> Input.t
val declaration_digest : t -> Chatml_source.Digest.t
val parser_input : t -> Input.t
val start_byte : t -> int
val end_byte : t -> int
val script_id : t -> string
val script_digest : t -> Chatml_source.Digest.t
val key : t -> string

(** Both inputs must have portable paths: comparison scans at most 1024 path
    bytes and fixed 64-byte revisions. Invalid overlong paths never compare equal. *)
val same_input : Input.t -> Input.t -> bool

val input_key : Input.t -> checkpoint:(unit -> unit) -> string

val matches_parsed
  :  t
  -> Prompt.Chat_markdown.Parsed.t
  -> checkpoint:(unit -> unit)
  -> bool

(** Return the actual replayed declaration after the same complete binding check. *)
val find_declaration
  :  t
  -> Prompt.Chat_markdown.Parsed.t
  -> checkpoint:(unit -> unit)
  -> Prompt.Chat_markdown.Declaration.t option

val same_declaration : t -> t -> bool

(** Compare an entry's exact bytes with the actual matching parser declaration. *)
val matches_entry
  :  t
  -> Prompt.Chat_markdown.Parsed.t
  -> string
  -> checkpoint:(unit -> unit)
  -> bool
