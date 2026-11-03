open! Core

(** Bounded, purely textual terminal suggestions. Keyword spellings are lexical;
    binding names must be supplied by the owner of authoritative accepted reads.
    Neither origin certifies current scope or the type of an unfinished draft. *)
module Item : sig
  type origin =
    | Command
    | Keyword
    | Accepted_binding
    | Session_binding
  [@@deriving equal, sexp_of]

  type t

  val label : t -> string
  val origin : t -> origin
end

module Context : sig
  type t

  val path : t -> string list
  val prefix : t -> string
  val equal : t -> t -> bool
end

(** A bounded lexical session-read site, including qualified module paths. No
    command/topic, opaque context or empty unqualified prefix is a read site. *)
val context : source:string -> cursor_byte:int -> Context.t option

type t

(** Captures exact source bytes/cursor (byte offset), with at most 16 items.
    Sources above 64 KiB, invalid cursors, opaque/unscanned lexical context,
    qualified member positions without session names and empty identifier prefixes yield [None].
    At most the first 128 supplied names are inspected, each at most 128 bytes.
    This local catalog intentionally makes no claim of complete session scope. *)
val query : source:string -> cursor_byte:int -> binding_names:string list -> t option

(** Adds server-supplied names for the exact current session/draft site. The
    caller owns revision/currentness checks; names themselves are validated as
    bounded ASCII identifiers. Session names take precedence over local names. *)
val query_with_session_names
  :  source:string
  -> cursor_byte:int
  -> binding_names:string list
  -> session_names:string list
  -> t option

val items : t -> Item.t list
val selected_index : t -> int
val selected : t -> Item.t
val next : t -> t
val previous : t -> t

(** Applies the selected item's full-token replacement only if both source bytes
    and cursor still match the captured request. Returns new source and byte
    cursor; the editor owner must validate its own byte/line/grapheme limits. *)
val accept : t -> source:string -> cursor_byte:int -> (string * int) option
