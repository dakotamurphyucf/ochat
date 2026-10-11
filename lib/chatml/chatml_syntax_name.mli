open Core

(** Source spelling with optional exact parser-owned occurrence. Canonical
    comparison/equality and sexp representation use spelling ONLY. Sexp decoding
    and generated/legacy construction produce unlocated names, never fake IDs.
    Resolver clients explicitly strip this metadata before runtime publication. *)
type t [@@deriving sexp, compare, equal]

val unlocated : string -> t
val located : string -> Chatml_syntax_sites.Id.t -> t
val name : t -> string
val site : t -> Chatml_syntax_sites.Id.t option
