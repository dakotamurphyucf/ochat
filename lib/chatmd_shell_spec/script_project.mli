open Core

(** Authored selection metadata, not a resolved project or native authority.
    Native request admission validates paths, environment membership and grants.
    Derived wire decoding alone is not that admission. *)
type t =
  { project : string
  ; environment : string option [@jsonaf.option]
  }
[@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

(** Reject blank or NUL-containing names. Relative project paths belong to the
    actual declaring ChatMD input, independently of script [src]. No path lookup,
    normalization or environment discovery occurs here. *)
val create : project:string -> environment:string option -> t Or_error.t
