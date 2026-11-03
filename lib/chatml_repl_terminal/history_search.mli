open! Core

(** Pure reverse search over a captured, newest-first local history. Creating,
    editing or selecting a match never evaluates or submits it. *)
type t

(** Retains at most 1024 entries and 8 MiB total, stopping before the first entry
    that exceeds that bound. Borrowed history strings remain immutable. *)
val create : history:string list -> t

val query : t -> string
val matched_source : t -> string option
val match_index : t -> int option

(** Query text is literal, case-sensitive and limited to 256 UTF-8 bytes.
    Invalid UTF-8 or an oversized edit is rejected without accepting a prefix.
    An edit restarts selection from the newest matching entry. *)
val insert : t -> string -> t Or_error.t

(** Removes one UTF-8 grapheme cluster from the query. *)
val backspace : t -> t

(** Selects an older matching entry, wrapping to the newest match. *)
val next : t -> t
