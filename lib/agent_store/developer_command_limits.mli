open! Core

(** Finite command-row and terminal bounds within the single developer-run
    document. This is immutable configuration, not a new work allowance. *)
type t

(** Positive bounds: at most 16,384 rows and 16 MiB of aggregate complete row
    JSON. A terminal is bounded by [max_terminal_bytes], no greater than
    [max_bytes]. The enclosing Store additionally requires [max_bytes] to fit
    its actual whole-document cap. *)
val create : max_commands:int -> max_bytes:int -> max_terminal_bytes:int -> t Or_error.t

(** 1,024 rows, aggregate bytes capped by the smaller of the document's byte
    cap and 16 MiB, terminal bytes capped by the smaller of that and 64 KiB.
    Tiny document caps may admit no command; admission never enlarges them. *)
val default : document:Document_schema.Limits.t -> t

val max_commands : t -> int
val max_bytes : t -> int
val max_terminal_bytes : t -> int
