(** Optional byte adapter for two terminal Shift+Enter encodings. Recognized
    keys become Meta+Enter for Notty's existing decoder. Bracketed paste is
    never rewritten. Unrecognized bytes pass through unchanged. *)
type t

val create : unit -> t
val feed : t -> string -> string

(** Whether a standalone Escape outside bracketed paste awaits the caller's
    short input timeout. Longer recognized prefixes, and every prefix inside
    paste, remain buffered across arbitrary delays (at most nine bytes).
    An initial Escape followed by a delayed sequence after this timeout is
    inherently ambiguous with a separate Escape key. *)
val escape_pending : t -> bool

(** Emit and consume only a standalone Escape outside paste; otherwise return
    the empty string without changing buffered bytes or paste state. *)
val flush_escape : t -> string
