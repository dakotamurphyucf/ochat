(** Pure helper functions used by the Chat-TUI implementation.  These are
    entirely side-effect free and therefore easy to unit-test and reuse from
    the upcoming refactored modules. *)

(** [sanitize ?strip s] replaces all control characters (except for newline)
    with spaces and expands TAB characters to four spaces.  When [strip] is
    [true] (default) leading and trailing whitespace of the resulting string
    is removed. *)
val sanitize : ?strip:bool -> string -> string

(** [truncate ?max_len s] shortens [s] to at most [max_len] (default 300)
    characters and appends an ellipsis if it was longer.  Leading and trailing
    whitespace is trimmed first. *)
val truncate : ?max_len:int -> string -> string

(** [wrap_line ~limit s] splits the UTF-8 encoded string [s] into a list of
    slices whose {e byte length} does not exceed [limit].  A split is never
    performed in the middle of a multi-byte UTF-8 scalar value, guaranteeing
    that every item in the returned list is valid UTF-8. *)
val wrap_line : limit:int -> string -> string list

