(** Pure helper helpers for the text-based user-interface.

    All functions in this module are:

    • Fully {e side-effect free} – they do not mutate global state, perform I/O
      or rely on wall-clock time.
    • Small, single-purpose utilities that are shared by various Chat-TUI
      sub-modules.

    The helpers focus on preparing arbitrary user-supplied strings for safe
    terminal output:   sanitising control characters, truncating overly long
    messages and wrapping UTF-8 input into slices that respect a byte budget.

    {1 Functions}
*)

(** [sanitize ?strip s] returns [s] with every ASCII control character – all
    bytes in the ranges [[\x00–\x1F]] and [[\x7F]] – replaced by a space.

    Special-cases:
    • TAB characters are expanded to four spaces.
    • NEWLINE ([\n]) is preserved so the caller can keep intentional line
      breaks.

    When [?strip] (defaults to [true]) the result is additionally trimmed with
    {!Core.String.strip}.  In particular, leading and trailing newlines that
    survived the replacement step are removed.

    @param strip whether to apply {!Core.String.strip} (default [true])

    Sanitising a string that contains a bell ([\007]) and a tab:
    {[ let clean = Chat_tui.Util.sanitize "be\007\tboop" in
       assert (String.equal clean "be    boop") ]}
*)
val sanitize : ?strip:bool -> string -> string

(** [truncate ?max_len s] cuts [s] to at most [max_len] bytes.  If the limit is
    hit, the Unicode ellipsis character ("…", U+2026) is appended so the caller
    can tell that data was lost.

    The function always calls {!Core.String.strip} first so whitespace at the
    boundaries does not count towards the budget.

    @param max_len maximum number of bytes to preserve from [s] (default 300)

    Keeping only the first 5 visible bytes:
    {[
      let s = Chat_tui.Util.truncate ~max_len:5 "  abcdefg" in
      assert (String.equal s "abcde…")
    ]}
*)
val truncate : ?max_len:int -> string -> string

(** [wrap_line ~limit s] splits [s] into chunks with a maximum {e byte} length
    of [limit].  Splits {b never} occur in the middle of a multi-byte UTF-8
    scalar value; every element of the returned list is therefore valid UTF-8
    in isolation.

    The byte budget is a pragmatic choice: we use it because the downstream
    rendering layer (Notty text widgets) counts one code-point per byte and we
    are primarily interested in avoiding very large allocations when dealing
    with streamed assistant responses.

    The function does {i not} try to honour grapheme clusters nor display-width
    (East-Asian wide glyphs are still counted as one unit) – callers that need
    a more sophisticated layout strategy must post-process the result.

    @raise Failure never – malformed UTF-8 is tolerated; bytes that do not fit
           the official prefix patterns are treated as single-byte code-points
           to guarantee progress.

    Splitting a multi-byte string into 4-byte slices:
    {[
      let parts = Chat_tui.Util.wrap_line ~limit:4 "👍👍👍" in
      (* Each 👍 is four bytes in UTF-8 *)
      assert (parts = ["👍"; "👍"; "👍"])
    ]}
*)
val wrap_line : limit:int -> string -> string list
