(** Shared helpers for {!Controller_insert} and {!Controller_normal}.

    This tiny module exists only to break the dependency cycle between
    the two controller implementations.  It currently exposes a single
    utility, {!line_bounds}.  Additional helpers that must be shared
    between the controllers but do **not** belong in {!Model} or other
    more general modules should also live here. *)

open Core

(** [line_bounds s pos] returns the half-open `[start_idx, end_idx)` byte
    span of the line that contains byte index [pos] inside [s].

    • [start_idx] is the index of the first byte of the line.
    • [end_idx]   is the index of the byte *after* the terminating
      newline, or [String.length s] when the line is the last one in the
      string.

    The function treats the input as a raw byte sequence – *not*
    UTF-8-aware today – mirroring the semantics of {!String.get} and
    {!String.length} from [Core].  The caller therefore must ensure that
    [pos] lies in the inclusive range [0, String.length s].

    Example — highlight the current line under an editor cursor:
    {[ let start_idx, end_idx = line_bounds buffer cursor_pos ]}

The implementation is tail-recursive and allocates no intermediate
strings. *)

let line_bounds (s : string) (pos : int) : int * int =
  let len = String.length s in
  let rec find_start i =
    if i <= 0
    then 0
    else if Char.equal (String.get s (i - 1)) '\n'
    then i
    else find_start (i - 1)
  in
  let rec find_end i =
    if i >= len
    then len
    else if Char.equal (String.get s i) '\n'
    then i
    else find_end (i + 1)
  in
  let start_idx = find_start pos in
  let end_idx = find_end pos in
  start_idx, end_idx
;;
