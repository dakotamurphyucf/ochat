(** Source code helper utilities.

    This module models a *source document* (typically the contents of a file)
    together with convenient helpers to query individual characters or
    extract substrings based on *spans* – explicit left/right positions inside
    the text.  It is designed to be the minimal dependency for lexers, parsers
    and type-checkers that need to keep track of locations for error reporting.

    A span is represented by a pair of absolute positions ([left] and [right])
    expressed in **bytes** counted from the start of the document.  A
    [position] also stores the corresponding (1-based) line and column numbers
    so that clients can render human-friendly messages.  Invariants:

    • [offset] is in the range [0, Source.length src) where [src] is the parent
      document.
    • For any span [sp], [sp.left.offset <= sp.right.offset].  The
      implementation does *not* enforce these invariants at construction time;
      it is the responsibility of the caller to supply consistent data.

    All helpers are **pure** – they never mutate the underlying string – and
    run in *O(1)* or *O(length)* where length is the size of the returned
    fragment. *)

(** {1 Types} *)

(** {1 Types} *)

(** A complete source document. *)
type t = private
  { path : string option
    (** Absolute or relative path of the originating
                                file if available.  [None] for in-memory
                                sources created with {!make}. *)
  ; content : string (** Full contents of the document. *)
  }
[@@deriving sexp]

(** Absolute position inside a source document. *)
and position =
  { line : int (** 1-based line number (\>= 1). *)
  ; column : int
    (** 0-based column within [line].  Tab expansion is caller
                      defined – this module does not normalise tabs. *)
  ; offset : int (** Absolute byte offset from the start of [content]. *)
  }
[@@deriving sexp]

(** [left, right) half-open interval inside a document. *)
and span =
  { left : position (** Inclusive left bound. *)
  ; right : position (** Exclusive right bound. *)
  }
[@@deriving sexp]

(** {1 Constructors} *)

(** [make contents] builds an in-memory source document with the given
    [contents].  [path] is set to [None].

    Example creating a source from a string:
    {[
      let src = Source.make "hello" in
      Source.length src = 5
    ]} *)
val make : string -> t

(** [from_file filename] reads the whole file [filename] into memory and
    returns a corresponding source document.  The [path] field of the result
    is [Some filename].  The file is read with
    {!Stdlib.In_channel.open_text}, hence it honours the current locale’s
    newline conversion rules.

    @raise Sys_error if the file cannot be opened or read. *)
val from_file : string -> t

(** {1 Accessors} *)

(** Total number of characters (bytes) in the document.  Runs in O(1). *)
val length : t -> int

(** [at src offset] returns [Some c] where [c] is the character at absolute
    [offset] inside [src], or [None] if [offset] is out of bounds.

    Equivalent to
    {[
      if 0 <= offset && offset < Source.length src then
        Some src.content.[offset]
      else None
    ]}
    but without raising [Invalid_argument].  Runs in O(1). *)
val at : t -> int -> char option

(** [read src span] extracts the substring designated by [span] from [src].

    The function is forgiving: if [span] extends outside the document it is
    automatically clamped to {[0, Source.length src)}.  Therefore it never
    raises beyond the usual out-of-memory errors.

    Complexity is O(n) where n is the length of the returned substring. *)
val read : t -> span -> string

(** {1 Span helpers} *)

(** [merge sp1 sp2] returns the smallest span that contains both [sp1] and
    [sp2].  Its left bound is [sp1.left] and its right bound is [sp2.right].
    The caller must guarantee that [sp1] lies entirely *before* [sp2]; the
    behaviour is undefined otherwise. *)
val merge : span -> span -> span
