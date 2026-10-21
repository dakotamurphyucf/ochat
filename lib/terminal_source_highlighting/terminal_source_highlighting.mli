open Core

(** Bounded ChatML/ChatMD terminal source styling. Consume the exact lexical
    snapshot rather than invoking TextMate/Oniguruma on source text. The
    snapshot's revision and work limits are preserved; this module merely maps
    its lexical roles to the shared terminal theme and line spans. It has no
    parser, source loading, execution or global registry. *)

module Limits : sig
  type t

  (** Positive output ceilings checked before allocating per-line spans. An
      individual line over [max_line_bytes] or a document over [max_lines] is
      unavailable to this renderer, including a single long malformed line. *)
  val create : max_lines:int -> max_line_bytes:int -> t Or_error.t
end

module Error : sig
  type t =
    | Too_many_lines
    | Line_too_long of int (** zero-based line index *)
  [@@deriving compare, equal, sexp_of]
end

type span = Terminal_highlighting.Highlight_tm_engine.span
type t

val snapshot : t -> Language_highlighting.Highlight_snapshot.t
val lines : t -> span list list

(** [render] walks exact source bytes and ordered lexical ranges once, splitting
    at CR, LF and CRLF without modifying them. Lines omit delimiters and all
    nonempty fragments reconstruct each line exactly. The theme owns colors;
    no second palette or semantic inference is used. [checkpoint] is called
    during input-sized work; its exception, including cancellation, propagates.
    Output may contain non-newline control bytes; the frontend must sanitize
    before Notty rendering. A truncated snapshot styles only its retained
    prefix; its status remains available through [snapshot]. *)
val render
  :  Language_highlighting.Highlight_snapshot.t
  -> theme:Terminal_highlighting.Highlight_theme.t
  -> limits:Limits.t
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t
