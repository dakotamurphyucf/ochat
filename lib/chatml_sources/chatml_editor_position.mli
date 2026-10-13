open Core

(** Pure editor-coordinate projection of one immutable {!Chatml_source.t}.
    Compiler spans remain byte based and authoritative. Constructing this index
    neither changes source bytes nor decides whether ChatML accepts them. No
    filesystem, runtime, transport, UI, or execution authority is retained. *)

module Encoding : sig
  type t =
    | Utf8_bytes
    | Utf16_units
  [@@deriving compare, equal, sexp_of]
end

module Error : sig
  type t =
    | Invalid_index_limit
    | Index_limit_exceeded
    | Invalid_coordinate
    | Reversed_range
    | Line_out_of_bounds of int
    | Column_out_of_bounds of
        { line : int
        ; column : int
        }
    | Byte_offset_out_of_bounds of int
    | Invalid_utf8 of { byte_offset : int }
    | Inside_utf8_scalar of { byte_offset : int }
    | Inside_utf16_surrogate_pair of
        { line : int
        ; column : int
        }
    | Inside_crlf of { byte_offset : int }
    | Foreign_location
    | Canonical_location_unavailable
  [@@deriving compare, equal, sexp_of]
end

module Position : sig
  (** Zero-based line and column; the column's unit is chosen by the conversion
      call. A value is nonnegative but becomes source-valid only after conversion. *)
  type t [@@deriving compare, equal, sexp_of]

  val create : line:int -> column:int -> (t, Error.t) Result.t
  val line : t -> int
  val column : t -> int
end

module Range : sig
  (** Ordered half-open editor range. Endpoints use the same selected encoding.
      Source bounds and scalar boundaries are checked by [location_of_range].
      This value contains coordinates only, so a caller must bind it to the
      intended capture ID and digest before applying it to an index. *)
  type t [@@deriving compare, equal, sexp_of]

  val create : start:Position.t -> finish:Position.t -> (t, Error.t) Result.t
  val start : t -> Position.t
  val finish : t -> Position.t
end

(** Immutable on-demand index for the exact captured ID, digest and bytes.
    [create] accepts malformed UTF-8 so raw byte coordinates remain usable;
    [Utf16_units] conversion then returns [Invalid_utf8] at the first malformed
    byte anywhere in the source. It does not repair text or make compilation
    reject a source it otherwise accepts. The index shares the source's bytes.

    Construction scans at most twice in O(source bytes) and retains at most
    [max_index_entries] line descriptors plus UTF-16 checkpoints combined.
    A sparse checkpoint covers no more than 4096 source bytes; callback polling
    also occurs before a scalar or CRLF step would cross that byte limit.
    Retained words are O(index entries), with no per-character table or global
    cache. An invalid limit or exhausted entry budget returns an error without
    publishing an incomplete index. [checkpoint] is polled during input-sized
    work, and its exceptions (including cancellation) propagate unchanged; it
    is not retained. *)
type t

val create
  :  Chatml_source.t
  -> max_index_entries:int
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val source_id : t -> Chatml_source.Id.t
val source_digest : t -> Chatml_source.Digest.t
val line_count : t -> int

(** Exact retained line descriptors plus UTF-16 checkpoints. This lets a
    caller share one index-entry budget across multiple captured revisions. *)
val index_entry_count : t -> int

(** Lines start at zero. Columns exclude CR, LF and CRLF terminators. Empty
    source has one empty line; a final terminator creates a final empty line,
    whose column zero is EOF. CR and LF independently end a line except that
    CRLF is one indivisible terminator. The byte offset between its CR and LF
    is not an editor position and returns [Inside_crlf]. There is no newline
    normalization.

    [Utf8_bytes] counts exact source bytes and remains available for malformed
    source. Where a complete valid UTF-8 scalar starts in the source, offsets
    inside its byte sequence return [Inside_utf8_scalar]; malformed bytes remain
    individually addressable rather than replaced or hidden. [Utf16_units]
    requires the entire source to be valid UTF-8; supplementary scalars consume
    two code units and the position between the surrogate units returns
    [Inside_utf16_surrogate_pair]. Both directions reject out-of-bounds input.

    Queries binary-search line/checkpoint tables and scan at most 4096 bytes
    beyond a selected checkpoint; [checkpoint] exceptions propagate unchanged.
    Neither direction clamps or rounds a coordinate. *)
val byte_offset_of_position
  :  t
  -> encoding:Encoding.t
  -> Position.t
  -> checkpoint:(unit -> unit)
  -> (int, Error.t) Result.t

val position_of_byte_offset
  :  t
  -> encoding:Encoding.t
  -> byte_offset:int
  -> checkpoint:(unit -> unit)
  -> (Position.t, Error.t) Result.t

(** Convert only against this index's exact captured source. Result locations
    come from {!Chatml_source.locate}, preserving canonical byte spans. A foreign
    ID or digest returns [Foreign_location], even if contents happen to match.
    Original source locations inside a scalar or CRLF remain valid compiler
    spans but cannot be projected as editor ranges; this conversion rejects
    them. Empty ranges, including at EOF, are allowed. *)
val location_of_range
  :  t
  -> encoding:Encoding.t
  -> Range.t
  -> checkpoint:(unit -> unit)
  -> (Chatml_source.Location.t, Error.t) Result.t

val range_of_location
  :  t
  -> encoding:Encoding.t
  -> Chatml_source.Location.t
  -> checkpoint:(unit -> unit)
  -> (Range.t, Error.t) Result.t
