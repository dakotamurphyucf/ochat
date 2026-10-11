open Core

(** Exact captured source data shared by preparation, compilation and diagnostics.
    All operations are in-memory. No path is opened, no source is interpreted and
    no value grants filesystem, network, process, provider or tool authority. *)

module Id : sig
  (** Package namespace plus exact normalized portable relative path. Namespace
      is a nonempty opaque byte label supplied by preparation; it is never
      interpreted as a filesystem path. No namespace UTF-8, maximum-length or
      package-name syntax is invented here. Path validation reuses the existing generated-source
      portable-path rules: at most 1024 bytes, valid UTF-8, nonempty components,
      no absolute path, '.', '..', backslash, colon, NUL or line breaks.

      Accepted input is already normalized; construction does not rewrite it,
      guess an extension, case-fold identity or consult cwd. Equality is the pair
      of exact strings. Inventory admission separately rejects ASCII-case-colliding
      and file/directory-conflicting paths within one package. This reuses the
      existing collision policy, not a Unicode normalization or host-filesystem
      equivalence claim. *)
  type t [@@deriving compare, equal, hash, sexp_of]

  include Comparator.S with type t := t

  val create : package_namespace:string -> relative_path:string -> t Or_error.t
  val package_namespace : t -> string
  val relative_path : t -> string
end

module Digest : sig
  (** SHA-256, encoded as exactly 64 lowercase hexadecimal characters. This is
      content identity, never provenance authentication or execution authority.
      Existing Source_ref/artifact digest strings enter through [of_hex]. *)
  type t [@@deriving equal, sexp_of]

  val of_hex : string -> t Or_error.t
  val to_hex : t -> string
  val of_contents : string -> checkpoint:(unit -> unit) -> t
end

module Location : sig
  (** A validated half-open byte range tied to one source identity AND exact
      content revision. Only [locate]/[of_span] construct it. Empty ranges,
      including EOF, are legal. Coordinates are byte based, never UTF-16 or
      Unicode character indices. No arbitrary public record admits stale or
      inconsistent line/column metadata. *)
  type t [@@deriving sexp_of]

  val source : t -> Id.t
  val source_digest : t -> Digest.t
  val span : t -> Source.span
end

(** Immutable exact bytes and their independently computed digest. Strings are
    retained as immutable OCaml values; contents are not trimmed, UTF-8 repaired,
    newline-normalized, parsed or preprocessed. Code/string byte semantics remain
    compatible with existing single-file compilation. *)
type t

(** Checks [max_bytes] before hashing; [None] omits this host-selected byte ceiling,
    while [Some n] requires positive n. Hash in bounded chunks, polling checkpoint
    between chunks. [expected_digest], when supplied by admitted host metadata,
    must equal the computed digest. It is not trusted without recomputation.

    Checkpoint exceptions, including cancellation, propagate unchanged. A failure
    or cancellation returns/publishes no partially constructed source. No global
    digest/source cache is mutated. This constructor cannot enforce aggregate
    limits across separately created sources; the capture owner and subsequent
    Prepared_program admission enforce those limits. *)
val create
  :  Id.t
  -> contents:string
  -> expected_digest:Digest.t option
  -> max_bytes:int option
  -> checkpoint:(unit -> unit)
  -> t Or_error.t

val id : t -> Id.t
val contents : t -> string
val byte_length : t -> int
val digest : t -> Digest.t

(** Validate offsets against these exact bytes and compute canonical Source.span
    positions. Lines are 1 based, columns are 0 based byte offsets within a line;
    LF, CR and CRLF are line breaks, with CRLF counted once as in the ChatML lexer.
    Between CR and its following LF, the position remains on the preceding line;
    the break is complete after LF. Every byte offset is admitted.
    The source index is immutable. A scan/index build polls checkpoint and is
    invocation-local; it cannot retain an Eio control or mutable global cache. *)
val locate
  :  t
  -> start_byte:int
  -> end_byte:int
  -> checkpoint:(unit -> unit)
  -> Location.t Or_error.t

(** Adapter for existing parser/checker spans. Validates byte bounds and canonical
    line/column metadata against the source rather than trusting Source.span's
    unrestricted record construction. No substring-clamping admission. *)
val of_span : t -> Source.span -> checkpoint:(unit -> unit) -> Location.t Or_error.t

(** Exact selected bytes; a stale identity/revision or invalid range returns an
    error instead of clipping the span or looking up live files. *)
val read : t -> Location.t -> string Or_error.t
