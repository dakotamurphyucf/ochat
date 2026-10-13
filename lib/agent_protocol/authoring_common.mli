(** Versioned, transport-neutral authoring DTOs. All identities and source sites
    are comparison coordinates, never read, write, analysis or execution grants.
    The host reauthorizes the authenticated actor on every method, including
    cached reads. Codecs validate bounded wire values before domain admission.
    Every optional field in this family uses absence for [None] and a concrete
    value for [Some]; explicit JSON null is invalid, including response fields.
    Unknown and duplicate object fields are invalid. Integers must be exact
    bounded JSON integers, never floats or exponent notation.
    Method failures use the existing typed protocol error channel:
    [Permission_denied] for unauthorized access, [Lease_stale] for a lost
    context lease, [Conflict] for stale source/context revisions,
    [Invalid_request] with a bounded position-reason tag for invalid editor
    coordinates, [Resource_limit] for work/admission ceilings and [Interrupted]
    for cancellation. These wire codes do not reveal whether a denied source
    exists. Later semantic queries return unavailable facts as typed results
    rather than protocol errors.
    This module has no compiler, Eio, filesystem, runtime or UI dependency. *)

module Features : sig
  (** Initialization advertises [core] only after a host installs the
      source/context handlers. Codecs and {!Command.supported_methods} alone
      do not make any method available. Bundle and later analysis features
      are selected separately only after their handlers are installed. *)
  val core : string

  (** Separately negotiated immutable upload access. An upload-only principal
      need not select [core] or open a source context. *)
  val bundle : string
end

module Encoding : sig
  type t = Chatml_editor_position.Encoding.t =
    | Utf8_bytes
    | Utf16_units
  [@@deriving compare, equal, sexp_of]

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Method : sig
  type t =
    | Capabilities
    | Roots
    | Host_list
    | Host_read
    | Context_open
    | Context_refresh
    | Context_close
    | Source_list
    | Source_read
    | Document_version
    | Document_replace
    | Document_close
    | Bundle_begin
    | Bundle_chunk
    | Bundle_commit
    | Bundle_status
  [@@deriving compare, equal, sexp_of]

  (** Closed stable [authoring.*] method names. A method's presence in this
      enumeration or the decoder does not advertise an installed handler. *)
  val to_string : t -> string

  val of_string : string -> (t, Error.t) Result.t
end

module Failure : sig
  (** Bounded authoring-specific reasons in [Protocol_error.data]. The ordinary
      protocol code remains authoritative; malformed or mismatched detail
      decodes as [Other] instead of changing that code's meaning. No path,
      source existence, supplied bytes or private host policy is exposed. *)
  type invalid_position =
    | Invalid_utf8
    | Out_of_bounds
    | Inside_scalar
    | Inside_surrogate
    | Inside_crlf
    | Foreign_source
  [@@deriving compare, equal, sexp_of]

  type reason =
    | Unauthorized
    | Context_lease_stale
    | Stale_context
    | Stale_source
    | Invalid_position of invalid_position
    | Source_unavailable
    | Environment_unavailable
    | Work_limit
    | Cancelled
    | Other
  [@@deriving compare, equal, sexp_of]

  (** [attach] admits only reason/code pairs defined by this protocol family.
      [classify] never string-matches messages. Server code may create an
      ordinary error when no more specific safe detail applies. *)
  val attach : Error.t -> reason -> (Error.t, Error.t) Result.t

  val classify : Error.t -> reason
end

module Limits : sig
  (** Largest exact JSON counter admitted for generations and editor revisions. *)
  val maximum_wire_integer : int

  (** Actual per-host method limits, not claims about OCaml heap or wall time.
      [max_request_bytes] is also bounded by the generic initialize limit.
      Constructors check positivity, representable integer bounds and local
      cross-field consistency. They do not prove a host can honor a limit;
      the handler selects ceilings at initialization and checks each request
      against them and its current principal quota before allocation. *)
  type t = private
    { max_sources : int
    ; max_source_bytes : int
    ; max_context_source_bytes : int
    ; max_request_bytes : int
    ; max_result_bytes : int
    ; max_path_candidates : int
    }
  [@@deriving sexp_of]

  val create
    :  max_sources:int
    -> max_source_bytes:int
    -> max_context_source_bytes:int
    -> max_request_bytes:int
    -> max_result_bytes:int
    -> max_path_candidates:int
    -> (t, Error.t) Result.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Capabilities : sig
  (** Actual installed handler methods, selected position encodings and policy
      bounds. [encodings] is empty for an upload-only principal with no source
      position service; it never advertises an unavailable editor conversion.
      A codec may know a method that is absent here. [authoring.capabilities]
      requires selected [Features.core] or [Features.bundle], according to the
      installed handler and authenticated principal. *)
  type t = private
    { methods : string list
    ; encodings : Encoding.t list
    ; limits : Limits.t
    }
  [@@deriving sexp_of]

  val create
    :  methods:string list
    -> encodings:Encoding.t list
    -> limits:Limits.t
    -> (t, Error.t) Result.t

  (** Unknown future [authoring.*] method names are retained but do not make
      an older client callable; [supports] tests only its typed known methods. *)
  val supports : t -> Method.t -> bool

  val supports_encoding : t -> Encoding.t -> bool
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Source : sig
  (** Exact captured source revision. Optional [editor_revision] is absent for
      host/bundle base bytes and present as a positive monotonically increasing
      client revision for an open overlay. Wire [null] is invalid: absence and
      a value are the only admitted states. Byte length is nonnegative and
      digest is SHA-256; neither claims that the bytes are currently readable. *)
  type t [@@deriving equal, sexp_of]

  val create
    :  id:Chatml_source.Id.t
    -> digest:Chatml_source.Digest.t
    -> byte_length:int
    -> editor_revision:int option
    -> (t, Error.t) Result.t

  val id : t -> Chatml_source.Id.t
  val digest : t -> Chatml_source.Digest.t
  val byte_length : t -> int
  val editor_revision : t -> int option
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Position : sig
  (** Zero-based coordinates. Construction rejects negatives; applying a
      position to a source additionally validates CRLF, scalar, surrogate and
      EOF boundaries with {!Chatml_editor_position}. *)
  type t = Chatml_editor_position.Position.t

  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end

module Site : sig
  (** Exact source owner plus ordered half-open editor coordinates in one
      explicit encoding. A site is accepted for a query only after the service
      matches its source ID, digest, byte length and editor revision to the
      current context and projects through the captured position index. An
      [Approximate] site denotes declaration-level attribution, never an exact
      embedded byte map; OCH-259 owns that provenance projection. *)
  type precision =
    | Exact
    | Approximate
  [@@deriving compare, equal, sexp_of]

  type t [@@deriving sexp_of]

  val create
    :  source:Source.t
    -> encoding:Encoding.t
    -> start:Position.t
    -> finish:Position.t
    -> precision:precision
    -> (t, Error.t) Result.t

  val source : t -> Source.t
  val encoding : t -> Encoding.t
  val start : t -> Position.t
  val finish : t -> Position.t
  val precision : t -> precision
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Error.t) Result.t
end
