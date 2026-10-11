open! Core

(** Immutable ChatMD parser input, shared with source-custody consumers.
    There is no serializer, source loader, provider or native capability here. *)

module Role : sig
  type t =
    | Root
    | Import
  [@@deriving compare, equal, sexp_of]
end

module Error : sig
  type t =
    | Empty_path
    | Input_byte_limit
    | Import_changed
    | Invalid_range
  [@@deriving sexp_of]
end

type t
type input = t

(** Own supplied immutable strings without copying. [path] is the source loader's
    logical path, NOT a native authority or a promise of portable capture.
    Artifact admission additionally validates its portable path and1024-byte cap.
    Root accepts
    effective bytes actually supplied by the preparation owner; construction
    cannot prove that a provider/preprocessor produced them. Import REQUIRES
    exact equality with authored bytes before hashing. Bounds are per input;
    the capture owner additionally charges the whole authored/private inventory.
    Exact-equal effective bytes reuse the authored string after verification.
    Checkpoints during bounded equality/hash/index work propagate unchanged. *)
val create
  :  role:Role.t
  -> path:string
  -> authored:string
  -> effective:string
  -> max_input_bytes:int
  -> checkpoint:(unit -> unit)
  -> (t, Error.t) Result.t

val role : t -> Role.t
val path : t -> string
val authored : t -> string
val effective : t -> string
val authored_sha256 : t -> string
val effective_sha256 : t -> string
val authored_byte_length : t -> int
val effective_byte_length : t -> int

(** Stable parser-input v1 identity: length-prefixed role, exact logical path,
    authored/effective SHA256 and byte lengths. Matches persisted custody keys.
    This hashes metadata, not a native locator; callers separately admit path
    size/portable policy. Input-sized fields poll in4096-byte batches; callback
    exceptions propagate unchanged. *)
val identity_digest : t -> checkpoint:(unit -> unit) -> string

(** Bounded exact authored-byte comparison for replay/admission. *)
val authored_matches : t -> string -> checkpoint:(unit -> unit) -> bool

val effective_matches : t -> string -> checkpoint:(unit -> unit) -> bool

module Range : sig
  (** Exact nonempty half-open BYTE endpoints in this input's effective string.
      No claim of a contiguous extracted body; RAW markup and serialization may
      change the body. A range belongs to its input owner, not an arbitrary file. *)
  type t

  val create
    :  input
    -> start_byte:int
    -> end_byte:int
    -> checkpoint:(unit -> unit)
    -> (t, Error.t) Result.t

  val input : t -> input
  val start_byte : t -> int
  val end_byte : t -> int
  val start_position : t -> Chatmd_shell_spec.Source_ref.position
  val end_position : t -> Chatmd_shell_spec.Source_ref.position
end

(** Position index scans effective bytes once using the existing captured-source
    block-index mechanics. Lines and byte columns are one-based for Source_ref
    display (Source.position's column plus one). LF and bare CR advance line;
    CRLF advances once. UTF-8 advances byte columns, never scalar/UTF-16 columns.
    Two private integer arrays retain one slot per4096 bytes plus initial slot;
    checkpointed position lookup scans at most one4096-byte block. *)
