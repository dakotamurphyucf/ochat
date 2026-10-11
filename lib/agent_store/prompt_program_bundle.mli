open Core

(** Storage custody for named program metadata and private content-addressed
    payloads. No compiler, graph validation, acquisition or execution lives here.
    The semantic owner must admit metadata and reconstruct programs separately. *)
module Payload : sig
  type t

  (** Reject payloads over the existing 8 MiB source ceiling before hashing.
      The stored name is the verified SHA-256 digest, never an authored path. *)
  val create : string -> (t, Store_error.t) Result.t

  val contents : t -> string
  val sha256 : t -> string
  val byte_length : t -> int
end

module Manifest : sig
  type t [@@deriving equal, sexp_of]

  (** Validated storage inventory only. Unknown metadata remains in the embedded
      universal document; its semantic kind is checked by the owning restorer.
      Optional presence in the outer prompt manifest distinguishes no bundle. *)
  val of_json : Jsonaf.t -> (t, Document_schema.Error.t) Result.t

  val to_json : t -> Jsonaf.t

  (** Validate the complete inventory encoding before allocation, then hash all
      metadata including unknown fields and payload descriptors. Callback failures
      propagate unchanged; no outer revision or timestamp participates. *)
  val fingerprint
    :  t
    -> limits:Document_schema.Limits.t
    -> checkpoint:(unit -> unit)
    -> (string, Document_schema.Error.t) Result.t

  val shape : Document_schema.Shape.t
  val metadata : t -> Document_schema.Document.t
  val payloads : t -> (string * int) list
  val payload_bytes : t -> int
end

type t

(** Metadata is bounded to the existing 262144-byte manifest ceiling before
    manifest admission. At most 256 unique payloads totaling 8 MiB; duplicate
    digest entries reject even when equal. The enclosing artifact also charges
    authored sources against these SAME aggregate limits, and bounds the complete
    manifest including this metadata. No separate additional allowance. *)
val create
  :  metadata:Document_schema.Document.t
  -> payloads:Payload.t list
  -> (t, Store_error.t) Result.t

(** Exact inventory restoration; reject missing, surplus, duplicate and mismatched
    payloads. Payload constructors verify bytes, not supplied digest assertions. *)
val restore : Manifest.t -> Payload.t list -> (t, Store_error.t) Result.t

val manifest : t -> Manifest.t
val payloads : t -> Payload.t list
val max_payload_count : int
val max_payload_bytes : int
