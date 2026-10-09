open! Core

(** Immutable whole terminal outcome custody. This module does not grant receipt
    authority. Construction preserves the original JSON subtree, including future
    fields and numeric lexemes, and validates Success/Failure before publication. *)
type t

module Reference : sig
  type t [@@deriving equal, compare, sexp_of]

  val digest : t -> string
  val encoded_bytes : t -> int
  val to_jsonaf : t -> Jsonaf.t
  val of_jsonaf : Jsonaf.t -> (t, Document_schema.Error.t) result
end

type value =
  | Success of Jsonaf.t
  | Failure of Agent_protocol.Error.t

(** [pending_custody] is the original raw Pending subtree, kept as a storage-only
    envelope member to avoid colliding with terminal value/error fields. The base
    outcome document remains individually16MiB/1Mfields/2Mnodes/depth256. Custody
    independently validates as a synthetic same-kind/version universal document
    under that same profile. Combined bounds are32MiB/2Mfields/4Mnodes/depth256. *)
val create : ?pending_custody:Jsonaf.t -> Jsonaf.t -> (t, Store_error.t) result

val max_encoded_bytes : int
val value : t -> value
val jsonaf : t -> Jsonaf.t
val document : t -> Document_schema.Document.t
val reference : t -> Reference.t

(** Exact validated immutable document bytes. *)
val to_string : t -> string

(** Digest/size verification precedes universal document/domain decoding. *)
val decode : Reference.t -> string -> (t, Store_error.t) result
