(** Immutable exact metadata publications selected before private intent effects.
    Restart admission preserves original bytes; cleanup prefixes use these bytes
    rather than converted/reencoded documents. No I/O or content bytes here. *)
open! Core

type t

val create : Blob_metadata_document.t -> (t, Document_schema.Error.t) Result.t

val of_publications
  :  temporary_bytes:string
  -> durable_bytes:string
  -> (t, Document_schema.Error.t) Result.t

val temporary : t -> Blob_metadata_document.t
val durable : t -> Blob_metadata_document.t
val temporary_bytes : t -> string
val durable_bytes : t -> string
val equal : t -> t -> bool

(** A stage's temporary and durable publications have the same explicit owner
    target and exact original document fields; only [durable] differs. The
    released conversation version-one stage encoding is unchanged. *)
