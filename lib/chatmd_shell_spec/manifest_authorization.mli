open Core

(** Validated immutable execution-v2 projection; private construction helper. *)
type t [@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

val create
  :  resource_sha256:string
  -> Manifest_prepared_hook.t list
  -> checkpoint:(unit -> unit)
  -> t Or_error.t

val prepared_hooks : t -> Manifest_prepared_hook.t list
val resource_sha256 : t -> string
val compare_string : string -> string -> checkpoint:(unit -> unit) -> int

(** Exact checked encoding length, before indexes/sorted copies or output.
    Order does not affect the encoded byte count. *)
val canonical_size
  :  Manifest_prepared_hook.t list
  -> resource_json:string
  -> max_bytes:int
  -> checkpoint:(unit -> unit)
  -> int Or_error.t

(** Encode the same immutable facts/resource with the exact already-admitted
    length returned by canonical_size; private caller invariant. Chunked copy,
    escaping and hash; output mutation never escapes. *)
val encode
  :  t
  -> resource_json:string
  -> canonical_bytes:int
  -> checkpoint:(unit -> unit)
  -> string * string
