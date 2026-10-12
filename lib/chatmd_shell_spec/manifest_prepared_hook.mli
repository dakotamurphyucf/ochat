open Core

(** Pure validated projection. Publicly exposed as Manifest.Prepared_hook. *)
type t [@@deriving sexp, compare, equal, hash, bin_io, jsonaf]

val create
  :  id:string
  -> kind:Chatmd_script_spec.kind
  -> entrypoint:string
  -> program_fingerprint:string
  -> checkpoint:(unit -> unit)
  -> t Or_error.t

val id : t -> string
val kind : t -> Chatmd_script_spec.kind
val entrypoint : t -> string
val program_fingerprint : t -> string

(** Shared fixed-width digest validation; no input-sized traversal. *)
val valid_digest : string -> bool
