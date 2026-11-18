open! Core

(** Immutable private request text at fixed User priority. This value grants no
    source authority or tool and does not create history/transcript identities.
    Authored System/Developer instructions remain separate. *)
type t

(** Exact UTF-8 text, nonempty and at most 512 KiB, plus a nonblank private
    provenance label of at most 1024 UTF-8 bytes. The complete escaped admission
    representation must satisfy [limits]. Provenance is captured host evidence,
    never a credential, permission proof or public observation field. *)
val create
  :  text:string
  -> provenance:string
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) Result.t

val text : t -> string
val provenance : t -> string
val equal : t -> t -> bool

(** Private admission JSON, including the fixed User role and exact provenance.
    This is not a public configuration or provider wire projection. *)
val to_json : t -> Jsonaf.t
