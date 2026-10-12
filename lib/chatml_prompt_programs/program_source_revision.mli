open Core

(** A graph-local source identity at one exact immutable revision. Global custody
    may retain several revisions of an ID; each individual graph admits at most
    one revision per ID. This is source identity, not acquisition provenance. *)
type t [@@deriving compare, equal, sexp_of]

include Comparator.S with type t := t

val create
  :  Chatml_source.Id.t
  -> digest:Chatml_source.Digest.t
  -> byte_length:int
  -> t Or_error.t

val of_source : Chatml_source.t -> t
val id : t -> Chatml_source.Id.t
val digest : t -> Chatml_source.Digest.t
val byte_length : t -> int
val matches : t -> Chatml_source.t -> bool

(** Canonical length-framed ID/revision key; hashing polls borrowed checkpoint. *)
val key : t -> checkpoint:(unit -> unit) -> string
