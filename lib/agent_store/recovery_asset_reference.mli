(** Private identity of an exact original asset in a session's recovery history.
    Owning a reference alone grants no read permission. The caller obtains it
    from the committed member part, and rechecks that selection and the
    original actor's authority before releasing any result. *)
open! Core

module P = Agent_protocol

type t

type error =
  | Corrupt
  | Unsupported
  | Limit
[@@deriving equal, sexp_of]

val create
  :  P.Recovery_window.Citation.t
  -> part:int
  -> blob:P.Blob.Metadata.t
  -> (t, error) result

val equal : t -> t -> bool
val citation : t -> P.Recovery_window.Citation.t
val part : t -> int
val blob : t -> P.Blob.Metadata.t
val allowed_use : t -> string
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, error) result
