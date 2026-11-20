open! Core

(** A canonical version names an occurrence revision. An effective version also
    identifies how its immutable neutral projection was obtained. Its digest
    covers that member's projection; it does not attest to provider submission. *)
type origin =
  | Unchanged of History.Content_revision.t
  | Inserted of
      { incarnation : int64
      ; change_id : int
      }
  | Replaced of
      { incarnation : int64
      ; change_id : int
      ; base : History.Content_revision.t
      }
  | Captured of { session_revision : int64 }
[@@deriving compare, equal, sexp_of]

type t = private
  | Canonical of History.Content_revision.t
  | Effective of
      { origin : origin
      ; digest : Chatml_source.Digest.t
      }
[@@deriving compare, equal, sexp_of]

val canonical : History.Content_revision.t -> t

(** [incarnation] is the committed session revision establishing the current
    overlay namespace. A legacy projection lacking change provenance must use
    [Captured] with its exact source revision. *)
val effective
  :  origin:origin
  -> digest:Chatml_source.Digest.t
  -> (t, Protocol_error.t) result

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Protocol_error.t) result
