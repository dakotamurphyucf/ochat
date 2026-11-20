open! Core
module P = Agent_protocol

type kind = Recovery_window_location.Source.archive_kind =
  | Summary
  | Rollover
[@@deriving equal]

(** Physical positions in one exact archived projection. [request_ordinal]
    identifies the original prepared member; it is never a source position. *)
module Position : sig
  type t

  val create
    :  request_ordinal:int
    -> effective_ordinal:int
    -> canonical_ordinal:int option
    -> (t, P.Error.t) result

  val request_ordinal : t -> int
  val effective_ordinal : t -> int
  val canonical_ordinal : t -> int option
  val equal : t -> t -> bool
end

(** Body-free relative reference within the owning session and generation of
    the preparation. The archive writer and Store establish physical custody. *)
type t

val create
  :  kind:kind
  -> operation:P.Id.Operation.t
  -> revision:int64
  -> digest:Chatml_source.Digest.t
  -> Position.t list
  -> (t, P.Error.t) result

val kind : t -> kind
val operation : t -> P.Id.Operation.t
val revision : t -> int64
val digest : t -> Chatml_source.Digest.t
val positions : t -> Position.t list
val find : ?work:Recovery_source_work.t -> t -> request_ordinal:int -> Position.t option
val equal : t -> t -> bool
val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, P.Error.t) result
val shape : Document_schema.Shape.t
