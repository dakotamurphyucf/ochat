(** Private immutable Goal book DATA; the carrier preserves unknown children. *)
module P = Agent_protocol

module PC = Policy_canonical_usage
module Receipt = Goal_policy_receipt
module Pending = Receipt.Pending

module History : sig
  type t =
    | Known_from_creation
    | Historical_unknown
  [@@deriving equal, sexp_of]
end

module Last : sig
  type t =
    | Confirmed of Pending.t
    | No_writer_closed of Pending.t

  val pending : t -> Pending.t
  val equal : t -> t -> bool
  val shape : Document_schema.Shape.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
end

module Slot : sig
  type t =
    { stream : PC.Stream.t
    ; confirmed_prefix : int64
    ; last_nonce : int64
    ; pending : Pending.t option
    ; last : Last.t option
    ; projection : PC.Projection.t option
    }

  val validate
    :  t
    -> owner:P.Session_ref.t
    -> goal_id:P.Id.Goal.t
    -> (unit, P.Error.t) result

  val equal : t -> t -> bool
  val shape : Document_schema.Shape.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
end

module Book : sig
  type t =
    { goal_id : P.Id.Goal.t
    ; history : History.t
    ; slots : Slot.t list
    }

  val equal : t -> t -> bool
  val shape : Document_schema.Shape.t
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, P.Error.t) result
end

type t =
  { owner : P.Session_ref.t
  ; books : Book.t list
  }

val limits : Document_schema.Limits.t
val slot_limits : Document_schema.Limits.t
val max_books : int
val max_streams : int
val equal : t -> t -> bool
val validate : t -> (unit, P.Error.t) result
val shape : Document_schema.Shape.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, P.Error.t) result
val find_book : t -> P.Id.Goal.t -> Book.t option
val find_slot : t -> string -> (Book.t * Slot.t) option
val set_book : t -> Book.t -> t
val set_slot : t -> goal_id:P.Id.Goal.t -> Slot.t -> (t, P.Error.t) result
val validate_original_children : Jsonaf.t -> (unit, P.Error.t) result
