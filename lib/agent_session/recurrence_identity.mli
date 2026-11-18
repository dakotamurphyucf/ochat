(** Validated controller-qualified recurrence coordinates. These immutable keys
    convey no admission, source or lifecycle authority. Definition revisions,
    occurrence slots and alarm sequences are nonnegative int64 values; codecs
    use canonical decimal strings and bound input before recursive decoding. *)

module Definition : sig
  type t [@@deriving compare, equal, sexp_of]

  val create
    :  controller:Agent_protocol.Session_ref.t
    -> recurrence_id:Agent_protocol.Id.Recurrence.t
    -> revision:int64
    -> (t, Agent_protocol.Error.t) result

  val controller : t -> Agent_protocol.Session_ref.t
  val recurrence_id : t -> Agent_protocol.Id.Recurrence.t
  val revision : t -> int64
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  val shape : Document_schema.Shape.t
end

module Occurrence : sig
  type t [@@deriving compare, equal, sexp_of]

  val create : definition:Definition.t -> slot:int64 -> (t, Agent_protocol.Error.t) result
  val definition : t -> Definition.t
  val slot : t -> int64
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  val shape : Document_schema.Shape.t
end

module Alarm : sig
  type t [@@deriving compare, equal, sexp_of]

  val create
    :  definition:Definition.t
    -> sequence:int64
    -> (t, Agent_protocol.Error.t) result

  val definition : t -> Definition.t
  val sequence : t -> int64
  val to_json : t -> Jsonaf.t
  val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
  val shape : Document_schema.Shape.t
end
