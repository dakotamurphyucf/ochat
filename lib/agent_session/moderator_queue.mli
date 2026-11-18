open! Core

(** Data-only captured provenance; constructing it grants no live authority. *)
module Capture : sig
  type t

  val unavailable : t

  val create
    :  producer:Queue_producer.t
    -> origin:Agent_protocol.Inference_origin.t option
    -> (t, Agent_protocol.Error.t) result
end

module Entry : sig
  type t

  val equal : t -> t -> bool
  val ordinal : t -> int64
  val payload_sha256 : t -> string
  val producer : t -> Queue_producer.t option
  val origin : t -> Agent_protocol.Inference_origin.t option
end

module Consumption : sig
  type t =
    | Keep
    | Consume_head
  [@@deriving equal, sexp]
end

(** One bounded, contiguous queue lifetime. Structural ordinals on historical
    values have unavailable producer/origin and grant no execution authority.
    Same-source empty queues retain their monotone next ordinal. *)
type t

val source : t -> Agent_protocol.Run_source.t
val head_ordinal : t -> int64
val next_ordinal : t -> int64
val entries : t -> Entry.t list

val bootstrap
  :  source:Agent_protocol.Run_source.t
  -> queue:Session.Snapshot.t list
  -> (t, Agent_protocol.Error.t) result

(** The real caller supplies consumption explicitly. Payload equality validates
    preservation only; it never selects which occurrence was consumed. *)
val advance
  :  t
  -> before:Session.Snapshot.t list
  -> after:Session.Snapshot.t list
  -> consume:Consumption.t
  -> appended:Capture.t list
  -> (t, Agent_protocol.Error.t) result

val validate_queue : t -> Session.Snapshot.t list -> (unit, Agent_protocol.Error.t) result

(** Validate an admissible keep/single-head-consume transition from declared
    frontiers. Does not reconstruct live action, permission or an execution. *)
val validate_transition
  :  previous:t
  -> before:Session.Snapshot.t list
  -> incoming:t
  -> after:Session.Snapshot.t list
  -> (unit, Agent_protocol.Error.t) result

val to_jsonaf : t -> Jsonaf.t
val of_jsonaf : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t

(** Check the complete original/merged JSON child, including retained unfamiliar
    fields, against the same finite metadata profile used by decoding. *)
val validate_encoded_capacity : Jsonaf.t -> (unit, Agent_protocol.Error.t) result
