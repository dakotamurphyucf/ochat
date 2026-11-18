open! Core

(** One immutable notification identity tied to the current nominal frontier.
    An alarm is no Schedule, Run, target reservation or execution authority. *)
type t [@@deriving equal, sexp_of]

val create
  :  key:Recurrence_identity.Alarm.t
  -> slot:int64
  -> due_at:Time_ns.Alternate_sexp.t
  -> (t, Agent_protocol.Error.t) result

val key : t -> Recurrence_identity.Alarm.t
val slot : t -> int64
val due_at : t -> Time_ns.Alternate_sexp.t

(** The frontier validates definition/slot/deadline and monotone sequence against
    its actual retained cursor/lookahead and high-water before applying a plan.
    Compatible unknowns belong to that enclosing carrier. *)
val to_json : t -> Jsonaf.t

val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result
val shape : Document_schema.Shape.t
val limits : Document_schema.Limits.t
