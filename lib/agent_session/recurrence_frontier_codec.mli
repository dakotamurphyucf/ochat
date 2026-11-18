open! Core

(** Private frontier wire and intrinsic invariants. Complete source/cursor
    association is checked by the frontier owner. *)
val limits : Document_schema.Limits.t

val encode : Recurrence_frontier_records.State.t -> Jsonaf.t

val decode
  :  Jsonaf.t
  -> (Recurrence_frontier_records.State.t, Agent_protocol.Error.t) result

val validate
  :  Recurrence_frontier_records.State.t
  -> (unit, Agent_protocol.Error.t) result

val codec : Recurrence_frontier_records.State.t Document_schema.Domain_codec.t
val detail_json : Recurrence_frontier_records.Detail.t -> Jsonaf.t
val shape : Document_schema.Shape.t
val batch_json : Recurrence_frontier_records.Batch.t -> Jsonaf.t
val batch_has_extensions : Recurrence_frontier_records.Batch.t -> bool
val control_json : Recurrence_frontier_records.Control_detail.t -> Jsonaf.t
