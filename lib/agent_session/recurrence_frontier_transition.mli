open! Core
module V = Recurrence_frontier_records

val validate_associations
  :  V.State.t
  -> definitions:Recurrence_definition.t list
  -> cursor:Recurrence_cursor.t
  -> (unit, Agent_protocol.Error.t) result

val withdraw
  :  V.State.t
  -> definition:Recurrence_definition.t
  -> preserve_lookahead:bool
  -> (V.State.t, Agent_protocol.Error.t) result

val consume_range
  :  V.State.t
  -> definition:Recurrence_definition.t
  -> first_slot:int64
  -> last_slot:int64
  -> last_at:Time_ns.Alternate_sexp.t
  -> selected:V.Stored_nominal.t list
  -> omitted:(int64 * int64 * V.Reason.t) list
  -> (V.State.t, Agent_protocol.Error.t) result

val collect_calendar
  :  V.State.t
  -> definition:Recurrence_definition.t
  -> nominal:V.Stored_nominal.t
  -> (V.State.t, Agent_protocol.Error.t) result

val finish_calendar
  :  V.State.t
  -> definition:Recurrence_definition.t
  -> lookahead:V.Stored_nominal.t option
  -> (V.State.t, Agent_protocol.Error.t) result

val arm
  :  V.State.t
  -> definition:Recurrence_definition.t
  -> slot:int64
  -> due_at:Time_ns.Alternate_sexp.t
  -> (V.State.t, Agent_protocol.Error.t) result

val validate_current
  :  V.State.t
  -> definition:Recurrence_definition.t
  -> cursor:Recurrence_cursor.t
  -> (unit, Agent_protocol.Error.t) result

val find_mark
  :  V.State.t
  -> Recurrence_definition.t
  -> (V.Mark.t, Agent_protocol.Error.t) result

val retire_alarm : V.State.t -> definition:Recurrence_definition.t -> V.State.t

(** Private exact-state bookkeeping shared by settlement and ordinary planning. *)
val update_mark : V.State.t -> V.Mark.t -> V.State.t

val candidates : V.State.t -> V.Stored_nominal.t list

val archive
  :  V.State.t
  -> Recurrence_definition.t
  -> V.Control_detail.kind
  -> Jsonaf.t
  -> V.State.t
