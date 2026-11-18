open! Core

(** Private record inside the execution owner's complete bounded JSON. Decode is
    called only after that owner's full raw size/depth/field/node validation. *)
type t

val request : t -> Recurrence_execution_attempt.t
val rejection : t -> Agent_protocol.Error.code option
val uncertainty : t -> Agent_protocol.Error.code option
val retry_resolution : t -> Recurrence_retry_resolution.t option
val origin : t -> Recurrence_run_origin.t option
val terminal : t -> Recurrence_run_reference.t option
val is_resolved : t -> bool
val to_json : t -> Jsonaf.t

val of_json
  :  Jsonaf.t
  -> intent:Recurrence_execution_intent.t
  -> (t, Agent_protocol.Error.t) result

val create
  :  intent:Recurrence_execution_intent.t
  -> Recurrence_execution_attempt.t
  -> (t, Agent_protocol.Error.t) result

val record_rejection
  :  t
  -> code:Agent_protocol.Error.code
  -> (t, Agent_protocol.Error.t) result

val mark_uncertain
  :  t
  -> code:Agent_protocol.Error.code
  -> (t, Agent_protocol.Error.t) result

val resolve_retry
  :  t
  -> intent:Recurrence_execution_intent.t
  -> Recurrence_retry_resolution.t
  -> (t, Agent_protocol.Error.t) result

val associate
  :  t
  -> intent:Recurrence_execution_intent.t
  -> Recurrence_run_origin.t
  -> (t, Agent_protocol.Error.t) result

val settle : t -> Recurrence_run_reference.t -> (t, Agent_protocol.Error.t) result
val validate_transition : t -> incoming:t -> (unit, Agent_protocol.Error.t) result
val reserved_growth : t -> (Capacity_growth.t, Agent_protocol.Error.t) result
val capacity_projection : t -> Jsonaf.t
