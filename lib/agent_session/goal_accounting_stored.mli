(** Internal context-free claims codec. The public contract is exposed through
    Goal_accounting.Stored with abstract resolved accounting. *)
type accounting = Goal_accounting_state.t

type t

val capture : accounting -> t
val equal : t -> t -> bool
val goal_id : t -> Agent_protocol.Id.Goal.t
val session : t -> Agent_protocol.Session_ref.t
val revision : t -> int64
val admitted_attempts : t -> int64
val admitted_turns : t -> int64
val untracked_attempts : t -> int64
val untracked_turns : t -> int64
val ordinals : t -> int64 list
val run_bindings : t -> (Agent_protocol.Id.Run.t * Agent_protocol.Run_source.t) list
val shape : Document_schema.Shape.t
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Agent_protocol.Error.t) result

val resolve
  :  t
  -> goal:Agent_protocol.Goal.t
  -> ledger:Inference_ledger.t
  -> (accounting, Goal_accounting_state.Error.t) result

val validate_update : t -> incoming:t -> (unit, Agent_protocol.Error.t) result

val validate_transition
  :  t
  -> incoming:t
  -> before:Inference_ledger.t
  -> after:Inference_ledger.t
  -> (unit, Agent_protocol.Error.t) result

(** Shape/capacity probes only, never decoded as accounting evidence. *)
val retirement_projection : t -> ordinal:int64 option -> Jsonaf.t
