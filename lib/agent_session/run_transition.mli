(** Pure actor-side run/action composition. No I/O, authorization lookup, provider
    calls or callback execution. The actor establishes live scope/authority and
    passes a validated prospective state from its actual staged-work delta. New
    custody is captured even without an authored action. It persists the returned
    delta with its prospective moderator checkpoint and actual event receipt. *)
type t

val prepare
  :  Session_state.t
  -> scope:Run_scope.t
  -> executing:Agent_protocol.Moderator_execution.t
  -> work_state:Session_state.t
  -> owned_work:Agent_protocol.Run_work.t list
  -> action:Agent_protocol.Run_action.t option
  -> now:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

val run : t -> Agent_protocol.Run.t
val delta : t -> Session_delta.t
val payloads : t -> Agent_protocol.Event.Durable.Payload.t list

(** Join the actual root terminal transaction. Captures immutable owner evidence
    and resolves a pending Finish only after all retained owned work has settled.
    This never starts another worker or stops the session. *)
val settle_operation
  :  Session_state.t
  -> operation_id:Agent_protocol.Id.Operation.t
  -> outcome:Agent_protocol.Run_work.Terminal.outcome
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t list, Agent_protocol.Error.t) result

(** Pair existing turn admission with only its exact applied handler request or
    bound subscription-delivery wake. Consumes the retained action once and binds
    the actual operation; unrelated input never resumes a waiting run. *)
val admit_turn
  :  Session_state.t
  -> operation:Agent_protocol.Operation.t
  -> events:Agent_protocol.Moderator_execution.t list
  -> deliveries:Agent_protocol.Delivery.t list
  -> now:Agent_protocol.Timestamp.t
  -> (Agent_protocol.Id.Run.t list * Session_delta.t list, Agent_protocol.Error.t) result

(** Resume only the exact pending Wait matched by a host-validated private queue
    frame. Consumes it once and captures the actual callback execution as owned
    work. The actor validates current authority for every returned run ID, then
    commits these deltas with the same new moderator execution receipt. Retired,
    duplicate, unrelated and terminal occurrences must not invoke this path. *)
val claim_wake
  :  Session_state.t
  -> executing:Agent_protocol.Moderator_execution.t
  -> now:Agent_protocol.Timestamp.t
  -> (Agent_protocol.Id.Run.t list * Session_delta.t list, Agent_protocol.Error.t) result
