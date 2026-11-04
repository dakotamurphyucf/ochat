(** Fresh host-turn policy at the actual persistence boundary. The prospective
    association/policy must already have passed the owner's normal admission and
    CAS checks. Charges always come from the actual before ledger/accounts,
    including a legitimate Held-to-Enabled controller candidate. No permission,
    Goal episode, operation or accounting reset is created by this planner. *)
type decision =
  | Allow
  | Held of
      { goal_id : Agent_protocol.Id.Goal.t
      ; delta : Session_delta.t option
      }

val prepare
  :  previous:Session_state.t
  -> candidate:Session_state.t
  -> payloads:Agent_protocol.Event.Durable.Payload.t list
  -> now:Agent_protocol.Timestamp.t
  -> (decision, Agent_protocol.Error.t) result
