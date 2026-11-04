(** Original caller and genuine controller observation for recording an immutable
    occurrence request. This value grants no target, retry or execution authority. *)
type t

type decision =
  | Retained of Recurrence_execution.t
  | New of
      { catalog : Recurrence_catalog.t
      ; execution : Recurrence_execution.t
      }

(** Classify the exact original intent/attempt before applying new-work gates.
    Both branches require current Send_messages and current host policy for the
    exact retained definition, independently of stored audit principals. The
    trusted [authorize] callback must not yield. Store observation may yield.

    Only New requires the enabled current definition/control, installed source,
    automatic birth lifecycle and the actual caller as attempt principal. Actor
    runtime/grace and complete persistence preflight remain separate checks. *)
val prepare
  :  Session_state.t
  -> controller:Recurrence_controller.t
  -> actor:Operator_authorization.t
  -> authorize:
       (Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> intent:Recurrence_execution_intent.t
  -> attempt:Recurrence_execution_attempt.t
  -> (t, Agent_protocol.Error.t) result

val decision : t -> decision
val entry : t -> Recurrence_entry.t
val nominal : t -> Recurrence_nominal.t

(** Synchronous final original-policy, genuine Current and exact captured basis
    check. New-work source/control checks never turn retained historical data
    into execution authority. Do not reuse after the persistence owner's own
    canonical projection changes; observe freshly after ACK if returning data. *)
val check_current : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result

(** Callback-free exact controller Current/session/revision/retained entry and
    new-work source/control/caller predicates. Original authentication and policy
    remain required separately; this data basis never authorizes persistence. *)
val check_basis : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
