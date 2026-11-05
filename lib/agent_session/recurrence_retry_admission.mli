(** Current controller selection for one pure retry candidate. Opaque target
    no-effects/source custody remains in Session_actor; this helper grants none. *)
type t

type decision =
  | Retained of Recurrence_execution.t
  | New of
      { catalog : Recurrence_catalog.t
      ; execution : Recurrence_execution.t
      }

val prepare
  :  Session_state.t
  -> controller:Recurrence_controller.t
  -> actor:Operator_authorization.t
  -> authorize:
       (Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> intent:Recurrence_execution_intent.t
  -> previous:Recurrence_execution_attempt.t
  -> resolution:Recurrence_retry_resolution.t
  -> next:Recurrence_execution_attempt.t
  -> (t, Agent_protocol.Error.t) result

val decision : t -> decision
val entry : t -> Recurrence_entry.t
val nominal : t -> Recurrence_nominal.t
val check_policy : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
val check_basis : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
