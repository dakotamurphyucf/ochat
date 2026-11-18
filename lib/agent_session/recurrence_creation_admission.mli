(** Actual current controller/source and original target-policy admission. These
    observations grant no child publication permission and are never serialized.
    The actor additionally checks its runtime fence, actual clocks/grace, full
    persistence preflight and ACK before issuing a process-local permit. *)
type t

(** [authorize] checks current host target policy for this exact immutable
    definition, independently of its stored creator and captured selectors.
    Original Create_sessions scope/currentness is checked before and after the
    callback and store observation. This trusted callback must not yield. The
    genuine store observation may yield; final checks do not. *)
val prepare
  :  Session_state.t
  -> controller:Recurrence_controller.t
  -> actor:Operator_authorization.t
  -> authorize:
       (Session_state.t
        -> Recurrence_definition.t
        -> (unit, Agent_protocol.Error.t) result)
  -> occurrence:Recurrence_identity.Occurrence.t
  -> expected_control_revision:int64
  -> (t, Agent_protocol.Error.t) result

val entry : t -> Recurrence_entry.t
val nominal : t -> Recurrence_nominal.t
val reservation : t -> Recurrence_child_reservation.t option

(** Repeat original proof, genuine Current, installed source and exact captured
    entry/frontier basis without host callbacks or IO, immediately before commit.
    An old observation cannot be reused after the persistence owner's own
    lifecycle projection update; observe again before post-ACK permit issuance. *)
val check_current : t -> Session_state.t -> (unit, Agent_protocol.Error.t) result
