(** Factual selection beneath the actual controller Actor. These checks neither
    retain a runtime capability nor reserve accounting. The Actor authenticates
    the original, native invocation if any, actual controller/source, lifecycle
    and expected State before selection and again at the canonical writer. *)
type selection =
  | Operator of Recurrence_command.Goal_owner_request.t
  | Native of Goal_work_attribution.t

(** Initial definition only. Verifies the named local Goal and its current
    Workflow episode against the retained Run, link and exact Admission receipt.
    A native caller supplies its actual selected attribution, not a Goal ID. *)
val capture
  :  Session_state.t
  -> owner:Agent_protocol.Session_ref.t
  -> source:Agent_protocol.Run_source.t
  -> selection:selection
  -> now:Agent_protocol.Timestamp.t
  -> (Recurrence_definition.Goal_binding.Capture.t, Agent_protocol.Error.t) result

(** Positive Edit refreshes explicit Goal CAS but copies the birth association. *)
val retain
  :  Recurrence_definition.Goal_binding.Capture.t
  -> Session_state.t
  -> owner:Agent_protocol.Session_ref.t
  -> source:Agent_protocol.Run_source.t
  -> request:Recurrence_command.Goal_owner_request.t
  -> now:Agent_protocol.Timestamp.t
  -> (Recurrence_definition.Goal_binding.Capture.t, Agent_protocol.Error.t) result

(** Fresh activation against the same local Goal/source/account. Creator Run
    settlement, retention and subsequent Goal revisions cannot erase the birth
    association. Already-admitted effects and factual import have separate checks. *)
val check_current
  :  Recurrence_definition.Goal_binding.Capture.t
  -> Session_state.t
  -> owner:Agent_protocol.Session_ref.t
  -> source:Agent_protocol.Run_source.t
  -> now:Agent_protocol.Timestamp.t
  -> (unit, Agent_protocol.Error.t) result
