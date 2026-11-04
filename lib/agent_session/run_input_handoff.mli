(** Pure same-Run root attachment. Caller separately supplies actual held input
    custody, runtime/worker, authorization and persistence. No second admission,
    parsed input execution, receipt or executable capability is manufactured. *)
type t

val prepare
  :  Session_state.t
  -> run:Agent_protocol.Run.t
  -> request:Run_admission_request.t
  -> input:Runtime_input_preparation.Prepared_input.t
  -> operation:Agent_protocol.Operation.t
  -> pending:Pending_turn.t
  -> now:Agent_protocol.Timestamp.t
  -> (t, Agent_protocol.Error.t) result

val delta : t -> Session_delta.t
