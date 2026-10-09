(** Durable loss of run execution custody. Actual source replacement rotates the
    installation exactly once and retires pending actions in the same index.
    Matching restore uses [recover] and preserves installation identity. Neither
    path executes or retries an external effect. Unsettled owned occurrences get
    explicit Unconfirmed evidence, never fabricated success. *)
val replace
  :  Run_state.t
  -> change:Run_source_installation.Change.t
  -> session_revision:int64
  -> now:Agent_protocol.Timestamp.t
  -> (Run_state.t, Agent_protocol.Error.t) result

val recover
  :  Run_state.t
  -> session_revision:int64
  -> now:Agent_protocol.Timestamp.t
  -> (Run_state.t, Agent_protocol.Error.t) result

(** Retire one actual host-owned run without changing unrelated runs. Preserves
    recorded terminal outcomes and marks only unsettled custody Unconfirmed;
    retires its pending actions/carriers and saves one immutable terminal receipt. *)
val interrupt_run
  :  Run_state.t
  -> run_id:Agent_protocol.Id.Run.t
  -> session_revision:int64
  -> now:Agent_protocol.Timestamp.t
  -> (Run_state.t, Agent_protocol.Error.t) result

(** As [interrupt_run], including actor-proved definitive owner outcomes from the
    same transaction. Evidence must be new, exact owned work at the next revision;
    existing terminal facts remain immutable and only other custody is Unconfirmed.
    This records one revision and one terminal receipt, never an intermediate run. *)
val interrupt_with_evidence
  :  Run_state.t
  -> run_id:Agent_protocol.Id.Run.t
  -> evidence:Agent_protocol.Run_work.Terminal.t list
  -> session_revision:int64
  -> now:Agent_protocol.Timestamp.t
  -> (Run_state.t, Agent_protocol.Error.t) result
