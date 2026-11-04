(** Pure projection of an actor-validated actual terminal attempt, before a retry
    successor replaces the latest job. Never executes a callback or grants its
    authority. Immutable evidence and the exact awaited frame share the actual
    job completion transaction. *)
val awaits : Session_state.t -> Agent_protocol.Job.t -> bool

(** Whether a live run retains this exact current/planned job occurrence. Does
    not grant completion or control authority; the actor supplies its actual job. *)
val owns : Session_state.t -> Agent_protocol.Job.t -> bool

(** [running] is the actual current execution attempt; [terminal] is its validated
    terminal snapshot and stored completion, [successor] the already selected
    terminal job or exact queued retry. The actor determines the truthful outcome
    before bounded diagnostic normalization. Retains failed attempt evidence and
    captures the next retry attempt as distinct owned work. The shared actual-work
    settlement law may terminalize a quiescent Run in that same next revision and
    canonical receipt; Waiting remains live until its actual queued callback is
    claimed. Both Async_tool stored completions and Model raw terminal results
    are validated through Job.terminal_result, without rewriting the Job schema. *)
val prepare
  :  Session_state.t
  -> running:Agent_protocol.Job.t
  -> terminal:Agent_protocol.Job.t
  -> successor:Agent_protocol.Job.t
  -> outcome:Agent_protocol.Run_work.Terminal.outcome
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t list, Agent_protocol.Error.t) result

(** Definitive cancellation of an actual current Queued Async_tool or Model_call
    before its planned attempt starts. Records Cancelled for the exact planned
    owned occurrence without inventing execution or a terminal frame. An exact
    Wait that cannot receive an actual attempt completion retires Interrupted
    with known cancellation and truthful custody evidence for unresolved work.

    Non-waiting Model cancellation uses the shared aggregate settlement law in
    this same next Run revision. Existing non-waiting queued Async_tool behavior
    retains its lifecycle and immutable evidence. The actor persists the actual
    cancelled Job with this candidate and supplies the genuine completion sample
    if the Model branch creates a terminal receipt. *)
val cancel_queued
  :  Session_state.t
  -> job:Agent_protocol.Job.t
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t list, Agent_protocol.Error.t) result
