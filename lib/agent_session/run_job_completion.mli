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
    captures the next retry attempt as distinct owned work; Wait is not consumed
    until an actual queued callback is claimed. *)
val prepare
  :  Session_state.t
  -> running:Agent_protocol.Job.t
  -> terminal:Agent_protocol.Job.t
  -> successor:Agent_protocol.Job.t
  -> outcome:Agent_protocol.Run_work.Terminal.outcome
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t list, Agent_protocol.Error.t) result

(** Definitive cancellation of a current Queued job before its planned attempt
    starts. Records Cancelled for the exact planned owned occurrence without
    inventing an execution or a terminal frame for it. An exact Wait that cannot
    receive an actual attempt completion retires Interrupted with that known
    cancellation and truthful custody evidence for other unresolved work. Other
    live runs retain their lifecycle and the immutable cancellation evidence. *)
val cancel_queued
  :  Session_state.t
  -> job:Agent_protocol.Job.t
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t list, Agent_protocol.Error.t) result
