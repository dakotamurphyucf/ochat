(** Exact queued or claimed job occurrence. A Queued job retains its last attempted
    number and reserves exactly the next claim; all other statuses identify the
    current attempt. This mapping does not admit, claim, run or retry any work. *)
val work
  :  Agent_protocol.Job.t
  -> (Agent_protocol.Run_work.t, Agent_protocol.Error.t) result

(** Validate one exact ID/generation/attempt against its actual job owner. Queued
    integer exhaustion is false; it never wraps or accepts arbitrary later tries.
    Source/creator authorization remains with the actor and wake owner. *)
val matches : Agent_protocol.Job.t -> work:Agent_protocol.Run_work.t -> bool
