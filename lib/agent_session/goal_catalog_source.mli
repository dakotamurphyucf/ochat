(** Pure actor-owned source planning. Persisted identity grants no execution
    authority. Capturing before the first Run creates only installation metadata;
    it must commit atomically with the goal command, never as a fabricated Run. *)
val capture
  :  Session_state.t
  -> (Run_state.t * Agent_protocol.Run_source.t, Agent_protocol.Error.t) result

(** Called after actual Run source retirement selection. Source replacement and
    generation reset hold enabled goals in the same existing whole-state commit. *)
val prepare_change
  :  Session_state.t
  -> delta:Session_delta.t
  -> (Session_delta.t, Agent_protocol.Error.t) result

(** Existing recovery transaction shares both Run retirement and goal hold.
    No new user receipt slot is required and no work is replayed. *)
val recovery_delta
  :  Session_state.t
  -> now:Agent_protocol.Timestamp.t
  -> (Session_delta.t option, Agent_protocol.Error.t) result
