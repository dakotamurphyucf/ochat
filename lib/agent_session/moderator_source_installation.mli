(** Capture source installation from the actual validated checkpoint. This pure
    result contains durable facts only: no Run, receipt or execution permission.
    The caller must own checkpoint admission and commit the index atomically with
    its operation. Matching installation preserves its epoch. *)
val capture
  :  Session_state.t
  -> (Run_state.t * Agent_protocol.Run_source.t, Agent_protocol.Error.t) result

(** Project the same source/index facts from an explicitly captured checkpoint.
    This validates checkpoint DATA and preserves a matching installed epoch;
    a different installed source rejects. It neither installs the checkpoint nor
    grants authorization. The caller must retain the actual before/installation
    basis and save the projected index only with its SAME checkpoint ACK. *)
val capture_checkpoint
  :  Session_state.t
  -> checkpoint:Session.Moderator_state.Identity_snapshot.t
  -> (Run_state.t * Agent_protocol.Run_source.t, Agent_protocol.Error.t) result
