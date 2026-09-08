(** Pure admission and completion checks for actor-owned queued moderator events.
    Failed/interrupted claims block queued execution for that source/generation,
    including after other handlers change the checkpoint. Explicit retirement is
    a separate host operation, not an implicit retry in this handoff.
    Successful consumption may emit an identical event. The actor persists the
    claim before invoking any script effects. *)
val claim
  :  state:Session_state.t
  -> id:Agent_protocol.Id.Moderator_execution.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> now:Agent_protocol.Timestamp.t
  -> ( Agent_protocol.Moderator_execution.t * Session.Snapshot.t
       , Agent_protocol.Error.t )
       result

(** Validate unchanged source and preservation of the old queue tail. The trusted
    engine supplies the prospective snapshot and runtime requests; the actor must
    atomically save both this receipt and that checkpoint while retaining its borrow. *)
val complete
  :  claimed:Agent_protocol.Moderator_execution.t
  -> before:Session.Moderator_state.Identity_snapshot.t
  -> snapshot:Session.Moderator_state.Identity_snapshot.t
  -> requests:Agent_protocol.Invocation.follow_up
  -> (Agent_protocol.Moderator_execution.t, Agent_protocol.Error.t) result
