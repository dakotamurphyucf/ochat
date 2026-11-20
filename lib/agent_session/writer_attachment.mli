(** Inspect the writer mode and lease recorded in one session snapshot. The host
    supplies the current wall clock. A caller admitting a remote command must
    also authenticate its principal and connection ownership of this attachment. *)

val require
  :  Session_state.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> (Agent_protocol.Session.Attachment.t, Agent_protocol.Error.t) result

(** Validate the current lease and the owning principal for recovery management. *)
val require_recovery_owner
  :  Session_state.t
  -> principal:Agent_protocol.Id.Principal.t
  -> attachment_id:Agent_protocol.Id.Attachment.t
  -> now:(unit -> Agent_protocol.Timestamp.t)
  -> (unit, Agent_protocol.Error.t) result

val any : Session_state.t -> now:(unit -> Agent_protocol.Timestamp.t) -> bool
