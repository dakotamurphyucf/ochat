open! Core

(** Transport-neutral daemon administration reads. Mutations that require an
    attachment are exposed by {!Session_handle}. *)

(** [list_sessions connection] returns every session visible to the current
    principal, up to the protocol administrative page limit. *)
val list_sessions
  :  Connection.t
  -> (Agent_protocol.Session.t list, Agent_protocol.Error.t) result

(** [get_session connection session_id] returns the authoritative snapshot
    without attaching. *)
val get_session
  :  Connection.t
  -> Agent_protocol.Id.Session.t
  -> (Agent_protocol.Snapshot.t, Agent_protocol.Error.t) result
