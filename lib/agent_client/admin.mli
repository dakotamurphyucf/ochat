open! Core

(** Transport-neutral daemon administration reads. Mutations that require an
    attachment are exposed by {!Session_handle}. *)

(** [list_sessions connection] returns every session visible to the current
    principal. Enumerates at most 100000 sessions over 100 pages, returning an
    error on bound exhaustion or concurrent catalog change rather than truncating. *)
val list_sessions
  :  Connection.t
  -> (Agent_protocol.Session.t list, Agent_protocol.Error.t) result

(** [get_session connection session_id] returns the authoritative snapshot
    without attaching. *)
val get_session
  :  Connection.t
  -> Agent_protocol.Id.Session.t
  -> (Agent_protocol.Public.Snapshot.t, Agent_protocol.Error.t) result

val list_sessions_page
  :  Connection.t
  -> Agent_protocol.Session.List_request.t
  -> ( Agent_protocol.Session_catalog.t Agent_protocol.Page.t
       , Agent_protocol.Error.t )
       result

(** Explicitly bounded full enumeration. A nonempty initial cursor is invalid.
    Refresh conflicts are returned; callers choose whether to restart the query. *)
val enumerate_sessions
  :  Connection.t
  -> query:Agent_protocol.Session.List_request.t
  -> max_sessions:int
  -> max_pages:int
  -> (Agent_protocol.Session_catalog.t list, Agent_protocol.Error.t) result
