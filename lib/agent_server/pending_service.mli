(** Nonactivating inspection under current authenticated transcript visibility.
    [read] must use the registry immutable authorization/reader boundary; it may
    not construct a runtime, repair state or acquire execution capacity. *)
type t

val create
  :  Agent_protocol.Principal.t
  -> read:
       (Agent_protocol.Id.Session.t
        -> (Agent_session.Session_state.t, Agent_protocol.Error.t) result)
  -> pagination:Pagination.t
  -> (t, Agent_protocol.Error.t) result

val list
  :  t
  -> Agent_protocol.Pending_query.Request.t
  -> (Agent_protocol.Pending_query.View.t, Agent_protocol.Error.t) result

val lookup
  :  t
  -> Agent_protocol.Pending_query.Lookup_request.t
  -> (Agent_protocol.Pending_query.Outcome.t, Agent_protocol.Error.t) result
