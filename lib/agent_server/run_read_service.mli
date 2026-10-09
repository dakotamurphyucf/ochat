(** Payload-free run projection over the existing immutable session reader.
    No runtime selection, execution, credential resolution or independent broker. *)
type t

val create
  :  Agent_protocol.Principal.t
  -> server_id:Agent_protocol.Id.Server.t
  -> read:
       (Agent_protocol.Id.Session.t
        -> (Agent_session.Session_state.t, Agent_protocol.Error.t) result)
  -> pagination:Pagination.t
  -> (t, Agent_protocol.Error.t) result

(** Filters foreign-principal runs unless the current principal is an actual
    administrator. Cursor binds current authority, host/session, data and order;
    authority changes reject reuse rather than disclose cached foreign records. *)
val list
  :  t
  -> Agent_protocol.Run_query.Request.t
  -> ( Agent_protocol.Run_query.View.t Agent_protocol.Page.t
       , Agent_protocol.Error.t )
       result

(** Missing or foreign-principal retained ID is Unavailable; session authority failures remain errors.
    Terminal references remain immutable even when underlying content is unavailable;
    obtaining a reference never grants authority to read the referenced content. *)
val lookup
  :  t
  -> Agent_protocol.Run_query.Lookup_request.t
  -> (Agent_protocol.Run_query.Outcome.t, Agent_protocol.Error.t) result
