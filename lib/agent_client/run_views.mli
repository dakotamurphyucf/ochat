(** Transport-neutral, nonactivating run inspection. The server retains current
    authority and immutable lifecycle evidence; this client owns no executor. *)
type t

val create : Connection.t -> server_id:Agent_protocol.Id.Server.t -> t

(** Requires the completed handshake and matching host-qualified session. *)
val page
  :  t
  -> Agent_protocol.Run_query.Request.t
  -> ( Agent_protocol.Run_query.View.t Agent_protocol.Page.t
       , Agent_protocol.Error.t )
       result

val lookup
  :  t
  -> Agent_protocol.Run_query.Lookup_request.t
  -> (Agent_protocol.Run_query.Outcome.t, Agent_protocol.Error.t) result

(** A read-only observer for the caller's existing subscribed Session_handle.
    Install the returned nonyielding callback before that handle's initial snapshot.
    It coalesces revision changes into one pending refresh; it does not acquire
    another notification lease, attach, activate a runtime or retry mutations.
    The supplied Switch owns the daemon refresh fiber. On scope closure the
    callback becomes inert. Callback failures and cancellation propagate from
    the owned fiber; fresh authorization failures and stale-projection diagnostics are delivered to
    [on_error]. *)
val watch
  :  t
  -> sw:Eio.Switch.t
  -> request:Agent_protocol.Run_query.Lookup_request.t
  -> on_result:(Agent_protocol.Run_query.Outcome.t -> unit)
  -> on_error:(Agent_protocol.Error.t -> unit)
  -> Projection.t
  -> unit
