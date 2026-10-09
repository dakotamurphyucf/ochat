(** Pure attention projection. Reads do not acknowledge anything. The snapshot
    and catalog must describe one exact retained session revision; unavailable
    live-call state is supplied explicitly by the immutable reader. *)
type t

val create
  :  server_id:Agent_protocol.Id.Server.t
  -> principal:Agent_protocol.Principal.t
  -> now:Agent_protocol.Timestamp.t
  -> t

val observe
  :  t
  -> Agent_protocol.Snapshot.t
  -> catalog:Agent_protocol.Session_catalog.t
  -> transient:Agent_protocol.Session_activity.Transient.t
  -> usage:Agent_protocol.Inference_query.Summary.t
  -> (Agent_protocol.Session_activity.t, Agent_protocol.Error.t) result
