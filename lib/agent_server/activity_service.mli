(** Bounded immutable observation service. Injected readers own all filesystem/
    actor resources and must recheck current principal visibility before IO and
    against the returned identity. This module owns no executor or resources. *)
type t

module Observation : sig
  type t =
    { snapshot : Agent_protocol.Snapshot.t
    ; transient : Agent_protocol.Session_activity.Transient.t
    ; usage : Agent_protocol.Inference_query.Summary.t
    }
end

val create
  :  server_id:Agent_protocol.Id.Server.t
  -> principal:Agent_protocol.Principal.t
  -> now:Agent_protocol.Timestamp.t
  -> read:(Agent_protocol.Id.Session.t -> (Observation.t, Agent_protocol.Error.t) result)
  -> t

(** [catalog] is already checked, currently authorized and filtered by the same
    catalog query. Rejects its total scan bound before invoking any reader.
    Returned rows retain the catalog's selected order; failures never silently
    drop sessions or advertise exhaustive results. *)
val observe
  :  t
  -> Agent_protocol.Activity_query.t
  -> catalog:Agent_protocol.Session_catalog.t list
  -> (Agent_protocol.Session_activity.t list, Agent_protocol.Error.t) result

(** Single-session immutable observation. No load or runtime activation. *)
val work
  :  t
  -> Agent_protocol.Session_work.Query.t
  -> (Agent_protocol.Session_work.t list, Agent_protocol.Error.t) result
