(** Shared initialized-connection negotiation for independently owned authoring
    clients. This checks feature selection, actual handler capabilities, exact
    initialize response identity and method/result affinity. It grants no host
    authority and owns no context, upload or retry state. *)

type t

val create : Connection.t -> feature:string -> (t, Agent_protocol.Error.t) result
val require_current : t -> (unit, Agent_protocol.Error.t) result

(** Check the immutable initialize selection used by this live connection.
    Method capability is checked separately by [send]. *)
val require_feature : t -> string -> (unit, Agent_protocol.Error.t) result

val capabilities : t -> Agent_protocol.Authoring.Capabilities.t
val host : t -> Agent_protocol.Id.Server.t
val principal : t -> Agent_protocol.Id.Principal.t

val send
  :  t
  -> Agent_protocol.Authoring.Wire.Request.t
  -> (Agent_protocol.Authoring.Wire.Response.t, Agent_protocol.Error.t) result
