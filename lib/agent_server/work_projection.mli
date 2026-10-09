(** Pure, payload-free projection of retained work. No execution, acknowledgement,
    resource acquisition or mutation occurs. The caller must establish current
    session visibility before supplying a snapshot. Both transcript and security
    view scopes are required independently of richer work-read scopes. *)
type t

val create
  :  server_id:Agent_protocol.Id.Server.t
  -> principal:Agent_protocol.Principal.t
  -> t

(** Rejects contradictory session identities and duplicate work keys. Historical
    generation records are omitted; at most 4096 current records are admitted.
    Unknown future extension statuses are represented as Unsupported. *)
val work
  :  t
  -> Agent_protocol.Snapshot.t
  -> (Agent_protocol.Session_work.t list, Agent_protocol.Error.t) result
