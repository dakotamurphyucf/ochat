(** One owner per daemon data root; caller holds its exclusive daemon lock and
    retains the opened directory capability until the store is closed. *)
type t

type initialization =
  | Create_if_missing
  | Require_existing

val create_ephemeral : server_id:Agent_protocol.Id.Server.t -> t

val open_owned
  :  directory:_ Eio.Path.t
  -> server_id:Agent_protocol.Id.Server.t
  -> initialization:initialization
  -> (t, Store_error.t) result

val close : t -> unit
val snapshot_checked : t -> (Organization_state.t, Store_error.t) result

val mutate
  :  t
  -> principal:Agent_protocol.Principal.t
  -> audit:Idempotency_store.Command_audit.t
  -> now:Agent_protocol.Timestamp.t
  -> candidate:Organization_state.Candidate.t option
  -> Organization_state.Mutation.t
  -> (Agent_protocol.Organization_result.t, Agent_protocol.Error.t) result

val receipt
  :  t
  -> principal:Agent_protocol.Principal.t
  -> now:Agent_protocol.Timestamp.t
  -> key:Idempotency_store.Key.t
  -> request_digest:string
  -> (Agent_protocol.Organization_result.t option, Agent_protocol.Error.t) result

(** Before actor idempotency admission or command.receipt: require current checked
    host, Manage_organization and owner/admin identity for every requested Set/add
    reference, including retained tombstones. Does not require live groups, mutate,
    enqueue/wait for an actor, or keep its mutex after return. Cleanup passes empty.
    Owned tombstoned Set/add may authorize a fresh no-op only if already present
    in raw membership; a newly introduced ID is rejected by live commit admission.
    Authorize never revives tombstones. Absent IDs reject before visibility tests,
    using the same generic not-found response as hidden/retired references. *)
val authorize_membership
  :  t
  -> principal:Agent_protocol.Principal.t
  -> host_id:Agent_protocol.Id.Server.t
  -> references:Agent_protocol.Session_organization.Values.t
  -> (unit, Agent_protocol.Error.t) result

(** Serialize fresh additions with group mutation/delete. Require live referenced
    groups and current scopes/ownership under the mutex, then invoke callback at
    most once. Callback may yield only in current actor persistence, never await
    an actor/reenter org authority/acquire catalog or index locks. Capture exceptions
    inside mutex and rethrow after unlock without poisoning it. Host/org uncertainty
    denies before callback. This operation does not mutate organization revision. *)
val with_live_membership
  :  t
  -> principal:Agent_protocol.Principal.t
  -> host_id:Agent_protocol.Id.Server.t
  -> additions:Agent_protocol.Session_organization.Values.t
  -> commit:(unit -> (unit, Agent_protocol.Error.t) result)
  -> (unit, Agent_protocol.Error.t) result
