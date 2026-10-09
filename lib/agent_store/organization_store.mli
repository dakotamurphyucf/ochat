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
