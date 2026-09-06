(** Principal-scoped projections shared by RPC and HTTP, including replay.
    Transcript-only principals receive tool-content placeholders and finalized
    messages; recoverable streams require security-state scope because provider
    deltas may contain tool arguments. Hidden durable events retain positions. *)

val snapshot
  :  Agent_protocol.Principal.t
  -> Agent_protocol.Snapshot.t
  -> Agent_protocol.Snapshot.t

val durable
  :  Agent_protocol.Principal.t
  -> Agent_protocol.Event.Durable.t
  -> Agent_protocol.Event.Durable.t

val recoverable
  :  Agent_protocol.Principal.t
  -> Agent_protocol.Event.Recoverable.t
  -> Agent_protocol.Event.Recoverable.t option

val result
  :  Agent_protocol.Principal.t
  -> Agent_protocol.Method_result.t
  -> Agent_protocol.Method_result.t

val export_use : Agent_protocol.Principal.t -> string

val can_read_blob
  :  Agent_protocol.Principal.t
  -> Agent_store.Blob_store.Metadata.t
  -> bool

val scope_identity : Agent_protocol.Principal.t -> string
