(** Principal-scoped projections shared by RPC and HTTP, including replay.
    Transcript-only principals receive tool-content placeholders and finalized
    messages; recoverable streams require security-state scope because provider
    deltas may contain tool arguments. Hidden durable events retain positions. *)

val snapshot
  :  Agent_protocol.Principal.t
  -> Agent_protocol.Snapshot.t
  -> (Agent_protocol.Public.Snapshot.t, Agent_protocol.Error.t) result

(** Project one already session-authorized canonical entry. Requires transcript
    scope explicitly; security scope alone never grants readable text. The
    caller owns source-size limits and initial prompt/provenance exclusion. *)
val readable_history_entry
  :  Agent_protocol.Principal.t
  -> Agent_protocol.History.entry
  -> (Agent_protocol.Public_history.t, Agent_protocol.Error.t) result

val durable
  :  Agent_protocol.Principal.t
  -> Agent_protocol.Event.Durable.t
  -> (Agent_protocol.Public.Durable.t, Agent_protocol.Error.t) result

val recoverable
  :  Agent_protocol.Principal.t
  -> Agent_protocol.Event.Recoverable.t
  -> Agent_protocol.Event.Recoverable.t option

val result
  :  Agent_protocol.Principal.t
  -> Agent_protocol.Method_result.t
  -> (Agent_protocol.Public.Result.t, Agent_protocol.Error.t) result

val export_use : Agent_protocol.Principal.t -> string

val can_read_blob
  :  Agent_protocol.Principal.t
  -> Agent_store.Blob_store.Metadata.t
  -> bool

val scope_identity : Agent_protocol.Principal.t -> string

(** Same known Full/Visible/Redacted policy as snapshot history, with an explicit
    current transcript gate before exposing one pending/canonical occurrence. *)
val pending_history_entry
  :  Agent_protocol.Principal.t
  -> Agent_protocol.History.entry
  -> (Agent_protocol.Public_history.t, Agent_protocol.Error.t) result
