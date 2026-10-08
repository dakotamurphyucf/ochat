(** Read-only assertions over public wire views; no conversion to canonical input. *)
val snapshot_to_json : Agent_protocol.Public.Snapshot.Fields.t -> Jsonaf.t

val non_history : Agent_protocol.Public.Result.t -> Agent_protocol.Method_result.t

val shared_payload
  :  Agent_protocol.Public.Durable.t
  -> Agent_protocol.Event.Durable.Payload.t

val history_text : Agent_protocol.Public.History.t -> string list

(** Assertions requiring full evidence fail on Visible/Redacted. *)
val full_payload : Agent_protocol.Public.History.t -> History_entry.Payload.t

val has_header : Agent_protocol.Public.History.t -> Transcript.Header.t -> bool

(** Projects an actual stored canonical oracle to Full read evidence; never imports a public view. *)
val history_of_internal : Agent_protocol.History.entry -> Agent_protocol.Public.History.t

val payload
  :  Agent_protocol.Public.Durable.t
  -> Agent_protocol.Public.Durable.payload option

val visibility
  :  Agent_protocol.Public.Durable.t
  -> Agent_protocol.Event.Durable.visibility

val shared_payload_opt
  :  Agent_protocol.Public.Durable.t
  -> Agent_protocol.Event.Durable.Payload.t option
