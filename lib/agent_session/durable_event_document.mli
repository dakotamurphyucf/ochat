open! Core

(** Immutable full event document; payload and unknown fields are retained. *)
type t

val value : t -> Agent_protocol.Event.Durable.t
val document : t -> Document_schema.Document.t

val decode
  :  limits:Document_schema.Limits.t
  -> Document_schema.Document.t
  -> (t, Document_schema.Error.t) result

val create
  :  Agent_protocol.Event.Durable.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val to_jsonaf : Agent_protocol.Event.Durable.t -> Jsonaf.t

val validate
  :  ?limits:Document_schema.Limits.t
  -> Agent_protocol.Event.Durable.t
  -> (unit, Agent_protocol.Error.t) result
