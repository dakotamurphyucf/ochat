open! Core

val id : int -> History_entry.Id.t
val entry : int -> History_entry.Payload.Semantic.view -> History_entry.t
val examples : unit -> History_entry.t list

val limits
  :  ?max_bytes:int
  -> ?max_work_steps:int
  -> ?json_limits:Document_schema.Limits.t
  -> unit
  -> History_chatmd.Rendering.Limits.t

val full : History_entry.t -> Agent_protocol.Public.History.t
