open! Core

type t

val session_id : Agent_protocol.Id.Session.t
val operation_id : Agent_protocol.Id.Operation.t
val after : Agent_protocol.Mutation_result.t
val operation : Agent_protocol.Operation.state -> Agent_protocol.Operation.t
val foreign_projection : Agent_protocol.Id.Session.t -> Agent_client.Projection.t
val with_handle : on_update:(Agent_client.Projection.t -> unit) -> (t -> unit) -> unit
val handle : t -> Agent_client.Session_handle.t
val detach_count : t -> int
val emit : t -> sequence:int64 -> Agent_protocol.Event.Durable.Payload.t -> unit
val replace_idle : t -> sequence:int64 -> unit
val fail : t -> Agent_protocol.Error.t -> unit
val end_transport : t -> unit
val ok : ('a, Agent_protocol.Error.t) Result.t -> 'a
