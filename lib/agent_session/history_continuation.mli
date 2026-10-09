(** Pure retained-outcome reconciliation for a quiescent history continuation.
    No tools, observers, providers or runtime activation are executed. The actor
    owns quiescence checks and commits these ingredients with an actual Turn. *)
type t

val prepare
  :  Session_state.t
  -> retiring_history:bool
  -> (t, Agent_protocol.Error.t) result

val deltas : t -> Session_delta.t list
val payloads : t -> Agent_protocol.Event.Durable.Payload.t list

(** Retirement forbids appended output and allocator movement. Failure directs
    the caller to Save_only followed by standalone Continue, whose normal current
    carrier can append outcomes already recorded by invocation recovery. *)
