(** Pure selection from the existing pending enqueue/adopt recipe. No runtime,
    writer, source, original proof or execution permission is created here. *)
type t

val prepare
  :  Session_state.t
  -> enqueue:Pending_transition.t
  -> entry:Agent_protocol.History.entry
  -> runtime_admission_open:bool
  -> (t, Agent_protocol.Error.t) result

val entries : t -> Agent_protocol.History.entry list
val accepted : t -> bool
val enqueue_delta : t -> Session_delta.t
val turn_deltas : t -> Session_delta.t list
val deferred_payloads : t -> Agent_protocol.Event.Durable.Payload.t list
