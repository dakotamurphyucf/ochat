(** Immutable host proposal; no mutation, queue insertion or authentication.
    The actor supplies authenticated producer/current source and serializes commit.
    Admission time is assigned again at actual commit, never backdated to planning. *)
type t = private
  { session_id : Agent_protocol.Id.Session.t
  ; generation : int
  ; revision : int64
  ; source : Agent_protocol.Invocation.observer
  ; producer : Agent_protocol.Id.Principal.t
  ; previous : External_ingress.t
  ; candidate : External_ingress.t
  ; receipt : External_ingress.receipt
  }

type decision =
  | Duplicate of External_ingress.receipt
  | Enqueue of t

val prepare
  :  state:Session_state.t
  -> source:Agent_protocol.Invocation.observer
  -> producer:Agent_protocol.Id.Principal.t
  -> registration_id:Agent_protocol.Id.Capability.t
  -> namespace:string
  -> key:Agent_protocol.Idempotency_key.t
  -> payload:Jsonaf.t
  -> now:Agent_protocol.Timestamp.t
  -> create_event_id:(unit -> Agent_protocol.Id.Ingress_event.t)
  -> (decision, Agent_protocol.Error.t) result

val frame : t -> (Chat_response.Ingress_delivery.t, Agent_protocol.Error.t) result

(** Check captured revision/registration and re-run actual admission with current
    time and the reserved event ID. The timestamp is absent from the private frame,
    so assigning the commit time cannot change the already prepared queue append. *)
val revalidate
  :  state:Session_state.t
  -> now:Agent_protocol.Timestamp.t
  -> t
  -> (External_ingress.t * External_ingress.receipt, Agent_protocol.Error.t) result
