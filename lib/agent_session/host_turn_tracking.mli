(** Canonical host-turn tracking for actual Operation_started/terminal payloads.
    The returned delta retains real Turn_handle admission facts and reconciles
    all stable Goal partitions with the same before/after canonical ledger.
    This is evidence planning, not authorization or a worker-start capability.
    Compaction never admits a host turn. *)
val apply
  :  previous:Session_state.t
  -> Session_state.t
  -> Session_delta.t
  -> Agent_protocol.Event.Durable.Payload.t list
  -> (Session_state.t * Session_delta.t, Agent_protocol.Error.t) result
