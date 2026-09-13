(** Retain the unloaded runtime owner, then the quiescent actor, publisher,
    idempotency cache and coordinated blob store while reconciling private result
    preparations. Active ownership defers without deleting. Historical storage,
    replay, cached responses, disk agent cache and auxiliary exports/logs are roots;
    corrupt, unknown, linked, incomplete or oversized roots reject the attempt.
    Workspaces and prompt source trees are user inputs, not managed artifact stores. *)
val collect
  :  runtime:Runtime_owner.t
  -> actor:Agent_session.Session_actor.t
  -> publisher:Agent_store.Job_result_store.Publisher.t
  -> handle:Agent_store.Session_store.Handle.t
  -> journal:Agent_store.Journal.t
  -> persistence:Agent_session.Session_persistence.t
  -> durable_events:Agent_session.Durable_event_log.t
  -> idempotency_store:Agent_store.Idempotency_store.t
  -> limits:Agent_store.Job_result_store.Publisher.collection_limits
  -> max_frame_bytes:int
  -> max_events:int
  -> ( Agent_store.Job_result_store.Publisher.collection_stats option
       , Agent_protocol.Error.t )
       result
