(** Recovery-only safe retirement under the established store recovery owner.
    Retains the genuine opaque lifecycle observation and owned Handle; no
    revision-only witness is constructed. Pending canonical projection repair is
    permitted. This cannot authorize create/enable, runtime or target admission. *)
type t

(** Reconcile retained lifecycle receipt acknowledgements under the same owned
    recovery Handle. Checks the original exact observation before any completion,
    invokes the existing store acknowledgement owner for each unacknowledged
    receipt, then returns a fresh genuine observation only if session identity,
    lifecycle revision, status and admission remain unchanged. Receipt documents
    and Handle epochs may change. A stale or foreign original observation fails
    before invoking [complete]. The caller retains its target recovery fence;
    the callback completes only the original durable result, not lifecycle or
    runtime work. This observation grants no create, enable or launch authority.
    Expected store failures are mapped to protocol errors; cancellation propagates. *)
val reconcile_lifecycle_receipts
  :  Agent_store.Session_store.Lifecycle.Observation.t
  -> store:Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> complete:
       (key:Agent_store.Idempotency_store.Key.t
        -> request_digest:string
        -> Agent_store.Session_archive_record.Outcome.t
        -> (unit, Agent_store.Store_error.t) result)
  -> (Agent_store.Session_store.Lifecycle.Observation.t, Agent_protocol.Error.t) result

val prepare
  :  Session_state.t
  -> store:Agent_store.Session_store.t
  -> handle:Agent_store.Session_store.Handle.t
  -> observation:Agent_store.Session_store.Lifecycle.Observation.t
  -> (t, Agent_protocol.Error.t) result

val delta : t -> Session_delta.t option

(** Re-read exact lifecycle authority before recovery planning/commit. Changes or
    corrupt/unavailable ownership fail closed; cancellation remains propagated. *)
val check : t -> (unit, Agent_protocol.Error.t) result
