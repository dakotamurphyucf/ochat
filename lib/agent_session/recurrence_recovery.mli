(** Recovery-only safe retirement under the established store recovery owner.
    Retains the genuine opaque lifecycle observation and owned Handle; no
    revision-only witness is constructed. Pending canonical projection repair is
    permitted. This cannot authorize create/enable, runtime or target admission. *)
type t

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
