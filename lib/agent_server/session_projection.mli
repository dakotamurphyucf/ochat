(** Private unchanged durable metadata and scheduling projections from the actual
    native session state. No state or authority is admitted by these pure views. *)
val metadata : Agent_session.Session_state.t -> Agent_store.Session_store.Metadata.t

val index_entry : Agent_session.Session_state.t -> Agent_store.Session_index.Entry.t
