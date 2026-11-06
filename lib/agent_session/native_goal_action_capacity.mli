(** Capacity-only native action growth from the actual complete State's
    Invocation subtrees. Uses the same shape/retained-field merge as State;
    maximum across reachable phases, additive across distinct Invocation IDs.
    Byte/field/node reservations and exact host depth are bounded by [limits].
    Probes never become domain values or runtime authority. The State owner must
    subtract this growth alongside its other reservations and admit the complete
    original document under the remaining profile. *)
val growth
  :  invocations:Agent_protocol.Invocation.t list
  -> Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> (Capacity_growth.t, Document_schema.Error.t) result
