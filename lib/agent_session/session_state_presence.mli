(** Wire presence of the two optional run fields in an immutable named state
    template. This metadata never grants execution authority or changes native
    state. Rows are keyed by validated typed delivery IDs; duplicate identities
    fail. Capture is O(deliveries * log deliveries), queries O(log deliveries). *)
type t

val authored : t

val of_document
  :  Document_schema.Document.t
  -> limits:Document_schema.Limits.t
  -> (t, Document_schema.Error.t) result

val run_state_is_absent : t -> bool
val delivery_binding_is_absent : t -> Agent_protocol.Id.Delivery.t -> bool
