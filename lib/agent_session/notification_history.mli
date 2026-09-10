(** Provider-compatible runtime data framing. The provider receives one supported
    user input message with fixed explanatory text and a versioned JSON envelope.
    Host provenance remains Runtime_notification in canonical history, never an
    inferred property of user-supplied text. Rendering does not mutate the retained
    completion or request another turn. *)

val create
  :  id:History_entry.Id.t
  -> Agent_protocol.Delivery.t
  -> (Agent_protocol.History.entry, Agent_protocol.Error.t) result

(** Verify exact framing, result/correlation references, provenance and disclosure
    state against the delivery. Redacted projections cannot be reused as canonical
    provider input. Identity/acknowledgement/commit ownership checks remain with the
    actor's publication transaction. *)
val validate
  :  delivery:Agent_protocol.Delivery.t
  -> Agent_protocol.History.entry
  -> (unit, Agent_protocol.Error.t) result
