open Core

(** Named-field nested checkpoint component. Its enclosing state/delta document
    owns conversion and the extension carrier using [shape]. *)
val encode : Session.Moderator_state.Identity_snapshot.t -> Jsonaf.t

val shape : Document_schema.Shape.t

(** Shared identity-safe decoding for runtime restoration and delegated authority.
    This reads persisted identity/state; it grants no manager or execution lease. *)
val decode
  :  Jsonaf.t option
  -> (Session.Moderator_state.Identity_snapshot.t option, Agent_protocol.Error.t) result

val observer
  :  Jsonaf.t option
  -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result

val is_halted : Jsonaf.t option -> (bool, Agent_protocol.Error.t) result

(** Optional durable occurrence metadata. Historical absent/null children remain
    unavailable; a present current child is validated strictly. *)
val queue : Jsonaf.t option -> (Moderator_queue.t option, Agent_protocol.Error.t) result

val encode_with_queue
  :  Session.Moderator_state.Identity_snapshot.t
  -> queue:Moderator_queue.t
  -> (Jsonaf.t, Agent_protocol.Error.t) result

(** Complete raw optional queue child bounds, including its unknown fields. *)
val validate_encoded_capacity : Jsonaf.t option -> (unit, Agent_protocol.Error.t) result

(** Existing receipt checkpoint digest over the complete known identity snapshot.
    This is a data comparison, not proof of an installed runtime or authority. *)
val digest : Session.Moderator_state.Identity_snapshot.t -> string
