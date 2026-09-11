open Core

(** Shared identity-safe decoding for runtime restoration and delegated authority.
    This reads persisted identity/state; it grants no manager or execution lease. *)
val decode
  :  Jsonaf.t option
  -> (Session.Moderator_state.Identity_snapshot.t option, Agent_protocol.Error.t) result

val observer
  :  Jsonaf.t option
  -> (Agent_protocol.Invocation.observer option, Agent_protocol.Error.t) result

val is_halted : Jsonaf.t option -> (bool, Agent_protocol.Error.t) result
