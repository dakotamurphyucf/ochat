(** Verify the exact committed history coverage of an authored checkpoint. *)
open! Core

module P = Agent_protocol

val capture_checkpoint
  :  Session_state.t
  -> scope:P.Recovery_window.Scope.t
  -> principal:P.Id.Principal.t
  -> operation:P.Id.Operation.t
  -> invocation:P.Invocation.t
  -> key:Chatml_source.Digest.t
  -> (Recovery_authored_checkpoint.t, P.Error.t) result

val check_checkpoint_registration
  :  Session_state.t
  -> Recovery_rollover.Checkpoint.t
  -> (unit, P.Error.t) result

val check_metadata
  :  Session_state.t
  -> scope:P.Recovery_window.Scope.t
  -> Recovery_authored_checkpoint.t
  -> (unit, P.Error.t) result

val check_checkpoint
  :  ?checkpoint_history:P.History.entry list
  -> Session_state.t
  -> Recovery_rollover.Checkpoint.t
  -> (unit, P.Error.t) result
