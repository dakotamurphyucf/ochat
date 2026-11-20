(** Reconstruct a settled root from durable evidence in the exact prestate. *)
open! Core

module P = Agent_protocol

val capture
  :  Session_state.t
  -> scope:P.Recovery_window.Scope.t
  -> operation:P.Id.Operation.t
  -> Chat_response.Settled_tool_batch.t
  -> (Recovery_rollover_batch.t, P.Error.t) result

val validate
  :  Session_state.t
  -> scope:P.Recovery_window.Scope.t
  -> requester:Recovery_rollover.Requester.t
  -> Recovery_rollover_batch.t
  -> (P.History.entry list option, P.Error.t) result
