(** Shared exact retained-attempt and canonical call/result evidence. *)
open! Core

module P = Agent_protocol

val records : Session_state.t -> Recovery_effective_record.t list

val check_terminal
  :  Session_state.t
  -> operation:P.Id.Operation.t
  -> Recovery_effective_attempt.t
  -> (Inference_ledger.Handle.t * Inference.Event.Terminal.t, P.Error.t) result

val require_attempt
  :  Session_state.t
  -> scope:P.Recovery_window.Scope.t
  -> operation:P.Id.Operation.t
  -> ( Recovery_effective_record.t * Inference_ledger.Handle.t * Inference.Event.Terminal.t
       , P.Error.t )
       result

val semantics : P.History.entry -> (History_entry.Payload.Semantic.view, P.Error.t) result
val original : P.History.entry -> bool

val pair_is_bound
  :  call:P.History.entry
  -> output:P.History.entry
  -> (unit, P.Error.t) result

val published : Session_state.t -> Recovery_rollover_batch.pair -> P.Invocation.t -> bool
