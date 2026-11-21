(** Recheck a capacity cut against the exact prestate at admission and replay.
    Process-local worker, provider and credential checks remain with the actor. *)
open! Core

module P = Agent_protocol

val capture
  :  Session_state.t
  -> operation:P.Id.Operation.t
  -> trigger:Recovery_rollover_capacity.Trigger.t
  -> Chat_response.Capacity_rollover.stage
  -> (Recovery_rollover_capacity.t, P.Error.t) result

val validate : Session_state.t -> Recovery_rollover_capacity.t -> (unit, P.Error.t) result
