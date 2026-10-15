(** Pure bounded transition policy for one admitted developer inference row.
    Physical Claim, actor, Store CAS and callback authority remain outside. *)
open! Core

type disposition =
  | Changed of Inference.Observation.Attempt_record.t
  | Unchanged

type error =
  | Conflict
  | Limit_exceeded

val started : Inference.Observation.Attempt_record.t -> (disposition, error) result

val observed
  :  Inference.Observation.Attempt_record.t
  -> Inference.Observation.t
  -> (disposition, error) result

val finished
  :  Inference.Observation.Attempt_record.t
  -> Inference.Event.Terminal.t
  -> (disposition, error) result

val interrupted
  :  Inference.Observation.Attempt_record.t
  -> reason:Inference.Observation.Attempt_record.interruption
  -> delivery:Inference.Event.Terminal.delivery
  -> (disposition, error) result
