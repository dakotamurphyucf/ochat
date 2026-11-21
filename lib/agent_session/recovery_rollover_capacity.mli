(** Body-free durable evidence for a foreground cut with no tool batch. The
    actor authenticates the original worker before this value is admitted. *)
open! Core

module P = Agent_protocol
module Trigger = Chat_response.Capacity_rollover.Trigger

type boundary =
  | Unsent
  | Rejected
[@@deriving equal, sexp_of]

type t = private
  { operation : P.Id.Operation.t
  ; trigger : Trigger.t
  ; boundary : boundary
  ; attempt_high_water : int64
  ; configuration_revision : int64
  ; configuration : (Inference.Observation.Configuration.t[@sexp.opaque])
  ; history : Chatml_source.Digest.t
  }
[@@deriving equal, sexp_of]

val create
  :  operation:P.Id.Operation.t
  -> trigger:Trigger.t
  -> boundary:boundary
  -> attempt_high_water:int64
  -> configuration_revision:int64
  -> configuration:Inference.Observation.Configuration.t
  -> history:Chatml_source.Digest.t
  -> (t, P.Error.t) result

val trigger_name : Trigger.t -> string
val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, P.Error.t) result
