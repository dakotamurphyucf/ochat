(** The two no-tool-batch boundaries of a single actual foreground worker. *)
open! Core

module Trigger : sig
  type t =
    | Soft_input_limit
    | Hard_input_limit
    | Provider_context_limit
  [@@deriving compare, equal, sexp_of]
end

type rejection

(** Construct only after the original worker has observed an empty local
    candidate stream and the host accounting completion has returned. An exact
    HTTP rejection has no complete response array. A streamed rejection must
    attest its complete empty output. No provider output crosses this port. *)
val rejection
  :  attempt:Inference_runtime.Attempt.t
  -> receipt:Inference_runtime.Receipt.t
  -> rejection option

val attempt : rejection -> Inference_runtime.Attempt.t
val terminal : rejection -> Inference.Event.Terminal.t

type stage =
  | Unsent of Inference_runtime.Prepared.t
  | Rejected of rejection

val same_stage : stage -> stage -> bool

type pressure =
  | No_transition
  | Checkpoint_opportunity
  | Hard_limit
  | Qualified_rejection

(** Supplied to the exact foreground worker by its actor. Inspection has no
    rollover or submission effect. A Continue disposition never authorizes
    sending an oversized or already rejected request. *)
type t =
  { inspect : stage -> (pressure, Agent_protocol.Error.t) result
  ; request :
      trigger:Trigger.t
      -> stage:stage
      -> (Settled_tool_batch.decision, Agent_protocol.Error.t) result
  }
