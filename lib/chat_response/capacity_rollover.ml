open! Core

module Trigger = struct
  type t =
    | Soft_input_limit
    | Hard_input_limit
    | Provider_context_limit
  [@@deriving compare, equal, sexp_of]
end

type rejection =
  { attempt : Inference_runtime.Attempt.t
  ; terminal : Inference.Event.Terminal.t
  }

let rejection ~attempt ~receipt =
  let module T = Inference.Event.Terminal in
  let module Receipt = Inference_runtime.Receipt in
  let terminal = Receipt.terminal receipt in
  if
    T.is_context_window_rejection terminal
    && Transcript.Scope.equal (Inference_runtime.Attempt.scope attempt) (T.scope terminal)
    && List.is_empty (Receipt.output receipt)
    &&
    match Receipt.output_coverage receipt, T.outcome terminal with
    | Response_output, _ | Observed_prefix, Failed (Transport Http_context_limit) -> true
    | Observed_prefix, _ -> false
  then Some { attempt; terminal }
  else None
;;

let attempt t = t.attempt
let terminal t = t.terminal

type stage =
  | Unsent of Inference_runtime.Prepared.t
  | Rejected of rejection

let same_stage a b =
  match a, b with
  | Unsent a, Unsent b -> phys_equal a b
  | Rejected a, Rejected b ->
    phys_equal a.attempt b.attempt && Inference.Event.Terminal.equal a.terminal b.terminal
  | (Unsent _ | Rejected _), _ -> false
;;

type pressure =
  | No_transition
  | Checkpoint_opportunity
  | Hard_limit
  | Qualified_rejection

type t =
  { inspect : stage -> (pressure, Agent_protocol.Error.t) result
  ; request :
      trigger:Trigger.t
      -> stage:stage
      -> (Settled_tool_batch.decision, Agent_protocol.Error.t) result
  }
