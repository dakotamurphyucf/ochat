(** Optional, request-only recovery material for a selected successor operation.
    The Factory owns each source read and its physical-delivery guard. *)
open! Core

module Reference = Recovery_rollover_hint_ref
module Guard = Inference_runtime.Submission_guard

type fragment

(** [text] is already authorized for this exact reference. Each fragment
    retains its source guard until the actual provider write. *)
val fragment
  :  reference:Reference.t
  -> text:string
  -> more:bool
  -> guard:Guard.t
  -> (fragment, Agent_protocol.Error.t) result

(** The callback captures the current successor operation, selected references
    and a delivery guard for the recipient. Source failures omit that reference.
    The complete frame has User priority and a 16 KiB limit after JSON escaping. *)
val create
  :  select:
       (unit
        -> ( Reference.Selection.t
             * Guard.t
             * (Reference.t -> (fragment, Agent_protocol.Error.t) result)
             , Agent_protocol.Error.t )
             result)
  -> Chat_response.Agent_guidance.t
