(** Trusted foreground port for one checkpoint-only preparation. Selection
    grants no tool authority. The original foreground dispatcher still owns
    execution and canonical call/result publication. *)
open! Core

module P = Agent_protocol
module R = Inference_runtime
module Preparation = Inference_client.Prepared_selection

type outcome =
  | Settled of Settled_tool_batch.pair
  | Ineligible
  | Interrupted

module Selected : sig
  type t =
    { candidate : Preparation.candidate
    ; for_attempt : R.Attempt.t -> R.Submission_guard.t
    ; approve_call : R.Attempt.t -> name:string -> arguments:string -> bool
    ; settle : R.Attempt.t -> outcome -> (unit, P.Error.t) result
    }

  val completed_call : t -> R.Attempt.t -> R.Receipt.t -> Inference.Event.t option
end

type tools =
  | Unselected
  | Register
  | Register_and_write
[@@deriving equal]

type select =
  tools:tools
  -> request:Inference.Request.t
  -> prepare_isolated:
       (Inference.Request.t -> (Preparation.candidate, R.Preparation_error.t) result)
  -> R.Prepared.t
  -> Selected.t option

(** Current ordinary tool moderation vetoed or altered the proposed effect. *)
exception Tool_refused
