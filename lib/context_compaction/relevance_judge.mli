open! Core

(** Importance grading through the selected no-tool execution. Three samples,
    exact finite [0,1] parse, mean aggregation. Expected inference/scalar failure
    contributes the established 0.5 fallback; missing selection is a configuration
    error, never an offline guess. Cancellation/callback exceptions propagate. *)
val score_relevance
  :  inference:Inference_client.Execution.t
  -> Config.t
  -> prompt:string
  -> float

val is_relevant
  :  inference:Inference_client.Execution.t
  -> Config.t
  -> prompt:string
  -> bool

module For_testing : sig
  val score_samples
    :  sample:(unit -> (string, Inference_client.Execution.Completion_error.t) Result.t)
    -> float
end
