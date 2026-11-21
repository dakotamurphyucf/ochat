(** Opt-in, body-free estimate of one finalized OpenAI Responses request. *)
val encoder : Inference_runtime.Context_capacity.Encoder.t

val estimator : Inference.Observation.Estimator.t
val source : Responses_request.t -> Inference_runtime.Context_capacity.Source.t
