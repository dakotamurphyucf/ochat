open! Core

(** Existing model services bound to one authentic admitting owner. A stale
    callback rejects; it never falls back to construction services. *)
type t =
  { spawn_model : recipe:string -> payload:Jsonaf.t -> (string, string) result
  ; call_model :
      recipe:string
      -> payload:Jsonaf.t
      -> execute:
           (inference_context:Inference_runtime.Context.t
            -> allocate_inference:Inference_allocation.Port.t
            -> before_inference_attempt:(Inference_runtime.Attempt.t -> unit)
            -> capture_recipe_target:
                 (Inference.Request.Target.t -> (unit, Agent_protocol.Error.t) result)
            -> (Chat_response.Moderation.Capabilities.model_call_result, string) result)
      -> (Chat_response.Moderation.Capabilities.model_call_result, string) result
  }

type factory =
  { initialization : t
  ; for_execution : Model_job_admission.t -> t
  }
