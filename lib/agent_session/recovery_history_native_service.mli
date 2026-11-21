(** Trusted host service for the physical foreground actor. Installing it does
    not select a tool or authorize invocation. *)
type delivery =
  { custody : Inference_runtime.Protected_delivery.custody
  ; materialize :
      unit
      -> ( History_entry.Payload.Output.t
           , Inference_runtime.Protected_delivery.error )
           result
  ; refresh : scope:Transcript.Scope.t -> unit -> bool
  }

type t =
  tool:Recovery_history_native_request.Tool.t
  -> Native_tool_invocation.Admission.t
  -> binding:Chat_response.Tool_capability.binding
  -> Jsonaf.t
  -> (delivery, Agent_protocol.Error.t) result
