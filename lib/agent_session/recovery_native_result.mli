(** Select a body-free native result from its exact, still-active admission.
    The handler verifies its own source and recipient authority before selection
    and supplies fresh checks for the actual next model disclosure. *)
val acknowledge
  :  Native_tool_invocation.Admission.t
  -> binding:Chat_response.Tool_capability.binding
  -> (unit, Agent_protocol.Error.t) result

val deliver
  :  Native_tool_invocation.Admission.t
  -> binding:Chat_response.Tool_capability.binding
  -> materialize:
       (unit
        -> ( History_entry.Payload.Output.t
             , Inference_runtime.Protected_delivery.error )
             result)
  -> custody:Inference_runtime.Protected_delivery.custody
  -> refresh:(scope:Transcript.Scope.t -> unit -> bool)
  -> (unit, Agent_protocol.Error.t) result
