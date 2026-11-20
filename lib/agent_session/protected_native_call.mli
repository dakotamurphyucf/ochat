(** Process-local arguments and result selection for a single protected model
    invocation. Nothing here serializes or authorizes access to source data. *)
type result_selection =
  | Acknowledged
  | Deliver of
      (call:History_entry.t
       -> output:History_entry.t
       -> Inference_runtime.Protected_delivery.t)

val with_input
  :  invocation:Agent_protocol.Invocation.t
  -> binding:Chat_response.Tool_capability.binding
  -> input:Jsonaf.t
  -> (unit -> 'a)
  -> 'a * result_selection option

(** Available only during the matching live dispatch. *)
val input
  :  Agent_protocol.Host_invocation.context
  -> binding:Chat_response.Tool_capability.binding
  -> Jsonaf.t option

(** Called by the trusted native effect after its own current authorization
    check. Refuses a second selection or an expired invocation. *)
val select
  :  Agent_protocol.Invocation.t
  -> binding:Chat_response.Tool_capability.binding
  -> result_selection
  -> (unit, Agent_protocol.Error.t) result

val selected
  :  Agent_protocol.Host_invocation.context
  -> binding:Chat_response.Tool_capability.binding
  -> bool
