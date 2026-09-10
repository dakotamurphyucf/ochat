(** Capture the admitted standalone tool's original result policy and stable
    permission identities without execution or resource reads. *)
val capture
  :  prepared:Chat_response.Extension_compiler.t
  -> current_capabilities:Chat_response.Tool_capability.t
  -> (Agent_protocol.Completion_contract.t, Agent_protocol.Error.t) result

(** Rebind the publisher and its exact dependency ceiling after reload. A
    same-name changed implementation or wider current registry grants nothing. *)
val rebind
  :  Agent_protocol.Completion_contract.t
  -> current_capabilities:Chat_response.Tool_capability.t
  -> (Chat_response.Tool_capability.t, Agent_protocol.Error.t) result

(** Validate eventual data using the saved policy, returning a bounded error
    without including rejected business data. Does not mutate a job or create a
    notification; the host must retain independent delivery evidence. *)
val validate_result
  :  Agent_protocol.Completion_contract.t
  -> Agent_protocol.Completion.t
  -> (unit, Agent_protocol.Invocation.tool_error) result

(** A checked projection of a published standalone Pending call's owned terminal
    job. Rejected data produces only the fixed public failure; the original job
    result remains unchanged. Does not admit a delivery or wake the model. *)
type projection = private
  { receipt : Agent_protocol.Completion_projection.t
  ; completion : Agent_protocol.Completion.t
  ; disclosure_pins : (string * string) list
  }

(** Requires current publisher/dependency authority and an exact materialization
    of the retained job result, including artifact digest verification. *)
val project
  :  invocation:Agent_protocol.Invocation.t
  -> job:Agent_protocol.Job.t
  -> completion:Agent_protocol.Completion.t
  -> current_capabilities:Chat_response.Tool_capability.t
  -> (projection, Agent_protocol.Error.t) result

(** Pure replay validation of owner, attempt, contract, original result digest,
    disclosure pins and projected output. Inline rejections are recomputed;
    artifact rejections bind the descriptor verified at admission. This is not a
    substitute for [project] when admitting a new delivery. *)
val validate_projection
  :  invocation:Agent_protocol.Invocation.t
  -> job:Agent_protocol.Job.t
  -> Agent_protocol.Delivery.t
  -> (unit, Agent_protocol.Error.t) result
