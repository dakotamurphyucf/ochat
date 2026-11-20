open! Core

(** Resolve each named startup qualification through the same immutable prompt
    capture and provider resolver used by the Factory. Missing, changed or
    unsupported targets contribute no numeric policy. The stock host requires a
    durable enabled window and only qualifies Manual until a checkpoint recipe
    and fallback have been independently qualified. *)
val create
  :  Recovery_capacity_configuration.t
  -> prompts:Agent_session.Prompt_catalog.t
  -> inference:Session_factory.inference_policy
  -> (Agent_session.Session_context_capacity.Host.t, Agent_protocol.Error.t) result
