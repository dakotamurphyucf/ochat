open! Core

(** Pure selection edits. IO/authorization/current profile checks belong to the
    host policy and actor commit. The planner never changes runtime/job authority. *)
val target
  :  Session_state.t
  -> (Inference.Request.Target.t, Agent_protocol.Error.t) Result.t

val apply
  :  Inference.Request.Target.t
  -> patch:Agent_protocol.Session_configuration.Patch.t
  -> profile_target:Inference.Request.Target.t option
  -> (Inference.Request.Target.t, Agent_protocol.Error.t) Result.t

val compatible_identity
  :  Inference.Request.Target.t
  -> proposed:Inference.Request.Target.t
  -> bool

val safe_view
  :  Inference.Request.Target.t
  -> (Inference.Observation.Configuration.t, Agent_protocol.Error.t) Result.t

val redact_identity
  :  Agent_protocol.Session_configuration.t
  -> (Agent_protocol.Session_configuration.t, Agent_protocol.Error.t) Result.t
