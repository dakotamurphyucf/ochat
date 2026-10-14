(** Pure bounded queued Model_call construction. The physical root/native owner
    supplies its already checked owner, capture, live launch and provider-issued
    nonsecret Target; this module grants none of those authorities. *)
open! Core

val create
  :  owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> launch:Agent_protocol.Developer_job.Launch.t
  -> request:Agent_protocol.Model_spawn_request.t
  -> target:Inference.Request.Target.t
  -> limits:Document_schema.Limits.t
  -> created_at:Agent_protocol.Timestamp.t
  -> (Agent_protocol.Developer_job.t, Agent_protocol.Error.t) result
