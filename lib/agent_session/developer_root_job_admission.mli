open! Core

(** Pure construction of a fresh developer-owned background job from an already
    validated, reconstructible request. This module cannot reserve capacity,
    persist a job, run it, or authorize its captured source and selected tools.
    The physical root owner performs those checks before publication. The
    conversation [Script_job_service] keeps its existing owner and behavior;
    its later owner-neutral adapter may call this constructor. *)

module Prepared : sig
  type t

  val job : t -> Agent_protocol.Developer_job.t
end

(** Decode the request's canonical bytes under the selected host [policy] so
    stored resource limits cannot exceed that ceiling. Check its captured
    capability pins against the exact selected registry, then construct a fresh
    [Async_tool] job with the supplied validated launch, attempt zero, queued
    status and the existing [Never] retry policy. The root action supplies
    [Run_root] without a parent; this terminal coordinator's native scope
    supplies [Invocation] naming its exact durable Dispatching record with no
    parent job. A later live job-parent adapter must prove any claimed parent
    before constructing/publishing such a launch. The pure constructor grants
    no parent authority. [request] is a typed [Background_request], never arbitrary
    caller JSON. It may be Tool or Script; its source/artifact association must
    already have been admitted by the trusted host. The returned value is data,
    not a launch or capacity grant. Caller checkpoint exceptions propagate. *)
val background
  :  owner:Agent_protocol.Execution_owner.developer
  -> capture_fingerprint:Chatml_source.Digest.t
  -> launch:Agent_protocol.Developer_job.Launch.t
  -> selected:Chat_response.Tool_capability.t
  -> policy:Chat_response.One_off_request.policy
  -> request:Chat_response.Background_request.t
  -> created_at:Agent_protocol.Timestamp.t
  -> checkpoint:(unit -> unit)
  -> (Prepared.t, Agent_protocol.Error.t) result
