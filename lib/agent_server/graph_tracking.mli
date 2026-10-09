open! Core

(** Tracking for one actual constructed resource graph. It grants no execution
    authority and remains live for authorized borrowers until that graph excludes
    new work and joins. It is not tied to the actor's current worker or operation. *)
module Upstream : sig
  type t =
    { new_preparation_id : unit -> string
    ; on_admitted :
        scope:Transcript.Scope.t
        -> accounting_id:Inference.Observation.Observation_id.t
        -> unit
    ; on_attempt : Inference_runtime.Attempt.t -> unit
    ; on_observation : Inference.Observation.t -> unit
    ; on_completion : Inference_client.Completion.t -> unit
    }
end

(** Structured tracking failure from a strict callback; no request/provider body. *)
exception Rejected of Agent_protocol.Error.t

type t

(** Acquire before graph constructors/initializers. The actor qualifies this
    actual graph source with its session identity; one graph reuses one binding.
    Owner acknowledgement and binding construction are cancellation-protected.
    A cancelled caller can receive the binding, install its cleanup without
    yielding, and then observe the original cancellation at its next effect.
    All optional operation/invocation associations remain absent unless actual
    per-execution ownership is supplied by a future explicit port.

    [run_preparation] is explicit constructor custody. The constructor clears
    this shared cell on every exit before a runtime escapes; later callbacks use
    ordinary commits. Each mailbox request captures its current token and the
    actor revalidates it at commit, never substitutes a later preparation. *)
val create
  :  ?run_preparation:Agent_session.Run_preparation.t option ref
  -> Agent_session.Session_actor.t
  -> source:Transcript.Source_id.t
  -> upstream:Upstream.t
  -> (t, Agent_protocol.Error.t) Result.t

(** Sole durable allocator. Prepared admission, including an Untracked ordinal,
    is acknowledged before on_admitted and f. f runs exactly once and encloses
    scope validation, Prepared.start, acknowledgement, run and completion.
    Routing is removed on every exit, including pre-f observer failure. Cleanup
    preserves a primary exception/backtrace; normal cleanup failures propagate.
    Only actual in-flight executions occupy the map, not retained ledger rows. *)
val identity : t -> Inference_client.Identity.t

(** Tracking acknowledgement precedes the corresponding strict upstream callback.
    Exact late observations remain admissible through the actor's retained handle
    API; graph routing itself does not retain an unbounded late-event index. *)
val on_attempt : t -> Inference_runtime.Attempt.t -> unit

(** After routing release, only an exact retained row under this live owner may
    supply admission proof. Retired/untracked/unknown scopes reject rather than
    inventing a handle; explicitly held Handle callbacks use the actor API and
    can acknowledge a known no-charge ignored disposition. No late tombstones. *)
val on_observation : t -> Inference.Observation.t -> unit

val on_completion : t -> Inference_client.Completion.t -> unit

(** Seal only when the actual graph excludes new calls. Finish only after its
    work joins, while the actor and durable writer are live. Residual Prepared is
    definitely not submitted; Running is conservatively possibly submitted.
    Ordinary Stopped/worker detach does not seal a retained borrowed graph. *)
val seal : t -> (unit, Agent_protocol.Error.t) Result.t

val finish : t -> (unit, Agent_protocol.Error.t) Result.t
