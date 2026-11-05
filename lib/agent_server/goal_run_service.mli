open! Core

(** Starts a new Goal episode through the actual Factory source lifetime and
    existing admitted-runtime consumers. This service owns no scheduler or
    separate executor. *)
type t

(** [authorize] is the host's current nonyielding policy for the original actor
    and actual session state. The existing command, source and runtime owners
    independently enforce their scopes, original authority and final checks. *)
val create
  :  factory:Session_factory.t
  -> authorize:
       (Operator_authorization.t
        -> Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> t

module Activation_result : sig
  (** These are result data, never execution permission. [Activated] acknowledges
      input or authored startup handoff, not Goal achievement or Run completion.
      [Activation_failed] retains known admission after a later failure; it does
      not promise rollback, no effects, a still-held Run or a fresh admission. *)
  type t =
    | Activated of Agent_protocol.Run_receipt.t
    | Retained of Agent_protocol.Run_receipt.t
    | Admission_uncertain of Agent_protocol.Error.t
    | Activation_failed of
        { admission : Agent_protocol.Run_receipt.t
        ; error : Agent_protocol.Error.t
        }
end

module Attempt : sig
  (** Wraps the sole actual Factory attempt after its source cleanup has joined.
      Negative callback results remain errors throughout that cleanup. *)
  type t

  (** Classify the original completed result using only admission facts learned
      from the actual Actor response. Pre-admission errors remain errors. *)
  val result : t -> (Activation_result.t, Agent_protocol.Error.t) result

  (** The actual Factory's joined observation, when available. Neither a missing
      receipt nor this service's result classification can create one. *)
  val no_effects : t -> Agent_session.Session_actor.No_effects_observation.t option
end

module Outcome : sig
  type t =
    | Retained of Agent_protocol.Run_receipt.t
    | Attempted of Attempt.t
end

(** Start only a new episode: the Goal has no episode or has an exact terminal
    predecessor. Resuming a live paused episode uses a separate control path.
    Goal/catalog revision intent and the original headless request remain exact;
    admission atomically records enablement, Run, episode and immutable link.

    Authenticate the actual Factory entry and exact retained Goal association
    before borrowing a runtime, so active workflows can return retained data.
    A retained receipt found later by the generic Factory is revalidated against
    the same Goal request; an ordinary Run receipt cannot substitute for it.
    Absence grants no retry or activation permission.

    Native, initialization and input work uses the existing neutral admitted
    consumer only after actual Goal admission. Callback errors cross full joined
    Factory cleanup before classification. Cancellation and unexpected exceptions
    propagate after factual cleanup; completed effects are not rolled back. *)
val start_episode
  :  t
  -> entry:Session_registry.entry
  -> actor:Operator_authorization.t
  -> session:Agent_protocol.Session_ref.t
  -> request:Agent_session.Goal_activation_request.t
  -> (Outcome.t, Agent_protocol.Error.t) result
