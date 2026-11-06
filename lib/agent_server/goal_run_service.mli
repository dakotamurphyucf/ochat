open! Core

(** Starts new Goal episodes and resumes existing episodes through the actual
    Factory source lifetime and shared runtime consumers. This service owns no
    scheduler or separate executor. *)
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
      not promise rollback, no effects, a still-held Run or a fresh admission.
      Settled retains the actual Admission and independently validated terminal
      episode receipt; it does not claim constructor installation or handoff. *)
  type t =
    | Activated of Agent_protocol.Run_receipt.t
    | Controlled of
        { admission : Agent_protocol.Run_receipt.t
        ; control : Agent_session.Session_actor.Goal_callback_control.t
        }
    | Settled of
        { admission : Agent_protocol.Run_receipt.t
        ; settlement : Agent_protocol.Goal_episode.Settlement.t
        }
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

module Resume_result = Agent_session.Session_actor.Goal_resume_result

(** Resume the same held nonterminal Goal episode. Authenticate the actual
    registered target and check its retained command receipt before borrowing a
    runtime, including while the owner is busy. A retained receipt is historical
    data; it cannot reconstruct runtime custody or replay execution.

    A fresh command uses the Factory's actual selected source, both the existing
    Run original and the new operator proof, and the shared runtime owner. An
    already Waiting Run preserves its exact wake without constructing a runtime.
    Loaded resume does not rerun initialization. Cold resume runs the genuine
    Session_resume lifecycle before the existing worker can start a continuation.

    Source scopes join before result classification. Rejected is a prewriter
    rejection; Uncertain means the first command acknowledgement is unknown.
    A known command acknowledgement followed by any expected construction,
    operation or cleanup failure returns Resume_failed with the exact receipt.
    Resumed describes an actual Started, Waiting or Settled outcome and never
    creates a second Run, Admission, or account. Entry/authentication failures
    before source association remain outer errors. A callback error or exception
    remains primary if cleanup also fails; cleanup failure replaces only a
    successful callback result. Primary cancellation and unexpected exceptions
    propagate with their original backtraces after factual cleanup. *)
val resume_episode
  :  t
  -> entry:Session_registry.entry
  -> actor:Operator_authorization.t
  -> session:Agent_protocol.Session_ref.t
  -> request:Agent_session.Goal_resume_request.t
  -> (Resume_result.t, Agent_protocol.Error.t) result

(** Finite native command consumer. The opaque selected Invocation owns its
    original host policy and same Admission loan. This function reserves the
    existing real source bracket before actual claim, and returns after joined
    handoff/cleanup rather than workflow completion. No actor reconstructed from
    principal data or callback borrow is accepted. *)
val resume_native_episode
  :  Session_factory.t
  -> entry:Session_registry.entry
  -> selection:Agent_session.Session_actor.Goal_native_resume.Selection.t
  -> (Resume_result.t, Agent_protocol.Error.t) result
