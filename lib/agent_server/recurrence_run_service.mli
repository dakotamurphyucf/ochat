open! Core

(** One authenticated host activation through the existing Factory, target Actor,
    controller Actor and Runtime_owner. This service neither selects occurrences
    nor creates captured children, schedules work or loops over retry attempts. *)
type t

(** [authorize] is the composition root's current, nonyielding host visibility
    policy for the exact original actor and state, as in Run_start_service.
    The service additionally requires the actual Send_messages/read scopes and
    current original proof through the existing Actor command/observation ports.
    Source, semantic, shell, MCP, inference and lifecycle policies remain the
    actual Factory/Actor policies; this callback cannot grant their custody.

    The Factory alone supplies its actual registry/Store association. No separate
    server ID, registry, observer, Boolean activation gate or caller-created
    controller/target proof is accepted by service construction.

    [automatic_policy] is the existing lower settlement metadata policy, consulted
    only for a NEW definitive fixed-delay settlement after actual automatic/source/
    birth/control eligibility. It neither activates the target nor substitutes
    for either original proof. It must reflect the host's current configured
    recurrence policy and must not yield. It is retained for explicit recovery/
    terminal reconciliation; a constant denial must not silently consume terminal
    evidence that should receive a successor under the actual host policy. *)
val create
  :  factory:Session_factory.t
  -> authorize:
       (Operator_authorization.t
        -> Agent_session.Session_state.t
        -> (unit, Agent_protocol.Error.t) result)
  -> automatic_policy:
       (Agent_session.Session_state.t
        -> Agent_session.Recurrence_definition.t
        -> (bool, Agent_protocol.Error.t) result)
  -> t

module Retained : sig
  (** Known target Admission is preserved even when current controller
      reconciliation is denied or fails. The receipt is DATA already observed
      under current target authority. The exact import result preserves settlement
      and timing metadata; reconciliation never creates live custody. *)
  type t =
    { admission : Agent_protocol.Run_receipt.t
    ; reconciliation :
        (Agent_session.Session_actor.Recurrence_import.t, Agent_protocol.Error.t) result
    }
end

module Activation_result : sig
  (** These outcomes are DATA, not a new live owner. [Activated] means the real
      input-root or authored-start handoff ACK returned the original Admission;
      a Workflow can still be running. [Retained] never recreates Held custody.
      [Activation_failed] preserves known admission even when later activation failed;
      it makes no promise of rollback, absence of effects, still-held custody or
      synchronous terminal settlement. An acknowledged handoff can remain factual
      when a subsequent reply/cleanup fails. *)
  type t =
    | Activated of Agent_protocol.Run_receipt.t
    | Retained of Retained.t
    | Admission_uncertain of Agent_protocol.Error.t
    | Activation_failed of
        { admission : Agent_protocol.Run_receipt.t
        ; error : Agent_protocol.Error.t
        }
end

module Attempt : sig
  (** Immutable result wrapper created only AFTER the actual Factory CPS joins.
      It retains that sole actual Factory attempt and factual receipt/uncertainty
      learned from the real target response. It is not a second preparation,
      activation owner or proof issuer. *)
  type t

  (** Classify the completed primary result without changing the primary that
      crossed Factory cleanup. A real lower Error was returned as Error inside
      that CPS, including errors after known Admission and Admission uncertainty.
      Only here may known receipt turn that exact error into Activation_failed
      or Retained.reconciliation Error, or actual uncertainty into
      Admission_uncertain. A pre-admission error remains Error. *)
  val result : t -> (Activation_result.t, Agent_protocol.Error.t) result

  (** Delegate to the sole actual Factory attempt. A computed failure/receipt,
      missing lookup, exception or observed uncertainty cannot mint NoEffects. *)
  val no_effects : t -> Agent_session.Session_actor.No_effects_observation.t option

  (** Same actual Factory/Actor joined prewriter retry, never rebuilt from data. *)
  val retry : t -> Agent_session.Session_actor.Recurrence_retry.t option
end

module Outcome : sig
  (** Early retained lookup has no attempt envelope. Preserve both known receipt
      and subsequent reconciliation result. All attempted work retains its sole
      actual Factory envelope behind Attempt, never reconstructing eligibility. *)
  type t =
    | Retained of Retained.t
    | Attempted of Attempt.t
end

(** Validate both full Intent references against the actual registered entries
    and the Factory's actual Store/Actor/Owner association before source work.
    Foreign/unregistered or substituted Actor/Owner/Handle entries fail, including
    equal serialized IDs. Actual Factory-created linked children are supported.
    Stopped/recovered entries remain eligible for factual reconciliation; this
    association check adds no running/runtime eligibility policy.
    The same actual Actor selects self-target owned ACK methods; distinct Actors
    use the ordinary controller record/import methods. Lower exact-reference,
    same-Store, current policy, original and final source checks remain required.

    The initial request is reconstructed from [attempt] and [intent] unchanged.
    Before acquiring source/runtime custody, an actual target observation checks
    the frozen attempt principal, current Send_messages and transcript-read
    authority together. A retained admission reconciles from that SAME opaque
    observation without acquiring source/runtime custody, including while its
    Workflow is still active. Missing evidence grants no NoEffects proof and
    enters the unchanged fixed source lifetime.

    The fixed source lifetime records execution, admits the actual inert Held Run,
    imports its opaque target observation, obtains current reciprocal activation
    custody, then invokes the existing rich-input or authored-start consumer.
    No constructor, native setup, implicit Agent, initializer or input work occurs
    before those acknowledged owners. Existing-session input and both captured
    Single_turn/Workflow modes retain their existing support.

    Every lower domain error remains Error THROUGH the full Factory CPS, even
    after known Admission/retained lookup/uncertainty. Attempt.result classifies
    that error only AFTER joined cleanup using factual target response data.
    Factory's envelope preserves the exact primary Error and only its actual
    joined-close issuer can supply NoEffects. Rejected source work is not assumed
    effect-free. Unknown Admission remains [Admission_uncertain], never permission
    to retry. An early retained receipt returns [Outcome.Retained];
    a receipt discovered in the selected source callback can return inner Retained.
    Both reconcile actual origin/controller evidence without runtime replay and
    retain the known receipt beside a typed reconciliation failure. Neither error
    is reported as a fresh pre-admission rejection.

    All acquired permits/source scopes are released and joined under protected
    cleanup. Cancellation and unexpected exceptions propagate unchanged after
    factual cleanup; they do not become Rejected, NoEffects or activation success.
    Acknowledged transfer remains truthful if a later reply/cleanup is lost. *)
val activate
  :  t
  -> controller:Session_registry.entry
  -> controller_actor:Operator_authorization.t
  -> target:Session_registry.entry
  -> target_actor:Operator_authorization.t
  -> intent:Agent_session.Recurrence_execution_intent.t
  -> attempt:Agent_session.Recurrence_execution_attempt.t
  -> (Outcome.t, Agent_protocol.Error.t) result

(** Explicitly consume the real old NoEffects observation into one frozen next
    attempt through the existing Factory retry source lifetime. The controller
    must ACK the exact resolve+append before target Held admission can execute.
    Same-target bookkeeping advances only its actual preparation while preserving
    original CAS/key/digest. A normal prewriter source failure may return the same
    resumable opaque Retry through Factory's envelope; it never issues independent
    NEXT NoEffects. An attempted/unknown controller ACK cannot replay or reconstruct
    live custody, even if later decoded DATA reports its actual next row.
    This function attempts once; callers decide whether to resume explicitly. *)
val retry
  :  t
  -> controller:Session_registry.entry
  -> controller_actor:Operator_authorization.t
  -> target:Session_registry.entry
  -> target_actor:Operator_authorization.t
  -> observation:Agent_session.Session_actor.No_effects_observation.t
  -> intent:Agent_session.Recurrence_execution_intent.t
  -> previous:Agent_session.Recurrence_execution_attempt.t
  -> request:Agent_session.Run_admission_request.Headless.t
  -> (Attempt.t, Agent_protocol.Error.t) result

(** Re-enter only the same joined unpublished retry. Intent/next request come
    from that actual Actor owner; no replacement ordinal/key/CAS/principal/input
    is accepted. All source/authorization/rotation/activation and cleanup checks
    repeat. Frozen next-principal checks remain the Actor's existing contract. *)
val resume_retry
  :  t
  -> controller:Session_registry.entry
  -> controller_actor:Operator_authorization.t
  -> target:Session_registry.entry
  -> target_actor:Operator_authorization.t
  -> retry:Agent_session.Session_actor.Recurrence_retry.t
  -> (Attempt.t, Agent_protocol.Error.t) result

module Reconciliation : sig
  (** Actual target observation and controller import DATA only. [Absent] is a
      current lookup result, never a NoEffects/retry certificate. [Retained] is
      exact original receipt/origin evidence; it grants no startup/worker right. *)
  type t =
    | Absent
    | Retained of Retained.t
end

(** Reobserve the original immutable attempt through the live target Store,
    validate its retained origin and import through the ordinary controller port.
    Once a receipt is known, controller policy/import failure stays beside that
    receipt in [Retained.reconciliation], rather than erasing known admission.
    This is also the recovery/lost-reply path: never load the execution runtime,
    parse input, recreate Held, rotate a key or reissue NoEffects from absence.
    New definitive fixed-delay terminal evidence uses the configured existing
    settlement metadata policy and actual lower eligibility/paired completion
    evidence. It is not first imported with a fabricated historical-only choice:
    exact duplicate evidence can no longer reconsider that choice. Scheduler
    execution remains outside this service. Original visibility and both actual
    entry/Store checks still apply. *)
val reconcile
  :  t
  -> controller:Session_registry.entry
  -> controller_actor:Operator_authorization.t
  -> target:Session_registry.entry
  -> target_actor:Operator_authorization.t
  -> intent:Agent_session.Recurrence_execution_intent.t
  -> attempt:Agent_session.Recurrence_execution_attempt.t
  -> (Reconciliation.t, Agent_protocol.Error.t) result
