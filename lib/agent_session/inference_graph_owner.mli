open! Core

(** One actual constructed resource graph. This token is process local and has
    no codec or inference authority. Its issuing actor checks physical membership
    before every allocation and acknowledgement. *)
type t

module Phase : sig
  type t =
    | Open
    | Sealed
    | Finished
end

module Factual_usage : sig
  type t

  module Correction_outcome : sig
    type t =
      | Settled
      | Awaiting_initial_confirmation
  end

  module Drain_outcome : sig
    type t =
      | Settled
      | Deferred
  end

  module Host : sig
    (** Prebuild before canonical writer. confirm_initial owns SAME actual Plan,
        canonical ACK/Set/host and exact initial receipts, retaining the complete
        confirmed vector before success. Neither procedure calls an all-facts
        drain, admission port, provider, nor submission/current-policy guard.
        correct_usage maps origin's wait-v3 to the distinct Awaiting outcome. *)
    val create
      :  confirm_initial:(unit -> (unit, Agent_protocol.Error.t) result)
      -> correct_usage:(unit -> (Correction_outcome.t, Agent_protocol.Error.t) result)
      -> t

    (** Positive canonical ledger/usage State ACK only. Nonallocating assignment
        of the already-installed monotone ledger revision, not metadata ACK. *)
    val invalidate : t -> ledger_revision:int64 -> unit

    (** Domain first. Includes unconfirmed/running initial duty AND dirty/running
        correction. Neither Deferred nor failed election satisfies cleanliness. *)
    val pending : t -> (bool, Agent_protocol.Error.t) result

    (** Domain-first real initial completion, independent of dirty corrections. *)
    val initial_confirmed : t -> (bool, Agent_protocol.Error.t) result
  end

  (** Elect/join SAME initial procedure. True success means full exact initial
      ACK vector retained. Failure/cancellation resolves all followers with same
      primary Error or exception/backtrace, restores retryable Idle duty, and
      retains actual receipts; later progress invokes only lawful exact
      retry/reconciliation, never another H or entered unknown writer. *)
  val confirm_initial : t -> (unit, Agent_protocol.Error.t) result

  (** Initial must already be confirmed; otherwise returns Deferred immediately,
      never recursively calls initial/global drain or joins a correction that is
      waiting on this initial. Settled clears ONLY captured dirty revision if
      unchanged across await. Awaiting/newer frontier keeps dirty and Deferred. *)
  val settle_correction : t -> (Drain_outcome.t, Agent_protocol.Error.t) result
end

module Registration : sig
  type t

  (** Original initial preparation captured by the actual canonical registration
      owner. Immutable custody only, never current authorization. *)
  val initial_preparation : t -> Run_preparation.t option

  (** Immutable facts custody on this exact native registration. Neither pending
      nor settled facts authorize native callbacks without the original row. *)
  val factual_usage : t -> Factual_usage.t option

  val handle : t -> Inference_ledger.Handle.t
  val configuration : t -> Inference.Observation.Configuration.t
  val attempt : t -> Inference_runtime.Attempt.t option
  val acknowledge : t -> Inference_runtime.Attempt.t -> unit
  val goal_id : t -> Agent_protocol.Id.Goal.t option
  val policy_is_current : t -> bool
  val invalidate_policy : t -> unit
end

(** State access for the existing serialized graph owner. These operations do
    not authenticate an issuer, allocate a ledger row or grant provider use.
    The session actor alone installs a token in its existing membership list. *)
module Host : sig
  type nonrec owner = t

  val create
    :  issuing_actor:unit ref
    -> source:Transcript.Source_id.t
    -> generation:int
    -> qualified_source:Transcript.Source_id.t
    -> t

  val issuing_actor : t -> unit ref
  val source : t -> Transcript.Source_id.t
  val generation : t -> int
  val phase : t -> Phase.t
  val qualified_source : t -> Transcript.Source_id.t option

  (** Live execution registration only. Joined membership cannot authorize
      provider refresh, acknowledgement or a second release. *)
  val registration : t -> ordinal:int64 -> Registration.t option

  (** Exact original opaque Handle membership, live or positively joined.
      Restricted to factual observation and settlement by the actual actor. *)
  val retained_registration : t -> Inference_ledger.Handle.t -> Registration.t option

  val register
    :  t
    -> initial_preparation:Run_preparation.t option
    -> handle:Inference_ledger.Handle.t
    -> configuration:Inference.Observation.Configuration.t
    -> goal_id:Agent_protocol.Id.Goal.t option
    -> unit

  module Prepared_registration : sig
    type t

    (** SAME prebuilt new record, retained after preparation maps are consumed.
        Immutable custody only; neither this reference nor its getter proves
        canonical installation or grants native/provider use. *)
    val registration : t -> Registration.t

    (** SAME original map/qualified source and Open phase, before writer. *)
    val check_basis : t -> (unit, Agent_protocol.Error.t) result

    (** Private exact-State-ACK hook, one-shot assignment only. Candidate pruning
        is already composed; never recheck the old map after own ACK pruning.
        Actor must prevent later common pruning from dropping the new facts. *)
    val install : t -> unit
  end

  (** NEW governed canonical Plan only. Prebuild actual registration, facts Some,
      candidate-ledger prune+insert map and qualified source before writer. Keep
      all existing fact cells through prospective ACK invalidation, even if clean
      and their native row retires. Ordinary register stays unchanged. *)
  val prepare_registration
    :  t
    -> ledger_after:Inference_ledger.t
    -> initial_preparation:Run_preparation.t option
    -> handle:Inference_ledger.Handle.t
    -> configuration:Inference.Observation.Configuration.t
    -> goal_id:Agent_protocol.Id.Goal.t option
    -> factual_usage:Factual_usage.t
    -> (Prepared_registration.t, Agent_protocol.Error.t) result

  module Native_join : sig
    type t
    type acknowledgement

    (** Sealed owner without prior native ACK. Prebuild post-join map, SAME
        registrations to join and immutable receipt before native writer. Keep
        all fact cells through own ACK invalidation. No native ledger mutation
        or lexical callback/provider join is performed/proved here. *)
    val prepare
      :  owner
      -> ledger_after:Inference_ledger.t
      -> (t, Agent_protocol.Error.t) result

    val check_basis : t -> (unit, Agent_protocol.Error.t) result

    (** Private changed/unchanged native State-ACK hook: join SAME records and
        assign prebuilt map/Some receipt once, retaining owner for external facts.
        No old-map recheck after own pruning; no map/list/closure allocation. *)
    val install : t -> unit

    (** Prebuilt receipt; actual install evidence requires SAME current owner ACK. *)
    val acknowledgement : t -> acknowledgement

    (** Domain first. SAME installed identity on retry, including Finished,
        never another native writer. The retained identity has no owner/map or
        callback reference and cannot reinstall registrations. *)
    val current : owner -> (acknowledgement option, Agent_protocol.Error.t) result

    (** Domain and SAME actually installed owner-held identity, including after
        final clear. Rejects alien or merely prebuilt/uninstalled identities. *)
    val check_acknowledgement
      :  owner
      -> acknowledgement
      -> (unit, Agent_protocol.Error.t) result
  end

  (** Live execution registrations only. *)
  val iter_registrations : t -> f:(Registration.t -> unit) -> unit

  val iter_retained_registrations : t -> f:(Registration.t -> unit) -> unit
  val iter_factual_usage : t -> f:(Factual_usage.t -> unit) -> unit

  (** Positive release ACK only. Keep joined exact membership only for a retained
      canonical row or unresolved facts; drop live Attempt and retire policy. *)
  val release : t -> ledger:Inference_ledger.t -> ordinal:int64 -> unit

  (** After positive complete State installation only. Drop joined membership
      whose exact row retired only after initial/factual duties settle. Actor
      invalidates affected facts before this ACK pruning. Live registrations
      retain their existing lifetime; unresolved facts block fresh admission. *)
  val retain_joined : t -> Inference_ledger.t -> unit

  val seal : t -> unit

  (** Actual final-clear hook only after native ACK, true lexical joins and all
      facts settled. Clears registration map/callbacks, retaining only the
      installed opaque unit-ref identity for exact idempotent reentry. Old
      receipts/plans cannot reinstall/rejoin a Finished owner. *)
  val finish : t -> unit
end
