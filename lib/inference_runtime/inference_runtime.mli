open! Core

(** Effectful inference boundary. A prepared value owns its actual adapter and
    host credential resolver; callers cannot pair it with another backend or
    account at dispatch. No canonical history or tool execution lives here. *)
module Preparation_error : sig
  type t =
    | Invalid_request of Inference.Request.Error.t
    | Target_mismatch
    | Target_unavailable
    | Target_denied
    | Reauthorization_required
    | Unsupported_input
    | Unsupported_setting
    | Incompatible_replay
    | Asset_unavailable
    | Invalid_preparation
    | Request_limit of Document_schema.Error.t
  [@@deriving equal, sexp_of]
end

module Contract_error : sig
  type t =
    | Scope_mismatch
    | Accounting_identity_mismatch
    | Configuration_mismatch
    | Conflicting_candidate
    | Missing_candidate
    | Invalid_candidate
    | Invalid_usage
    | Conflicting_usage
    | Backend_terminal
    | Delivery_regression
    | Evidence_limit
  [@@deriving equal, sexp_of]
end

(** A trusted adapter violated its declared contract. Contains no provider body,
    exception message, endpoint or request data. This is not a transport retry
    signal. Strict caller callback exceptions propagate as their original values
    and backtraces, rather than being converted to this exception. *)
exception Contract_violation of Contract_error.t

module Limits : sig
  type t

  (** Positive bounds. Byte limits count encoded evidence, not OCaml heap size.
      A candidate is admitted under [event_limits] before publication; retained
      candidates additionally share [max_evidence_bytes]. *)
  val create
    :  event_limits:Document_schema.Limits.t
    -> max_candidates:int
    -> max_evidence_bytes:int
    -> (t, Preparation_error.t) Result.t

  (** 16 MiB per event, 64 MiB retained candidate evidence, 100,000 candidates. *)
  val default : t

  val event_limits : t -> Document_schema.Limits.t
  val max_candidates : t -> int
  val max_evidence_bytes : t -> int
end

module Receipt : sig
  type output_coverage =
    | Response_output
    | Observed_prefix
  [@@deriving equal, sexp_of]

  type t

  (** Trusted adapter construction. Output contains only unique Candidate_ready
      events in actual response order, with the terminal's full scope. Usage is
      one authoritative Usage observation in that scope. Response_output means
      the complete supplied response array, including an incomplete/failed
      response; it does not imply successful generation. Observed_prefix retains
      only actually validated evidence when no complete response was received.
      No history commit or tool admission is asserted. *)
  val create
    :  terminal:Inference.Event.Terminal.t
    -> usage:Inference.Observation.t
    -> output:Inference.Event.t list
    -> output_coverage:output_coverage
    -> limits:Limits.t
    -> (t, Contract_error.t) Result.t

  val terminal : t -> Inference.Event.Terminal.t
  val usage : t -> Inference.Observation.t
  val output : t -> Inference.Event.t list
  val output_coverage : t -> output_coverage
end

module Plan : sig
  type t

  (** Trusted backend preparation. The closure captures the immutable lowered
      request, selected driver and exact host auth resolver. Its request is
      private. Configuration is the safe projection of this request's target,
      with the actual opaque preparation identity, transport and declarations.
      Fingerprint is private admission evidence, never public configuration.

      [run] emits only provisional Live/Candidate_ready events. It returns the
      terminal receipt; the outer Attempt owns final usage and terminal callback
      publication. [note_delivery] records conservative monotonic progress, with
      Response_started requiring actual response evidence. Missing progress is
      conservatively Possibly_submitted once backend execution begins.

      Expected auth/transport/provider failures return a truthful receipt.
      Unexpected exceptions, Eio cancellation and observer failures propagate.
      The closure performs no retry, transport fallback or interactive login.
      It delivers callbacks serially in this attempt's dynamic lifetime; none may
      escape after return. Concurrent or detached callbacks violate ownership. *)
  val create
    :  request:Inference.Request.t
    -> configuration:Inference.Observation.Configuration.t
    -> fingerprint:string
    -> run:
         (sw:Eio.Switch.t
          -> scope:Transcript.Scope.t
          -> accounting_id:Inference.Observation.Observation_id.t
          -> note_delivery:(Inference.Event.Terminal.delivery -> unit)
          -> on_event:(Inference.Event.t -> unit)
          -> on_observation:(Inference.Observation.t -> unit)
          -> Receipt.t)
    -> (t, Preparation_error.t) Result.t
end

module Adapter : sig
  type t

  (** Trusted composition-root registration, not an ambient global registry.
      Binding/preparation perform no credential lookup or provider I/O. [bind]
      checks the actual captured profile/account/endpoint/revision against host
      policy. [prepare] returns a plan bound to this exact request and identity. *)
  val create
    :  id:string
    -> limits:Limits.t
    -> bind:(Inference.Request.Target.t -> (unit, Preparation_error.t) Result.t)
    -> prepare:
         (preparation_id:string
          -> Inference.Request.t
          -> (Plan.t, Preparation_error.t) Result.t)
    -> (t, Preparation_error.t) Result.t
end

module Attempt : sig
  type t
  type run_error = Already_started [@@deriving equal, sexp_of]

  val scope : t -> Transcript.Scope.t
  val accounting_id : t -> Inference.Observation.Observation_id.t
  val configuration : t -> Inference.Observation.Configuration.t

  (** Conservative delivery evidence remains inspectable after cancellation or
      observer failure. A normal receipt may establish definitely-not-submitted
      more precisely than the conservative state during execution. *)
  val delivery : t -> Inference.Event.Terminal.delivery

  (** Single use, under the caller's switch and cancellation context. No lazy
      stream escapes. Final receipt candidates reconcile exact prior evidence;
      unseen terminal-only candidates publish before final usage and exactly one
      matching terminal. Duplicate callback/receipt evidence never represents a
      second call. Callback failures retain their exception and backtrace.
      A normal return's usage is the same ID/revision/value published to the
      observer, not another consumption increment. No local Source_finished,
      canonical commit, tool effect or host-turn completion occurs here. *)
  val run
    :  t
    -> sw:Eio.Switch.t
    -> on_event:(Inference.Event.t -> unit)
    -> on_observation:(Inference.Observation.t -> unit)
    -> (Receipt.t, run_error) Result.t
end

module Prepared : sig
  type t

  val target : t -> Inference.Request.Target.t
  val preparation_id : t -> string
  val configuration : t -> Inference.Observation.Configuration.t
  val fingerprint : t -> string

  (** Local allocation only, before credential resolution or network effects.
      The trusted host supplies fresh actual scope/accounting identities and
      durably admits tracking before run. Reusing a scope across attempts is a
      host ownership error; this value is not a durable identity allocator.
      Repeated starts are explicit new attempts, never hidden retries. *)
  val start
    :  t
    -> scope:Transcript.Scope.t
    -> accounting_id:Inference.Observation.Observation_id.t
    -> (Attempt.t, Preparation_error.t) Result.t
end

module Context : sig
  type t

  val create
    :  Adapter.t
    -> target:Inference.Request.Target.t
    -> (t, Preparation_error.t) Result.t

  val target : t -> Inference.Request.Target.t

  (** The request must match this complete selected target. Final history,
      guidance, tools and immutable assets are already captured. A plan with a
      different request, preparation identity or safe configuration rejects. *)
  val prepare
    :  t
    -> preparation_id:string
    -> Inference.Request.t
    -> (Prepared.t, Preparation_error.t) Result.t

  (** Same adapter/profile/revision/account/endpoint; explicit model/settings
      edits only. Omitted child overrides inherit by making no edit. A different
      provider/account/endpoint requires an explicit host resolver/policy. *)
  val derive : t -> target:Inference.Request.Target.t -> (t, Preparation_error.t) Result.t
end

type resolver = Inference.Request.Target.t -> (Context.t, Preparation_error.t) Result.t
