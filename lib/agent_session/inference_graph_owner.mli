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

module Registration : sig
  type t

  (** Original initial preparation captured by the actual canonical registration
      owner. Immutable custody only, never current authorization. *)
  val initial_preparation : t -> Run_preparation.t option

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

  (** Live execution registrations only. *)
  val iter_registrations : t -> f:(Registration.t -> unit) -> unit

  val iter_retained_registrations : t -> f:(Registration.t -> unit) -> unit

  (** Positive release ACK only. Keep joined exact membership only for a retained
      canonical row; drop the live Attempt and retire submission policy. *)
  val release : t -> ledger:Inference_ledger.t -> ordinal:int64 -> unit

  (** After positive complete State installation only. Drop joined membership
      whose exact row retired; live registrations retain their existing lifetime.
      Joined growth is bounded by actual canonical retained rows. *)
  val retain_joined : t -> Inference_ledger.t -> unit

  val seal : t -> unit
  val finish : t -> unit
end
