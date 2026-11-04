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
  val registration : t -> ordinal:int64 -> Registration.t option

  val register
    :  t
    -> handle:Inference_ledger.Handle.t
    -> configuration:Inference.Observation.Configuration.t
    -> goal_id:Agent_protocol.Id.Goal.t option
    -> unit

  val iter_registrations : t -> f:(Registration.t -> unit) -> unit
  val release : t -> ordinal:int64 -> unit
  val seal : t -> unit
  val finish : t -> unit
end
