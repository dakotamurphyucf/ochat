open! Core
module Capacity = Inference_runtime.Context_capacity

module Mode : sig
  type t =
    | Manual
    | Automatic
  [@@deriving equal, sexp_of]
end

module Host : sig
  type t
  type error = Duplicate_target [@@deriving equal, sexp_of]

  (** Startup default. No session policy is selected and no count runs. *)
  val off : t

  (** Exact immutable targets and encoder policies supplied by the owning host.
      [mode_for_session] is a nonyielding lookup for the full durable session
      identity. None leaves that session disabled. [automatic] attests that the
      host separately qualified the checkpoint and fallback for this policy. *)
  val create
    :  mode_for_session:(Agent_protocol.Session_ref.t -> Mode.t option)
    -> targets:(Capacity.Policy.t * [ `Manual | `Automatic ]) list
    -> (t, error) Result.t
end

module Selection : sig
  type t

  (** Only an actual durable, recovery-capable host may select a mode. A
      requested Automatic mode falls back to Manual without an exact automatic
      qualification; absent numeric policy yields unavailable evidence. *)
  val select
    :  Host.t
    -> owner:Agent_protocol.Session_ref.t
    -> target:Inference.Request.Target.t
    -> t option

  val policy : t -> Capacity.Policy.t option
  val equal : t -> t -> bool
end

type availability = private
  { requested : Mode.t
  ; effective : Mode.t
  }

val availability : Selection.t -> availability

type status =
  | Disabled
  | Awaiting_preparation of availability
  | Stale_preparation of availability
  | Current of
      { availability : availability
      ; phase : Agent_protocol.Session_configuration.phase
      ; configuration : Inference.Observation.Configuration.t
      ; assessment : Capacity.Assessment.t
      }

module Port : sig
  type t

  (** The owning actor binds this read to the exact operation. This port
      returns observation data and grants no execution or rollover authority. *)
  val create : read:(unit -> (status, Agent_protocol.Error.t) Result.t) -> t

  val read : t -> (status, Agent_protocol.Error.t) Result.t
end
