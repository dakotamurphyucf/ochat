open! Core

module Encoder : sig
  (** Version of the adapter's model-facing encoding, including count coverage. *)
  type t = Openai_responses_v1 [@@deriving equal, sexp_of]
end

module Policy : sig
  type model_limit =
    | Undeclared
    | Unbounded
    | Bounded of
        { total : int64
        ; input : int64 option
        ; output : int64 option
        }

  type allowance =
    { host_total : int64 option
    ; session_total : int64 option
    ; omitted_output_default : int64 option
    ; checkpoint_reserve : int64
    ; warning_lead : int64
    }

  type estimate =
    { estimator : Inference.Observation.Estimator.t
    ; margin : int64
    }

  type t

  type error =
    | Invalid_limits
    | Invalid_reservations
  [@@deriving equal, sexp_of]

  (** Trusted host/profile qualification for the exact immutable target and
      encoder. Token declarations are positive. Optional input/output limits
      refine the total; absence declares no additional model constraint.
      The output default only applies to actual omission. Checkpoint and warning
      reserves leave positive ordinary capacity for a bounded declaration. *)
  val create
    :  target:Inference.Request.Target.t
    -> encoder:Encoder.t
    -> model_limit:model_limit
    -> allowance:allowance
    -> estimate:estimate option
    -> (t, error) Result.t

  val target : t -> Inference.Request.Target.t
end

module Source : sig
  type incomplete =
    | Image
    | Document
    | Opaque_input
    | Unknown_wire_shape
    | Counter_limit
    | Counter_unavailable
  [@@deriving equal, sexp_of]

  type reading =
    | Covered of Inference.Observation.Count.t
    | Incomplete of incomplete

  type t

  (** Trusted adapter contribution from the same immutable finalized payload
      used for dispatch. [Covered] asserts full material input coverage. Use
      Actual only for a qualified exact preflight counter, never billable or
      historical provider usage. The callback is pure, bounded and non-yielding.
      The first completed reading is retained for this source, independent of
      policy. Concurrent cache misses may duplicate bounded computation. *)
  val create : encoder:Encoder.t -> read:(unit -> reading) -> t
end

module Assessment : sig
  type unavailable =
    | Policy_missing
    | Target_changed
    | Encoder_changed
    | Source_missing
    | Limit_undeclared
    | Limit_unbounded
    | Output_unresolved
    | Output_outside_limit
    | Reserves_exhaust_limit
    | Coverage_incomplete of Source.incomplete
    | Estimate_unqualified
  [@@deriving equal, sexp_of]

  type evidence =
    | Measured of int64
    | Estimated of Inference.Observation.Context_estimate.t

  type pressure =
    | Below_warning
    | Near_limit
    | Checkpoint_due
    | Hard_limit_exceeded
  [@@deriving equal, sexp_of]

  type remaining =
    | Available of int64
    | Exceeded_by of int64
    | Excess_out_of_range
  [@@deriving equal, sexp_of]

  type view =
    | Unavailable of unavailable
    | Assessed of
        { evidence : evidence
        ; total : int64
        ; output : int64
        ; hard_input : int64
        ; soft_input : int64
        ; warn_input : int64
        ; margin : int64
        ; remaining : remaining
        ; pressure : pressure
        }

  type t

  (** Safe labels and numeric evidence only. No request, private target,
      fingerprint or provider usage are retained in this value. *)
  val preparation_id : t -> string

  val view : t -> view
end

module Binding : sig
  type t

  (** Trusted runtime binding. A new preparation gets its own private
      non-serializable identity, independent of the public observation ID. *)
  val create
    :  target:Inference.Request.Target.t
    -> configuration:Inference.Observation.Configuration.t
    -> source:Source.t option
    -> t

  val assess : ?policy:Policy.t -> t -> Assessment.t

  (** This check is local evidence. The host must still verify that the
      preparation and policy are current for the intended dispatch. *)
  val matches : ?policy:Policy.t -> t -> Assessment.t -> bool
end
