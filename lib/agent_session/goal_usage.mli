(** Bounded six-component algebra. This stores aggregate numbers and bounded
    estimator provenance, never provider rows/configuration or authority. *)
module Counter = Goal_accounting_counter

module Component = Inference.Observation.Usage.Component
module Sum = Agent_protocol.Inference_query.Metric

module Unknown : sig
  type t [@@deriving equal, sexp_of]

  val count : t -> Inference.Observation.Count.unknown_reason -> int64
end

module Metric : sig
  type t [@@deriving equal, sexp_of]

  val actual : t -> Sum.sum
  val actual_attempts : t -> int64
  val estimated : t -> Sum.sum
  val estimated_attempts : t -> int64
  val mixed_estimators : t -> bool

  (** Bounded method/version witness for composition of read-only partitions.
      It is a uniform estimator claim only when [mixed_estimators] is false. *)
  val first_estimator : t -> (Inference.Observation.Estimator.method_ * int) option

  val unknown : t -> Unknown.t
  val untracked_attempts : t -> int64
end

type t [@@deriving equal, sexp_of]

val empty : t
val attempts : t -> int64
val untracked_attempts : t -> int64
val component : t -> Component.t -> Metric.t

(** Internal accounting algebra over canonical row values. Explicit Actual zero
    stays distinct from missing usage. Each component receives exactly one
    contribution; inclusion subsets are never added into their containers.
    Token sum overflow is labelled; lifetime count exhaustion returns Error.
    No Goal_accounting API accepts arbitrary aggregate imports. *)
val add_record : t -> Inference.Observation.Attempt_record.t -> (t, Counter.error) result

val add_untracked : t -> (t, Counter.error) result
