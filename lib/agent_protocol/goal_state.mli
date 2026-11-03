(** Objective disposition and admission policy. This state is independent of
    execution's Run lifecycle and contains only immutable closure evidence. *)
module Hold : sig
  type t =
    | Operator_pause
    | Needs_input of Goal_text.Reason.t
    | Budget_gate of Goal_budget.Dimension.t
    | Usage_uncertain of Inference.Observation.Usage.Component.t
    | Source_review
  [@@deriving equal, sexp_of]
end

module Closure : sig
  type t =
    | Achieved of Goal_achievement.t
    | Abandoned of Goal_text.Reason.t
  [@@deriving equal, sexp_of]
end

type admission =
  | Enabled
  | Held of Hold.t
[@@deriving equal, sexp_of]

type t =
  | Open of admission
  | Closed of Closure.t
[@@deriving equal, sexp_of]

val to_json : t -> Jsonaf.t
val of_json : Jsonaf.t -> (t, Error.t) result
