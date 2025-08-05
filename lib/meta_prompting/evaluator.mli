(** Prompt–answer evaluation framework.

    This module provides a composable set of {i judges} together with a
    container that aggregates their individual scores into a single
    scalar.  The design centres around two first-class module
    signatures:

    • {!module-type:Judge} – scores a single candidate answer in
      isolation.
    • {!module-type:Pairwise_judge} – compares an incumbent answer with a
      challenger and outputs the win-probability of the challenger.

    An {!type:t} bundles one or more such judges and exposes
    {!val:evaluate}.  The function runs every judge (concurrently when
    an [Eio] environment is supplied), combines the raw numbers with an
    {!module:Aggregator} and memoises the result so that repeated calls
    with the same candidate are O(1).

    All scores are normalised to the closed interval \[0, 1\] where 0
    denotes a poor / losing answer and 1 denotes an excellent / winning
    answer.  Callers should treat the absolute value as an un-calibrated
    heuristic.  When a judge fails (exception, timeout, missing API key)
    the framework converts the error into a neutral score of 0.5 to keep
    downstream logic simple and deterministic in CI.
*)

open! Core

(** {1 Primitive types} *)

(** A floating-point score in the range \[0, 1\].  Higher is better. *)
type score = float

(** {1 Judge interfaces} *)

module type Judge = sig
  (** Human-readable identifier used in logs. *)
  val name : string

  (** [evaluate ?env candidate] returns a quality [score] for
      [candidate].  [env] provides Eio resources (network, filesystem
      …) and may be omitted for offline or pure judges. *)
  val evaluate : ?env:Eio_unix.Stdenv.base -> string -> score
end

module type Pairwise_judge = sig
  val name : string

  (** [evaluate ~incumbent ~challenger ?env ()] compares two answers and
      returns the win-probability of [challenger] (1 = win,
      0 = loss, 0.5 = tie). *)
  val evaluate
    :  incumbent:string
    -> challenger:string
    -> ?env:Eio_unix.Stdenv.base
    -> unit
    -> score
end

type judge =
  | Judge of (module Judge)
  | Pairwise_judge of (module Pairwise_judge)
  (** Existential wrapper that allows heterogenous judges to coexist in
      the same list. *)

(** {1 Built-in judges}

    The constants below expose commonly used judges so that callers do
    not need to instantiate them manually.  All of them degrade
    gracefully to a deterministic score of 0.5 when the required
    OpenAI credentials are absent. *)

(** Elo-style arena that repeatedly pits prompts against each other and
    updates their ratings using the logistic Elo formula.  The returned
    score is the updated win-probability of the challenger. *)
val pairwise_arena_judge : (module Pairwise_judge)

(** JSON-based rubric critic grading {e correctness}, {e completeness},
    {e depth}, {e style} and {e safety}.  Returns the mean of the five
    sub-scores normalised to \[0, 1\]. *)
val rubric_critic_judge : (module Judge)

(** High-fidelity reward model powered by OpenAI’s *grader* endpoint. *)
val prompt_reward_model_judge : (module Judge)

(** Same reward-model judge but with a custom rubric for *tool
    descriptions* (i.e. OpenAI function calling). Returns a scalar in
    the range \[0,1\] where higher indicates a better description. *)
val tool_description_reward_model_judge : (module Judge)

(** Low-level access to the underlying judge modules.  These are useful
    when callers need *first-class* modules instead of the pre-packed
    functions above.  Most users should prefer the aliases exposed via
    {!val:prompt_reward_model_judge} and
    {!val:tool_description_reward_model_judge}. *)
module Reward_model_judge : Judge

module Tool_description_reward_model_judge : Judge

(** {1 Self-consistency wrapper} *)

type sc_strategy =
  | Mean (** Arithmetic mean of the [k] runs. *)
  | Majority (** Majority vote interpreting a score > 0.5 as success. *)

val string_of_sc_strategy : sc_strategy -> string

(** [wrap_self_consistency_judge ~k ~strategy base] runs [base]
    [k] times on the same candidate and combines the outcomes according
    to [strategy]. *)
val wrap_self_consistency_judge
  :  k:int
  -> strategy:sc_strategy
  -> (module Judge)
  -> (module Judge)

(** {1 Evaluator container} *)

(** A collection of judges together with a caching layer and an
    aggregation function.  Use {!val:create} to obtain a fresh value. *)
type t

(** [create ?judges ?pw_judges ?aggregate ()] constructs a new evaluator.
    • [judges] – list of single-answer judges (defaults to a stub that
      always returns 0.5).
    • [aggregate] – function used to combine the individual scores.
      Defaults to {!Aggregator.mean}. *)
val create : ?judges:judge list -> ?aggregate:Aggregator.t -> unit -> t

(** Singleton using the module default parameters. *)
val default : t

(** [evaluate ?env t ?best candidate] obtains a score for [candidate] by
    running all judges in [t] and aggregating the results.  Results are
    cached, therefore repeated calls with the same [candidate] are
    effectively free. *)
val evaluate
  :  ?env:Eio_unix.Stdenv.base
  -> t
  -> ?best:string (** incumbent answer in a pairwise setting *)
  -> string (** candidate answer *)
  -> score

(** Legacy helper equal to {!Aggregator.mean}.  New code should pass an
    explicit [aggregate] argument to {!val:create}. *)
val aggregate : score list -> score

(** Shortcut for [evaluate {!val:default}]. *)
val evaluate_default : string -> score
