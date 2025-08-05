(** Recursive meta-prompting – an iterative prompt refinement engine.

    {1 Overview}

    The [Recursive_mp] module implements the refinement loop described in
    "Self-refine: Recursive chunk-based prompting for large language models" and
    similar papers.  Starting from an initial prompt it repeatedly applies a
    {e transformation strategy} to obtain a candidate prompt, evaluates the
    candidate with a set of {e judges}, and decides – using Bayesian early
    stopping as well as an optional Thompson-sampling multi-armed bandit –
    whether to keep refining or return the current best.

    The algorithm is completely generic:

    • Transformation is provided by a value of type {!transform_strategy}.
    • Evaluation is delegated to the {!module:Evaluator} abstraction.

    This makes it easy to plug in a heuristic string manipulation, an LLM-based
    re-writer, or any other strategy without changing the control flow.
*)

(** re-exported to allow using [Int.t] etc. in record fields *)
open! Core

(***********************************************************************
 *  Transformation strategies                                          *
 ***********************************************************************)

(** A {e transformation strategy} takes the current prompt, the iteration
    counter, and an execution [Context.t] and returns a (hopefully improved)
    prompt.

    Implementations {b must} be deterministic with respect to their explicit
    arguments (they may of course consult an LLM or random generator
    internally), and {b should not} mutate the input prompt value. *)
type transform_strategy =
  { name : string (** Human-readable identifier used for logging and bandit arms. *)
  ; apply :
      Prompt_intf.t
      -> ?env:Eio_unix.Stdenv.base
      -> iteration:int
      -> context:Context.t
      -> Prompt_intf.t
    (** [apply p ~iteration ~context] must return a new prompt.  The
            optional [env] can be used when the strategy needs network or file
            system access. *)
  }

(***********************************************************************
 *  Internal monad (rarely needed by library users)                    *
 ***********************************************************************)

type 'a t =
  | Return of 'a
  | Bind of 'a t * ('a -> 'a t)
  (** Lightweight continuation monad used to stage recursive calls inside the
    refinement loop.  It is {b not} intended for general consumption but is
    exposed – together with the constructors – so that expect tests and helper
    utilities can pattern-match on intermediate results. *)

(***********************************************************************
 *  Parameter record                                                   *
 ***********************************************************************)

(** Fine-tuning knobs for {!refine}.  Use {!default_params} or
    {!make_params} for construction. *)
type refine_params =
  { evaluator : Evaluator.t (** Scoring function used to rank candidates. *)
  ; max_iters : int (** Hard upper bound on the number of refinement iterations. *)
  ; score_epsilon : float
    (** Minimal score improvement considered meaningful.  Values ≤ this
            threshold are treated as "no progress" for Bayesian convergence
            detection. *)
  ; plateau_window : int (** Deprecated.  Kept for backwards compatibility but ignored. *)
  ; bayes_alpha : float
    (** Significance level for the Bayesian success-rate estimate.  The
            loop stops once the posterior expectation of making further
            progress falls below [bayes_alpha]. *)
  ; bandit_enabled : bool (** Enable Thompson sampling over {!strategies}. *)
  ; strategies : transform_strategy list
    (** Ordered list of candidate strategies.  At least one element is
            required. *)
  ; proposer_model : Openai.Responses.Request.model option
    (** Override the default OpenAI model used by transformation
            strategies that call the LLM. *)
  ; executor_model : Openai.Responses.Request.model option
    (** Reserved for future use – at present only logged. *)
  }

(***********************************************************************
 *  Builders and defaults                                              *
 ***********************************************************************)

(** A transformation strategy that delegates to an LLM via the OpenAI
    {e /responses} API.  The model can be overridden with the
    [~proposer_model] parameter of {!make_params} or {!refine}. *)
val default_llm_strategy : transform_strategy

(** [default_params ()] returns a parameter set that performs up to three
    iterations using {!default_llm_strategy} and the default
    {!module:Evaluator}.  Bayesian convergence is active with
    [~bayes_alpha = 0.05] and [~score_epsilon = 1e-6]. *)
val default_params : unit -> refine_params

(** Flexible constructor mirroring the record fields.  When [~judges] is
    supplied a fresh {!Evaluator.t} is created internally.  Otherwise the
    default evaluator is used.  Unspecified optional arguments fall back to the
    values of {!default_params}. *)
val make_params
  :  ?judges:Evaluator.judge list
  -> ?max_iters:int
  -> ?score_epsilon:float
  -> ?plateau_window:int
  -> ?bayes_alpha:float
  -> ?bandit_enabled:bool
  -> ?strategies:transform_strategy list
  -> ?proposer_model:Openai.Responses.Request.model
  -> ?executor_model:Openai.Responses.Request.model
  -> unit
  -> refine_params

(***********************************************************************
 *  Refinement entry point                                             *
 ***********************************************************************)

(** [refine p] returns an improved variant of [p].

    The algorithm proceeds as follows:

    {ol
      {- Score the current best prompt using [evaluator].}
      {- Select a transformation strategy (uniformly at first, then via
         Thompson-sampling if [~bandit_enabled] was set).}
      {- Apply the strategy, score the candidate, and update the best prompt if
         it improved.}
      {- Update Bayesian success statistics and stop early when further
         progress is deemed unlikely.}}

    Optional arguments mirror {!make_params} for convenience so that users can
    tweak a single knob without constructing a full parameter record.  A
    dedicated [context] can be provided to supply an [env], guidelines or
    prompt-type metadata. *)
val refine
  :  ?context:Context.t
  -> ?params:refine_params
  -> ?judges:Evaluator.judge list
  -> ?max_iters:int
  -> ?score_epsilon:float
  -> ?plateau_window:int
  -> ?bayes_alpha:float
  -> ?bandit_enabled:bool
  -> ?strategies:transform_strategy list
  -> ?proposer_model:Openai.Responses.Request.model
  -> ?executor_model:Openai.Responses.Request.model
  -> Prompt_intf.t
  -> Prompt_intf.t

(***********************************************************************
 *  Lwt-style monadic sugar (internal use)                             *
 ***********************************************************************)

module Let_syntax : sig
  val return : 'a -> 'a t
  val bind : 'a t -> ('a -> 'a t) -> 'a t
end
[@@ocaml.doc
  "Monad operations exposed mainly for let-syntax inside the\n\
  \               implementation.  External users will rarely need these."]

(***********************************************************************
 *  Low-level monad helpers (exported for tests)                        *
 ***********************************************************************)

(** Alias of {!Let_syntax.return} exposed at the toplevel for convenience and
    for property-based tests shipped with the library. *)
val return : 'a -> 'a t

(** Monadic bind – see {!Let_syntax.bind}. *)
val bind : 'a t -> ('a -> 'a t) -> 'a t

(** Flatten one level of monadic structure. *)
val join : 'a t t -> 'a t
