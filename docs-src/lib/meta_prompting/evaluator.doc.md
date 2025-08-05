# `Meta_prompting.Evaluator`

Flexible, failure-tolerant framework to **score the quality of prompts and answers**.
The module bundles one or more *judges* – small pluggable components that
output a floating-point value in the closed interval \[0, 1\] – and combines
their opinions into a single number.

It was designed for the *recursive meta-prompting* pipeline but is
independent of the surrounding control-flow and can be reused wherever a
scalar reward signal is required.

---

## 1  High-level overview

```
┌───────────────────────────────────────────────────────────────────────┐
│                       Meta_prompting.Evaluator                      │
│                                                                     │
│   ┌───────────────┐    ┌───────────────┐    ┌───────────────┐        │
│   │  Judge (LLM)  │    │ Judge (regex) │ …  │  Judge (Elo)  │        │
│   └───────────────┘    └───────────────┘    └───────────────┘        │
│           │                 │                    │                  │
│           ▼                 ▼                    ▼                  │
│       score₁            score₂               scoreₙ                │
│                 ──────────┬───────────╌╌╌╌╌                           │
│                           ▼                                         │
│            Aggregator (mean / median / …)                           │
│                           ▼                                         │
│                     final score ∈ [0, 1]                            │
└───────────────────────────────────────────────────────────────────────┘
```

Key ideas:

* **Single-answer judges** (signature `Judge`) rate one candidate in
  isolation.  Examples: length penalty, OpenAI reward model, rubric critic.
* **Pairwise judges** (signature `Pairwise_judge`) compare two answers and
  return the win-probability of the challenger.  The built-in
  `pairwise_arena_judge` keeps Elo ratings for every unique answer string.
* The `Evaluator.t` container runs *all* configured judges (in parallel when
  an `Eio` environment is supplied), guards them with time-outs and
  exception handling, then delegates to an `Aggregator.t` (arithmetic mean
  by default).
* Results are **memoised** so that expensive LLM calls run at most once per
  distinct candidate string.

The implementation purposefully degrades to a deterministic score of **0.5**
when a network error occurs or the required `OPENAI_API_KEY` is missing. This
behaviour makes unit tests reproducible and avoids crashing production code
in partial-failure scenarios.

---

## 2  Types and signatures

| Name | Description |
|------|-------------|
| `score = float` | Normalised quality metric (0 = bad, 1 = excellent). |
| `Judge`, `Pairwise_judge` | First-class module signatures capturing the `name` and `evaluate` function. |
| `judge` | Existential wrapper: `Judge of (module Judge)` \| `Pairwise_judge of (module Pairwise_judge)`. |
| `t` | Main container bundling a list of judges, a score *cache* and an `Aggregator.t`. |
| `sc_strategy` | Self-consistency combination strategy: `Mean` &#124; `Majority`. |

---

## 3  Public API

### 3.1  Constructing an evaluator

```ocaml
val create
  :  ?judges:judge list
  -> ?pw_judges:(module Pairwise_judge) list
  -> ?aggregate:Aggregator.t
  -> unit
  -> t
```

Example creating an evaluator that combines the *reward model* with a
length-penalty and uses the median to aggregate:

```ocaml
open Meta_prompting.Evaluator

let ev : t =
  create
    ~judges:
      [ Judge (module Reward_model_judge)
      ; Judge (module Logprob_judge)
      ]
    ~aggregate:Aggregator.median
    ()
```

### 3.2  Evaluating a candidate answer

```ocaml
val evaluate
  :  ?env:Eio_unix.Stdenv.base  (** enables parallel execution *)
  -> t
  -> ?best:string              (** incumbent for pairwise judges *)
  -> string                    (** candidate *)
  -> score
```

When an `Eio` environment is supplied the evaluator spawns one *fiber* per
judge (subject to the `EVAL_POOL_SIZE` limit) and cancels any fiber that runs
longer than `EVAL_JUDGE_TIMEOUT` seconds (400 by default).

### 3.3  Convenience helpers

* `default : t` – evaluator with a single `Mock_judge` returning 0.5.
* `evaluate_default : string -> score` – shorthand for `evaluate default`.
* `aggregate : score list -> score` – historic alias for
  `Aggregator.mean`.

### 3.4  Self-consistency wrapper

```ocaml
val wrap_self_consistency_judge
  :  k:int                (* number of runs *)
  -> strategy:sc_strategy (* Mean or Majority *)
  -> (module Judge)
  -> (module Judge)
```

The functor executes the underlying judge *k* times on the same candidate
and aggregates the list of scores.  Use a stochastic judge (e.g. an LLM with
`temperature > 0`) to benefit from the technique.

---

## 4  Built-in judges

| Judge | Purpose | Offline fallback |
|-------|---------|------------------|
| `Guidelines_judge` | Rewards prompts that separate reasoning from answer. | 0.5 |
| `Logprob_judge` | Length penalty (shorter answers score higher). | – |
| `Answer_regex_judge` | Returns 1.0 if a supplied POSIX regexp matches. | – |
| `Llm_judge` | Generic “score 0–10” grader using Chat Completions API. | 0.5 |
| `Rubric_critic_judge` | Parses a JSON rubric (correctness, …). | 0.5 |
| `Reward_model_judge` | Calls OpenAI *grader* endpoint (high fidelity). | 0.5 |
| `Pairwise_arena_judge` | Elo arena producing win-probabilities. | 0.5 |

---

## 5  Examples

### 5.1  Offline unit test

```ocaml
let%expect_test "reward_model offline fallback" =
  let module J = (val Meta_prompting.Evaluator.prompt_reward_model_judge
                     : Meta_prompting.Evaluator.Judge)
  in
  J.evaluate "Arbitrary answer" |> printf "%.1f" ;
  [%expect "0.5"]
```

### 5.2  Using the pairwise arena

```ocaml
let compare a b =
  let module Arena = (val Meta_prompting.Evaluator.pairwise_arena_judge
                         : Meta_prompting.Evaluator.Pairwise_judge)
  in
  Arena.evaluate ~incumbent:a ~challenger:b ()

let () =
  let winprob = compare "first answer" "second answer" in
  printf "Challenger win-probability = %.2f\n" winprob
```

---

## 6  Environment variables

| Variable | Default | Effect |
|----------|---------|--------|
| `OPENAI_API_KEY` | *unset* | Enables all LLM-powered judges. |
| `EVAL_JUDGE_MODEL` | `o3` | Overrides the model used by the LLM judges. |
| `EVAL_JUDGE_TIMEOUT` | `400.` | Per-judge timeout (seconds). |
| `EVAL_POOL_SIZE` | `20` | Max parallel fiber count. |
| `ARENA_ELO_K` | `32.` | K-factor for the Elo update. |

---

## 7  Limitations & future work

* **No automatic weighting** – the aggregator treats each judge equally.
  Data-driven weight tuning could improve correlation with human ratings.
* **Shared cache** – currently keyed by the full answer string.  A robust
  fingerprinting scheme (hash + normalisation) would reduce memory usage.
* **Pairwise judges are not integrated** into `Evaluator.evaluate` yet.
  They are intended for bandit-style optimisation loops and require manual
  invocation.

---

© 2025 OChat –– Documentation auto-generated by `ochat-doc-bot`.  No
copyright claims.

