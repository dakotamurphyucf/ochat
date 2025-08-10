# `Meta_prompting.Evaluator`

Comprehensive documentation of the *evaluator* sub-system that lives in
`lib/meta_prompting/evaluator.{mli,ml}`.

The OCaml interface already exposes detailed odoc comments; this document
adds a tutorial-style walk-through, usage snippets, and design notes
that do not fit into the inline API reference.

---

## 1  Purpose

`Evaluator` is the *scoring engine* that turns a free-form answer
(string) into a single floating-point score in `[0,1]`.  The score can
then be used for

* automated offline grading,
* reinforcement-learning style reward signals, or
* ranking of alternative prompts / answers in a *meta-prompting* setup.

The module achieves this by structuring the problem in three layers:

1. **Judge** — produces a score for *one* candidate in isolation.
2. **Pairwise_judge** — compares two candidates and outputs the win
   probability of the *challenger*.
3. **Evaluator container** — holds an arbitrary mix of judges and
   provides caching, aggregation and optional parallel execution.

---

## 2  Quick start

```ocaml
open Meta_prompting

let () =
  (* 1. Select a judge.  Reward-model requires an OPENAI_API_KEY      *)
  let (module Reward : Evaluator.Judge) = Evaluator.prompt_reward_model_judge in

  (* 2. Bundle it into an evaluator container. *)
  let ev = Evaluator.create ~judges:[ Evaluator.Judge (module Reward) ] () in

  (* 3. Score a prompt. *)
  let score = Evaluator.evaluate ev "Explain quick-sort in two sentences." in
  Printf.printf "Score = %.3f\n" score
```

Running the snippet with a valid API key prints a number between 0 and 1.
Without a key the reward-model judge automatically degrades to the
neutral score `0.5` allowing offline execution.

---

## 3  Judges in detail

The library ships with a collection of ready-to-use judges:

| Judge                                    | Type              | When to use                                                  |
|------------------------------------------|-------------------|--------------------------------------------------------------|
| `Guidelines_judge`                       | `Judge`           | Cheap heuristic rewarding prompts that follow the guide      |
| `Rubric_critic_judge` (`rubric_critic…`) | `Judge`           | JSON-based grading along *correctness*, *completeness* …     |
| `Reward_model_judge` (`prompt_reward…`)  | `Judge`           | High-quality scalar reward via OpenAI *grader* endpoint      |
| `Tool_description_reward_model_judge`    | `Judge`           | Specialised reward-model for tool / function descriptions    |
| `Pairwise_arena_judge` (`pairwise_arena`) | `Pairwise_judge` | Elo style arena — useful for tournaments of many prompts    |
| `Mock_judge`                             | `Judge`           | Always returns `0.5`; ideal for unit tests                   |

All judges are exposed as *first-class modules*.  To cooperate inside a
single evaluator they are wrapped in the existential `Evaluator.judge`
variant:

```ocaml
let judges : Evaluator.judge list =
  [ Evaluator.Judge (module Evaluator.Rubric_critic_judge)
  ; Evaluator.Pairwise_judge Evaluator.pairwise_arena_judge
  ]
```

### 3.1  Self-consistency wrapper

To stabilise inherently stochastic judges (e.g. those using an LLM with
`temperature > 0`) the functor
`Evaluator.Self_consistency_judge` can be used.  It repeats the base
judge *k* times and combines the outcomes using either the arithmetic
mean or a majority vote.

```ocaml
(* Run the reward model 3 times and average the scores *)
let sc_reward : (module Evaluator.Judge) =
  Evaluator.wrap_self_consistency_judge
    ~k:3
    ~strategy:Mean
    Evaluator.prompt_reward_model_judge
```

---

## 4  Aggregation strategies

`Evaluator.Aggregator` (see separate module) defines common combinators
such as `mean`, `geom_mean`, `min`, `max`, or user-supplied lambdas.
The choice largely depends on how strongly you trust the individual
judges.

Example favouring strict judges (take the minimum):

```ocaml
let ev = Evaluator.create ~aggregate:Aggregator.min ~judges ()
```

---

## 5  Caching behaviour

Results are cached in-memory inside a `string -> float` hash-table.  The
cache key is the *entire candidate string*.  If your application
produces very long answers consider hashing them to keep memory usage
in check.

---

## 6  Environment / tuning via variables

Several runtime knobs are exposed via environment variables so you can
experiment without recompiling:

* `OPENAI_API_KEY` — enables all LLM-backed judges.
* `EVAL_JUDGE_MODEL` — overrides the OpenAI model (e.g. switch from `o3`
  to `gpt-4o`).
* `ARENA_ELO_K` — *K-factor* employed by the Elo arena.
* `EVAL_JUDGE_TIMEOUT` — seconds before a judge is forcefully cancelled.
* `EVAL_POOL_SIZE` — maximum number of concurrent fibers evaluating
  judges.

---

## 7  Known limitations

1. **Persistence** — the cache and Elo ratings live only in RAM.
   Persisting them would require an additional storage backend.
2. **Timeout granularity** — timeouts are coarse and per-judge; a
   long-running OpenAI call cannot be interrupted until the HTTP request
   itself times out.
3. **Pure OCaml only** — the module deliberately avoids platform-
   specific accelerations; if you need CUDA / TPU powered reward models
   you have to integrate them separately.

---

## 8  Further reading

* "Self-Consistency Improves Chain of Thought Reasoning in LLMs"
  (Wang et al., 2022)
* OpenAI *grader* documentation (experimental):
  <https://platform.openai.com/docs/guides/retrieval/retrieval-grader>
* Elo rating system: <https://en.wikipedia.org/wiki/Elo_rating_system>

