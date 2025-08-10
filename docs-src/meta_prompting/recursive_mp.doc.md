Recursive Meta-Prompting (`Recursive_mp`)
=======================================

`Recursive_mp` provides a production-ready implementation of *recursive
prompt self-refinement* – an algorithm that starts from an initial prompt and
iteratively improves it until no further progress can be measured.  The
implementation is generic, fully parameterisable, and ships with sensible
defaults so that you can get started with two lines of code:

```ocaml
let improved_prompt =
  Prompt_intf.make ~body:"Summarise TCP congestion control." ()
  |> Recursive_mp.refine
  |> Prompt_intf.to_string
```

Behind the scenes the module will:

1. Evaluate the starting prompt with the built-in evaluator.
2. Call an LLM that proposes a refined prompt.
3. Re-score the candidate and keep it if it beats the incumbent.
4. Decide whether another iteration is worthwhile using a Bayesian test.

The entire loop stops automatically when any of the following happens:

* The user-supplied `max_iters` was reached (defaults to **3**).
* The expected probability of seeing a *meaningful* improvement (as defined by
  `score_epsilon`) drops below the threshold `bayes_alpha` (defaults to
  **0.05**).

Module Architecture
-------------------

The public surface consists of four main concepts:

* **`transform_strategy`** — a record encapsulating *how* to turn the current
  prompt into a better one.  Strategies can be handcrafted OCaml functions or
  sophisticated LLM agents.  Several defaults are provided:
  * `default_llm_strategy`
  * `meta_factory_online_strategy`
* **`Evaluator.t`** — pluggable scoring backend responsible for comparing two
  prompts and returning a float in the range `[0; 1]`.
* **`refine_params`** — a bag-of-knobs that tunes the termination criteria,
  bandit behaviour, evaluator, and more.  Use `default_params` or
  `make_params` instead of building the record manually.
* **`refine`** — the one-shot entry point that ties everything together.

Function reference
------------------

### `default_llm_strategy : transform_strategy`
Uses the OpenAI `/responses` endpoint to produce a revised prompt.  The model
is resolved in the following order of precedence: explicit
`refine_params.proposer_model`, `META_PROPOSER_MODEL` environment variable,
hardcoded default (`"o3"`).

### `meta_factory_online_strategy : transform_strategy`
Wraps the *Meta-Prompt Factory* template hosted in the same repository.  It
converts the current prompt into the template variables, sends the filled
template to the LLM, and extracts the `<Revised_Prompt>` section.

### `default_params : unit -> refine_params`
Returns a parameter record equivalent to

```ocaml
make_params ~max_iters:3 ~bayes_alpha:0.05 ()
```

using `default_llm_strategy` and the default evaluator.

### `make_params : ... -> refine_params`
Low-level builder that mirrors the record fields.  Only tweak what you need;
every optional argument falls back to the `default_params` value.

### `refine : Prompt_intf.t -> Prompt_intf.t`
Runs the recursive loop and returns the best prompt found.  Optional labelled
arguments are forwarded to `make_params` so you can write:

```ocaml
let better = Recursive_mp.refine ~max_iters:6 ~bandit_enabled:true prompt
```

Examples
--------

### 1. Quick default run

```ocaml
let improved =
  Prompt_intf.make ~body:"Translate the following text to Latin." ()
  |> Recursive_mp.refine
  |> Prompt_intf.to_string
```

### 2. Enabling the Thompson-sampling bandit

```ocaml
let strategies =
  [ Recursive_mp.default_llm_strategy
  ; Recursive_mp.meta_factory_online_strategy
  ]

let params = Recursive_mp.make_params ~bandit_enabled:true ~strategies ()

let improved = Recursive_mp.refine ~params prompt
```

Environment variables
---------------------

* `OPENAI_API_KEY` – required for any LLM interaction.
* `META_PROPOSER_MODEL` – override the default proposer model.
* `META_PROMPT_BUDGET` – soft limit on the number of output tokens.
* `META_PROMPT_GUIDELINES` – toggle the addition of extra system guidelines.
* `META_PROMPT_CTX_K` – number of context snippets retrieved from the vector-DB.

Known limitations
-----------------

* The current success/failure counts for Bayesian convergence are simplistic
  and treat any Δ ≤ `score_epsilon` as a failure, regardless of direction.
  This is sufficient in practice but could be refined.
* The Thompson-sampling bandit uses an uninformative Beta(1, 1) prior.  Heavy
  users may want to expose this as a parameter.
* `plateau_window` is ignored and only kept for backwards compatibility.

---
Last updated: {{date}}

