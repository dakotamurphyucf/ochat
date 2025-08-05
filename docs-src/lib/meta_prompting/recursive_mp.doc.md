# Recursive_meta_prompting (`Recursive_mp`)

Iteratively improves a prompt by alternating **generation** (a transformation
strategy) and **evaluation** (a judge‐based scorer).  The loop stops when it is
either exhausted its iteration budget or the observed improvement is unlikely
to continue according to a Bayesian credibility test.

---

## Quick example

```ocaml
open Core
open Meta_prompting

let () =
  let prompt =
    Prompt_intf.make ~body:"Translate the following text to French…" ()
  in
  let improved = Recursive_mp.refine prompt in
  printf "%s\n" improved.body
```

With default settings the function performs at most 3 refinement rounds using
the *default LLM strategy* and the *default evaluator*.

---

## Anatomy of the refinement loop

1. **Evaluation** – The current best prompt is scored by an
   `Evaluator.t` (default: self-consistency reward model).
2. **Strategy selection** – One of the supplied
   [`transform_strategy`](../../../../lib/meta_prompting/recursive_mp.mli) values
   is chosen.  When `bandit_enabled` is `true` Thompson sampling is used to
   favour strategies that produced larger improvements in the past.
3. **Transformation** – The selected strategy receives the prompt plus some
   context (iteration counter, optional Eio environment, guidelines…) and
   returns a candidate prompt.
4. **Bayesian stop test** – The candidate is scored; the loop continues while
   the probability of achieving an improvement larger than `score_epsilon` is
   above `1 – bayes_alpha`.

The procedure is cheap for small `max_iters` (the default is 3) which makes it
ideal for interactive tools.

---

## Configuration

```ocaml
let params =
  Recursive_mp.make_params
    ~max_iters:5
    ~bandit_enabled:true
    ~judges:[ (* custom Evaluator.judge list *) ]
    ~strategies:[ Recursive_mp.default_llm_strategy ]
    ()

let better = Recursive_mp.refine ~params prompt
```

All optional arguments of `refine` mirror the record fields of
`refine_params`, allowing one-off tweaks without allocating a full record.

---

## Transformation strategies

* **`default_llm_strategy`** – sends the prompt to the OpenAI *responses* API
  with an instruction to improve clarity, structure and completeness.  The
  model can be overridden via the `proposer_model` field.
* **Heuristic placeholder** – the library ships a very small deterministic
  example strategy that only tags the iteration number.  It is useful for unit
  tests and as a template for custom strategies.

Implementing your own strategy is a matter of providing a `name` and an
`apply` callback:

```ocaml
let my_strategy =
  let apply p ?env:_ ~iteration ~context:_ =
    Prompt_intf.add_metadata p ~key:"rev" ~value:(Int.to_string iteration)
  in
  { Recursive_mp.name = "meta-tag"; apply }
```

`apply` {b must} be pure with respect to its arguments.  Internal calls to an
LLM or the file-system are fine as long as the function does not mutate the
original prompt value.

---

## Known limitations

* The Bayesian convergence criterion uses a simple Beta-Bernoulli model which
  assumes independent Bernoulli trials.  Shape of the score distribution is
  ignored.
* Only the improvement of the **top-1** candidate is considered.  N-best search
  could produce better results for the same number of LLM calls.
* `plateau_window` is accepted for backwards compatibility but no longer used.

---

## API reference

See the inline interface documentation in
[`recursive_mp.mli`](../../../../lib/meta_prompting/recursive_mp.mli).

