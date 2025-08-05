# `Context_compaction.Relevance_judge`

Message-relevance scorer used by the context-compaction pipeline.

---

## Overview

When compressing a long chat conversation we first need to decide which
messages are worth keeping. `Relevance_judge` solves exactly this
problem: given the *text* of a single message it returns a floating-point
importance score in the range **[0, 1]**.  The score can then be
compared to a user-defined threshold to decide whether the message is
kept or discarded.

Under the hood the module delegates to `Meta_prompting.Evaluator` – a
small framework that combines one or more *judges* into an ensemble.
For relevance scoring we currently use a single judge called
“importance” whose logic is:

* If an **`OPENAI_API_KEY`** is available the judge calls OpenAI’s
  public *grader* endpoint with a specialised prompt that instructs the
  model to assess how indispensable the message is for continuing the
  conversation.

* If no key is present (e.g. in CI) the judge returns the deterministic
  fallback value **0.5**.  This ensures the library remains completely
  offline-friendly and makes unit tests reproducible.

The evaluator wraps the judge in a *self-consistency* layer that calls
the grader three times and averages the results.  The extra calls can
be disabled or replaced in the future without affecting the public API.

---

## Public Interface

### `score_relevance`

```ocaml
val score_relevance :
  ?env:Eio_unix.Stdenv.base ->
  Config.t ->
  prompt:string ->
  float
```

Returns the raw importance score of `prompt` on **[0, 1]**.

Parameters:

* `env` – optional Eio standard environment providing network/FS
  capabilities.  Pass this when you want the judge to perform real LLM
  calls.  Omitting the parameter forces the offline fallback.
* `Config.t` – configuration record from
  `Context_compaction.Config`.  Only the `relevance_threshold` field is
  currently used.
* `prompt` – the message text to evaluate.

Returns: the averaged score (0 = irrelevant, 1 = crucial).

#### Example – offline default

```ocaml
let score =
  Relevance_judge.score_relevance
    Config.default
    ~prompt:"I’ll be back after lunch."
(* => 0.5 *)
```

### `is_relevant`

```ocaml
val is_relevant :
  ?env:Eio_unix.Stdenv.base ->
  Config.t ->
  prompt:string ->
  bool
```

Convenience wrapper that returns `true` if the importance score is **≥**
`cfg.relevance_threshold`.

#### Example – custom threshold

```ocaml
let strict_cfg =
  { Config.default with relevance_threshold = 0.8 } in

let keep =
  Relevance_judge.is_relevant strict_cfg ~prompt:"Great, thanks!";;
(* => false *)
```

---

## Behaviour in Offline Mode

`Relevance_judge` is designed to run inside build pipelines and test
suites where outbound network access and API keys are usually
unavailable.  In such environments:

* `score_relevance` = **0.5** for any input.
* `is_relevant` therefore returns `true` when the threshold ≤ 0.5 and
  `false` otherwise.  This matches the expectations in unit tests
  shipped with the library.

---

## Known Limitations

1. **Single-message granularity** – relevance is assessed per message
   without looking at the surrounding context.  Future work could feed
   a sliding window into the judge to improve accuracy.

2. **Latency & cost** – scoring with real grader calls introduces
   network round-trips and potential cost.  Cache invalidation and
   batching are not yet implemented.

3. **Grader availability** – the OpenAI grader API is in alpha.  Its
   schema or availability may change.

---

## Change Log

* **v0.1** – Initial implementation: single importance judge, optional
  network calls, offline fallback.

