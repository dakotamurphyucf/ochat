# `Meta_prompting` – Composable Prompt Generators & Self-Improvement

> “Let the LLM write the prompt that teaches the LLM how to solve the task.”  
> *— recap of the meta-prompting idea*

`Meta_prompting` is the **library layer** that introduces

* **Prompt generators** – functor `Meta_prompting.Make` maps a *typed* task
  description to a fully-fledged [`Chatmd.Prompt.t`].  Think of it as a
  *single-shot compiler* from a *domain specific record* to the markdown that
  your LLM will consume.
* **Recursive refinement** – module [`Recursive_mp`] wraps any existing prompt
  in a *monad* that can *iteratively* call an evaluator, apply a transformation
  strategy and keep the better variant.
* **Flexible evaluation** – module [`Evaluator`] bundles a set of *judges*
  (regex check, log-prob proxy, LLM ensemble, …) into an aggregate score that
  drives the refinement loop.

All pieces are deliberately kept **orthogonal**: swap any evaluator, inject a
custom transformation, or re-use the monad with an *entirely different*
prompt type.

---

## Table of contents

1. [Quick start](#quick-start)
2. [API overview](#api-overview)
3. [`Recursive_mp.refine`](#recursivemprefine) – how the loop works
4. [Context retrieval & environment variables](#context-retrieval--environment-variables)
5. [Integration guide](#integration-guide)
6. [Writing a custom evaluator](#writing-a-custom-evaluator)
7. [Limitations & next steps](#limitations--next-steps)

---

## Quick start

The snippet below shows how *ten* lines of code are enough to turn a plain data
record into a ready-to-send ChatMarkdown prompt **and** run one self-improvement
cycle:

```ocaml
open Meta_prompting

(* 1️⃣  Define a minimal task type *)
module Task = struct
  type t = { subject : string; word_limit : int }
  let to_markdown { subject; word_limit } =
    Printf.sprintf "Summarise **%s** in at most %d words." subject word_limit
end

(* 2️⃣  Provide a thin façade over Chatmd.Prompt *)
module Prompt = struct
  type t = Chatmd.Prompt.t
  let make ?header ?footnotes ?metadata ~body () =
    Chatmd.Prompt.make ?header ?footnotes ?metadata ~body ()

  (* Convenience helpers re-exported to users *)
  let to_string = Chatmd.Prompt.to_string
  let add_metadata = Chatmd.Prompt.add_metadata
end

(* 3️⃣  Instantiate the functor *)
module Mp = Meta_prompting.Make (Task) (Prompt)

(* 4️⃣  Generate & refine the prompt *)
let () =
  let task    = { Task.subject = "GRU v LSTM"; word_limit = 120 } in
  let prompt0 = Mp.generate task in           (*  ➜ Prompt.t *)
  let prompt1 = Recursive_mp.refine prompt0   (* 1-step self-improvement *) in
  print_endline (Prompt.to_string prompt1)
```

Running the program prints a prompt that already includes the default *meta*
header as well as an `iteration=1` metadata field injected by
`Recursive_mp.transform_prompt` (a placeholder until the transformation is
replaced by an actual LLM call).

---

## API overview

### Functor `Meta_prompting.Make`

```ocaml
module Make
  (Task   : sig type t val to_markdown : t -> string end)
  (Prompt : sig
              type t
              val make
                :  ?header:string
                -> ?footnotes:string list
                -> ?metadata:(string * string) list
                -> body:string
                -> unit
                -> t
            end) : sig
  val generate : Task.t -> Prompt.t
end
```

*Supply two small adapters* – one for your task type and one for the concrete
prompt implementation – and the functor gives back a module exposing a single
`generate` function.

### Monad `Recursive_mp`

```ocaml
val return  : 'a -> 'a Recursive_mp.t
val bind    : 'a Recursive_mp.t -> ('a -> 'b Recursive_mp.t) -> 'b Recursive_mp.t
val join    : 'a Recursive_mp.t Recursive_mp.t -> 'a Recursive_mp.t

val refine
  :  ?params:Recursive_mp.refine_params
  -> Prompt.t
  -> Prompt.t
```

The monad is *minimal*: it only supports the constructs required by the
recursive refinement loop.  `refine` evaluates the current prompt, creates a
candidate via `transform_prompt`, keeps the best of the two, and repeats until
either the score plateaus or `max_iters` is reached.

### Evaluator

```ocaml
type Evaluator.t

val Evaluator.create    : ?judges:(module Evaluator.Judge) list -> unit -> t
val Evaluator.evaluate  : t -> string -> float
val Evaluator.default   : t
```

Provide a list of *judges* – that is, functions `string -> float` – and the
evaluator will *aggregate* their individual scores, cache the result and return
the combined value.

---

## `Recursive_mp.refine`

Below is the essence of the algorithm stripped of logging and error handling:

```ocaml
let rec loop current iter best_score best_prompt =
  if iter >= max_iters then best_prompt else
  let candidate   = transform_prompt current ~iteration:iter in
  let score       = evaluate candidate in
  let improvement = score -. best_score in
  if improvement <= score_epsilon
  then best_prompt                  (* plateau – stop *)
  else loop candidate (iter + 1) score candidate
```

The *opinionated* parts are therefore **only** the evaluator and the
transformation strategy – both pluggable.

---

## Context retrieval & environment variables

`Recursive_mp.ask_llm` can enrich the refinement step with **vector database**
context.  When enabled, the current prompt string is embedded, the top
`k` neighbours are fetched from the on-disk index and appended to the system
context shown to the LLM.  This typically boosts the quality of the suggested
transformations when the prompt is part of a larger project.

Configure the behaviour via these variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `META_PROMPT_CTX_K` | *(unset)* | Number of neighbours to retrieve.  Set to `0` or leave unset to disable retrieval. |
| `VECTOR_DB_FOLDER` | `./vector_db` | Folder that holds `index.bin` and `metadata.bin` as produced by `vector_db build`. |

Both variables are read *once* at start-up.  Missing indices are ignored so you
can safely deploy the same binary with or without a bundle.

---

## Integration guide

`meta_prompting` powers several higher-level user interfaces:

1. **Chat-TUI** – Toggle the `/meta_refine` command (or press *Ctrl-r*) to run
   `Recursive_mp.refine` on the draft **before** it is sent.  The TUI renderer
   shows a live **diff** so that users stay in the loop and can cancel if the
   refinement looks wrong.
2. **CLI `mp_prompt`** – Run batch refinement jobs from scripts:

   ```console
   $ mp-prompt -prompt-file tasks/design.chatmd \
               -output-file sessions/design.chatmd \
               -meta-refine
   ```

3. **MCP tool** – Remote agents can call the `meta_refine` JSON-RPC endpoint to
   refine prompts on demand without linking OCaml code.

Each integration uses the *same* `Recursive_mp.refine` API so improvements
automatically propagate across all front-ends.

---

## Writing a custom evaluator

```ocaml
module My_judge : Evaluator.Judge = struct
  let name = "starts_with_hello"
  let evaluate s = if String.is_prefix s ~prefix:"Hello" then 1.0 else 0.0
end

let my_ev = Evaluator.create ~judges:[ (module My_judge) ] ()

let score = Evaluator.evaluate my_ev "Hello world!" (* ➜ 1.0 *)
```

Judges *must not* raise – add your own exception guards or rely on the
built-in helper `Evaluator.with_exception_guard`.

---

## Limitations & next steps

1. **Transformation is a stub** – upcoming tasks will replace
   `transform_prompt` with an LLM-powered proposer and integrate vector DB
   context retrieval.
2. **Evaluator parallelism** – heavy judges will soon run in an `Io.Task_pool`.
3. **Category-theory proofs** – functor and monad laws are checked via
   Quickcheck but need formal documentation.


