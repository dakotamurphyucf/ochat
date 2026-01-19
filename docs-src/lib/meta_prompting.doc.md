# `Meta_prompting` – Composable Prompt Generators & Self-Improvement

> “Let the LLM write the prompt that teaches the LLM how to solve the task.”  
> *— recap of the meta-prompting idea*

`Meta_prompting` is the **library layer** that introduces

* **Prompt generators** – functor [`Meta_prompt.Make`] maps a *typed* task
  description to a fully-fledged prompt record.  Think of it as a
  *single-shot compiler* from a *domain specific record* to the markdown that
  your LLM will consume.
* **Recursive refinement** – module [`Recursive_mp`] implements an iterative
  loop that can call an evaluator, apply a transformation strategy and keep
  the better variant.  When run with an Eio environment it can delegate the
  heavy lifting to OpenAI models and an optional vector database.
* **Flexible evaluation** – module [`Evaluator`] bundles a set of *judges*
  (regex check, reward model, LLM ensemble, …) into an aggregate score that
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
module Mp = Meta_prompt.Make (Task) (Prompt)

(* 4️⃣  Generate & refine the prompt *)
let () =
  let task    = { Task.subject = "GRU v LSTM"; word_limit = 120 } in
  let prompt0 = Mp.generate task in           (*  ➜ Prompt.t *)
  let prompt1 = Recursive_mp.refine prompt0   (* 1-step self-improvement *) in
  print_endline (Prompt.to_string prompt1)
```

Running the program prints a prompt that already includes the default *meta*
header as well as an `iteration=1` metadata field injected by
`Recursive_mp.refine`.  In this minimal configuration the transformation
remains local and deterministic; the same API is also used by the `Mp_flow`
helpers and the `mp-refine-run` CLI to drive the full LLM-backed pipeline
when an [`Eio_unix.Stdenv.base`] environment is available.

---

## API overview

### Functor `Meta_prompt.Make`

```ocaml
module Make
  (Task   : sig
              type t
              val to_markdown : t -> string
            end)
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
  val generate
    :  ?env:< fs : Eio.Fs.dir_ty Eio.Path.t ; .. >
    -> ?template:string
    -> ?params:(string * string) list
    -> Task.t
    -> Prompt.t
end
```

*Supply two small adapters* – one for your task type and one for the concrete
prompt implementation – and the functor gives back a module exposing a
`generate` function that can either render an on-disk template or fall back
to the task’s Markdown representation.

### Monad `Recursive_mp`

```ocaml
val return  : 'a -> 'a Recursive_mp.t
val bind    : 'a Recursive_mp.t -> ('a -> 'b Recursive_mp.t) -> 'b Recursive_mp.t
val join    : 'a Recursive_mp.t Recursive_mp.t -> 'a Recursive_mp.t

val refine
  :  ?context:Context.t
  -> ?params:Recursive_mp.refine_params
  -> ?judges:Evaluator.judge list
  -> ?max_iters:int
  -> ?score_epsilon:float
  -> ?plateau_window:int
  -> ?bayes_alpha:float
  -> ?bandit_enabled:bool
  -> ?strategies:Recursive_mp.transform_strategy list
  -> ?proposer_model:Openai.Responses.Request.model
  -> ?executor_model:Openai.Responses.Request.model
  -> Prompt_intf.t
  -> Prompt_intf.t
```

The monad is *minimal*: it only supports the constructs required by the
recursive refinement loop.  `refine` evaluates the current prompt, uses one or
more transformation strategies to propose candidates, and repeats until either
the Bayesian success estimate drops below [`bayes_alpha`] or [`max_iters`] is
reached.  The optional [`context`] and parameters control which models are
used, whether a bandit is enabled, and whether vector-DB retrieval should
participate.

### Evaluator

```ocaml
type Evaluator.t

val Evaluator.create
  :  ?judges:Evaluator.judge list
  -> ?aggregate:Aggregator.t
  -> unit
  -> Evaluator.t

val Evaluator.evaluate
  :  ?env:Eio_unix.Stdenv.base
  -> Evaluator.t
  -> ?best:string
  -> string
  -> float

val Evaluator.default   : Evaluator.t
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

When run with an [`Eio_unix.Stdenv.base`] environment, `Recursive_mp` and the
higher-level helpers can enrich the refinement step with **vector-DB context**
and control various OpenAI parameters via environment variables.

### Vector-DB context

`Recursive_mp.ask_llm` embeds the current prompt, runs a hybrid
vector/BM25 query and appends the top `k` snippets to the system prompt.

Configure the behaviour via these variables:

| Variable | Default | Description |
|----------|---------|-------------|
| `META_PROMPT_CTX_K` | *(unset)* | Number of neighbours to retrieve.  Set to `0` or leave unset to disable retrieval. |
| `VECTOR_DB_FOLDER` | `.md_index` | Folder that holds `vectors.ml.binio` and `bm25.ml.binio` as produced by tools such as `index_markdown_docs` and `index_ocaml_code`. |

Missing indices are ignored so you can safely deploy the same binary with or
without a bundle.

### OpenAI and other control knobs

In addition, the implementation honours:

- `OPENAI_API_KEY` – enables LLM-based transformation and evaluation.
- `META_PROPOSER_MODEL` – default model for the proposer agent, if no
  explicit model is passed.
- `META_PROMPT_BUDGET` – soft limit for the number of output tokens.
- `META_PROMPT_GUIDELINES` – when set to a falsy value (`0`, `false`, `off`)
  disables injection of extra guidelines into the system prompt.

Most callers do not need to set these manually; [`Mp_flow`] and
[`mp-refine-run`](../bin/mp_refine_run.doc.md) provide reasonable defaults.

---

## Integration guide

`Meta_prompting` powers several higher-level entry points:

1. **ChatMD preprocessor** – `Meta_prompting.Preprocessor.preprocess` is wired
   into `Chatmd.Prompt.parse_chat_inputs`.  When either the environment
   variable `OCHAT_META_REFINE` is truthy or the prompt contains the marker
   `<!-- META_REFINE -->`, the raw ChatMarkdown is routed through
   `Recursive_mp.refine` before parsing.  In this mode the library runs in a
   conservative "metadata-only" configuration and never calls the network
   (no `Eio` environment is passed), which makes it safe for tests and offline
   workflows.
2. **Built-in tool `meta_refine`** – exposed from `Functions.meta_refine`.  When
   you declare `<tool name="meta_refine"/>` in a ChatMD prompt the assistant
   can delegate prompt improvement to a full LLM-backed refinement run driven
   by [`Mp_flow.first_flow`].  This is the easiest way to plug meta-prompting
   into agents and workflows.
3. **CLI `mp-refine-run`** – batch refinement from the command line:

   ```console
   $ mp-refine-run -task-file task.md > prompt.txt
   ```

   See [`docs-src/bin/mp_refine_run.doc.md`](../bin/mp_refine_run.doc.md) for
   the complete flag reference.

4. **MCP / remote tools** – when a ChatMD prompt that declares `meta_refine`
   is served via the MCP server, remote agents can call the tool using the
   standard MCP tool protocol; there is no bespoke JSON-RPC method.

All of these integrations reuse the same underlying `Recursive_mp` and
`Evaluator` modules so improvements automatically propagate across the stack.

---

## Writing a custom evaluator

```ocaml
module My_judge : Evaluator.Judge = struct
  let name = "starts_with_hello"
  let evaluate ?env:_ s =
    if String.is_prefix s ~prefix:"Hello" then 1.0 else 0.0
end

let my_ev =
  Evaluator.create ~judges:[ Evaluator.Judge (module My_judge) ] ()

let score = Evaluator.evaluate my_ev "Hello world!" (* ➜ 1.0 *)
```

Judges *must not* raise – add your own exception guards or rely on the
built-in helper `Evaluator.with_exception_guard`.

---

## Limitations & next steps

1. **Offline vs online modes** – when no `Eio` environment is provided
   `Recursive_mp.refine` degrades to a metadata-only transformation.  This is
   intentional for safety, but it means that the preprocessor hook will not
   call the network unless you go through [`Mp_flow`] / `mp-refine-run` or
   provide a [`Context.t`] with [`env`] set.
2. **Evaluator failure semantics** – most judges degrade to a neutral `0.5`
   score on errors, but the reward-model based judges still have a few error
   paths that raise.  Treat them as experimental and prefer offline judges in
   CI.
3. **Docs & examples** – the meta-prompting stack is evolving quickly; for the
   most accurate signatures always refer to the OCaml interface files (`*.mli`)
   in `lib/meta_prompting/` and the specialised docs under
   `docs-src/meta_prompting/*.doc.md`.


