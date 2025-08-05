# `Meta_prompting.Context`

Context values are the **thread that ties together every stage of the meta-prompting pipeline**.  Rather than relying on global mutable state the pipeline passes an immutable record of type `Context.t` from one helper to the next.  Each helper can read the information it needs (randomness, environment, preferred model, …) and – if necessary – create an updated copy using the `with_*` helper functions.

## The record

```ocaml
type t = {
  proposer_model    : Model.model option;  (** LM that proposes prompts *)
  rng               : Random.State.t;      (** deterministic randomness *)
  env               : Eio_unix.Stdenv.base option; (** OS resources *)
  guidelines        : string option;       (** extra system-level advice *)
  model_to_optimize : Model.model option;  (** target of the optimisation *)
  action            : action;              (** generate vs. update run  *)
  prompt_type       : prompt_type;         (** general vs. tool prompt  *)
}
```

Field semantics are documented in the interface, but the guiding rule is simple: **if two separate stages need to agree on a value, add it to the context.**

### Discriminators

* `prompt_type`  – distinguishes between free-form assistant prompts (`General`) and prompts specialised for tool calls (`Tool`).
* `action`       – tells later stages whether they are _creating_ a new prompt (`Generate`) or _refining_ an existing one (`Update`).

## Constructors and helpers

| Function | Use-case |
|----------|---------|
| `default` | Start a fresh run with deterministic RNG and sensible defaults (`Model.O3`, `Generate`, `General`). |
| `with_proposer_model` | Override the language model that proposes candidate prompts. |
| `with_guidelines` | Inject additional textual guidelines (e.g. “Be concise”). |

All helpers return **new values** – the original context remains unmodified.

## Examples

### Minimal

```ocaml
open Meta_prompting

let ctx = Context.default ()
```

### Customising the proposer model

```ocaml
open Meta_prompting

let ctx =
  Context.default ()
  |> Context.with_proposer_model
       ~model:(Some Openai.Responses.Request.Model.Gpt4)
```

### Using the context inside `Eio_main`

```ocaml
let () =
  Eio_main.run @@ fun env ->
  let ctx =
    Meta_prompting.Context.default ()
    |> Meta_prompting.Context.with_guidelines
         ~guidelines:(Some "Prioritise clarity over brevity.")
    |> fun c -> { c with env = Some env } in

  Meta_prompting.run ctx
```

## Limitations / Future work

* The record grows organically as the pipeline evolves – over time we
  may want to replace some boolean-ish fields with more structured
  sub-records to improve clarity.
* Call-sites that only require a small subset of the fields currently
  need to accept the full `t`.  A future breaking change may split the
  record into smaller capability-oriented fragments.

