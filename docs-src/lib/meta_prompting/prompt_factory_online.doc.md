Prompt_factory_online
=====================

High-level overview
-------------------

`Prompt_factory_online` is a *thin* wrapper around the experimental
OpenAI “/v1/responses” endpoint.  It is used by the meta-prompting
pipeline to perform *online* prompt optimisation:

1. The caller supplies the **goal** and the **current prompt**.
2. The helper composes a short system + user exchange and sends it to
   the language model.
3. The assistant replies with a multi-section document that must contain
   a `Revised_Prompt` section.
4. The module extracts that section and returns it to the caller.

All side effects (network, file reads, logging) are hidden
inside the implementation so the external API remains purely
functional – failures are reported via `option`.

Public API
----------

### `extract_section`

```ocaml
val extract_section : text:string -> section:string -> string option
```

Parse a large text blob and return the contents of a
section designated by its heading (the heading must include the trailing
newline).  Recognised headings are hard-coded and follow the house style
used in all meta-prompting templates:

* `Overview`
* `Issues_Found`
* `Minimal_Edit_List`
* `Revised_Prompt`
* `Optional_Toggles`
* `API_Parameter_Suggestions`
* `Test_Plan`
* `Telemetry`

If the section cannot be found, or if it is empty/contains only
whitespace, the function returns `None`.

Example:

```ocaml
# let text =
    "Overview\nfoo\nRevised_Prompt\nbar\nTelemetry\nbaz\n";;
val text : string = "..."

# Prompt_factory_online.extract_section
    ~text ~section:"Revised_Prompt\n";;
- : string option = Some "bar"
```

### `iterate_revised_prompt`

```ocaml
val iterate_revised_prompt :
  env:Eio_unix.Stdenv.base ->
  goal:string ->
  current_prompt:string ->
  proposer_model:Openai.Responses.Request.model option ->
  string option
```

Contact the OpenAI service and ask it to produce an improved version of
`current_prompt` with respect to `goal`.  The request uses the *system*
template `meta-prompt/templates/iteration_prompt_v2.txt` (fallback is a
small hard-coded stub) and always appends the repository-wide
integration guardrails.

The language model can be overridden via `~proposer_model`; when left
`None` the helper defaults to `Openai.Responses.Request.O3` (fast / low
cost).  The call blocks until the response is received.

Possible outcomes:

* **`Some revised`** – success; the revised prompt is returned.
* **`None`** – any of the pre-conditions failed:
  * `OPENAI_API_KEY` missing;
  * network error or invalid JSON response;
  * `Revised_Prompt` section absent in the reply.

### `create_pack_online`

```ocaml
val create_pack_online :
  env:Eio_unix.Stdenv.base ->
  agent_name:string ->
  goal:string ->
  proposer_model:Openai.Responses.Request.model option ->
  string option
```

Generate a full *prompt pack* from scratch.  The function

1. loads `meta-prompt/templates/generator_prompt_v2.txt` (or falls back
   to an inline stub),
2. appends the same integration guard-rails used by
   `iterate_revised_prompt`,
3. builds a user message that describes the target `agent_name`, the
   `goal` and several default generation parameters, and
4. performs a single blocking call to `Openai.Responses.post_response`.

On success the assistant output is returned **verbatim** – callers are
expected to parse out the individual sections (e.g. `System_Prompt`,
`Evaluation_Criteria`, …) depending on their needs.  `None` is returned
under the same failure modes as `iterate_revised_prompt` (missing API
key, transport error, malformed payload, etc.).

Usage example:

```ocaml
open Eio.Std

let bootstrap_agent env ~agent_name ~goal =
  match Prompt_factory_online.create_pack_online
          ~env ~agent_name ~goal ~proposer_model:None with
  | Some pack -> pack
  | None -> failwith "Unable to obtain initial prompt pack"
```

Usage example
-------------

```ocaml
open Eio.Std

let optimise_prompt env ~goal ~prompt =
  match Prompt_factory_online.iterate_revised_prompt ~env ~goal ~current_prompt:prompt ~proposer_model:None with
  | Some revised -> revised
  | None ->
      (* Fallback to offline heuristic *)
      Prompt_factory.iterate_pack
        { goal
        ; desired_behaviors = []
        ; undesired_behaviors = []
        ; safety_boundaries = []
        ; stop_conditions = []
        ; reasoning_effort = `Low
        ; verbosity_target = `Low
        ; use_responses_api = false
        }
        ~current_prompt:prompt
```

Known limitations
-----------------

* Relies on the unreleased "responses" API – availability and schema
  stability are not guaranteed.
* No automatic pagination of large assistant outputs; sections larger
  than a couple of hundred kilobytes may cause out-of-memory failures.
* Uses simple string matching; heading typos or alternative capitalisation
  will prevent `extract_section` from finding the desired block.

Implementation notes
--------------------
The module purposefully avoids additional dependencies and keeps the
implementation <200 lines.  Any unexpected exception is caught in
`iterate_revised_prompt`, logged via `Log.emit` at `Debug` level and
translated to `None` so that callers can silently fall back to a local
strategy.

Runtime expectations
--------------------

* `OPENAI_API_KEY` – must be exported for any online call to succeed.
* Optional template overrides can be dropped in
  `meta-prompt/templates` under the current working directory:
  * `iteration_prompt_v2.txt`
  * `generator_prompt_v2.txt`
* Repository-wide guard-rails live in
  `meta-prompt/integration/system_prompt_guardrails.txt`.  A baked-in
  fallback is used when the file is missing.

Behaviour overrides
-------------------

Put key–value pairs in a plain-text file and point the environment
variable `META_PROMPT_ONLINE_CONFIG` to it to tweak defaults without
recompilation:

```text
# iteration API
iterate.reasoning_effort: high
iterate.verbosity: medium

# generator API
create.domain = finance
create.markdown_allowed = false
```

See `load_kv_overrides` in the implementation for the full list of
available keys.

