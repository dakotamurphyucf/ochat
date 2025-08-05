# Prompting_guides module

> Location: `lib/meta_prompting/prompting_guides.ml`

## Overview

`Prompting_guides` is a thin wrapper around a handful of **large multi-line
strings** that the rest of the `ochat` code-base can embed directly into LLM
prompts.  The strings capture the project’s collective knowledge about how to
prompt different families of OpenAI models and are meant to be transmitted to
the model verbatim (e.g., in the **system** or **developer** message of an
OpenAI chat request).

No parsing or manipulation happens in this module – it simply exposes the
constants so that callers do not have to copy-and-paste the text or load it
from external files.

## Types

```ocaml
type t = string
```

The type alias makes the public API a little clearer – callers deal with
`Prompting_guides.t` rather than a raw `string`, signalling intent.

## Values

| Value | Description |
|-------|-------------|
| `gpt4_1_model_prompting_guide` | Best-practice guide aimed at the GPT-4.1 family of models. |
| `o_series_prompting_guide`    | Guidance tailored to the O-series *reasoning* models (`o1`, `o3`, `o4-mini`, …). |
| `general_prompting_guide`     | Model-agnostic principles – things that apply to (almost) every modern LLM. |
| `combined_headers`            | A short introductory header that precedes the combined guide. |
| `combined_guides`             | The result of `String.concat ~sep:"\n\n"` on `combined_headers`, `general_prompting_guide`, `o_series_prompting_guide`, and `gpt4_1_model_prompting_guide` – ready to be injected as a single prompt fragment. |

All guides are plain strings, so regular `string` operations from
`Core.String` (e.g., `String.prefix`, `String.substr`, `String.concat`)
can be used if further processing is needed.

## Usage Examples

### 1. Attaching the full guide to a system prompt

```ocaml
let system_prompt = Prompting_guides.combined_guides in

Openai.Chat.request
  ~model:"gpt-4o-preview"
  ~system:system_prompt
  ~messages:user_messages
  ()
```

### 2. Sending different guides to different models

```ocaml
let prompt_for_model model =
  match model with
  | "gpt-4.1" -> Prompting_guides.gpt4_1_model_prompting_guide
  | "o4-mini" -> Prompting_guides.o_series_prompting_guide
  | _         -> Prompting_guides.general_prompting_guide

(* later *)
Openai.Chat.request ~model ~system:(prompt_for_model model) ~messages ()
```

### 3. Appending project-specific instructions

```ocaml
let custom_system_message =
  Prompting_guides.combined_guides
  ^ "\n\n# Project-specific constraints\n"
  ^ "- Do *not* produce code with external side effects." in

(* ... *)
```

## Known limitations

1. **String size** – The guides are embedded directly in the executable.
   While this is convenient, it increases the binary size slightly.  If the
   guides grow considerably it may become preferable to load them from
   external resource files.
2. **No internal structure** – The guides are plain strings.  Callers that
   wish to render only parts (e.g., *just* the “Function Calling” section)
   must slice the string manually or switch to a structured representation
   (e.g., Markdown AST).

## Relationship to other modules

`Prompting_guides` intentionally has **no dependencies** beyond `Core`.  All
consumers can therefore depend on it without creating cyclic build
dependencies.

## Changelog

| Version | Changes |
|---------|---------|
| 1.0     | Initial extraction of the prompting guides into a standalone module (this documentation). |

