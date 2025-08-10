(** Prompt_factory – one-stop **string-only** generator for the so-called
    *prompt packs* that drive the meta-prompting pipeline.

    A *prompt pack* is a single multiline string that contains every piece of
    guidance the assistant needs – from the high-level system message down to
    telemetry guidelines.  The exact sections, their names, and their order
    are dictated by the repository’s prompt specification (see
    [meta-prompt/spec/input_schema.json]) and must stay stable so that the
    downstream runner can parse them reliably.

    This module provides two high-level helpers:

    • {!create_pack} – build a brand-new pack from the user intent.
    • {!iterate_pack} – take an *existing* prompt and emit the textual diff /
      replacement instructions required to reach the next revision.

    Both helpers are **pure** – they perform *no* I/O.  That means they can be
    used inside expect tests and do not need mocking.  Any optional or
    missing field is filled with a conservative default that matches the
    current repository policies.  Adding a new optional field therefore never
    breaks old call-sites. *)

open! Core

(** {1:types Types}

    The concrete generators are parameterised by a handful of record types
    that capture *only* the information needed for the task at hand.  Keeping
    the records minimal helps unit-testing and prevents accidental divergence
    from the advisory schema. *)

(** Eagerness profile – how hard the assistant should push forward before
    handing control back to the user.  The level tunes the behaviour of the
    <agentic_controls/> section. *)
type eagerness =
  | Low
  | Medium
  | High

(** Parameters required to *create* a brand-new prompt pack.  Every field maps
    1-to-1 to the advisory schema so that upstream callers can serialise the
    value to JSON if required.

    Missing optional fields are filled with policy-driven defaults inside
    {!create_pack}. *)
type create_params =
  { agent_name : string
  ; goal : string
  ; success_criteria : string list
  ; audience : string option
  ; tone : string option
  ; domain : string option
  ; use_responses_api : bool
  ; markdown_allowed : bool
  ; eagerness : eagerness
  ; reasoning_effort : [ `Minimal | `Low | `Medium | `High ]
  ; verbosity_target : [ `Low | `Medium | `High ]
  }

(** [create_pack params ~prompt] assembles a complete *prompt pack*.

    • [prompt] – free-form user instruction that becomes the body of the
      system message.
    • [params] – see {!create_params}.

    The resulting string contains – in this order – every mandatory section
    understood by the runner:

    1. [<system_prompt/>]
    2. [<assistant_rules/>]
    3. [<tool_preambles/>]
    4. [<agentic_controls/>]
    5. [<context_gathering/>]
    6. [<formatting_and_verbosity/>]
    7. Optional [<domain_module/>] (only when [params.domain] = [Some _])
    8. [<safety_and_handback/>]
    9. [Recommended_API_Parameters]
   10. Smoke-test checklist and telemetry boilerplate.

    @return Fully-formed prompt pack – **ready to send to the model**. *)
val create_pack : create_params -> prompt:string -> string

(** Parameters used when *iterating* (refining) an existing prompt pack.
    All lists may be empty – the function will then emit placeholders to make
    manual review easier. *)
type iterate_params =
  { goal : string
  ; desired_behaviors : string list
  ; undesired_behaviors : string list
  ; safety_boundaries : string list
  ; stop_conditions : string list
  ; reasoning_effort : [ `Minimal | `Low | `Medium | `High ]
  ; verbosity_target : [ `Low | `Medium | `High ]
  ; use_responses_api : bool
  }

(** [iterate_pack params ~current_prompt] analyses [current_prompt] and emits
    an *update pack* – effectively a text-only pull request that explains what
    to change and why.

    The pack is structured into human-readable sections ([Issues_Found],
    [Minimal_Edit_List], …) so that a reviewer can quickly spot the relevant
    bits.

    Use this helper when the assistant has already produced a prompt but
    further fine-tuning is needed (e.g. add an extra safety boundary).

    @return Update pack string – safe for expect tests. *)
val iterate_pack : iterate_params -> current_prompt:string -> string
