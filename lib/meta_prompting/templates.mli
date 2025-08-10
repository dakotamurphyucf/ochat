(** Bundled prompt templates and integration guardrails.

    The *meta-prompting* subsystem needs a handful of rather lengthy
    prompt texts that are sent as *system* or *developer* messages to
    a Large Language Model (LLM).  Storing those texts in external
    files would introduce I/O at startup and brittle run-time
    path-search logic.  Therefore the canonical versions are embedded
    directly into the compiled binary and exposed via three
    constants.

    Each constant contains a fully-formed prompt including headline,
    task description, required output sections and helpful snippets.
    All texts are strictly ASCII – this avoids encoding issues when
    they are serialised to JSON and shipped over HTTP.

    Override strategy:  higher-level helpers such as
    {!Meta_prompting.Prompt_factory} allow callers to replace the
    embedded templates with custom ones at run-time.  The constants
    below therefore serve as **defaults** rather than hard-wired
    mandates.
*)

(** Full text of the *Prompt Iteration and Optimisation – version 2*
    template.

    Typical usage — send as *system* message to an LLM that shall act
    as a *prompt optimiser*:
    {[
      let open Openai.Chat in
      create
        ~model:"gpt-5"
        ~messages:[ `System Meta_prompting.Templates.iteration_prompt_v2
                   ; `User   (Prompt.to_string current_prompt)
                   ] ()
    ]}

    The template asks the model to ❶ diagnose issues in
    [CURRENT_PROMPT], ❷ emit a minimal edit list and ❸ return a revised
    version that meets the supplied success criteria.  See
    {!docs-src/meta_prompting/templates.doc.md} for a detailed walk-
    through of every section.
*)
val iteration_prompt_v2 : string

(** Full text of the *Prompt Pack Generator – version 2* template.

    Whereas {!iteration_prompt_v2} *fixes* an existing prompt, this
    template *creates* a production-ready prompt pack from scratch
    given structured metadata such as [GOAL], [DOMAIN] and
    [STOP_CONDITIONS].

    The resulting pack encompasses:
    • a system prompt
    • assistant rules
    • tool preambles
    • agentic controls
    • context-gathering rules and safety guardrails

    @see <docs-src/meta_prompting/templates.doc.md> for an in-depth
    description and example output.
*)
val generator_prompt_v2 : string

(** Repository-wide integration guardrails.

    A short system prompt fragment that reminds downstream agents of
    the *OChat* tool contract: precedence rules, tool-calling
    constraints, formatting requirements and context-gathering best
    practices.  Prepend this string to any bespoke system prompt when
    you need to ensure compatibility with the chat-agent harness.

    Example:
    {[
      let combined_system_prompt =
        Meta_prompting.Templates.system_prompt_guardrails ^ "\n" ^ my_custom_prompt
    ]}
*)
val system_prompt_guardrails : string
