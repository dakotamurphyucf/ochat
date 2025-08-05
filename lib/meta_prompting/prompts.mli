(** Predefined meta-prompts used by the {!module:Meta_prompting} subsystem.

    Each value is a fully-formed **system prompt template** that can be
    sent directly to a large-language model.  The templates encode the
    most up-to-date best practices for prompt engineering and therefore
    act as the building blocks for higher-level modules such as
    {!module:Recursive_mp}.

    There are two families of templates:

    • **OpenAI / GPT-4 series** – constants whose names start with
      [openai_].  They follow the public “GPT-4.1 Best Practices” white
      paper and tend to allow chain-of-thought reasoning in the agent’s
      output (unless the downstream task says otherwise).

    • **O-Series** – constants that end with [_o3].  They target the
      experimental “o3 / o4” models and therefore omit or adapt
      guidance that is known to have different effects on that model
      family (most notably: never exposing chain-of-thought).

    In normal usage the templates are picked automatically; they are
    exposed here mainly to facilitate experimentation, tests and
    debugging. *)

(** System-prompt template that **generates a brand-new prompt** from a
    plain task description.  The agent is instructed to apply GPT-4.1
    best practices, reason before concluding, add examples when
    beneficial, and spell out an explicit *Output Format* section.  The
    result is meant to be inserted verbatim into the [system]
    role of an OpenAI chat request. *)
val openai_system_instructions_prompt : string

(** System-prompt template that **revises an existing baseline prompt**.
    The agent must keep the original wording and order as intact as
    possible while surgically injecting improvements that comply with
    the GPT-4.1 reference guide.  The output must be the full revised
    prompt only – no commentary, no diff markers. *)
val openai_system_edit_instructions_prompt : string

(** {!openai_system_instructions_prompt} adapted for **O-Series**
    models.  The wording reflects O-Series guidelines such as *“no
    chain-of-thought exposure”* and *“duplicate agentic reminders in
    long prompts”*. *)
val openai_system_instructions_prompt_o3 : string

(** O-Series counterpart of {!openai_system_edit_instructions_prompt}. *)
val openai_system_edit_instructions_prompt_o3 : string

(** Template that converts a raw function or tool signature (delimited
    by `<raw-tool> … </raw-tool>`) into a polished, multi-bullet
    description ready to be used in the [`tools.description`] field of
    an OpenAI request.  The resulting text explains:

    – *When to use* / *When NOT to use* the tool;
    – All argument types, formats and validation rules;
    – Preconditions and common failure modes;
    – Decision boundaries when multiple tools overlap.

    The agent must output a single plain-text block without code fences.
*)
val openai_tool_description_prompt : string
