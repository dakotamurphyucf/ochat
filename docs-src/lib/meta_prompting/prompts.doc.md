# `Meta_prompting.Prompts`

Predefined meta-prompts that encapsulate the “secret sauce” of the meta-prompting
sub-system.  Each value is a plain `string` ready to be fed to an LLM – no
interpolation, no extra wrappers required.  The templates implement two kinds of
workflows:

* **System-prompt generation** – given a natural-language task description,
  produce a full-blown *system* prompt that follows state-of-the-art prompt
  engineering guidelines.
* **Prompt editing** – given an existing (baseline) prompt, revise it in-place so
  that it complies with best-practice check-lists without changing the original
  structure more than necessary.


## Quick example

```ocaml
open Meta_prompting

let my_system_prompt : string =
  Prompts.openai_system_instructions_prompt

(* send [my_system_prompt] as the [system] message when calling the OpenAI API *)
```

## Constants

| Value | Purpose | Target model family |
|-------|---------|----------------------|
| `openai_system_instructions_prompt` | Generate a new system prompt from a task description | GPT-4 / GPT-3.x |
| `openai_system_edit_instructions_prompt` | Revise an existing prompt in-place | GPT-4 / GPT-3.x |
| `openai_system_instructions_prompt_o3` | Same as the first entry, but tuned for the O-Series models | **O-Series** (o3, o4, …) |
| `openai_system_edit_instructions_prompt_o3` | Same as the second entry, tuned for O-Series | **O-Series** |
| `openai_tool_description_prompt` | Turn a raw tool/function signature into a fully-fledged `description` field | Any tool-calling capable model |


### `openai_system_instructions_prompt`

Creates a **new** system prompt from scratch.  The template instructs the agent
to:

* understand the task at hand,
* reason before concluding,
* include examples if they increase clarity,
* specify the output format explicitly, and
* keep the overall wording concise and action-oriented.

Use this when you only have a vague task description and need the agent to turn
it into a production-ready system prompt.


### `openai_system_edit_instructions_prompt`

Helps you **improve** an existing prompt rather than starting from scratch.  The
agent must *not* rewrite sections wholesale; instead it “surgically” adds or
removes text so that the revised prompt aligns with the GPT-4.1 Best Practices
reference.  The output is the full, self-contained prompt — no diffs, no
comments.


### `openai_system_instructions_prompt_o3` and `openai_system_edit_instructions_prompt_o3`

These variants target the O-Series model family.  The biggest differences
compared to the regular OpenAI versions are:

* **No chain-of-thought exposure** – the agent is explicitly told *not* to reveal
  its reasoning.
* **Agentic reminders** – persistence, tool-calling, and planning hints are
  emphasised because O-Series models are often used in autonomous agent
  scenarios.

Otherwise the structure and intent mirror their OpenAI counterparts.


### `openai_tool_description_prompt`

Turns a raw tool or function signature into a detailed description that can be
used in the `tools.description` field of an OpenAI request.  The resulting text
includes:

* When the tool **should** and **should NOT** be used.
* Validation constraints for every argument (type, format, default values).
* Common failure modes and how the caller can avoid them.
* Decision boundaries when multiple tools do similar things.


## Known limitations

1. The templates are hand-crafted and may lag behind future guideline
   revisions.
2. They assume the agent is allowed to see the full prompt; redact or encrypt
   sensitive data beforehand if that is not the case.
3. The strings are intentionally long (≈ 250–600 lines each); consider storing
   them in a compressed data section if binary size is a concern.


## Version history

* **v1.0** – initial public release, aligned with GPT-4.1 and early O-Series
  best practices.

