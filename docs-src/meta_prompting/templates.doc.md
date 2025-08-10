# `Meta_prompting.Templates`

Comprehensive documentation for the bundled prompt templates that live
in `lib/meta_prompting/templates.{mli,ml}`.

The module is intentionally tiny — it merely exposes **three** constant
strings — yet those strings are *the* backbone of the meta-prompting
sub-system:

| Constant                         | Purpose                                                      |
|----------------------------------|--------------------------------------------------------------|
| `iteration_prompt_v2`            | Fix and **optimise** an *existing* prompt                    |
| `generator_prompt_v2`            | **Generate** a brand-new prompt pack from structured inputs  |
| `system_prompt_guardrails`       | Inject repository-wide safety & formatting guard-rails       |

All texts are embedded at compile-time which eliminates run-time file
lookups and guarantees that the *canonical* versions are always
available.  Higher-level helpers such as
[`Prompt_factory`](prompt_factory.doc.md) allow callers to override the
defaults when needed.

---

## 1  Why keep prompts in code?

1. **Atomic versioning**   The templates evolve in lock-step with the
   code that uses them.  A single Git commit can adjust both the OCaml
   parsing logic **and** the referenced template sections, avoiding
   subtle mismatches.
2. **No I/O during start-up**   Embedding avoids disk access and
   `Sys.getcwd` shenanigans when the library is used from inside a
   sandbox or pre-compiled binary.
3. **Type safety**   The constants are plain `string` — there is no
   partial failure mode.  Reading an external file could raise
   `Sys_error` which would then have to be handled by every consumer.

---

## 2  `iteration_prompt_v2` — Prompt Iteration & Optimisation

### 2.1  High-level description

`iteration_prompt_v2` turns a **CURRENT_PROMPT** into an *improved*
version that

* removes contradictions,
* calibrates agentic behaviour (eagerness vs safety), and
* aligns with explicit *success criteria*.

The instructions are fairly prescriptive: the LLM must emit an
*Overview*, an *Issues_Found* list, a *Minimal_Edit_List* including
exact insert/delete/replace operations, the *Revised_Prompt* and a
small *Test_Plan & Telemetry* section.

### 2.2  Typical usage

```ocaml
open Meta_prompting

let optimise ~current_prompt ~goal ~success_criteria () =
  let messages =
    [ `System Templates.iteration_prompt_v2
    ; `User (Printf.sprintf {|CURRENT_PROMPT:
%s

GOAL: %s
SUCCESS_CRITERIA: %s|}
        current_prompt goal success_criteria)
    ]
  in
  Openai.Chat.create ~model:"gpt-5" ~messages ()
```

### 2.3  Output contract cheat-sheet

| Section                | Expected format                                          |
|------------------------|----------------------------------------------------------|
| `Overview`             | 2–3 sentences summarising detected problems and approach |
| `Issues_Found`         | Bullet list, one bullet per issue                        |
| `Minimal_Edit_List`    | *Add* / *Delete* / *Replace* pairs with **exact** text   |
| `Revised_Prompt`       | The full improved prompt                                 |
| `Optional_Toggles`     | `<persistence>` & `<bounded_exploration>` sub-tags       |
| `API_Parameter_Suggestions` | JSON-like key/value list (model, temp, …)          |
| `Test_Plan` & `Telemetry` | Free-form yet concise                                 |

---

## 3  `generator_prompt_v2` — Prompt-Pack Generator

### 3.1  When to use

You have *no prompt yet* but a fairly structured task description:

* **GOAL**, **SUCCESS_CRITERIA**
* **DOMAIN** information (programming language, business process …)
* **ENVIRONMENT_TOOLS** with rate limits and safety levels
* … and so forth

`generator_prompt_v2` converts those fields into a *complete* prompt
pack ready to be sent to a production LLM.

### 3.2  Sections of the resulting pack

1. `<system_prompt>`    Role, objective and instruction hierarchy
2. `<assistant_rules>`  Do/Don’t lists, stop conditions, knowledge boundaries
3. `<tool_preambles>`  Plan / progress / summary markers
4. `<agentic_controls>`  Persistence & bounded exploration toggles
5. `<context_gathering>`  Early-stop criteria and budget defaults
6. `<formatting_and_verbosity>`  Markdown allowance & verbosity policy
7. `<domain_module>`    Optional domain-specific add-on
8. `<safety_and_handback>`  Permissions and escalation rules

### 3.3  Smoke-test snippet

```ocaml
let%expect_test "prompt-pack generator returns required sections" =
  let open Yojson.Safe.Util in
  let output =
    (* minimal call leaving most optional fields empty *)
    Openai.Chat.simple_completion
      ~system:Meta_prompting.Templates.generator_prompt_v2
      ~user:"AGENT_NAME: Tutor\nGOAL: Teach OCaml basics\nSUCCESS_CRITERIA: Student writes a hello-world program"
  in
  (* naïve check — real code should use a proper parser *)
  assert (String.is_substring ~substring:"<system_prompt>" output);
  assert (String.is_substring ~substring:"<assistant_rules>" output)
```

---

## 4  `system_prompt_guardrails` — Repository Guard-rails

Short fragment that must be prepended to **every** system prompt when
interacting with an agent that lives inside the *OChat* runtime.

The guard-rails clarify:

* *precedence* between safety, codebase policy, and external schemas
* permitted **tool-calling** patterns and the single-message JSON rule
* how to handle **context gathering** and caching
* formatting constraints (no Markdown unless allowed, no giant outputs)

### 4.1  Quick merge helper

```ocaml
let with_guardrails prompt =
  Meta_prompting.Templates.system_prompt_guardrails ^ "\n" ^ prompt
```

Use the helper whenever you build system prompts dynamically.

---

## 5  Known limitations

1. **Template size**   The three constants together weigh in at ~16 kB
   of raw text.  While that is fine for modern LLM context windows it
   can be an overhead in ultra-low-latency settings.
2. **Static text**   Any change requires a recompilation.  If your
   application demands hot-swapping consider loading user overrides
   from a database and falling back to the constants only as last
   resort.

---

## 6  Further reading

* “Self-Consistency Improves Chain of Thought Reasoning in LLMs” — Wang
  *et al.* 2022 (the idea behind *minimal_reasoning_helper*)
* [OpenAI function calling & *Responses API*](https://platform.openai.com/docs/guides/function-calling)
* [Guard-rails vs. LLM prison breaks — best practices](https://arxiv.org/abs/2305.15324)

