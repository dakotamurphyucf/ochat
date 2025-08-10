# `Meta_prompting.Prompt_factory`

Generate and refine *prompt packs* – self-contained multi-section strings that
capture every rule, constraint, and behavioural switch the assistant must
follow.  The factory is **pure** and therefore trivial to unit-test; it never
hits the network and never mutates global state.

> **Why does the project wrap prompts this way?**  Keeping the entire
> instruction set in a single, machine-parseable blob avoids coordination
> issues between the orchestration layer and the model.  It also allows
> the runner to diff two prompts as plain text.

---

## API at a glance

| Function | Use-case |
|----------|----------|
| `create_pack` | Build a brand-new prompt pack from scratch. |
| `iterate_pack` | Produce an *update pack* that describes the minimal changes required to move an existing prompt to the next revision. |

Supporting record types encode the required metadata:

* `create_params` – metadata for the *create* flow.
* `iterate_params` – metadata for the *iterate* flow.
* `eagerness` – how aggressively the assistant should advance before handing back control.

---

## `eagerness`

```ocaml
type eagerness =
  | Low    (* stop early, favour safety *)
  | Medium (* default *)
  | High   (* push until task is solved in one go *)
```

The value influences the `<agentic_controls/>` section of the pack:

* `Low` – `bounded_exploration` *on*, `persistence` *off*.
* `Medium` – balanced defaults.
* `High` – `persistence` *on*, higher tool-call budget.

---

## `create_params`

| Field | Semantics |
|-------|-----------|
| `agent_name` | Human-readable role title used in the system message. |
| `goal` | Single-sentence objective – the “why” behind the assistant’s actions. |
| `success_criteria` | Bullet-point list the assistant will treat as *must-haves*. |
| `audience` | Target readership (“technical users”, “product managers”, …). Optional. |
| `tone` | Communication style hint (“neutral”, “light-hearted”, …). Optional. |
| `domain` | Activate domain-specific module (only `"coding"` is recognised today). |
| `use_responses_api` | Whether the OpenAI *Responses API* extension should be used. |
| `markdown_allowed` | Whether the assistant may use Markdown in free-form text. |
| `eagerness` | See above. |
| `reasoning_effort` | Hint for the model: how much internal thinking is expected. |
| `verbosity_target` | Target verbosity of *user-facing* text (does **not** limit code or diffs). |

Any field that is `option`al falls back to a policy-approved default.

---

## `iterate_params`

The *iterate* flow shares many fields with `create_params` but adds three more
lists so that the caller can specify desired and undesired behaviours as well
as explicit stop conditions.  All lists may be empty; placeholders will be
inserted to keep the resulting pack structurally valid.

---

## Examples

### 1. Generating a prompt pack for a coding assistant

```ocaml
open Meta_prompting

let pack =
  let params : Prompt_factory.create_params =
    { agent_name         = "Code-GPT"
    ; goal               = "Write idiomatic, well-tested OCaml code"
    ; success_criteria   =
        [ "Passes all existing expect tests"
        ; "Fits Jane Street style guide"
        ]
    ; audience           = Some "experienced OCaml developers"
    ; tone               = Some "concise"
    ; domain             = Some "coding"
    ; use_responses_api  = true
    ; markdown_allowed   = true
    ; eagerness          = High
    ; reasoning_effort   = `Medium
    ; verbosity_target   = `Low
    } in
  Prompt_factory.create_pack params
    ~prompt:"Please refactor module X for clarity." in

print_endline pack
```

### 2. Requesting a minimal tweak to an existing prompt

```ocaml
open Meta_prompting

let update =
  let p : Prompt_factory.iterate_params =
    { goal               = "Tighten safety constraints"
    ; desired_behaviors  = [ "Ask for confirmation before deleting files" ]
    ; undesired_behaviors = []
    ; safety_boundaries  = [ "No shell access w/o confirmation" ]
    ; stop_conditions    = []
    ; reasoning_effort   = `Low
    ; verbosity_target   = `Medium
    ; use_responses_api  = false
    } in
  Prompt_factory.iterate_pack p
    ~current_prompt:previous_pack in

print_endline update
```

---

## Known limitations / future work

* **Schema drift** – as the advisory JSON schema evolves the record types need
  manual alignment; failing to do so results in missing or mis-ordered
  sections.
* **Domain modules** – only `"coding"` is recognised.  Future versions may
  load snippets dynamically.
* **Formatting** – the generator does *not* wrap long lines.  Callers who plan
  to show packs in narrow UIs should pre-wrap if needed.

