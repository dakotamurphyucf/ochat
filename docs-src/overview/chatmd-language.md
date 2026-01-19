# ChatMarkdown ( ChatMD ) language

ChatMD combines the readability of Markdown with a minimal XML vocabulary.
The model sees **exactly** what you write – no hidden pre-processing.
ChatMD recognises a fixed set of tags; any other XML/HTML markup is preserved as literal text inside messages and passed through to the model unchanged.

| Element | Purpose | Important attributes |
|---------|---------|----------------------|
| `<config/>` | Current model and generation parameters. **Must appear once** near the top. | `model`, `max_tokens`, `temperature`, `reasoning_effort`, **`show_tool_call`** (flag), optional `id` label.  When `show_tool_call` is present the runtime embeds tool-call **arguments & results inline**; when absent they are written to disk and referenced via `<doc/>`. |
| `<tool/>` | Declare a tool that the assistant may call. | • `name` – function name.<br>• Built-ins only need `name` (`apply_patch`, …).<br>• **Shell wrappers** add `command="rg"` + optional `description`.<br>• **Agent-backed** tools add `agent="./file.chatmd"` (plus `local` if the agent lives on disk).<br>• **MCP-backed** tools add `mcp_server="…"` plus optional `name` / `include` / `includes` (to select tools), `strict` (flag), `client_id_env` and `client_secret_env` for auth.<br>• Exactly one of `command`, `agent` or `mcp_server` may be present; combining them is rejected. |
| `<msg>` | Generic chat message. | `role` one of `system,user,assistant,developer,tool`; optional `name`, `id`, `status`.  Assistant messages that *call a tool* additionally set `tool_call="true"` and provide `function_name` + `tool_call_id`. |
| `<user/>` / `<assistant/>` / `<developer/>` / `<system>` | **Shorthand** wrappers around `<msg …>` for the common roles. | Accept the same optional attributes (`name`, `id`, `status`). |
| `<tool_call/>` | Assistant *function invocation* shorthand – equivalent to `<msg role="assistant" tool_call …>` | Must include `function_name` & `tool_call_id`.  Body carries the JSON argument object (often wrapped in `RAW|…|RAW`). |
| `<tool_response/>` | Tool reply shorthand – equivalent to `<msg role="tool" …>` | Must include the matching `tool_call_id`. Body contains the return value (or error) of the tool. |
| `<reasoning>` | Chain-of-thought scratchpad emitted by reasoning models.  Normal prompts rarely use it directly. | `id`, `status`.  Contains one or more nested `<summary>` blocks. |
| `<import/>` | Include another file *at parse time*.  Keeps prompts small while re-using large policy docs. | `src` – relative path of the file to include. Imports are expanded inside user/system/developer messages and inside `<user>`, `<system>`, `<developer>` and `<agent>` bodies; elsewhere `<import>` is preserved as literal text. |

`<config>` currently wires through the subset of OpenAI chat parameters that Ochat understands: `model`, `max_tokens`, `temperature`, `reasoning_effort`, `show_tool_call` (flag) and an optional `id` label. Unknown attributes are ignored and additional parameters such as `response_format` or `top_p` are configured outside ChatMD today. Example:

```xml
<config model="gpt-4o" temperature="0.2" max_tokens="1024" reasoning_effort="detailed"/>
```

---

## Inline content helpers

Inside various element bodies you can embed richer content that is expanded **before** the request
is sent to OpenAI:

| Tag | Effect |
|-----|--------|
| `<img src="path_or_url" [local] />` | Embeds an image. If `local` is present the file is encoded as a data-URI so the API sees it. |
| `<doc src="path_or_url" [local] [strip] [markdown]/>` | Inlines the *text* of a document. <br>• `local` reads from disk.  <br>• Without it the file is fetched over HTTP.<br>• `strip` removes HTML tags (useful for web pages). <br>• `markdown` converts the document to Markdown format.<br>• If both `strip` and `markdown` are present, HTML-stripping wins and the content is treated as plain text. |
| `<agent src="prompt.chatmd" [local]> … </agent>` | Runs the referenced chatmd document as a *sub-agent* and substitutes its final answer.  Any nested content inside the tag is appended as extra user input before execution. |

### The `<agent/>` element – running sub-conversations

An **agent** lets you embed *another* chatmd prompt as a sub-task and reuse its answer as
inline text.  Think of it as a one-off function call powered by an LLM for prompt engineering.

• `src` is the file (local or remote URL) that defines the agent’s prompt.  
• Add the `local` attribute to read the file from disk instead of fetching over HTTP.  
• Any child items you place inside `<agent>` become *additional* user input that is appended
  to the sub-conversation *before* it is executed.

Example – call a documentation-summary agent and insert its answer inside the current
message:

```xml
<msg role="user">
  Here is a summary of the README:
  <agent src="summarise.chatmd" local>
     <doc src="README.md" local strip/>
  </agent>
</msg>
```

At runtime the inner prompt `summarise.chatmd` is executed with the stripped text of the
local `README.md` as user input, and the resulting summary is injected in place of the
`<agent>` tag.

---

## End-to-end example

```xml
<config model="gpt-4o" temperature="0.1" max_tokens="512"/>

<tool name="odoc_search" description="Search local OCaml docs"/>

<system>Answer strictly in JSON.</system>

<user>Find an example of Eio.Switch usage</user>
```

When the assistant chooses to call the tool Ochat inserts a `<tool_call>`
element, streams the live output, appends a `<tool_response>` block with the
result and finally resumes the assistant stream – **all captured in the same
file**.

---

## Useful Conceptual model

I have found it helpful to think about ChatMD, agents, and conversations in functional terms.

### Agents

You can think of an **agent** (defined as a ChatMD document) as a function — but instead of being implemented as a well-defined algorithm, it is defined as a **high-level description** of the algorithm (a compressed specification): what the agent is trying to do, what tools it may use, and any constraints/policies.

At runtime, the system “decompresses” (or compiles) that description into concrete steps **conditioned on the current input and context**, executes those steps, and returns the result.

That decompression is performed by the LLM: given the agent description, the accumulated context, and the current input, the model fills in the missing details of *how* to implement the high-level intent.

Operationally, running an agent is a loop:

1. Provide the LLM with:
   - the agent description (your ChatMD file),
   - the current context (conversation history, tool outputs, any imported artefacts) and
   - the current input (user message / tool result).
2. The LLM either:
   - produces a final output (an assistant message), or
   - produces a set of concrete steps to execute (for example: “call tool X with arguments Y”).
3. If the model requested steps:
   1. execute them (call tools as needed),
   2. collect the results, and
   3. feed those results back into the next iteration.

In this model, tool calls are the “instruction tape” for concrete actions, and the transcript is the ground truth record of what happened.

### Conversation/Session

A **conversation** can be described in functional terms as a reducer (fold): a loop that repeatedly applies “run agent” to successive inputs.

Each turn takes:

- an input, and
- the current agent state,

and produces:

- an output, and
- a new agent state whose description incorporates prior instructions plus runtime history.

In practice, that “state” is primarily the accumulated ChatMD transcript: the agent definition plus the sequence of user messages, assistant messages, tool calls, and tool results.

If you model a single turn as:

```text
run_agent : agent_state -> input -> (output * agent_state)
```

then a multi-turn conversation is just a fold over inputs:

```text
conversation : inputs -> agent_state0 -> agent_stateN
conversation inputs s0 = fold_left run_agent s0 inputs
```

### Putting it all together

This is the core mental model behind Ochat: “agents as text-defined functions” and “conversations as stateful folds”, with the LLM providing the dynamic expansion from high-level specification to concrete, tool-mediated execution. You can think of building agents as writing a programs composed of higher-order functions that leverage the LLM to fill in the implementation details, while conversations become straightforward applications of those functions over sequences of inputs. With this perspective, you can approach prompt engineering and agent design with the same principles you would use in traditional software development, focusing on modularity, clarity, and composability.

With this mental model in place, it becomes easier to reason about how to structure prompts, design tools, and manage state over time because it gives insight into how to think about prompt design and its role in agent behaviour. Thinking about the prompt as a compressed specification and the model as the dynamic decompressor leads to the insight that for the model to produce effective behaviour, the prompt must effectively compress the desired runtime behaviour in the high level description such that the model can reliably expand it into the intended sequence of actions. 

This means that prompt design becomes an exercise in finding the right abstractions and constraints to guide the model's behaviour, rather than trying to micromanage every detail of the execution. So it helps to think though your prompt and ask yourself: "Does this prompt effectively describe the high-level intent and constraints such that enough information is properly encoded with sufficent tools for the model to reliably expand it into the desired sequence of actions to accomplish the task?" 

---

## Here are some of the key lessons I learned while experimenting with ChatMD and tool-driven development:

### The prompt + tooling recipe that worked best

The highest-leverage change for reliability was: **make the workflow rules explicit**.

Tell the model:

- what the workflow is (inspect → propose → patch → test → iterate)
- what tools exist
- when to use which tool
- what constraints it must respect

Then give it a feedback loop that produces hard evidence:

- compiler errors
- test failures
- doc search hits
- tool errors and schema validation

### Iterative refinement beats clever prompting

The most reliable loop is:

1. propose a step
2. call a tool
3. observe concrete output
4. adjust

Repeat as needed — with evidence, not guesswork.

### Plan first, then execute

For big tasks, the two-stage approach works best:

1. make a plan with small, manageable subtasks
2. execute them sequentially, using results to guide what comes next

And for long sessions: compaction is not optional.
Keep the decisions/constraints, drop the noise.

### Why constraining tools helps (reduced search space)

Giving the model a defined tool vocabulary with clear constraints (i.e no general shell access):

- reduces the action space
- makes planning easier
- avoids “random shell command roulette”
- improves consistency at the cost of some flexibility

I’ve found “minimal tool set for the task” works best.
Too many tools increases the chance of the model going off-track or misusing them. And tools that are too general-purpose (e.g. arbitrary shell access) lead to unpredictable behaviour.

### Meta-prompting (refining prompts over time)

Meta-prompting is basically: use the model (and evaluators) to improve prompts iteratively.

- generate variations
- test them
- select the best
- repeat

### Other tips

Evals are the key ingredient because they turn “prompt vibes” into actual feedback you can iterate on.

XML-style boundaries are, in my experience, king when you want prompts to have clear separation between instructions, tools, and context. They also opened up interesting functionality that would be hard to do otherwise. The chat-tui app takes advantage of xml to allow the user to send messages to the model to guide in real time while the model is actively performing tool calls without interrupting the models flow. This is accomplished by embedding the messages within xml tags and appening it to the next tool call ouput. The xml boundary helps the model to clearly distinguish the user message from the tool call ouput.

Also:

- avoid logical inconsistencies and ambiguities
- reuse consistent terminology
- don’t assume the model “can’t do it” — usually there’s a way to prompt it into the behavior you want


