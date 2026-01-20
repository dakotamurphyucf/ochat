# ChatMarkdown (ChatMD) language reference

ChatMarkdown (ChatMD) is a **small, closed XML vocabulary embedded in Markdown** for authoring LLM conversations as plain files.

The core idea is simple: a ChatMD file is both:

- a **prompt** (model config, tool permissions, instructions, context), and
- an **auditable transcript** (tool calls + tool outputs + assistant replies).

This makes workflows **reproducible**, **diffable**, and easy to review.

## “What the model sees” (and the explicit exceptions)

Ochat tries hard to ensure the model sees exactly what’s in your ChatMD document, but there are a few **explicit, documented** mechanisms that transform the file:

- **HTML comments are stripped**: `<!-- ... -->` is removed before parsing and never reaches the model.
- **`<import/>` expands**: selected `<import src="..."/>` directives are replaced with the contents of the referenced file *at parse time*.
- **RAW blocks disable parsing**: `RAW| ... |RAW` is treated as literal text (no tag parsing inside).
- **Optional meta-refine preprocessing**: if enabled, the prompt may be rewritten before parsing (see “Meta-refine” below).

Everything else is intentionally boring: there are no hidden templates, implicit tool permissions, or “magic” side channels.

---

## 1) What is valid ChatMD?

ChatMD is **not** general HTML/XML. It recognises a **closed set of lowercase tag names**.

### 1.0 Official tag set

These are the tag names ChatMD recognises (lowercase, case-sensitive):

- Message / transcript structure: `msg`, `user`, `assistant`, `system`, `developer`
- Tools / tool trace: `tool`, `tool_call`, `tool_response`
- Inline helpers: `doc`, `img`, `agent`, `import`
- Reasoning: `reasoning`, `summary`
- Configuration: `config`

### 1.1 Top-level rule (important)

A ChatMD document must be a sequence of **recognised ChatMD elements at the top level**.

- ✅ Allowed at top-level: `<user>...</user>`, `<tool .../>`, `<config .../>`, etc.
- ✅ Whitespace between top-level elements is allowed.
- ❌ Plain text at top-level is an error.
- ❌ Unknown tags at top-level are an error (because unknown tags are treated as literal text).

Example:

```xml
<!-- ✅ valid -->
<user>Hello</user>

<!-- ❌ invalid: top-level text -->
Hello
```

### 1.2 Unknown tags

Unknown markup is preserved as literal text **inside** recognised elements. For example:

```xml
<user>Hello <b>world</b></user>
```

`<b>` is not a ChatMD tag, so it is preserved as literal text and passed through to the model.

### 1.3 Case sensitivity

Tags are case-sensitive. Use lowercase:

- ✅ `<user>...</user>`
- ❌ `<User>...</User>` (treated as unknown text; may break top-level validity)

### 1.4 Attribute syntax (practical rules)

- **Flag attributes** are supported and mean “present = true”, e.g. `<doc src="x" local/>`.
- Attributes can be quoted with **single** or **double** quotes.
- Quoted values support backslash-escaped quotes.
- A small set of HTML entities are decoded in **attribute values**: `&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;`.

---

## 2) RAW blocks (highly recommended)

RAW blocks let you embed arbitrary text without the ChatMD lexer interpreting `<tags>` inside it.

**Syntax:**

- opener: `RAW|`
- terminator: `|RAW`

Everything between them is treated as literal text.

This is the #1 way to embed:

- JSON tool arguments / results
- code containing `<` / `>` or XML-like snippets
- “literal ChatMD” examples inside a message

Example:

```xml
<user>
Here is JSON (no escaping needed):
RAW|
{ "path": "README.md", "offset": 0 }
|RAW
</user>
```

---

## 3) Core elements (top-level)

These elements form the “program” and the transcript.

### 3.1 Minimal skeleton

The smallest useful ChatMD file is:

```xml
<config model="gpt-4o" temperature="0"/>

<user>
Hello.
</user>
```

### 3.2 Conversation structure elements

| Element | Purpose | Notes / key attributes |
|---|---|---|
| `<config .../>` | Model and generation parameters | Optional. If multiple appear, the **first `<config/>` wins**. Flag attribute: `show_tool_call`. |
| `<tool .../>` | Declare tools available to the assistant | Builtin, shell, agent-backed, or MCP-backed. Exactly one of `command`, `agent`, `mcp_server` may appear. |
| `<user>...</user>` | User message | The most common input block. |
| `<assistant>...</assistant>` | Assistant message | Usually written by ochat into the transcript. Often uses RAW blocks for faithful round-tripping. |
| `<system>...</system>` | System message | High-priority instructions. |
| `<developer>...</developer>` | Developer message | Mid-priority instructions (below system). |
| `<msg role="...">...</msg>` | Generic message form | Escape hatch for less common roles/legacy. Roles supported by the runtime: `user`, `assistant`, `system`, `developer`, `tool`. |
| `<tool_call ...>...</tool_call>` | Tool invocation record | Typically written by ochat; see “Tool calls & tool responses”. |
| `<tool_response ...>...</tool_response>` | Tool output record | Typically written by ochat; see “Tool calls & tool responses”. |
| `<reasoning ...>...</reasoning>` | Reasoning record | Typically written by reasoning-capable models; requires `id` if authored manually. |

### 3.3 Inline content helpers (only inside message bodies)

These tags are recognised by the parser, but they are primarily meaningful **inside message bodies** (e.g. inside `<user>...</user>`):

| Inline tag | What it does |
|---|---|
| `<doc src="..." [local] [strip] [markdown] />` | Inline document text (local file or remote URL). |
| `<img src="..." [local] />` | Inline an image (remote URL or local file encoded as a data URI). |
| `<agent src="..." [local]> ... </agent>` | Run another ChatMD prompt as a sub-agent and insert its final answer. |
| `<import src="..."/>` | Parse-time include (only expands in certain places; see below). |

---

## 4) `<config/>`

`<config/>` controls model selection and generation parameters that ochat currently wires through.

Supported attributes:

- `model="..."` (string)
- `max_tokens="..."` (int)
- `temperature="..."` (float)
- `reasoning_effort="..."` (string; interpreted by the OpenAI client)
- `id="..."` (string label; optional)
- `show_tool_call` (**flag attribute**; presence enables inline tool payloads)

Example:

```xml
<config model="gpt-4o" temperature="0.2" max_tokens="1024"/>
```

### 4.1 `show_tool_call` (inline vs externalised tool payloads)

When `show_tool_call` is present, ochat persists tool arguments/results inline using RAW blocks.
When absent (default), large tool payloads are written to `./.chatmd/*.json` and referenced via `<doc .../>`.

See the “Tool calls & tool responses” section for exact layouts.

---

## 5) `<tool/>` declarations (capabilities)

`<tool/>` declarations define what actions the assistant is allowed to take.

ChatMD supports four tool “shapes”:

### 5.1 Built-in tools

```xml
<tool name="read_file"/>
<tool name="apply_patch"/>
```

### 5.2 Shell tools (wrap trusted commands)

```xml
<tool name="rg" command="rg" description="ripgrep search"/>
```

### 5.3 Agent-backed tools (prompt-as-tool)

```xml
<tool name="triage" agent="prompts/triage.chatmd" local description="Triage a bug report"/>
```

### 5.4 MCP-backed tools (import tools from an MCP server)

```xml
<tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker" strict/>
```

### 5.5 Validation rules (important)

- Exactly one of these attributes may be present: `command`, `agent`, `mcp_server`.
- For builtin/shell/agent tools, `name="..."` must be non-empty.
- For MCP tools:
  - `name="..."` selects a single tool name, **or**
  - `include="a,b"` / `includes="a,b"` selects a comma-separated list, **or**
  - neither means “no filter” (implementation-dependent; typically exposes the server’s tool catalog).

For a deeper tool reference (built-ins, schemas, and examples), see:
[`docs-src/overview/tools.md`](tools.md).

---

## 6) Messages: `<user>`, `<assistant>`, `<system>`, `<developer>`, `<msg>`

Most prompts use the shorthand message tags:

```xml
<system>You are careful and cite sources.</system>

<user>
Read README.md and propose a patch.
</user>
```

`<msg role="...">` exists as an escape hatch:

```xml
<msg role="user">Hello</msg>
```

Supported roles in the runtime conversion are:
`user`, `assistant`, `system`, `developer`, `tool`.

---

## 7) Tool calls & tool responses (`<tool_call>`, `<tool_response>`)

These elements represent the **on-disk execution trace** of tool usage.

They are usually written by ochat, not hand-authored.

### 7.1 Required attributes

Tool calls:

```xml
<tool_call function_name="read_file" tool_call_id="call_123">
...
</tool_call>
```

Tool responses:

```xml
<tool_response tool_call_id="call_123">
...
</tool_response>
```

There is also a special round-tripping convention for “custom tool calls”:

- `<tool_call type="custom_tool_call" ...>`
- `<tool_response type="custom_tool_call" ...>`

### 7.2 Persistence mode A: inline payloads (`show_tool_call`)

When `show_tool_call` is enabled, ochat persists payloads inline:

```xml
<tool_call tool_call_id="call_123" function_name="read_file" id="item_456">
RAW|
{ "path": "README.md" }
|RAW
</tool_call>

<tool_response tool_call_id="call_123">
RAW|
...tool output...
|RAW
</tool_response>
```

### 7.3 Persistence mode B: externalised payloads (default)

When `show_tool_call` is not set, payloads are written to `./.chatmd/` and referenced:

```xml
<tool_call function_name="read_file" tool_call_id="call_123" id="item_456">
  <doc src="./.chatmd/0.tool-call.call_123.json" local/>
</tool_call>

<tool_response tool_call_id="call_123">
  <doc src="./.chatmd/0.tool-call-result.call_123.json" local/>
</tool_response>
```

This keeps the main transcript readable even when tools exchange large JSON payloads.

---

## 8) Inline content helpers (the “power tools” inside messages)

### 8.1 `<doc src="..." .../>` — inline documents (local or remote)

```xml
<user>
Summarise this:
<doc src="README.md" local/>
</user>
```

Attributes:

- `src="..."` required
- `local` (flag): read from disk instead of HTTP
- `strip` (flag): if the doc is HTML, strip markup and collapse whitespace into readable text
- `markdown` (flag): convert HTML to Markdown (local file or remote URL)

Precedence:

- If `strip` is present, it takes precedence over `markdown`.

Local path resolution:

- Local docs are resolved against the prompt directory first; if still relative and not found, the process CWD is also consulted.

### 8.2 `<img src="..." [local]/>` — inline images

```xml
<user>
What’s wrong with this UI?
<img src="assets/screenshot.png" local/>
</user>
```

If `local` is present, the image is encoded as a data URI before being sent to the API.

### 8.3 `<agent src="..." [local]> ... </agent>` — call a sub-agent

An `<agent>` runs another ChatMD prompt and substitutes its final answer inline.

Example (summarise a document using a specialised agent prompt):

```xml
<user>
Here’s a summary produced by my sub-agent:
<agent src="prompts/summarise.chatmd" local>
  <doc src="README.md" local strip/>
</agent>
</user>
```

Notes:

- Any children of `<agent>` become the sub-agent’s runtime input.
- Results are cached (so repeated identical agent calls can be cheap).

---

## 9) `<import src="..."/>` — parse-time include (modularity)

`<import/>` keeps prompts maintainable by letting you reuse shared text (policies, glossaries, style guides).

**Where imports expand**

Imports are expanded recursively when they appear inside:

- `<user>...</user>`
- `<system>...</system>`
- `<developer>...</developer>`
- `<agent>...</agent>`
- `<msg role="user|system|developer">...</msg>`

**Where imports do not expand**

Everywhere else, `<import/>` is preserved as literal text (for example inside `<assistant>`, `<tool_call>`, `<tool_response>`, `<reasoning>`).

Example:

```xml
<system>
<import src="policies/safety.md"/>
</system>
```

---

## 10) Meta-refine (optional preprocessing)

If enabled, ochat can run a “meta-refine” pass before parsing ChatMD.

Enable it via:

- environment variable `OCHAT_META_REFINE` (truthy values like `1`, `true`, `yes`, `on`), or
- placing the marker comment `<!-- META_REFINE -->` anywhere in the prompt.

---

## 11) Common errors and gotchas

- **Top-level text is forbidden**: wrap content in `<user>...</user>`, etc.
- **Unknown tags at top-level fail**: unknown markup becomes literal text and triggers the top-level text rule.
- **Mismatched tags fail**: `<user>...</assistant>` is an error.
- **Unterminated quoted attribute values fail**: e.g. `alt="...` without closing quote.
- **Unterminated RAW blocks fail**: `RAW| ... |RAW` must be closed.
- **Tag names are lowercase and case-sensitive**.

