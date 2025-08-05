# Ochat – OCaml toolkit for building custom AI Agents, scripted LLM pipelines & vector search

### 📑 Persistent sessions – pause, resume & branch your chats

`chat_tui` (installed as `chat-tui` when you `opam install ochat`) now **persists the full conversation state automatically** under
`$HOME/.ochat/sessions/<id>` so you can close the terminal, pull the latest
commit and pick up the thread days later – tool cache and all.

Key facts at a glance:

* A *session* captures:  
  • the prompt that seeded the run (a copy is stored as `prompt.chatmd`)  
  • the complete message history (assistant, tool calls, reasoning deltas…)  
  • the per-session tool cache (`.chatmd/cache.bin`)  
  • misc metadata (task list, virtual-FS root, user-defined key/value pairs)

* Snapshots live in a single binary file `snapshot.bin` alongside the prompt
  copy – easy to back-up, copy or sync.

* When you open a prompt without explicit flags `chat_tui` hashes the prompt
  path and resumes the matching snapshot if present – **zero-config resume**.

CLI flags (all mutually-exclusive where it makes sense):

| Flag | Action |
|------|--------|
| `--list-sessions` | enumerate `(id\t<prompt_file>)` of every stored snapshot |
| `--session <ID>` | resume the given session (fails if it doesn’t exist) |
| `--new-session`  | ignore any existing snapshot for that prompt and start fresh |
| `--session-info <ID>` | print metadata (history length, timestamps, prompt path) |
| `--reset-session <ID>` | archive the current snapshot (timestamped) and restart; combine with `--keep-history` or change `--prompt-file` |
| `--rebuild-from-prompt <ID>` | delete history & cache, rebuild snapshot from the stored prompt copy – perfect after editing `prompt.chatmd` manually |
| `--export-session <ID> --out FILE` | convert a snapshot plus attachments to a standalone `.chatmd` document |

Interactive workflow examples:

```console
# 1️⃣  Enumeration
$ chat_tui --list-sessions
6f9ab3d5  prompts/interactive.md
a821c9f0  prompts/refactor.chatmd

# 2️⃣  Resume last week’s debugging chat
$ chat_tui --session 6f9ab3d5

# 3️⃣  Branch off a clean slate (keeps the old snapshot untouched)
$ chat_tui --session 6f9ab3d5 --new-session

# 4️⃣  Export a finished session to share with teammates
$ chat_tui --export-session a821c9f0 --out docs/refactor_walkthrough.chatmd

# 5️⃣  Reset but keep the conversation history and switch prompt
$ chat_tui --reset-session a821c9f0 --keep-history --prompt-file prompts/new_spec.md
```

`--auto-persist` saves on exit without confirmation; `--no-persist` drops
changes – useful in CI or when you want a quick throw-away run.

Under the hood **Session_store** migrates old snapshots transparently,
maintains advisory locks to prevent concurrent writes, and provides helpers
surfaced by the flags above.  Snapshot writes happen inside an Eio fiber so
the UI never blocks.

➡️  See `lib/session_store.mli` for the authoritative API contract.

### 🔁 Recursive meta-prompting – automate **prompt refinement**

Ochat now ships a **first-class prompt-improvement loop** powered by
_recursive meta-prompting_ and exposed via the `mp_refine_run` helper.  Give it

1. a **task** (what the prompt should accomplish) and
2. an optional **draft prompt**,

and it will iterate:

• generate *k* candidate prompts with an **O-series model** (e.g., `o3`),  
• score them via an OpenAI reward-model,  
• select the best using a Thompson bandit, and  
• stop when the score plateaus or the iteration budget is exhausted.

The refined prompt is printed to *stdout* or appended to a file – perfect for
CI pipelines where prompts live under version control.

CLI flags at a glance:

| Flag | Purpose |
|------|---------|
| `-task-file FILE` *(required)* | Markdown file describing the task |
| `-input-file FILE` | Existing prompt to refine (omit to start from scratch) |
| `-output-file FILE` | Append the result instead of printing to *stdout* |
| `-action generate\|update` | Create a new prompt or mutate an existing one |
| `-prompt-type general\|tool` | Assistant prompt vs tool description |

Quick examples:

```console
# 1️⃣  Draft a brand-new assistant prompt
$ mp_refine_run -task-file tasks/summarise.md

# 2️⃣  Improve an existing tool schema and persist the update
$ mp_refine_run \
    -task-file tasks/translate_task.md \
    -input-file  prompts/translate_draft.md \
    -output-file prompts/translate_refined.md \
    -action      update \
    -prompt-type tool
```

All heavy-lifting lives under `lib/meta_prompting` – functors, evaluators,
bandit logic and convergence checks.  The CLI is a thin wrapper around
`Mp_flow.first_flow`/`Mp_flow.tool_flow`; have a look at
`bin/mp_refine_run.ml` or the annotated API docs in
`lib/meta_prompting/mp_flow.mli` for the full story.


*Everything you need to prototype, run and embed modern LLM workflows without leaving the OCaml ecosystem.*

---

## Table of contents

1. [Why Ochat?](#why-ochat)
2. [Quick Start](#quick-start)
3. [Hands-on tutorial – your first ChatMD workflow](#hands-on-tutorial--your-first-chatmd-workflow)
4. [ChatMarkdown ( ChatMD ) language](#chatmarkdown--chatmd--language)
5. [Tools](#tools)
   * [Built-in toolbox](#built-in-toolbox)
   * [Declaring & calling tools in ChatMD](#declaring--calling-tools-in-chatmd)
   * [Authoring custom OCaml tools](#authoring-custom-ocaml-tools)
   * [End-to-end example – add and call *your* tool](#end-to-end-example--add-and-call-your-tool)
   * [Consuming remote MCP tools](#consuming-remote-mcp-tools)
6. [chat_tui – interactive terminal client](#chat_tui--interactive-terminal-client)
7. [MCP server – turn prompts into remote tools](#mcp-server--turn-prompts-into-remote-tools)
8. [Search, indexing & code-intelligence](#search-indexing--code-intelligence)
9. [Binaries cheat-sheet](#binaries-cheat-sheet)
10. [Embedding the libraries](#embedding-the-libraries)
11. [Composing full workflows](#composing-full-workflows)
12. [Key concepts & glossary](#key-concepts--glossary)
13. [Meta-prompting & self-improvement](#meta-prompting--self-improvement)
14. [Project layout](#project-layout)
15. [License](#license)


---

## Why Ochat?

LLM APIs mature rapidly; wiring them by hand soon turns into a tangle of JSON
snippets, retry loops and ad-hoc scripts. Ochat eliminates that boiler-plate
in pure OCaml:

* **ChatMarkdown ( ChatMD )** – a Markdown + XML dialect that stores *the full
  conversation* together with model parameters, tool declarations and
  persisted artefacts.
* **Context compaction** – one keystroke (`:compact`) summarises long
  transcripts in-place, shrinking token usage by 10-100× while preserving
  the essential dialogue. Perfect for marathon coding or research sessions.
* **chat_tui** – a Notty-powered, Vim-inspired UI that edits ChatMD files,
  executes tools and streams assistant output in real time.
* **Zero-boiler-plate tools** – promote any OCaml value, shell command or
  remote MCP endpoint to an OpenAI *function* with a single helper.
* **Built-in code intelligence** – index OCaml API docs, Markdown pages or
  source snippets and query them from prompts via `odoc_search`, `md_search`,
  `vector_db` …
* **Prompt-as-tool** – every `chatmd` file can be exported as a remote tool
  through the MCP server, turning prompt engineering into composable
  building blocks. Or as tool in another prompt.

  ```xml
  <!-- The “document” agent is itself a ChatMD prompt -->
  <tool name="document"
        agent="./prompts/tools/document.md"
        description="Document the given OCaml module"
        local/>
  ```

  The declaration above mounts *prompts/tools/document.md* as a **nested
  agent** that can be invoked like any other function call.  When the parent
  prompt emits `<tool_call name="document">{"module":"Vector_db"}</tool_call>`
  Ochat spawns a fresh assistant, runs the *document.md* workflow in-process,
  captures the Markdown it generates and streams it back to the main chat as
  a `<tool_response>` block.  This is recursive and fully composable – agents
  can mount other agents, enabling true LEGO-style workflow engineering.

> **OCaml-first, domain-agnostic.**  Ochat is *written* in OCaml and therefore
> integrates deeply with the ecosystem – `odoc_search`, `dune` helpers,
> Merlin-powered code navigation, etc.  We dog-food the stack every day to
> evolve the project itself.  **But nothing in the runtime cares about the
> target language**: the same ChatMD workflow can grep a Python repo, query a
> PostgreSQL database or orchestrate a Kubernetes cluster.  The magic lies in
> *crafting the right prompts and exposing the right tools* – Ochat just makes
> that process reproducible and git-committable.

---

## Quick Start

```sh
# Clone & build (creates a local opam switch under ./.opam)
opam switch create .
opam install . --deps-only
# Owl on Apple Silicon sometimes needs an explicit pin:
#   https://github.com/owlbarn/owl/issues/597#issuecomment-1119470934
#
# opam pin -n git+https://github.com/mseri/owl.git#arm64 --with-version=1.1.0
# PKG_CONFIG_PATH="/opt/homebrew/opt/openblas/lib/pkgconfig" opam install owl.1.1.0
dune build

# Run unit tests & docs
dune runtest
dune build @doc

# The `@doc` alias is generated only when the project’s `dune-project`
# file contains a `(documentation ...)` stanza.  If the command above
# fails simply add the stanza or omit the step.

# Fire up the interactive TUI on a sample prompt
dune exec chat_tui -- -file prompts/interactive.md
```

### 🔍 30-second smoke-test — *ochat* chat-completion CLI

Run a single command that verifies *ChatMD parsing → tool-calling → OpenAI round-trip* **before** you start hacking:

```console
$ ochat chat-completion \
    -prompt-file prompts/hello.chatmd \
    -output-file .chatmd/smoke.chatmd
```

Open `.chatmd/smoke.chatmd` and you should see something along the lines of:

```xml
<tool_call id="1" name="echo">{"text":"Hello ChatMD"}</tool_call>
<tool_response id="1">{"reply":"Hello ChatMD"}</tool_response>
```

If you do **not** get a reply check that `OPENAI_API_KEY` is set and reachable from the shell session.

---

## `ochat chat-completion` – script-friendly cousin of **chat_tui**

`chat_tui` is unbeatable for exploratory work, but when you need a
**fire-and-forget** assistant turn inside a shell script or CI job the
`chat-completion` sub-command shines.

```console
$ ochat chat-completion [flags]
```

### Frequently-used flags

| Flag | Purpose | Default |
|------|---------|---------|
| `-prompt-file` | File prepended **once** at the *start* of the transcript (usually a template with `<system>` / `<developer>` rules). | *(none)* |
| `-output-file` | Chat log that *persists* across invocations (created if absent, **appended** otherwise).  Use `$(mktemp)` or `/dev/stdout` when you want an *ephemeral* transcript. | `./prompts/default.md` |


### State lives in a **file**

The file supplied to `-output-file` is the *single* source of truth for the
conversation: tool-calls, reasoning deltas, assistant messages – everything
is captured in ChatMarkdown.  Re-run the command with the *same* output file
to extend the chat history:

```console
# Turn 1
$ ochat chat-completion -prompt-file prompts/hello.chatmd \
    -output-file .chatmd/tech_support.chatmd

# Turn 2 (assistant sees full history)
$ echo '<user>My computer is on fire!</user>' >> .chatmd/tech_support.chatmd
$ ochat chat-completion -output-file .chatmd/tech_support.chatmd
```

Open the result at any time in the interactive UI:

```console
$ dune exec chat_tui -- -file .chatmd/tech_support.chatmd
```

`chat_tui` lets you keep chatting as if the session
had always been interactive.

**Need an ephemeral run?**  Nothing prevents you from pointing `-output-file`
to a *tempfile* and piping the assistant’s markdown straight to standard
output:

```console
# Linux / macOS
$ tmp=$(mktemp /tmp/ochat.XXXX) \
  && ochat chat-completion -prompt-file prompts/hello.chatmd \
       -output-file "$tmp" \
  && cat "$tmp" \
  && rm "$tmp"

# Portable one-liner (store under /dev/shm when available)
$ ochat chat-completion -prompt-file ask_weather.chatmd \
       -output-file /dev/stdout
```

The first variant leaves **zero** artifacts after the run; the second streams
the final ChatMD document directly to the console while still giving the
runtime a valid *file descriptor* to append to — a requirement of the current
implementation.


### Exit codes

| Code | Meaning |
|------|---------|
| 0 | assistant replied successfully |
| 1 | prompt malformed or missing OpenAI key |
| 2 | at least one tool call failed |
| ≥3 | unexpected OCaml exception |

Run `ochat --help` for an exhaustive list of sub-commands and defaults.

---

## Hands-on tutorial – your first ChatMD workflow

This short walkthrough shows the *full* tool-chain end to end – from an empty folder to a running MCP server that publishes your prompt as a reusable remote tool.

> Time required: < 5 minutes  · No prior OCaml experience needed

### 1 Create and activate a project folder

```sh
$ mkdir ~/ochat-demo && cd ~/ochat-demo
$ git clone https://github.com/your-repo/ochat.git .
$ opam switch create . 5.1.1       # or any supported version ≥5.0
$ opam install . --deps-only    # fetches all transitive deps once

$ dune build                        # compile everything
```

### 2 Author a minimal prompt

Save the following *hello.chatmd* file next to your *dune-project* (**not** inside *_build/*):

```xml
<config model="gpt-4o" temperature="0"/>

<tool name="odoc_search" description="Search OCaml docs"/>

<system>You are the OCaml documentation oracle.</system>

<user>Find an example of Eio.Switch usage.</user>
```

Key take-aways:

1. One declarative `<tool …/>` line is enough – no JSON schema boiler-plate.
2. History lives *inside* the file, 100 % reproducible.

### 3 Open the prompt in *chat_tui*

```sh
dune exec chat_tui -- -file hello.chatmd
```

Useful shortcuts (Insert mode):  
• <kbd>Meta + Enter</kbd> send the draft  
• <kbd>Esc</kbd>     switch to Normal mode  
• <kbd>:w</kbd>     save the file  
• <kbd>:q</kbd>     quit

While the assistant is reasoning you will see live streaming deltas, tool-call placeholders and (when enabled) chain-of-thought blocks dimmed in grey.

### 4 Turn the prompt into a **remote tool**

Start an MCP server that watches the current directory:

```sh
dune exec mcp_server -- --http 8080 --prompts .
```

At this point:

* `GET http://localhost:8080/mcp` → Server-Sent-Events stream of notifications.  
* `POST /mcp {"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}`  
  returns a registry that **includes _hello_**.

You have just published a new function-callable endpoint without writing a single line of server code ✨.

### 5 Consume the tool from another prompt

```xml
<tool mcp_server="https://localhost:8080" name="hello"/>

<user>Ask remote oracle for Eio.Switch example.</user>
```

Run this second prompt in *chat_tui* (or embed it in your own OCaml program) – the assistant will pick the *hello* tool, the MCP client will tunnel the call over JSON-RPC/SSE, and the oracle prompt you created in step 2 will answer.

---

Environment variables:

| Variable            | Purpose                                        |
|---------------------|------------------------------------------------|
| `OPENAI_API_KEY`    | Required for completions / embeddings          |
| `MCP_PROMPTS_DIR`   | Extra folder scanned by `mcp_server`           |
| `PATH`              | `google-chrome`/`chromium` / `path/to/chrome` for `webpage_to_markdown` |

`webpage_to_markdown` uses the `google-chrome` binary as a backup to fetch web pages where most of the content is dynamic and javascript needs to be run for the content to render. Ensure that the path to the Chrome executable is included in your `PATH` environment variable or set it explicitly. The project will install a [`chrome-dump`](scripts/chrome_dump.sh)
executable on the system that will look for your Chrome binary. It detects the platform and uses the correct binary for your system.

---

## ChatMarkdown ( ChatMD ) language

ChatMD combines the readability of Markdown with a minimal XML vocabulary.
The model sees **exactly** what you write – no hidden pre-processing.

| Element | Purpose | Important attributes |
|---------|---------|----------------------|
| `<config/>` | Current model and generation parameters. **Must appear once** near the top. | `model`, `max_tokens`, `temperature`, `reasoning_effort`, **`show_tool_call`** (flag).  When the flag is present the runtime embeds tool-call **arguments & results inline**; when absent they are written to disk and referenced via `<doc/>`. |
| `<tool/>` | Declare a tool that the assistant may call. | • `name` – function name.<br>• Built-ins only need `name` (`apply_patch`, …).<br>• **Shell wrappers** add `command="rg"` + optional `description`.<br>• **Agent-backed** tools add `agent="./file.chatmd"` (plus `local` if the agent lives on disk). |
| `<msg>` | Generic chat message. | `role` one of `system,user,assistant,developer,tool`; optional `name`, `id`, `status`.  Assistant messages that *call a tool* additionally set `tool_call="true"` and provide `function_name` + `tool_call_id`. |
| `<user/>` / `<assistant/>` / `<developer/>` / `<system>` | **Shorthand** wrappers around `<msg …>` for the common roles. | Accept the same optional attributes (`name`, `id`, `status`). |
| `<tool_call/>` | Assistant *function invocation* shorthand – equivalent to `<msg role="assistant" tool_call …>` | Must include `function_name` & `tool_call_id`.  Body carries the JSON argument object (often wrapped in `RAW|…|RAW`). |
| `<tool_response/>` | Tool reply shorthand – equivalent to `<msg role="tool" …>` | Must include the matching `tool_call_id`. Body contains the return value (or error) of the tool. |
| `<reasoning>` | Chain-of-thought scratchpad emitted by reasoning models.  Normal prompts rarely use it directly. | `id`, `status`.  Contains one or more nested `<summary>` blocks. |
| `<import/>` | Include another file *at parse time*.  Keeps prompts small while re-using large policy docs. | `src` – relative path of the file to include. |


`<config>` supports *all* OpenAI chat parameters.  Example:

```xml
<config model="gpt-4o" temperature="0.2" max_tokens="1024" reasoning_effort="detailed"/>
```

When using a reasoning model, if `reasoning_effort` is not `none` the assistant may stream `<reasoning>`
blocks; Ochat renders them inline with dimmed colours so you can watch the
*chain-of-thought* develop while tools execute.


### Inline content helpers

Inside various element bodies you can embed richer content that is expanded **before** the request
is sent to OpenAI:

| Tag | Effect |
|-----|--------|
| `<img src="path_or_url" [local] />` | Embeds an image. If `local` is present the file is encoded as a data-URI so the API sees it. |
| `<doc src="path_or_url" [local] [strip] [markdown]/>` | Inlines the *text* of a document. <br>• `local` reads from disk.  <br>• Without it the file is fetched over HTTP.<br>• `strip` removes HTML tags (useful for web pages). <br>• `markdown` converts the document to Markdown format. |
| `<agent src="prompt.chatmd" [local]> … </agent>` | Runs the referenced chatmd document as a *sub-agent* and substitutes its final answer.  Any nested content inside the tag is appended as extra user input before execution. |

#### The `<agent/>` element – running sub-conversations

An **agent** lets you embed *another* chatmd prompt as a sub-task and reuse its answer as
inline text.  Think of it as a one-off function call powered by an LLM.

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

### End-to-end example

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

### Writing effective ChatMD prompts

Below is a condensed checklist distilled from months of day-to-day usage.  All tips are *optional* – ChatMD never hides or rewrites your words – but following them keeps both **tokens and frustration** low.

1. **Single authoritative `<config/>`.** Keep _all_ OpenAI parameters in one place near the top; avoid scattering overrides that compete for attention.  `chat_response.Config.of_elements` only reads the first occurrence anyway.
2. **System / developer message choice matters.** For GPT-series stick to one `<system>` message; for *reasoning* models such as **o3‐mini** place the rules in a `<developer>` message because these models follow the _chain-of-command_ spec more literally.
3. **System content = scope + constraints, _nothing else_.** One or two sentences suffice.  Large policy docs should be `<import>`-ed to keep the round-trip diff small.
4. **Declare tools _once_ (via `<tool …/>`) and never duplicate the schema in plain text.** The API already provides the JSON schema to the model and repeating it wastes tokens or introduces mismatches.
5. **Enable JSON-mode (`response_format="json_object"`) for machine-readable replies.** `o3-mini` and other reasoning models respect the constraint and will return *parsable* objects.
6. **“Show, don’t tell” — provide a single, minimal example.** Instead of lengthy prose like “Return valid JSON”, embed one assistant message that contains a canonical response shape:

   ```xml
   <assistant>
   {"reply":"<content>","sources":[]}
   </assistant>
   ```

   The model will pattern-match and follow the schema more reliably than with verbal instructions.
7. **Chain-of-thought: _on for GPT, off for reasoning_.** GPT models often benefit from an explicit “think step-by-step”; o-series models **already** plan internally – asking them to reveal the chain may hurt quality and cost.
8. **Zero-shot first, few-shot only if needed (reasoning models).** Most o-series prompts work with no examples; add them only when absolutely necessary and keep them perfectly aligned with the instructions.
9. **Leverage tools instead of giant context windows.** When a sub-task can be solved via search (`md_search`, `odoc_search`) or a remote micro-service, call the tool rather than pasting 100 k tokens of data.
10. **Reusable workflows = agents.** Encapsulate a sequence of steps into `my_flow.chatmd`, and mount it through `<agent src="my_flow.chatmd"/>` for LEGO-style composition.
12. **Mind the token budget.** O-series models (e.g., `o3`) can handle ~1 M tokens, but both latency and price grow linearly.  Check with `ochat tokenize -file foo.chatmd` before you hit *Send*.

### reasoning vs GPT quick‐reference

| Situation | Recommended model | Prompting style |
|-----------|------------------|-----------------|
| Complex, multi-step planning | **o3-mini / o3** | Concise goal + constraints, *no* step-by-step request; use `reasoning_effort` when you need depth. |
| Latency-sensitive UI chats | **O-series / GPT-4.1** | Explicit, detailed instructions; CoT or *think step-by-step* when logic matters. |
| Giant context (≥ 200k tokens) | **O-series / GPT-4.1** | Repeat key constraints at *top & bottom* of the prompt. |

Additional reminders for reasoning models:

1. Put non-negotiable rules in a `<developer>` message.
2. Prefer **zero-shot**; add examples only for rigid output formats.
3. `reasoning_effort="high"` often beats chain-of-thought at lower cost.


Below are **extra heuristics** that pay dividends in practice:
* **First-sentence anchoring.**  Make sure the very first 80 characters identify the *task*, otherwise the model may mis-detect the domain and pick a less efficient compression strategy.
* **Declarative personas, imperative goals.**  Instead of *“You are a world-class OCaml tutor”* try *“Explain each OCaml snippet in clear English suitable for a junior engineer.”*
* **Rapid error recovery.**  Wrap every non-idempotent tool invocation in a sub-agent: if the call fails, the agent can decide whether to retry, back-off or fallback to a degraded answer without polluting the main conversation history.

---

End-to-end example demonstrating *everything at once*:

```xml
<config model="gpt-4o" temperature="0.1" max_tokens="512" />

<tool name="markdown_search" description="Search docs" />
<tool mcp_server="https://api.mycorp.dev" name="weather" />

<system>
  You are a strict JSON API. Always respond with an object {reply, sources}.
</system>

<user>
  Create a one-paragraph summary of the MCP spec.
</user>

<!-- The assistant may now decide to call markdown_search or weather – or answer directly. -->
```

---

## Tools

### Built-in toolbox

| Name | Category | Description |
|------|----------|-------------|
| `apply_patch`         | repo      | Apply an *Ochat diff* (V4A) to the working tree |
| `read_dir`            | fs        | List entries (non-recursive) in a directory; returns plain-text lines |
| `get_contents`        | fs        | Read a file (UTF-8) |
| `get_url_content` *(experimental)* | web       | Download a raw resource and strip HTML to text |
| `webpage_to_markdown` | web       | Download a page & convert it to Markdown |
| `index_ocaml_code`    | index     | Build a vector index from a source tree |
| `index_markdown_docs` | index     | Vector-index a folder of Markdown files |
| `odoc_search`         | docs      | Semantic search over installed OCaml API docs |
| `md_search` / `markdown_search` | search | Query Markdown indexes created by `index_markdown_docs` |
| `query_vector_db`     | search    | Hybrid dense + BM25 search over source indices |
| `fork`                | misc      | Spawn a forked assistant with the exact same context to perform a task and return the results to the original assistant |
| `mkdir` *(experimental)*               | fs        | Create a directory (idempotent) |
| `append_to_file`      | fs        | Append text to a file, creating it if absent |
| `find_and_replace`    | fs        | Replace occurrences of a string in a file (single or all) |
| `meta_refine`         | meta      | Recursive prompt refinement utility |

<details>
<summary><strong>Deep-dive: 7 helpers that turn ChatMD into a Swiss-Army knife</strong></summary>

1. **`apply_patch`** – The bread-and-butter of autonomous coding sessions.  The assistant can literally rewrite the repository while you watch.  The command understands *move*, *add*, *delete* and multi-hunk updates in one atomic transaction.
2. **`webpage_to_markdown`** – Turns *any* public web page (incl. GitHub *blob* URLs) into clean Markdown ready for embedding or in-prompt reading.  JS-heavy sites fall back to a head-less Chromium dump.
3. **`odoc_search`** – Semantic search over your **installed** opam packages.  Because results are fetched locally there is zero network latency – ideal for day-to-day coding.
4. **`markdown_search`** – Complement to `odoc_search`.  Index your design docs and Wiki once; query them from ChatMD forever.
5. **`query_vector_db`** – When you need proper hybrid retrieval (dense + BM25) over a code base.  Works hand-in-hand with `index_ocaml_code`.
6. **`fork`** –  Spawn a *clone* of the assistant inside the same prompt.  Perfect for speculative tasks: let the clone draft a changelog while the main thread keeps chatting.
7. **`mkdir`** *(experimental)* – Sounds trivial but enables one-shot project scaffolding flows where the LLM both creates folders *and* patches files.

</details>

### Importing remote MCP tools – one line, zero friction

```xml
<!-- Mount the public Brave Search toolbox exposed by *npx brave-search-mcp* -->
<tool mcp_server="stdio:npx -y brave-search-mcp"/>

<!-- Or cherry-pick just two helpers from a self-hosted endpoint -->
<tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
```

Ochat converts every entry returned by the server’s `tools/list` call into a
local OCaml closure and forwards the **exact** JSON schema to OpenAI.  From the
model’s perspective there is no difference between `weather` (remote) and
`apply_patch` (local) – both are normal function calls.


> **Tip 💡** – All built-ins are **normal ChatMD tools** under the hood.  That means you can mount them remotely via MCP:

```xml
<!-- Consume read-only helpers from a sandboxed container on the CI runner -->
<tool mcp_server="https://ci-tools.acme.dev" includes="read_dir,get_contents"/>
```

or hide them from the model entirely in production by simply omitting the `<tool>` declaration.  No code changes required.


### Rolling your own OCaml tool – 20 lines round-trip

```ocaml
open Ochat_function

module Hello = struct
  type input = string

  let def =
    create_function
      (module struct
        type nonrec input = input
        let name        = "say_hello"
        let description = Some "Return a greeting for the supplied name"
        let parameters  = Jsonaf.of_string
          {|{"type":"object","properties":{"name":{"type":"string"}},"required":["name"]}|}
        let input_of_string s =
          Jsonaf.of_string s |> Jsonaf.member_exn "name" |> Jsonaf.string_exn
      end)
      (fun name -> "Hello " ^ name ^ "! 👋")
end


(* Gets the tools JSON and dispatch table *)
let tools_json, dispatch =
  Ochat_function.functions [ Hello.def ]

(* If you want to add to the current drivers (Chat_tui and the chat-completion command)
 then add tool to of_declaration in lib/chat_response/tool.ml example *)
 
```

Declare it once in ChatMD:

```xml
<tool name="say_hello"/>
```

That is **all** – the assistant can now greet users in 40+ languages without
touching an HTTP stack.

---

## Shell-command wrappers – *the 30-second custom tool*

> ⚠️ **Security note** – A `<tool command="…"/>` wrapper runs the specified
> binary with the *full privileges of the current user*.  Only mount such tools
> in **trusted environments** or inside a container / sandbox.  Never expose
> unrestricted shell helpers to untrusted prompts – limit the command and
> validate the arguments instead.

Not every helper deserves a fully-blown OCaml module.  Often you just want to
gate a **single shell command** behind a friendly JSON schema so the model can
call it safely.  ChatMD does this out-of-the-box via the `command="…"`

```xml
<!-- Pure viewer: let model know do use for write access → safe in read-only environments. (note: this is just a hint to the model. It could still call this with write ops. You need to implement proper access controls in your tool) -->
<tool name="sed"
      command="sed"
      description="read-only file viewer"/>

<!-- Pre-pinned arguments – the model cannot escape the pattern.          -->
<tool name="git_ls_files"
      command="git ls-files --exclude=docs/"
      description="show files tracked by git except docs/"/>

<!-- Mutation allowed, therefore keep it explicit and auditable →        -->
<tool name="git_pull"
      command="git pull"
      description="fetch from and integrate with a remote repository"/>
```

Behaviour in a nutshell

1. The JSON schema is inferred automatically: an *array of strings* called
   `arguments`.
2. At run-time Ochat executes

   ```sh
   <command> <arguments…>   # under the current working directory
   ```

3. Standard output and stderr is captured and
   appended to the `<tool_response>` block and sent back to the assistant;.

### Why wrapper tools beat *generic shell*

| Aspect | Generic `sh -c` | Targeted wrapper |
|--------|-----------------|-------------------|
| Search space @ inference | enormous | tiny – the model only sees *git_pull* / *sed* |
| Security                 | needs manual sandboxing | limited to pre-approved binaries |
| Reliability              | model must remember *all* flags | happy-path baked into `command` |

In practice:

* **Generalist agents** benefit from one broad hammer such as `bash`, but may
  waste tokens debating which flag to use or which command to run.
* **Specialist agents** (e.g. *CI fixer*, *release-bot*) shine when equipped
  with *exactly* the verbs they need — nothing more, nothing less.

#### Design guidelines

1. **Prefer idempotent actions**.  Read or list before you write or delete.
2. **Embed flags** that should never change directly in `command="…"`.
3. Add a verb-based **prefix** (`git_`, `docker_`, `kubectl_`) so the
   language model can reason via pattern matching.




#### Quick REPL tour

Fire up *chat_tui* on an empty prompt and experiment interactively:

```xml
<config model="gpt-4o" temperature="0"/>

<tool name="read_dir"/>
<tool name="get_contents"/>
<tool name="apply_patch"/>

<user>List every file under ./lib that contains “vector”.  Then show me the first 6 lines of the most relevant one.</user>
```

While the assistant reasons you will see:

1. A `<tool_call>` block appear with arguments that mirror the OCaml record schema.
2. Live streaming of the command output (thanks to Server-Sent Events).
3. A `<tool_response>` block once the helper terminates.
4. The assistant resuming its stream – now aware of the file contents.

Everything — calls, responses, thought process — ends up in the same `.chatmd` file so your colleagues can replay the analysis *byte-for-byte* months later.



<details>
<summary>Signal → act recipes</summary>

*Index your library then query it from ChatMD*

```xml
<tool name="index_ocaml_code"/>
<tool name="query_vector_db"/>

<user>
  1. Index ./lib and store vectors under .vector_db
  2. Search the DB for "tail-rec map" and display the first 3 hits as markdown list.
</user>
```

The assistant will first call `index_ocaml_code`, wait for completion, then feed the *call-id* of the created index into `query_vector_db` – no glue code required.

</details>

---

## Composing full workflows

One of ChatMD’s super-powers is that **every tool is just normal Markdown** –
there is *no* difference between calling an OpenAI function, a shell command
or a remote MCP micro-service.  This section shows how to chain a handful of
built-ins into a **self-contained research-→-edit-→-commit loop** that you can
paste straight into your editor.

> Scenario You are reading a blog post, notice that the README has grown out
> of date and want the assistant to fetch the latest article, summarise it and
> update *README.md* – all without leaving the terminal.

```xml
<config model="gpt-4o" temperature="0" />

<!-- 1️⃣  Helpers we need -->
<tool name="webpage_to_markdown" />           <!-- remote research -->
<tool name="apply_patch"        />            <!-- edit the repo   -->

<!-- 2️⃣  High-level task -->
<user>
  Read https://blog.example.com/2025/07/ai-git.md and inject a concise
  one-paragraph summary under the "Motivation" heading of README.md.
</user>
```

The assistant will:

1. **Call** `webpage_to_markdown`, receiving Markdown of the article.
2. **Generate** an `apply_patch` diff that inserts the summary.
3. **Invoke** `apply_patch` – Ochat applies the patch and persists the change.
4. **Confirm** success, all recorded in the same `.chatmd` file.

### Why it matters

* Works identically with **local** (`apply_patch`) and **remote**
  (`webpage_to_markdown`) helpers – the LLM does not care.
* The whole transaction is **auditable & replayable** – reviewers can inspect
  the patch, the external source, and the chain-of-thought in one diff.
* Encourages a *research-then-act* workflow that keeps prompts short and
  leverages tools instead of giant context windows. Though sometimes giant context windows are unavoidable, this pattern helps keep them manageable.

The same pattern scales to multi-step refactors, changelog generation, data
munging and beyond.

---

## Prompt blueprint – *Discovery bot*
Treat the snippet below as
a **blueprint** you can paste into your own repository and commit alongside
code. 

The prompt turns the assistant into a fully autonomous *research → summarise →
patch* agent.  You will recognise many of the concepts explained earlier: MCP
tools, built-ins, chain-of-thought, O-series vs reasoning models and, most
importantly, incremental file updates via `apply_patch`.

```xml
<config model="o3" max_tokens="100000" reasoning_effort="high" />

<!-- Built-ins -->
<tool name="webpage_to_markdown" />
<tool name="apply_patch" />

<!-- Remote MCP server for web search -->
<tool mcp_server="stdio:npx -y brave-search-mcp" />

<!-- Read-only helper so the LLM can *peek* at existing research files -->
<tool name="sed" command="sed" description="read-only file viewer" />

<system>
You are a meticulous web-research agent.  Your job is to fully resolve the
user’s query, writing detailed findings to <research_results.md> as you go.

Workflow (strict order):
1. Create or open the target markdown file using `apply_patch`.
2. Run **at least** 3 brave_web_search queries.
3. For each result:
   a. Fetch the page with `webpage_to_markdown`.
   b. Extract relevant facts, examples, citations.
   c. Immediately append a structured summary to the results file via
      `apply_patch`.
4. Continue until you cannot find new useful sources.
5. Reply with a JSON object `{reply, sources}` and **nothing else**.
</system>

<user>
store results in prompting_research_results.md
  Research best practices for prompting OpenAI’s latest models
  – include o3 reasoning models
  – include GPT-4.1 models
  – prompting in general
  – include any relevant academic literature
</user>
```

Why it’s interesting:

* Shows **MCP tool integration** – the runtime uses `tools/list` from the Brave MCP
  wrapper to discover *which* search endpoints the model can call and makes them availible.
* Demonstrates a **self-mutating** workflow: after each web request the LLM
  edits *prompting_research_results.md* so progress is never lost.
* Combines *local* (`apply_patch`, `sed`) and *remote* tools (Brave search)
  seamlessly.

Clone the snippet, tweak the `<config>` model, change the tool list and you
have an advanced research agent tailored to your environment.


### Inspiration – `prompt_examples/discovery.md`

The repository ships an *industrial-strength* research agent in
`prompt_examples/discovery.md`.  It demonstrates – in fewer than 120 lines – how
to chain **Brave web search (remote MCP tool)**, `webpage_to_markdown`, and
`apply_patch` into a self-healing loop that:

1. Executes three diverse search queries.
2. Converts every candidate web page to Markdown.
3. Summarises the findings in a dedicated notes file.
4. Appends new sources until diminishing returns kick in.

Fork the file, change the `<user>` goal, and you have a bespoke research bot in
seconds.

---

## OCaml ⚑ vs Language-Agnostic Design

Ochat’s code-base is pure **OCaml** which unlocks deep, first-class hooks into
the ecosystem: Merlin for code navigation, odoc for documentation search,
`dune describe` for build introspection and a parser that speaks `.ml`/`.mli`
fluently. Ocaml is a powerful tool for building robust applications and its type system really shines in projects like ChatMD.
I have also found that OCaml's strong type system and powerful compiler are a perfect match for llm code generation. They
make it easier to catch errors early in the development process and give the llm a powerful feedback loop with the compiler, which helps improve the quality of the generated code because the llm is forced to ensure its code is at least type-checked. With a combination of dune build, and dune runtest you can quickly have the model iterate on the code and test it and make it much more reliable.  This is a key part of the development workflow.
The project is written in OCaml, and we use it to help develop the project itself.

Yet nothing in the *runtime* cares about OCaml.  ChatMD is plain Markdown +
XML; tools exchange JSON; the MCP transport is language-agnostic.  Point at a Rust project, 
mount a Python-based micro-service via MCP, or let the assistant
drive `docker` – the workflow stays unchanged because **the power comes from
choosing the right prompts and exposing the right set of tools, not from the
host language**.

All artefacts are files.  They can be code-reviewed, linted, versioned and
replayed by CI.  If an LLM interaction matters you commit it alongside the
code it touches – *infrastructure-as-prompt*.

---

### Declaring & calling tools in ChatMD

```xml
<tool name="grep" command="rg" description="ripgrep search"/>

<user>Search every *.ml for TODO and list file + line numbers.</user>
```

The declaration alone is enough – Ochat builds the JSON schema, validates
arguments and runs the command under an Eio switch.  The assistant only has
to emit a normal function call.

#### Anatomy of a function call (end-to-end)

```xml
<tool name="grep" command="rg" description="ripgrep search"/>

<user>List every .ml file that contains the word `todo`.</user>

<!-- What happens under the hood → simplified trace -->
<tool_call id="1" name="grep">{"arguments":["-n","todo","*.ml"]}</tool_call>
<tool_response id="1">
src/foo.ml:14: (* TODO: remove allocation *)
...
</tool_response>
<assistant>
Found 7 TODO markers – see annotated listing above.
</assistant>
```

The entire lifecycle is captured in **one** durable `.chatmd` file – ideal for audit, diffing and reproducibility.


### End-to-end example – add and call *your* tool

Below is a **full cut-and-paste scenario** that shows *all* moving parts – OCaml side **and** prompt side – in under 30 lines.  You will end up with a prompt that calls a freshly compiled function **without** extra JSON glue.

1.  Drop the following module into `lib/my_echo.ml`:

   ```ocaml
   open Ochat_function

   module Echo = struct
     type input = string

     let def =
       create_function
         (module struct
           type nonrec input = input
           let name        = "echo"
           let description = Some "Return the input verbatim"
           let parameters  = Jsonaf.of_string
             {|{"type":"object","properties":{"text":{"type":"string"}},"required":["text"]}|}
           let input_of_string s = Jsonaf.of_string s |> Jsonaf.member_exn "text" |> Jsonaf.string_exn
         end)
         (fun s -> s)
   end
   ```

2.  Add the module to your `dune` file and run `dune build`.  The helper is now part of your workspace.

3. Add the following to the of_declaration function in `lib/chat_response/tool.ml`:

```ocaml
   let of_declaration ~sw ~(ctx : _ Ctx.t) ~run_agent (decl : CM.tool)
  : Ochat_function.t list
  =
  match decl with
  | CM.Builtin name ->
    (match name with
     | "apply_patch" -> [ Functions.apply_patch ~dir:(Ctx.dir ctx) ]
     | "read_dir" -> [ Functions.read_dir ~dir:(Ctx.dir ctx) ]
     | "get_contents" -> [ Functions.get_contents ~dir:(Ctx.dir ctx) ]
     | "webpage_to_markdown" ->
       [ Functions.webpage_to_markdown
           ~env:(Ctx.env ctx)
           ~dir:(Ctx.dir ctx)
           ~net:(Ctx.net ctx)
       ]
     | "fork" -> [ Functions.fork ]
     | "odoc_search" -> [ Functions.odoc_search ~dir:(Ctx.dir ctx) ~net:(Ctx.net ctx) ]
     | "index_markdown_docs" ->
       [ Functions.index_markdown_docs ~env:(Ctx.env ctx) ~dir:(Ctx.dir ctx) ]
     | "markdown_search" ->
       [ Functions.markdown_search ~dir:(Ctx.dir ctx) ~net:(Ctx.net ctx) ]
     | "echo" -> [ Echo.def ] (* Our custom function *)
     | other -> failwithf "Unknown built-in tool: %s" other ()
  | CM.Custom c -> [ custom_fn ~env:(Ctx.env ctx) c ]
  | CM.Agent agent_spec -> [ agent_fn ~ctx ~run_agent agent_spec ]
  | CM.Mcp mcp -> mcp_tool ~sw ~ctx mcp
;;
```

3.  Declare the tool once at the top of a prompt `echo_demo.chatmd`:

   ```xml
   <config model="gpt-4o" temperature="0"/>

   <!-- Made available by the OCaml code we just compiled -->
   <tool name="echo"/>

   <user>Repeat "ChatMD rocks" three times separated by commas.</user>
   ```

4.  Run it in the terminal UI:

   ```console
   $ dune exec chat_tui -- -file echo_demo.chatmd
   ```

The assistant will decide to call the tool, Ochat will route the call into the in-process OCaml function, capture the return value and dump everything back into `echo_demo.chatmd` as a `<tool_call>` / `<tool_response>` pair. **No HTTP, no YAML, no wrapper script.**


```xml
<!-- ➡ ChatMD declaration – *nothing* else required -->
<tool name="echo"/>

<user>Repeat "Hello" three times.</user>
```
The assistant calls `echo` internally; your `.ml` file handles the logic.  **No HTTP, no boiler-plate**, only a strongly-typed OCaml function.


`tools_json` is forwarded to OpenAI, while `dispatch_tbl` lets the driver
execute calls locally.

### Consuming remote MCP tools

```xml
<tool mcp_server="http://localhost:8080" name="weather"/>
```

`Mcp_tool.ochat_function_of_remote_tool` consumes the **standard** MCP
`tools/list` response and converts the JSON-schema of each selected tool into
a local `Ochat_function`.  As long as the endpoint speaks *vanilla MCP* the
origin does not matter – GitHub Codespaces, Cursor IDE plugins, your own
Rust-based micro-service, all integrate the same way.  Ochat does **not**
expect the server to run OCaml or to use the `ochat` code-base.

#### Selecting which remote tools are exposed

The `<tool>` tag gives you **three mutually‐exclusive attributes** to control
what is imported from the remote registry:

| Attribute | Example | Effect |
|-----------|---------|--------|
| `name`    | `name="weather"` | Expose *exactly* the tool called `weather`.  Fails during handshake if the server does not provide it. |
| `includes`| `includes="grep,apply_patch"` | Import the comma-separated subset.  Handy when you need more than one tool but do **not** want the entire registry (avoids token bloat). |
| *(absent)*| *no attribute* | Import **all** tools advertised by the server.  This is convenient during exploration but increases the schema size sent to OpenAI. |

```xml
<!-- Single tool -->
<tool mcp_server="https://tools.acme.dev" name="weather"/>

<!-- Selected subset -->
<tool mcp_server="https://tools.acme.dev" includes="grep,apply_patch"/>

<!-- Wild-card: every tool the server offers -->
<tool mcp_server="https://tools.acme.dev"/>
```

If both `name` and `includes` are supplied, **`name` wins** and `includes` is
ignored.  An `excludes` attribute is intentionally not provided – keep the
list small instead of black-listing.

#### Two-liner smoke test

```xml
<tool mcp_server="stdio:mcp_server --prompts ./prompts" name="echo"/>

<user>Ask the echo tool to repeat “hello world”.</user>
```

Run it in *chat_tui*: the client boots an in-process MCP registry, publishes the `echo` prompt found under `./prompts`, the assistant calls the remote function and you see the round-trip payload scroll by in the TUI — **all on localhost, zero config**.


### Consuming *local* MCP tools (any implementation)

When the MCP server lives *inside* the same workstation ‑ no TLS required ‑ the
fastest option is the **stdio transport** supported by *every* conforming
server:

```xml
<tool mcp_server="stdio:mcp_server --prompts ./ops-prompts" name="deploy"/>
```

How it works:

1. Ochat spawns the command after the `stdio:` part under an Eio switch.
2. It performs the JSON-RPC handshake (`initialize`).
3. It discovers the tool called `deploy` via `tools/list` and builds a
   matching JSON schema.

The tool is now callable exactly like a normal built-in function.  This is the
easiest way to embed **operations scripts, smoke-test prompts or data-science
experiments** living in a separate folder.

#### Controlling which MCP tools are exposed

`mcp_server` decides the registry.  You have several knobs:

| Knob | Default | What it does |
|------|---------|--------------|
| `--prompts DIR` (cli flag) | `./prompts` | Directory scanned for `.chatmd` files |
| `MCP_PROMPTS_DIR` (env)    | *(unset)*   | Extra folder added on top of `--prompts` |
| `--allow-shell-tools`      | *disabled*  | Also register `<tool command="…"/>` blocks found in prompts |
| `--require-auth`           | `false`     | Only list tools after successful OAuth token validation |

On the **client** side you can filter further by leaving out the `<tool …/>`
declaration in the ChatMD file – the model cannot call what it doesn’t know.

> Tip: run two servers – one started with `--prompts ./prod/` and another with
> `--prompts ./dev/` – and choose which one to mount in the ChatMD tool
> declaration.

### MCP in 60 seconds – the super-connector

Sometimes you just want the *TL;DR* for wiring an existing micro-service into
ChatMD:

```xml
<config model="gpt-4o" temperature="0"/>

<!-- Discover *and* register every tool exposed by the remote registry. -->
<tool mcp_server="https://tools.acme.dev"/>

<user>Convert README.md to PDF using the **export_pdf** tool.</user>
```

Behind the curtain:

1. Ochat performs the JSON-RPC handshake (`initialize` + `tools/list`).
2. Every advertised JSON schema is turned into a *local* OCaml closure via
   `Mcp_tool.ochat_function_of_remote_tool`.
3. When the assistant decides to call `export_pdf` the closure relays the
   invocation with `tools/call`, streams incremental progress events over
   SSE, and finally hands the result back to the model – **indistinguishable
   from a built-in tool**.

That’s it.  *MCP is the USB-C port of Ochat – plug in anything, it just
works.*

---

## chat_tui – interactive terminal client

```console
$ chat_tui -file prompts/interactive.md
```

Think of **chat_tui** as the *interactive face* of your prompt-as-code
workflow: each `.chatmd` file becomes a **self-contained agent** once you
declare a handful of tools.  Need a refactoring bot?  Draft
`prompts/refactor.chatmd`, mount `apply_patch`, `odoc_search` and a custom
`shell_check` wrapper, then open the file in the TUI:

```console
$ chat_tui -file prompts/refactor.chatmd
```

No servers to deploy, no runtime config – the static files plus your shell or
OCaml tool implementations *are* the application.  The same technique scales
from a quick one-off helper up to a fleet of purpose-built agents (release
manager, design-doc auditor, knowledge-base explainer) – each living in its
own `.chatmd` and selectable via `chat_tui`.

Below is a one-page *muscle-memory* cheat-sheet distilled from the daily
usage of the maintainers.  Print it, tape it to the wall, thank us later.

| Mode | Keys | Action |
|------|------|--------|
| **Insert** | `⌥ ↵` (Meta + Enter) | run the draft prompt |
|            | `Esc`               | switch to **Normal** mode |
|            | `Ctrl-k / Ctrl-u / Ctrl-w` | delete to *EOL* / *BOL* / previous word |
| **Normal** | `j / k`, `gg / G`  | navigate history |
|            | `o / O`             | insert line below / above and jump into Insert |
|            | `v`                 | start visual selection (useful for `apply_patch`) |
| **Cmd (:)** | `:w`, `:q`, `:wq`, `:compact` | write / quit / compact context |

Pro tip — while a request is streaming you can hit `v` to select the last
assistant code block, press `!apply_patch` + <kbd>Enter</kbd>, and watch the
repository mutate *while the model is still thinking*.


| Mode | Keys (subset) | Action |
|------|---------------|--------|
| **Insert** | *free typing* | edit the draft prompt |
| | `Meta+Enter` | submit draft |
| **Normal** | `j`/`k`, `gg`/`G` | navigate history |
| | `dd`, `u`, `Ctrl-r` | delete / undo / redo |
| **Cmd (`:`)** | `:w`, `:q`, `:wq`, `:compact` | save prompt, quit, compact context |

Features

* Live streaming of tool output, reasoning deltas & assistant text
* Auto-follow & scroll-history with Notty
* Manual **context compaction** via `:compact` (`:c`, `:cmp`) – summarises older messages when the history grows too large and replaces it with a concise summary, saving tokens and latency.
* Persists conversation under `.chatmd/` so you can resume later

#### Context compaction (`:compact`)

When a session grows beyond a comfortable token budget you can shrink it
on demand:

```text
:compact          # alias :c or :cmp
```

Ochat will:

1. Take a *snapshot* of the current history.
2. Score messages based on relevance to the current context via the relevance judge.
3. Pass the most-relevant messages to the summariser which produces a
   compacted version of the conversation. 
4. Replace the original messages *in-place* and update the viewport.
5. Archive the original history in a `<session>/archive` folder


> Tip Run `:w` immediately after `:compact` to persist the shrunken
> history before generating new messages.

##### Under the hood – how the compaction pipeline works

Calling `:compact` triggers a **four-stage pipeline** implemented in
`lib/context_compaction/`:

| Stage | Module | What happens |
|-------|--------|--------------|
| ① Load config | `Context_compaction.Config` | Reads `~/.config/ochat/context_compaction.json` (or XDG-override).  Missing file → hard-coded defaults `{ context_limit = 20_000 ; relevance_threshold = 0.5 }`. |
| ② Score relevance | `Context_compaction.Relevance_judge` | For **every** message the *Importance judge* asks a small reward-model to rate how indispensable the line is on a scale **0–1**.  No `OPENAI_API_KEY` or network?  It returns the deterministic stub `0.5`, keeping semantics reproducible in CI. |
| ③ Summarise keepers | `Context_compaction.Summarizer` | The messages whose score is ≥ `relevance_threshold` are passed to GPT-4.1 (or an offline stub) together with a purpose-built system prompt.  The model then writes a rich summary |
| ④ Rewrite history | `Context_compaction.Compactor` | The function returns a **new transcript** that contains the original *first* item (usually the `<system>` prompt) **plus** *at most one* extra `<system-reminder>` message that embeds the summary.  If anything blows up along the way the original history is returned verbatim – the feature can never brick the session. |

Configuration snippet

```jsonc
// ~/.config/ochat/context_compaction.json
{
  "context_limit": 10000,          // tighten character budget
  "relevance_threshold": 0.7       // be more aggressive when pruning
}
```

Programmatic use

```ocaml
let compacted =
  Context_compaction.Compactor.compact_history
    ~env:(Some stdenv)   (* pass Eio capabilities when network access is OK *)
    ~history:full_history
in
send_to_llm (compacted @ new_user_turn)
```


**Self-serve checklist – 10 seconds to first answer**

1. `dune exec chat_tui -- -file prompts/blank.chatmd` – starts in *Insert* mode with an empty history.
2. Type *“2+2?”*, hit **⌥ ↵** → an O-series model replies *“4”*.
3. Press **`:` w q** – the session is written to `prompts/blank.chatmd` for future audit.

### Power-user workflow – *code-edit-test* in one window

That’s it – *no* OpenAI dashboard visit, *no* shell scripts.  Everything, including model name and temperature, is stored in the document you can now commit to Git.


Programmatic embedding:

```ocaml
Io.run_main @@ fun env ->
  Chat_tui.App.run_chat ~env ~prompt_file:"prompts/interactive.md" ()
```

---

## MCP server – turn prompts into remote tools

> **Background – What is MCP?**  The *Model-Context-Protocol* is an
> **open JSON-RPC 2.0 based standard** for exchanging *Resources* (data),
> *Tools* (function-calls) and *Prompts* between any AI application and any
> external provider.  The official spec lives at
> <https://modelcontextprotocol.io/> and is implemented by many public
> services – from IDE extensions to cloud APIs.  Ochat is *just one*
> implementation; the client stack in `lib/mcp/` works with **any** MCP
> server that follows the spec.

Ochat ships two transport flavours:

1. **Stdio (default)** – line-delimited JSON over the server’s
   `stdin`/`stdout`.  Perfect for shell pipelines, CI jobs and
   scripting.
2. **HTTP + SSE** – request/response via JSON-RPC 2.0 `POST` and push
   notifications over Server-Sent-Events.  Enabled with `--http <PORT>`.

### Stdio mode

```console
# Start the server – it now waits for one JSON object per line on stdin
$ dune exec mcp_server -- &

# In another shell (or from any program) send a request:
echo '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}' \
  | socat - UNIX-CONNECT:/proc/$(pgrep mcp_server)/fd/0

# The server replies on its stdout (one line, shown here pretty-printed):
{"jsonrpc":"2.0","id":1,"result":{"tools":[{"name":"interactive", …}]}}
```

Because stdio is purely local no authentication is required – ideal for
**embedding mcp_server as a child process** and communicating with it from
another OCaml program:

```ocaml
let conn = Mcp_transport_stdio.connect ~env ~sw "stdio:mcp_server" in
Mcp_client.list_tools conn |> Result.ok_or_failwith |> ...
```

### HTTP / SSE mode

```console
$ dune exec mcp_server -- --http 8080 &

# Call a prompt remotely via HTTP
$ curl -s http://localhost:8080/mcp \
  -d '{"jsonrpc":"2.0","id":42,"method":"tools/call","params":{"name":"interactive","args":{"input":"42"}}}'
```

Every `.chatmd` under `prompts/` becomes:

* A **prompt** (`prompts/get::<name>`)
* A **tool**  (`tools/call::<name>`)

The server hot-reloads new files and emits `tools/list_changed`
notifications over SSE.

### Authentication & sessions

`mcp_server` can protect the HTTP endpoint with OAuth 2 *client-credentials*
flow:

```console
$ mcp_server --http 8080 --require-auth \
             --client-id my-bot --client-secret $SECRET
```

Clients must send a `Bearer` token (obtained via the discovery metadata
exposed by the server) **and** the `Mcp-Session-Id` header returned by the
initial `initialize` call.  SSE notifications are scoped to that session so
multiple IDE tabs do not leak messages to each other.

Transport matrix (works with **any** MCP server that advertises the transport):

| Transport | URI scheme | Strengths |
|-----------|------------|-----------|
| **stdio** | `stdio:<cmd>` | zero-config, lowest latency, easy to embed |
| **HTTP/2**| `https://`    | network transparency, browser-friendly SSE |

> See `lib/mcp/mcp_transport_http.doc.md` for protocol details and
> authentication hints.

### MCP in a nutshell – why should you care?

The *Model-Context-Protocol* is often described as the **USB-C port for AI applications** — plug any LLM-powered host (an IDE, Claude Desktop, `chat_tui`, …) into any data or tool provider by speaking one tiny JSON-RPC vocabulary.  The official site <https://modelcontextprotocol.io/> defines three first-class entities:

1. **Resources** – immutable blobs such as code files, PDFs or database rows.
2. **Prompts** – reusable templates (ChatMD in Ochat) that can be fetched and executed remotely.
3. **Tools** – side-effecting functions with arbitrary JSON arguments and results.

Every conforming server exposes at least the following RPC surface:

| Method          | Purpose                                          |
|-----------------|---------------------------------------------------|
| `tools/list`    | Return the tool registry (names + JSON schema)    |
| `tools/call`    | Invoke a tool and stream progress events          |
| `prompts/list`  | List registered prompt templates                  |
| `prompts/get`   | Fetch the raw prompt body                         |
| `resources/list`| *(optional)* Directory-like listing of resources  |
| `resources/read`| *(optional)* Fetch raw bytes / UTF-8 text         |

The **exact same envelope** can travel over local *stdio* **or** over HTTP/2 + Server-Sent-Events:

| Transport | URI scheme      | Framing                                |
|-----------|-----------------|----------------------------------------|
| stdio     | `stdio:<cmd>`   | One JSON object per line               |
| HTTP/2    | `https://`      | `POST /mcp` + SSE for push notifications|

Because Ochat wraps both transports behind a single `Mcp_client.t`, switching from a
local integration test to a cloud-hosted endpoint is as simple as editing **one attribute** in your ChatMD file:

```xml
<!-- Local smoke-tests during CI → spawn the server as a child process -->
<tool mcp_server="stdio:bin/mcp_server --prompts ./prompts" includes="apply_patch"/>

<!-- Production deployment over TLS → talk to a managed service -->
<tool mcp_server="https://mcp.acme.cloud" name="deploy"/>
```

#### End-to-end example – call a *weather* micro-service from ChatMD

```xml
<config model="gpt-4o" temperature="0.0" response_format="json_object"/>

<!-- The remote server exposes a single JSON-schema {"city":string} → {"temp":float} -->
<tool mcp_server="https://wx.example.net" name="weather"/>

<user>
Ask the weather tool for Berlin.
</user>

<!-- At run time chat_tui streams something like:

<tool_call id="1" name="weather">{"city":"Berlin"}</tool_call>


<tool_response id="1">
{"temp": 17.3}
</tool_response>

<assistant>
{"reply":"17.3 °C"}
</assistant>
-->
```

The same prompt works locally with the stdio transport by flipping one attribute:

```xml
<tool mcp_server="stdio:bin/mcp_server --prompts ./prompts" name="weather"/>
```

---

### Where does Ochat fit?

Ochat ships **both ends** of the wire:

1. **MCP *client*** – the `lib/mcp_client` library used by *chat_tui* and the ChatMD runtime.  It speaks stdio **and** HTTP transports out-of-the-box.
2. **MCP *server*** – `bin/mcp_server` turns every `.chatmd` file into a remote tool *and* exposes built-in helpers like `apply_patch`, `webpage_to_markdown`, … .

That means you can:

* Mount a self-contained workflow in your prompt via

  ```xml
  <tool mcp_server="https://deploy.acme.dev" name="deploy"/>
  ```

  – and run CI/CD operations from the comfort of *chat_tui*.

* Expose **your** prompt to *other* clients by running

  ```console
  $ dune exec mcp_server -- --http 8080 --prompts ./prompts
  ```

  Colleagues can now call `tools/call::your_prompt` from Python, Node, Java … 100 % language-agnostic.

### Lifecycle of a tool call (HTTP transport)

1. `chat_tui` sends a JSON-RPC envelope to `/mcp`:

   ```jsonc
   { "jsonrpc":"2.0", "id":42,
     "method":"tools/call",
     "params":{ "name":"grep", "args":{"pattern":"todo"} } }
   ```
2. The server acknowledges and streams progress events over *Server-Sent Events* (`event: function_call_output`).
3. When the tool finishes the final result is pushed, the HTTP stream closes and *chat_tui* appends a `<tool_response>` block to the `.chatmd` file.

### Security model recap

* **Local transports inherit the parent UID/GID.**  If you can spawn `mcp_server` you already have file access.
* **Remote HTTP transports default to OAuth 2 “client-credentials”.**  Run the server with `--require-auth` to enforce tokens.
* **Prompts are immutable resources.**  Once fetched they can be verified via SHA-256; supply `digest` in the `prompts/get` params to request an integrity check.

### Minimal troubleshooting flow

```console
$ curl -s http://localhost:8080/mcp -d '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}'
{"jsonrpc":"2.0","id":1,"result":{"session_id":"2fb4b6…"}}

$ curl -s http://localhost:8080/mcp -H 'Mcp-Session-Id: 2fb4b6…' -d '{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}'
{"jsonrpc":"2.0","id":2,"result":{"tools":[{"name":"interactive", …}]}}

# Tool not listed?  Check the server log – prompts with XML errors are skipped.
```


The **Model-Context-Protocol** (MCP) is the vendor-neutral JSON-RPC 2.0
standard that lets *any* AI application exchange **Resources**, **Tools** and
**Prompts** with *any* provider – over TCP, HTTP/2 or plain stdio.

Official spec  <https://modelcontextprotocol.io/>  (CC-BY-4.0)

| Transport | URI scheme | Framing | Streamed? | Auth |
|-----------|------------|---------|-----------|------|
| stdio      | `stdio:cmd …` | One JSON per line | yes | inherits parent |
| HTTP/2     | `https://…`   | `POST /mcp` + SSE  | yes | Bearer / OAuth 2 |

### Must-know RPC surface (Phase-1)

| Method             | What it does | Key params |
|--------------------|--------------|------------|
| `initialize`       | negotiate capabilities, returns `session_id` |
| `tools/list`       | list tool registry (paginated) | `cursor` |
| `tools/call`       | invoke a tool, server streams progress events | `name`, `args` |
| `prompts/list`     | discover prompt templates | – |
| `prompts/get`      | fetch raw `.chatmd` body | `name` |
| `resources/list`   | *(opt.)* directory-like listing | `path` |
| `resources/read`   | *(opt.)* download a blob | `uri` |


---

See `docs-src/bin/mcp_server.doc.md` for server flags and operational advice.  For a deep dive into the wire-format head over to <https://spec.modelcontextprotocol.io/>.

---

## Search, indexing & code-intelligence

| Corpus | Indexer | Searcher | Notes |
|--------|---------|----------|-------|
| OCaml docs (odoc HTML) | `odoc_index` | `odoc_search` | vector + BM25 per package |
| Markdown docs | `md_index` | `md_search` | overlapping 64-320 token windows |
| OCaml source | `ochat index` | `ochat query` | parses docstrings & embeds with OpenAI |

Use them directly in prompts via the built-in tools (`odoc_search`,
`md_search`, `query_vector_db`).

---

## Binaries cheat-sheet

| Binary | Purpose | Example |
|--------|---------|---------|
| `chat_tui` (`chat-tui`) | interactive TUI | `chat_tui -file notes.chatmd` |
| `ochat`    | misc CLI (index, query, tokenise …) | `ochat query -vector-db-folder _index -query-text "tail-rec map"` |
| `mcp_server` | serve prompts & tools over JSON-RPC / SSE | `mcp_server --http 8080` |
| `mp_refine_run` | refine prompts via *recursive meta-prompting* | `mp_refine_run -task-file task.md -input-file draft.md` |
| `md_index` / `md_search` | Markdown → index / search | `md_index --root docs`; `md_search --query "streams"` |
| `odoc_index` / `odoc_search` | ODoc HTML → index / search | `odoc-index --root _doc/_html` |

Run any binary with `-help` for details.

---

## Embedding the libraries

Every public binary is a thin wrapper over libraries available under `lib/`.
Re-use them in your own code:

```ocaml
let send ~env messages =
  Chat_response.Driver.run_completion_stream_in_memory_v1
    ~ctx:(Ctx.of_env ~env ~cache:(Cache.create ~max_size:256 ()))
    ~model:`Gpt4o
    ~messages ()
```

Need a TTL-LRU?  Use `Ttl_lru_cache`.

### Caching in practice

Most heavy helpers (`Embed_service`, `Converter`, `Fetch`) accept an explicit
`Cache.t` and look up remote calls through

```ocaml
Cache.find_or_add cache key ~ttl:(Time_ns.Span.min `Hour)` ~default
```

so you can tune memory footprint and freshness centrally.  The default TUI
instance persists the cache under `~/.chatmd/cache.bin`.

Need to embed docs?  `Odoc_indexer.index_packages` has you covered.

---

## Key concepts & glossary

| Concept | Module / binary | TL;DR |
|---------|-----------------|-------|
| **ChatMD** | `lib/chatmd` | Markdown + XML dialect that drives everything |
| **Tool / Ochat_function** | `lib/ochat_function`, `lib/functions` | Couples JSON-schema with a runtime OCaml function |
| **Prompt-agent** | `lib/mcp/mcp_prompt_agent` | Any `.chatmd` exported as a callable remote tool |
| **Vector DB** | `lib/vector_db` | Dense + BM25 hybrid retrieval, Owl matrices under the hood |
| **Bm25** | `lib/bm25` | Lightweight lexical scorer for up to ~50k snippets |
| **Cache** | `Ttl_lru_cache` | TTL-based LRU used all over the stack (OpenAI, agents …) |
| **MCP** | `bin/mcp_server`, `lib/mcp/*` | JSON-RPC + SSE transport so tools live anywhere (stdio, HTTP) |
| **Embed service** | `lib/embed_service` | Rate-limited, concurrent OpenAI embeddings pipeline |


> Refer to the generated odoc docs (`_doc/_html/index.html`) for full API references.

---

## Meta-prompting & self-improvement

`Meta_prompting` brings **prompt generators, evaluators and a recursive
self-improvement monad** under one roof.  It allows you to *treat prompt
engineering itself as a first-class program*:

```ocaml
module Mp = Meta_prompting.Make (My_task) (My_prompt)
let better_prompt = Mp.generate my_task |> Recursive_mp.refine
```

* **Generators** – `Meta_prompting.Make` maps a *task record* to a
  [`Chatmd.Prompt.t`].
* **Evaluators** – combine arbitrary *judges* into a single score.
* **Recursive refinement** – `Recursive_mp` loops until the score plateaus.

See the dedicated documentation page [`meta_prompting.doc.md`](docs-src/lib/meta_prompting.doc.md),
the runnable example under [`examples/meta_prompting_quickstart.ml`](examples/meta_prompting_quickstart.ml)
**or** try the zero-setup CLI helper:

```console
$ mp_refine_run -task-file tasks/my_task.md \
               -input-file prompts/draft.md \
               -output-file prompts/refined.md
```

The command maps *exactly* to `Mp_flow.first_flow` / `Mp_flow.tool_flow`
depending on the `-prompt-type` flag.

---

## Project layout

```
bin/         – chat_tui, mcp_server, ochat …
lib/         – re-usable libraries (chatmd, functions, vector_db …)
docs-src/    – Markdown docs rendered by odoc & included here
prompts/     – sample ChatMD prompts served by the MCP server
dune-project – dune metadata
```

---

## License

All original source code is licensed under the terms stated in
`LICENSE.txt`.

---

## ⚠️ Project status – expect rapid change

Ochat is a **research-grade** project that evolves nearly every day.  APIs,
tool schemas, file formats and even high-level design choices may shift
without prior notice while we explore what works and what does not.  If you
intend to build something on top of Ochat be prepared to

* pin a specific commit or tag,
* re-run the tests after every `git pull`, and
* embrace breaking changes as part of the fun.

Despite the experimental label, **nothing stops you from building real
value today** – the repository already enables powerful custom agent workflows.
I use it daily with custom agents for everything from developing and documentation generation, to writing emails and automating mundane tasks.

But please budget time for occasional refactors and breaking changes.
Bug reports and PRs are welcome – just keep in mind the ground may still be
moving beneath your feet. 🚧


