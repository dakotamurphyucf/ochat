# Ochat – text-first toolkit for custom AI agents, LLM workflows, and vector search

**Build custom AI agents and scripted LLM workflows as plain text files.**

Ochat is an **OCaml toolkit** for building **reproducible, composable, tool-using LLM workflows** without locking the workflow into a single UI or heavyweight framework.

Instead of hiding prompts, tool permissions, transcript state, and orchestration inside an application, Ochat keeps them in **static, diffable files** that you can version-control, review, branch, and run in different hosts.

If you like tools like Claude Code or Codex, Ochat operates at a more fundamental level: it gives you the building blocks to create **your own prompt packs, agents, and workflow systems**.

<div>
<a href="https://asciinema.org/a/gIelV4eAeA0LKvG7" target="_blank"><img height="700" width="900" src="https://asciinema.org/a/gIelV4eAeA0LKvG7.svg" /></a>
</div>

---

## Contents

- [Why Ochat exists](#why-ochat-exists)
- [Design Principles](#design-principles)
- [Ochat in one minute](#ochat-in-one-minute)
- [What is Ochat?](#what-is-ochat)
- [What makes Ochat different?](#what-makes-ochat-different)
- [How Ochat compares](#how-ochat-compares)
- [Who Ochat is for](#who-ochat-is-for)
- [Quick start](#quick-start)
- [What can I do with Ochat?](#what-can-i-do-with-ochat)
- [Common use cases](#common-use-cases)
- [First 10 minutes with Ochat](#first-10-minutes-with-ochat)
- [Example ChatMD prompts](#example-chatmd-prompts)
- [Build from source](#build-from-source-ocaml)
- [Core concepts](#core-concepts)
- [Architecture overview](#architecture-overview)
- [Documentation](#documentation)
- [OCaml integration](#ocaml-integration)
- [Future directions](#future-directions)
- [Project status](#project-status--expect-rapid-change)

---

## Why Ochat exists

Most LLM tools today either:
- hide workflows inside polished UIs,
- require you to rebuild everything in a code-first framework, or
- make prompts and agent state hard to inspect, reproduce, and evolve.

Ochat exists to make LLM workflows feel more like **engineering artifacts**.

With Ochat, prompts, tools, transcript state, and orchestration live in plain text files that you can:
- **version-control**
- **diff and review**
- **branch and resume**
- **compose into larger workflows**
- **run in the terminal, scripts, CI, or over MCP**

The goal is simple: make agent workflows **explicit, inspectable, portable, and reproducible**.

---

## Design principles

Ochat is built around a few core principles:

- **Everything important should be inspectable**
- **Workflows should be versionable**
- **Agent runs should be reproducible**
- **Tools should be explicit**
- **Custom workflows should not depend on a single UI**
- **Advanced orchestration should remain auditable**

---

## Ochat in one minute

- A workflow is usually a `.md` file written in **ChatMarkdown (ChatMD)**.
- That file can contain the prompt, model config, tool permissions, transcript, and execution artifacts.
- The same workflow can run in the TUI, the CLI, or over MCP.
- Workflows can call tools, other workflows, and an optional host-managed **ChatML** script.
- Because everything is stored as text, runs are diffable, reproducible, resumable, and easy to version-control.

---

## What is Ochat?

Ochat is a toolkit for building **agent workflows and orchestrations as static files**.

Its core format is **ChatMarkdown (ChatMD)**, a Markdown + XML dialect for defining and running agents. A single ChatMD file can contain:

- model and generation parameters
- tool declarations and permissions
- developer and user instructions
- the full conversation history
- tool calls and tool results
- imported artifacts such as documents or images
- optional host-managed scripting for orchestration and moderation

In Ochat, an agent is often just a `.md` file.

That file is not only a prompt — it can also be:
- the execution log of a run,
- a reusable workflow component,
- a tool callable by another agent,
- a portable artifact you can inspect and refine over time.

Because everything is captured in text files, workflows become:

- **reproducible** – the exact config and transcript are version-controlled
- **diffable** – reviews show exactly what changed and what the model did
- **composable** – workflows can call other workflows (prompt-as-tool)
- **portable** – prompts are plain text, not locked into one interface
- **editable** – use any text editor, IDE, or terminal workflow
- **LLM-friendly** – the XML structure is easy for models to parse and generate

The same `.md` workflow definition can be executed in multiple hosts:

- the **terminal UI** (`chat_tui`) for interactive work
- **scripts and CI** via `ochat chat-completion`
- a **remote MCP server** via `mcp_server`, so IDEs and other applications can call agents over stdio or HTTP/SSE

The ChatMD language provides a rich set of features for prompt engineering in a modular way, supporting workflows from simple prompts to more advanced orchestrations. See the [language reference](docs-src/overview/chatmd-language.md).

ChatMD prompts can also embed a single host-managed **ChatML** moderation script:

- declare it with `<script language="chatml" kind="moderator" ...>`
- keep it invisible to the model request itself
- prepend, append, replace, or delete effective transcript items
- inspect and construct structured items through the `Item` builtin module
- moderate tool calls by approving, rejecting, rewriting, or redirecting them
- persist only serializable runtime state between resumed sessions
- request another ordinary model turn after `turn_end` via `Runtime.request_turn()`
- schedule idle follow-up turns after background internal events or startup work
- orchestrate additional model-backed work via `Model.call` and `Model.spawn`
- defer user steering submitted during streaming until a safe model-input boundary

Prompts without a `<script>` keep the baseline behavior: the shared drivers, `chat_tui`, export flow, and nested agent execution continue to work without the moderation layer.

Ochat is implemented in **OCaml**, but the workflows themselves are **language-agnostic**. Tools exchange JSON, prompts are plain files, and the system makes no assumptions about the kinds of applications your workflows target.

> **Current provider support:** OpenAI only (via the Responses API).  
> The architecture is designed to support additional providers in the future.

---

## What makes Ochat different?

Ochat is not just:
- a coding assistant,
- a prompt playground,
- an orchestration framework,
- or an MCP wrapper.

It is a **text-first workflow toolkit** built around a few core ideas:

- **Prompt-as-program**  
  A workflow can live in a single file that contains config, tools, transcript state, and execution artifacts.

- **Transcript-as-artifact**  
  Runs are inspectable, diffable, branchable, and resumable.

- **Prompt packs instead of fixed apps**  
  Build your own Claude Code/Codex-style workflows instead of being limited to a single built-in agent UX.

- **Composable agents**  
  Mount one prompt as a tool inside another workflow.

- **Host-managed control logic**  
  Use ChatML scripts to moderate tool calls, manage workflow state, and orchestrate multi-step behavior.

- **Run anywhere**  
  Use the same workflow in the TUI, the CLI, or through MCP.

---

## How Ochat compares

Ochat occupies a different niche in the LLM tooling landscape than most agent tools.

### Compared to coding-agent products
Tools like **Claude Code**, **Codex-style CLIs**, or **Aider** are polished agent applications for working in a repo.

Ochat is more fundamental: it is a **toolkit for building your own agent workflows** as plain files. Instead of shipping a single hard-coded agent experience, Ochat lets you define prompts, tools, transcript state, and orchestration explicitly.

### Compared to orchestration frameworks
Frameworks like **LangGraph** and similar Python agent stacks are typically **code-first**: you build workflows in application code.

Ochat is **artifact-first**: workflows live as **text files** that can be version-controlled, diffed, composed, resumed, and run in different hosts.

### Compared to observability / prompt-management platforms
Platforms like **LangSmith**, **PromptLayer**, or **Humanloop** focus on tracing, evaluation, and hosted prompt management.

Ochat focuses on **authoring and running workflows locally as inspectable artifacts**. It is not primarily a dashboard or hosted prompt registry.

### Compared to MCP tools
MCP provides a protocol for exposing tools and resources to models and clients.

Ochat supports MCP, but it is broader than that. It is a **workflow system** with:
- ChatMD prompt files
- transcript persistence
- prompt-as-tool composition
- host-managed scripting
- TUI and CLI execution
- local indexing and retrieval tools

### The simplest way to think about it
If other tools are:
- **agent apps**
- **Python orchestration frameworks**
- **hosted observability platforms**
- or **protocol/tool adapters**

then Ochat is best thought of as:

**a text-first toolkit for reproducible, composable LLM workflows**

---

## Who Ochat is for

Ochat is for developers and power users who want:
- custom AI agents instead of fixed app behavior
- workflows they can version-control and diff
- explicit tool and transcript state
- reproducible runs across local, development, and CI environments
- a local-first, text-first workflow model

Ochat is probably not the best fit if you just want the simplest possible chat UI with minimal setup and no interest in workflow artifacts.

---

## Quick start

New to OCaml?

If you do not already have an OCaml environment set up, start here:

- [OCaml.org](https://ocaml.org)
- [Install OCaml, opam, and the toolchain](https://ocaml.org/install#linux_mac_bsd)
- [More detailed installation guide](https://ocaml.org/docs/installing-ocaml)

Ochat uses the standard OCaml tooling stack:
- **opam** for package management and compiler switches
- **dune** for builds and tests

Once your OCaml environment is installed, you can build and run Ochat as follows.

Build and run a minimal ChatMD prompt.

### 1. Install dependencies and build

```sh
opam switch create .
opam install . --deps-only

dune build
```



### 2. Create a prompt file

Create `prompts/hello.md`:

```xml
<config model="gpt-5.2" reasoning_effort="medium"/>

<developer>
You are a helpful assistant.
</developer>

<user>
Say hello and explain what Ochat is in one sentence.
</user>
```

### 3. Run it in the terminal UI

```sh
dune exec chat_tui -- -file prompts/hello.md
```

### 4. Or run it non-interactively

```sh
ochat chat-completion \
  -prompt-file prompts/hello.md \
  -output-file .chatmd/hello-run.md
```

The output file captures the run as a plain text artifact that you can inspect, diff, resume, or share.

---

## First 10 minutes with Ochat

A simple way to get a feel for Ochat:

1. **Set up OCaml tooling**  
   If needed, install OCaml, opam, and the toolchain:
   - [OCaml.org](https://ocaml.org)
   - [Install guide](https://ocaml.org/install#linux_mac_bsd)
   - [VSCode one click install walkthrough via OCaml Platform plugin](https://tarides.com/blog/2026-04-16-vscode-walkthrough-installing-ocaml-in-1-click/?utm_source=dlvr.it&utm_medium=linkedin)

2. **Build the project**
   ```sh
   opam switch create .
   opam install . --deps-only
   dune build
   ```

3. **Run a minimal prompt**
   Create `prompts/hello.md` and run it in the TUI:
   ```sh
   dune exec chat_tui -- -file prompts/hello.md
   ```

4. **Try a tool-using prompt**
   Run the refactor example:
   ```sh
   dune exec chat_tui -- -file prompts/refactor.md
   ```

5. **Inspect the workflow artifact**
   Export or save the run and open the resulting `.md` / `.chatmd` file to see:
   - the prompt
   - the transcript
   - tool calls and results
   - the exact workflow state captured as text

6. **Try a non-interactive run**
   ```sh
   ochat chat-completion \
     -prompt-file prompts/hello.md \
     -output-file .chatmd/hello-run.md
   ```

7. **Explore deeper features**
   From there, try:
   - agent-as-tool composition
   - MCP export via `mcp_server`
   - retrieval/indexing tools
   - ChatML moderator scripts

---

## What can I do with Ochat?

### Author workflows as plain files
Write agents as `.md` files using ChatMD. A file can act as both:
- a reusable prompt definition
- the execution log of a run

That means prompts, tool calls, results, and transcripts can all be version-controlled and diffed like code.

### Build tool-using agents
Combine:
- ChatMD prompt instructions
- built-in tools
- shell wrappers
- remote MCP tools
- other agents mounted as tools

Built-in tools include capabilities such as:
- repo-safe editing via `apply_patch`
- filesystem access via `read_dir` and `read_file`
- web ingestion via `webpage_to_markdown`
- retrieval over docs via `index_markdown_docs` and `markdown_search`
- retrieval over OCaml code via `index_ocaml_code` and `query_vector_db`
- image import via `import_image`

See [Tools – built-ins, custom helpers & MCP](docs-src/overview/tools.md).

### Compose agents into prompt packs
Build Claude Code/Codex-style applications out of multiple prompts:
- planning agents
- coding agents
- test agents
- documentation agents
- orchestration agents

Because prompts can be mounted as tools, you can create modular multi-agent systems without hard-coding everything into one app.

### Run the same workflow in different hosts
Use:
- `chat_tui` for interactive work
- `ochat chat-completion` for scripts, CI, and cron
- `mcp_server` to expose prompts as tools to IDEs and other clients

### Ground agents in your own corpus
Create indexes over docs or source trees and let prompts query them using natural language. See [Search, indexing & code intelligence](docs-src/guide/search-and-indexing.md).

### Refine prompts iteratively
Use the `mp-refine-run` binary to generate, evaluate, and improve prompts and tool descriptions through iterative meta-prompting.

### Version, branch, and resume runs
Because conversation state is stored in text files, you can export full runs, branch them, resume them later, and review exactly what changed.

---


## Common use cases

Ochat is useful anywhere you want LLM workflows to be explicit, reproducible, and easy to evolve.

Typical use cases include:

- **Repo-aware coding assistants**  
  Build agents that inspect a codebase, read files, propose patches, and run in a controlled local workflow.

- **Documentation agents**  
  Create prompts that summarize docs, update documentation, or answer questions over local documentation sets.

- **Planning / review / test workflows**  
  Compose multiple prompts into planning, implementation, review, and test stages.

- **Prompt packs for internal tools**  
  Define reusable sets of prompts and tools for domain-specific workflows without burying them inside a UI.

- **Retrieval-grounded assistants**  
  Index local docs or source trees and let prompts query them with natural language.

- **CI and scripted runs**  
  Run prompts non-interactively in scripts, CI jobs, or recurring automation tasks.

- **MCP-exposed prompt tools**  
  Publish prompts as MCP tools so IDEs and other hosts can call them over stdio or HTTP/SSE.

---


## Example ChatMD prompts

### Example: minimal prompt

Create `prompts/hello.md`:

```xml
<config model="gpt-5.2" reasoning_effort="medium"/>

<developer>
You are a helpful assistant.
</developer>

<user>
Say hello and explain what Ochat is in one sentence.
</user>
```

Run it:

```sh
dune exec chat_tui -- -file prompts/hello.md
```

Or:

```sh
ochat chat-completion \
  -prompt-file prompts/hello.md \
  -output-file .chatmd/hello-run.md
```

---

### Example: interactive refactor agent

Turn a `.md` file into a refactoring bot that reads files and applies patches under your control.

Create `prompts/refactor.md`:

```xml
<config model="gpt-5.2" reasoning_effort="medium"/>

<tool name="read_dir"/>
<tool name="read_file"/>
<tool name="apply_patch"/>

<developer>
You are a careful refactoring assistant. Work in small, reversible steps.
Before calling apply_patch, explain the change you want to make and wait for
confirmation from the user.
</developer>

<user>
We are in a codebase. Look under ./lib, find a small improvement and
propose a patch.
</user>
```

Open it in the TUI:

```sh
dune exec chat_tui -- -file prompts/refactor.md
```

From there you can ask the assistant to rename a function, extract a helper, or
update documentation. It will use `read_dir` and `read_file` to inspect the
code, then generate `apply_patch` diffs and apply them.

---

### Example: publish a prompt as an MCP tool

Export a `.md` file as a remote tool that other MCP-compatible clients can call.

Create `prompts/hello.md`:

```xml
<config model="gpt-5.2" reasoning_effort="medium"/>

<tool name="read_dir"/>
<tool name="read_file"/>

<developer>You are a documentation assistant.</developer>

<user>
List the files under docs-src/ and summarize what each top-level folder is for.
</user>
```

Start the MCP server so it exports `hello.md` as a tool:

```sh
dune exec mcp_server -- --http 8080
```

Any MCP client can now discover the `hello` tool via `tools/list` and call it
with `tools/call` over JSON-RPC. For example:

```sh
curl -s http://localhost:8080/mcp \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'
```

The response includes an entry for `hello` whose JSON schema is inferred from
the ChatMD file.

---

### Example: moderated ChatMD prompt

You can attach one ChatML moderator script to a prompt and keep it host-managed.

Create `prompts/review.chatmd`:

```md
<config model="gpt-5.2" reasoning_effort="medium"/>

<tool name="read_file"/>
<tool name="apply_patch"/>

<script language="chatml" kind="moderator" id="main">

type state =
  { reminded : bool }

type event =
  [ `Session_start
  | `Session_resume
  | `Turn_start
  | `Item_appended(item)
  | `Pre_tool_call(tool_call)
  | `Post_tool_response(tool_result)
  | `Turn_end
  ]

let initial_state : state =
  { reminded = false }

let on_event : context -> state -> event -> state task =
  fun ctx st ev ->
    match ev with
    | `Session_start ->
      let* () =
        Turn.prepend_system(
          "Before calling apply_patch, explain the change briefly."
        )
      in
      Task.pure(st)
    | _ ->
      Task.pure(st)

</script>

<developer>
You are a careful code assistant.
</developer>

<user>
Review lib/example.ml and suggest a small safe improvement.
</user>
```

This example prepends a system instruction at session start, requiring the assistant to explain changes briefly before using `apply_patch`.


Run it in the TUI or CLI:

```sh
dune exec chat_tui -- -file prompts/review.chatmd
```

```sh
ochat chat-completion \
  -prompt-file prompts/review.chatmd \
  -output-file .chatmd/review-run.chatmd
```

For a richer end-to-end example, see:
- [General Assistant – agent workflow](docs-src/guide/general-agent-workflow.md)
- [prompt-examples](prompt-examples/readme.md)

### Writing moderator scripts with `Item.*`

Moderator scripts receive `ctx.items`, where each item has the shape:

```ocaml
type item =
  { id : string
  ; value : json
  }
```

The `Item` module provides helpers so scripts do not need to hand-author raw
JSON for common cases:

- `Item.id(item)` returns the stable item id used by overlay operations
- `Item.value(item)` returns the underlying structured JSON payload
- `Item.kind(item)` reads the serialized item `"type"` field when present
- `Item.role(item)` extracts a message role when the item has one
- `Item.text_parts(item)` collects text fragments from common message-like items
- `Item.input_text_message(id, role, text)` builds a structured input message
- `Item.output_text_message(id, text)` builds a structured assistant message
- `Item.create(id, value)` wraps arbitrary structured JSON as an item

Example:

```ocaml
let first_text : string array -> string =
  fun parts ->
    if Array.length(parts) == 0 then "" else Array.get(parts, 0)

let on_event : context -> state -> event -> state task =
  fun ctx st ev ->
    match ev with
    | `Item_appended(item) ->
      let summary =
        Item.id(item)
        ++ ":"
        ++ Option.get_or(Item.role(item), "unknown")
        ++ ":"
        ++ first_text(Item.text_parts(item))
      in
      Task.bind(Turn.append_item(Item.output_text_message("summary", summary)), fun ignored ->
      Task.pure(st))
    | _ ->
      Task.pure(st)
```

Prefer `Turn.append_item`, `Turn.replace_item`, and `Turn.delete_item`.
The older `append_message`, `replace_message`, and `delete_message` names are
still accepted as aliases.

### Runtime semantics in `chat_tui`

When a prompt runs in `chat_tui`, moderation is split across three layers:

- `Moderator_manager` owns durable moderator state, overlay state, and queued
  internal events.
- `In_memory_stream` owns one active model/tool turn.
- `chat_tui` owns the session controller that drains wakeups while idle,
  refreshes visible transcript state, and schedules follow-up turns without
  creating fake user messages.

The visible transcript in the UI is a projection of canonical history through
the moderator overlay. During streaming, `chat_tui` still applies token and
tool patches directly for responsiveness, then reprojects the visible
transcript at safe points such as:

- idle/background moderator drains,
- the end of a streamed turn,
- the end of compaction,
- startup or resume moderation.

Two host behaviors are especially important:

1. **Idle async wakeups**

   Background producers such as `Model.spawn` completions may enqueue moderator
   internal events while no turn is active. The host wakes the session
   controller, drains those internal events, refreshes visible transcript
   state, and may schedule an idle follow-up turn if the moderator requests
   one.

2. **Deferred steering notes**

   If the user submits steering text while a turn is already streaming, the
   host does not splice a new canonical user message into the in-flight model
   request. Instead it stores a deferred steering note and injects it only at
   the next safe model-input boundary. This preserves the current reasoning and
   tool workflow while still letting the user steer the next request.

An end-to-end idle async completion looks like this:

1. a moderator script previously calls `Model.spawn(...)`
2. the spawned job finishes and is reinjected as an internal event
3. the idle `chat_tui` session receives a moderator wakeup
4. the reducer drains queued internal events through `Moderator_manager`
5. any overlay changes are reprojected into the visible transcript
6. if the moderator emitted `Runtime.request_turn()`, the host starts one more
   ordinary turn from the current session state

An end-to-end deferred-steering flow during a tool run looks like this:

1. the assistant is in the middle of a streamed turn or tool workflow
2. the user submits steering text
3. the host records a deferred steering note instead of appending a canonical
   user item mid-turn
4. the current turn reaches a safe point and eventually completes
5. the next request is prepared from moderator-effective history
6. the deferred steering note is appended as transient system input for that
   request only

---

## Build from source (OCaml)

Install dependencies, build, and run tests:

```sh
opam switch create .
opam install . --deps-only

dune build
dune runtest
```

> On Apple Silicon (macOS arm64), Owl's OpenBLAS dependency can sometimes fail
> to build during `opam install`. If you see BLAS/OpenBLAS errors while
> installing dependencies or running `dune build`, see
> [Build & installation troubleshooting](docs-src/guide/build-troubleshooting.md#owl--openblas-on-apple-silicon-macos-arm64)
> for a proven workaround.

Run an interactive session with the terminal UI:

```sh
dune exec chat_tui -- -file prompts/interactive.md
```

Or run a non-interactive chat completion over a ChatMD prompt as a smoke test:

```sh
ochat chat-completion \
  -prompt-file prompts/hello.md \
  -output-file .chatmd/smoke.md
```

For more on `ochat chat-completion` (flags, exit codes, ephemeral runs), see
[`docs-src/cli/chat-completion.md`](docs-src/cli/chat-completion.md).

---

## Core concepts

- **ChatMarkdown (ChatMD)**  
  A Markdown + XML dialect that stores model config, tool declarations, and the full conversation (including tool calls, reasoning traces, and imported artifacts) in a single `.md` file. See the [language reference](docs-src/overview/chatmd-language.md).

- **Tools**  
  Functions the model can call, described by explicit JSON schemas. They can be built-ins, shell wrappers, other ChatMD agents, or remote MCP tools. See [Tools – built-ins, custom helpers & MCP](docs-src/overview/tools.md).

- **chat_tui**  
  A Notty-based terminal UI for editing and running `.md` files. It turns each prompt into a terminal application with streaming output, persistent sessions, and export/branch workflows. See the [chat_tui guide](docs-src/guide/chat_tui.md).

- **CLI and helpers**  
  Binaries like `ochat`, `md-index`, and `md-search` provide script-friendly entry points for running prompts and building/querying indexes.

- **MCP server**  
  `mcp_server` turns `.md` files and selected tools into MCP resources and tools that other applications can list and call over stdio or HTTP/SSE. See the [mcp_server binary doc](docs-src/bin/mcp_server.doc.md).

- **Search & indexing**  
  Modules and binaries that build vector indexes over markdown docs and source code, powering tools like `markdown_search` and `query_vector_db`. See [Search, indexing & code intelligence](docs-src/guide/search-and-indexing.md).

- **Meta-prompting**  
  A library and CLI (`mp-refine-run`) for generating, scoring, and refining prompts in a loop. See the [`Meta_prompting` overview](docs-src/lib/meta_prompting.doc.md).

---

## Architecture overview

At a glance, Ochat treats workflows as text artifacts executed by a host runtime.

```text
                 ChatMD workflow file
   ┌──────────────────────────────────────────────┐
   │ config                                       │
   │ tools                                        │
   │ prompt messages                              │
   │ transcript / tool calls / tool results       │
   │ optional ChatML moderation script            │
   └──────────────────────────────────────────────┘
                           │
                           ▼
          ┌──────────────────────────────────┐
          │ Host runtime                     │
          │ - `chat_tui`                     │
          │ - `ochat chat-completion`        │
          │ - `mcp_server`                   │
          └──────────────────────────────────┘
              │                  │
              ▼                  ▼
      ┌───────────────┐   ┌──────────────────┐
      │ Model backend │   │ Tool execution   │
      │ OpenAI today  │   │ built-ins/shell/ │
      │ more later    │   │ MCP/other agents │
      └───────────────┘   └──────────────────┘
              │                  │
              └──────────┬───────┘
                         ▼
             Updated ChatMD transcript/state
           (diffable, reproducible, resumable)
```

At a high level, Ochat has two layers:

- **ChatMD** is the workflow document format  
  It stores prompts, tools, transcript state, and execution artifacts.

- **ChatML** is the optional host-managed scripting layer  
  It adds workflow logic such as moderation, transcript editing, policy enforcement, and multi-step orchestration.

A typical flow looks like this:

1. Load a ChatMD file
2. Parse config, tools, transcript, and optional script
3. Run it in a host (`chat_tui`, CLI, or MCP)
4. Let the model call tools and produce output
5. Optionally let a ChatML moderator inspect/modify the effective transcript or tool flow
6. Persist the resulting workflow state back as text artifacts

This split keeps workflows inspectable while still allowing advanced control logic.

For moderated interactive sessions, the host runtime has a more specific role
split:

- `Moderator_manager` keeps durable moderator state, overlay state, halted
  state, and queued internal events.
- `In_memory_stream` executes one active completion/tool-followup loop at a
  time and handles explicit safe points such as turn start, post-tool-result,
  and turn end.
- `chat_tui` runs a single-threaded session controller over its reducer loop.
  That controller reacts to moderator wakeups while idle, defers wakeups while
  a turn is active, refreshes visible transcript state from moderator-effective
  history at safe points, and starts follow-up turns only when the UI is idle.


---

## Documentation

Deep-dive docs live under `docs-src/`. Key entry points:

- [ChatMarkdown language reference](docs-src/overview/chatmd-language.md)
- [Built-in tools & custom tools](docs-src/overview/tools.md)
- [chat_tui guide & key bindings](docs-src/guide/chat_tui.md)
- [`ochat chat-completion` CLI](docs-src/cli/chat-completion.md)
- [MCP server & protocol details](docs-src/bin/mcp_server.doc.md)
- [Search, indexing & code intelligence](docs-src/guide/search-and-indexing.md)
- [Meta-prompting & Prompt Factory](docs-src/lib/meta_prompting.doc.md)
- [Real-world example session: updating the tools docs](real-world-example-session/update-tool-docs/readme.md)

---

## Binaries

| Binary | Purpose | Example |
|--------|---------|---------|
| `chat_tui` (`chat-tui`) | interactive TUI | `chat_tui -file notes.md` |
| `ochat` | misc CLI (index, query, tokenise …) | `ochat query -vector-db-folder _index -query-text "tail-rec map"` |
| `mcp_server` | serve prompts & tools over JSON-RPC / SSE | `mcp_server --http 8080` |
| `mp-refine-run` | refine prompts via recursive meta-prompting | `mp-refine-run -task-file task.md -input-file draft.md` |
| `md-index` / `md-search` | Markdown → index / search | `md-index --root docs`; `md-search --query "streams"` |
| `odoc-index` / `odoc-search` | (OCaml) odoc HTML → index / search | `odoc-index --root _doc/_html` |

Run any binary with `-help` for details.

---

## Project layout

```text
bin/         – chat_tui, mcp_server, ochat …
lib/         – re-usable libraries (chatmd, functions, vector_db …)
docs-src/    – Markdown docs rendered by odoc & included here
prompts/     – sample ChatMD prompts served by the MCP server
dune-project – dune metadata
```

---

## OCaml integration

Ochat is implemented in OCaml. While the workflows themselves are language-agnostic, Ochat has first-class support for OCaml development workflows.

### Why OCaml?

Ochat is heavy on:
- structured data and parsing
- symbolic transformations
- workflow state modeling
- reliability-sensitive orchestration
- compiler- and test-driven repair loops

These are all areas where OCaml is especially strong.

### OCaml-specific entry points

- **OCaml development environment guide**  
  See [`DEVELOPMENT.md`](DEVELOPMENT.md) for a walkthrough that sets up local OCaml documentation, search indexes, and related workflows.

- **OCaml API doc search**  
  `odoc-index` / `odoc-search` index and search generated odoc HTML.

- **Embedding as a library**  
  Use the OCaml libraries directly. See [Embedding Ochat in OCaml](docs-src/lib/embedding.md).

- **OCaml indexing & code intelligence**  
  Ochat can parse and index OCaml source directly (no LSP dependency) to build precise code search and code-aware agent workflows.

### Using builds/tests as an LLM feedback loop

When you run Ochat against an OCaml repository, the usual `dune build` / `dune runtest` loop becomes a high-signal feedback channel for LLM-generated edits: let an agent propose `apply_patch` diffs, run the build and tests, then feed compiler errors or failing tests back into the next turn.

### ChatML (experimental, but integrated for moderation)

The repository ships an experimental language called **ChatML**: a small, expression-oriented ML dialect with Hindley–Milner type inference extended with row polymorphism for records and variants.

Today ChatML is integrated primarily as the host-managed moderation layer for ChatMD:

- a prompt may declare one `<script language="chatml" kind="moderator" ...>`
- the script runs through the shared moderation manager used by `chat_tui`, file-backed drivers, nested agents, and MCP prompt wrappers
- moderator scripts receive `ctx.items` where each item is `{ id; value : json }`
- moderator scripts can modify transcript items and moderate tool calls
- moderator scripts can request another turn via `Runtime.request_turn()`
- moderator scripts can call host-registered model recipes via `Model.call`
- moderator scripts can spawn background model jobs via `Model.spawn`

Outside that moderation integration, ChatML is also available through the experimental `dsl_script` binary and the `Chatml_*` library modules.

For more details, see:
- [`docs-src/guide/chatml-language-spec.md`](docs-src/guide/chatml-language-spec.md)
- [`docs-src/guide/chatml-match-semantics.md`](docs-src/guide/chatml-match-semantics.md)
- [`docs-src/lib/chatml/chatml_lang.doc.md`](docs-src/lib/chatml/chatml_lang.doc.md)
- [`docs-src/lib/chatml/chatml_parser.doc.md`](docs-src/lib/chatml/chatml_parser.doc.md)
- [`docs-src/lib/chatml/chatml_resolver.doc.md`](docs-src/lib/chatml/chatml_resolver.doc.md)

---

## Future directions

Ochat is intentionally **agent-first**: the roadmap focuses on making ChatMD, the runtime, and `chat_tui` more expressive for building and operating fleets of custom agents.

Planned and experimental directions include:

- **Explicit control-flow & policy in ChatMD**  
  A design sketch for a rules layer over ChatMD that could express things like auto-compaction, tool validation policies, and declarative control flow without hiding logic from the transcript.

- **Richer session tracking, branching, and evaluation**  
  Better first-class support for branching conversations, long-term archives, and evaluation runs.

- **Session data backed by Irmin**  
  A planned per-session state and filesystem model with isolation, persistence, and versioning.

- **Additional LLM providers**  
  Today Ochat integrates with OpenAI; future work is intended to support additional backends while keeping ChatMD and tool contracts stable.

- **Broader ChatML scripting roles**  
  ChatML is currently focused on moderation and orchestration; the longer-term plan is to broaden that scripting role without sacrificing safety or auditability.

- **Custom OCaml functions as tools via Dune plugins**  
  A planned direction is to expose custom OCaml functions as tools via Dune plugins.

All of these directions share the same goal: make agents more reliable, composable, and expressive **without** sacrificing the “everything is a text file” property.

---

## Project status – expect rapid change

Ochat is a **fast-moving project**.

The core file-oriented workflow model is designed to stay stable, but APIs, tool schemas, and some higher-level design choices may continue to evolve as the project explores what works best.

Despite the experimental label, **you can build real value today**. The repository already enables powerful custom agent workflows for development, documentation, writing, and automation.

Please budget time for occasional refactors and breaking changes.

Bug reports, feature requests, and PRs are very welcome.

Documentation is also a work in progress. If you find gaps or rough edges, please open issues or PRs to help improve it.

---

## License

All original source code is licensed under the terms stated in `LICENSE.txt`.
