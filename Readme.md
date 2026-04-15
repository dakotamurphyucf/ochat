
# Ochat – text-first toolkit for custom AI agents, LLM workflows, and vector search

**Build custom AI agents and scripted LLM workflows as plain text files.**

Ochat is an **OCaml toolkit** for building **reproducible, composable, tool-using LLM workflows** without locking the workflow into a single UI or heavyweight framework.

Instead of hiding prompts, tool permissions, transcript state, and orchestration inside an application, Ochat keeps them in **static, diffable files** that you can version-control, review, branch, and run in different hosts.

If you like tools like Claude Code or Codex, Ochat operates at a more fundamental level: it gives you the building blocks to create **your own prompt packs, agents, and workflow systems**.

<div>
<a href="https://asciinema.org/a/gIelV4eAeA0LKvG7" target="_blank"><img height="700" width="900" src="https://asciinema.org/a/gIelV4eAeA0LKvG7.svg" /></a>
</div>

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

The goal is simple: make agent workflows **explicit, portable, and reproducible**.

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
- orchestrate additional model-backed work via `Model.call` and `Model.spawn`

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

Ochat sits in a different part of the LLM tooling landscape than most agent tools.

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

## Comparison summary

| Tool type | Typical model | How Ochat differs |
|-----------|---------------|-------------------|
| Coding agents | fixed end-user assistant | Ochat lets you build your own agent workflows |
| Orchestration frameworks | workflows in code | Ochat defines workflows as plain files |
| Prompt platforms | hosted dashboards | Ochat is local/artifact-first |
| MCP tools | protocol/tool exposure | Ochat is a full workflow system that can also speak MCP |

---

## Why this matters

As LLM workflows become more important, prompts, tools, transcript state, and orchestration increasingly need to behave like **real engineering artifacts**.

Ochat is designed around that idea.

---

## What can I do with Ochat?

- **Author agent workflows as static files**  
  Write agents as `.md` files (ChatMarkdown). A file can act as a reusable prompt *and* the execution log of running one: model config, tool permissions, tool calls/results, and the full transcript. You can version-control, diff, and refactor them like code. Supports branching: run a prompt and export the full conversation log with the original prompt to a new file that you can resume or fork later.

- **Compose unique agents via composition of tools (built-ins + your tools) and message inputs via ChatMD prompts**  

  You can mix:

  - **Well-crafted prompting via message inputs** 
    - Use the rich ChatMD language to express developer / user messages to drive agent behavior.

  - **built-in tools** for common building blocks like:
    - repo-safe editing: `apply_patch` 
    - filesystem reads: `read_dir` (directory listing), `read_file` *(alias: `get_contents`)*
    - web ingestion: `webpage_to_markdown` (HTML → Markdown + GitHub blob fast-path)
    - local semantic search over docs: `index_markdown_docs` + `markdown_search`, and `odoc_search`
    - hybrid retrieval over code: `index_ocaml_code` + `query_vector_db`
    - vision inputs: `import_image` (bring local screenshots/diagrams into the model)
  - **custom shell tools** to wrap any command you already trust (`git`, `rg`, linters, internal CLIs…)
  - **remote MCP tools** to import capabilities from other servers (or to export your own prompt pack as tools)
  - **agent-as-tool**: mount other `.md` files as tools inside a prompt.
  See [Tools – built-ins, custom helpers & MCP](docs-src/overview/tools.md).

  


- **Build Claude Code/Codex-style agentic applications via custom “prompt packs”**  
  You can implement this as a set of specialized agents (planning agent, coding agent, test agent, doc agent…) and wire them together in an orchestration agent via agent-as-tool. The “application” is just a set of ChatMD files and you can run it via the terminal ui (`chat_tui`) or via the chat-completion CLI (`ochat chat-completion`). The key is combining well crafted prompts with the right set of tools (e.g. `read_file`, `apply_patch`, `webpage_to_markdown`, etc) to achieve desired behavior. All without having to be vendor-locked into a specific UI or platform.

- **Run the same workflows in different hosts**  
  Use `chat_tui` for interactive sessions, `ochat chat-completion` for scripts/CI/cron, and `mcp_server` to expose prompts as tools to IDEs and other hosts.

- **Ground agents in your own corpus**  
  Build vector indexes for docs/source trees and provide query capabilities within prompts so the agent can use natural language to query them. See [Search, indexing & code intelligence](docs-src/guide/search-and-indexing.md).

- **Generate and continuously improve prompts**  
   Via a technique known as meta-prompt refinement, use the `mp-refine-run` binary to iteratively refine prompts and tool descriptions using an LLM to generate/refine drafts and judge quality over multiple iterations.

- **Provide reproducible conversation state**  
  Because everything is stored in text files, you can version-control, diff, and refactor conversations like code. You can also export the full conversation log (including tool calls/results) to a new file that you can share, resume or fork later.
---

## Example ChatMD prompts


### Example: interactive refactor agent

Turn a `.md` file into a refactoring bot that reads files and applies patches under your control.

1. Create `prompts/refactor.md`:

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

2. Open it in the TUI:

```sh
dune exec chat_tui -- -file prompts/refactor.md
```

From there you can ask the assistant to rename a function, extract a helper, or
update documentation. It will use `read_dir` and `read_file` to inspect the
code, then generate `apply_patch` diffs and apply them, where every tool call
and patch can be recorded in the `.md` file.

### Example: publish a prompt as an MCP tool

Export a `.md` file as a remote tool that other MCP‑compatible clients can call.

1. Create `prompts/hello.md`:

```xml
<config model="gpt-5.2" reasoning_effort="medium"/>

<tool name="read_dir"/>
<tool name="read_file"/>

<developer>You are a documentation assistant.</developer>

<user>
List the files under docs-src/ and summarize what each top-level folder is for.
</user>
```

2. Start the MCP server so it exports `hello.md` as a tool (by default it
   reads prompts from `./prompts`, or from `$MCP_PROMPTS_DIR` if set):

```sh
dune exec mcp_server -- --http 8080
```

Any MCP client can now discover the `hello` tool via `tools/list` and call it
with `tools/call` over JSON‑RPC. For example, a minimal HTTP request that lists
the available tools looks like:

```sh
curl -s http://localhost:8080/mcp \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}'
```

The response includes an entry for `hello` whose JSON schema is inferred from
the ChatMD file; calling that tool runs your prompt and streams the result back
to the client.

### Example: moderated ChatMD prompt

You can attach one ChatML moderator script to a prompt and keep it host-managed.
The script is parsed, validated, compiled once per prompt load, and then
instantiated per session by `chat_tui`, `ochat chat-completion`, nested
`run_agent` calls, and MCP prompt wrappers.

1. Create `prompts/review.chatmd`:

```md
<config model="gpt-5.2" reasoning_effort="medium"/>

<tool name="read_file"/>
<tool name="apply_patch"/>

<script language="chatml" kind="moderator" id="main">

(* Multi-step plan moderator script pattern:

   Goal:
   - Track a list of tasks (a “plan”)
   - When the assistant finishes a task, request history compaction
   - Append a new <user> message describing the next task
   - Request another ordinary turn so the next task starts immediately

   Conventions used by this script:
   - The assistant must explicitly mark task completion by including a sentinel
     string in its final assistant message for the task, e.g. "TASK_DONE".
   - Compaction is a host request (some embeddings honor it automatically, others log it).
   - Runtime.request_turn() is only called from Turn_end (allowed in v1).
*)

type task =
  { title : string
  ; prompt : string
  }

type state =
  { tasks : task array
  ; idx : int
  ; next_id : int
  }

type event =
  [ `Session_start
  | `Session_resume
  | `Turn_start
  | `Message_appended(item)
  | `Pre_tool_call(tool_call)
  | `Post_tool_response(tool_result)
  | `Turn_end
  | `Internal_event   (* not used directly here; internal events are raw values *)
  ]

let initial_state : state =
  { tasks =
      [ { title = "Task 1: Inspect repo"
        ; prompt =
            "Inspect the repository. Summarize the relevant modules and identify risks."
            ++ "\n\nWhen finished, print exactly: TASK_DONE"
        }
      , { title = "Task 2: Implement feature"
        ; prompt =
            "Implement the next change. Keep it small and testable."
            ++ "\n\nWhen finished, print exactly: TASK_DONE"
        }
      , { title = "Task 3: Run tests + summarize"
        ; prompt =
            "Run dune build and the smallest relevant tests. Summarize results."
            ++ "\n\nWhen finished, print exactly: TASK_DONE"
        }
      ]
  ; idx = 0
  ; next_id = 1
  }

let join_lines (xs : string array) : string =
  let i = ref(0) in
  let out = ref("") in
  while !i < Array.length(xs) do
    let line = Array.get(xs, !i) in
    if String.is_empty(!out) then out := line else out := !out ++ "\n" ++ line;
    i := !i + 1
  done;
  !out

let last_assistant_text (ctx : context) : string =
  (* Search from the end for an assistant-like item with text parts. *)
  let i = ref(Array.length(ctx.items) - 1) in
  let found = ref("") in
  while !i >= 0 do
    let it = ctx.items[!i] in
    match Item.role(it) with
    | `Some(role) ->
      if String.equal(role, "assistant") then (
        let parts = Item.text_parts(it) in
        if Array.length(parts) > 0 then (
          found := join_lines(parts);
          i := -1
        ) else (
          i := !i - 1
        )
      ) else (
        i := !i - 1
      )
    | `None ->
      i := !i - 1
  done;
  !found

let make_user_item (st : state) (text : string) : item =
  let id = "plan-user-" ++ to_string(st.next_id) in
  (* role is a string in Item.input_text_message(id, role, text) *)
  Item.input_text_message(id, "user", text)

let on_event : context -> state -> event -> state task =
  fun ctx st ev ->
    match ev with
    | `Session_start ->
      let* () =
        Turn.prepend_system(
          "This prompt uses a moderator-driven multi-step plan.\n"
          ++ "- The assistant must print TASK_DONE when a task is complete.\n"
          ++ "- After TASK_DONE, the moderator requests compaction, appends the next task as a user message, and requests another turn."
        )
      in
      Task.pure(st)

    | `Session_resume ->
      let user_text =
      "Session resumed; continuing plan at idx=" ++ to_string(st.idx)
      in
      let user_item = make_user_item(st, user_text) in
      let* () = Turn.append_item(user_item) in
      Task.pure(st)

    | `Turn_end ->
      (* Only here (and internal_event) is Runtime.request_turn allowed in v1. *)
      if st.idx >= Array.length(st.tasks) then
        (* Plan already finished *)
        Task.pure(st)
      else (
        let txt = last_assistant_text(ctx) in
        if String.contains(String.trim(txt), "TASK_DONE") then (
          let next_idx = st.idx + 1 in

          if next_idx >= Array.length(st.tasks) then (
            let* () = Runtime.request_compaction() in
            let* () = Runtime.end_session("Plan completed: all tasks finished.") in
            Task.pure({ st with idx = next_idx })
          ) else (
            let next_task = st.tasks[next_idx] in
            let user_text =
              "Next: " ++ next_task.title ++ "\n\n" ++ next_task.prompt
            in
            let user_item = make_user_item(st, user_text) in

            (* Request compaction after each completed task. *)
            let* () = Runtime.request_compaction() in

            (* Append a new user message so the next turn has explicit instructions. *)
            let* () = Turn.append_item(user_item) in
            let payload =
              agent_prompt_payload(
                "prompts/commit_message_agent.chatmd",
                true,
                "Add the current changes in the repo and write a concise git commit message based on the changes",
                ctx.session_id ++ ":commit-msg")
            in
            let* res_ = Model.call("agent_prompt_v1", payload) in
            
            (* Ask the host to run one more ordinary turn now that we appended the next task. *)
            let* () = Runtime.request_turn() in

            Task.pure({ st with idx = next_idx; next_id = st.next_id + 1 })
          )
        ) else
          Task.pure(st)
      )

    | `Turn_start
    | `Message_appended(_)
    | `Pre_tool_call(_)
    | `Post_tool_response(_) ->
    | `Internal_event ->
      Task.pure(st)
         
</script>

<developer>
You are a careful code assistant.
</developer>

```

2. Run it in the TUI or CLI:

```sh
dune exec chat_tui -- -file prompts/review.chatmd
```

```sh
ochat chat-completion \
  -prompt-file prompts/review.chatmd \
  -output-file .chatmd/review-run.chatmd
```

Notes:

- exactly one moderator `<script>` is supported per prompt in v1,
- `src="moderator.chatml"` is also supported if you want the script in a
  separate file,
- tool rejections produce synthetic tool outputs instead of executing the
  underlying tool,
- exported transcripts materialize moderation-visible inserts, deletions, and
  halt markers explicitly.

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
    | `Message_appended ->
      let item = ctx.items[Array.length(ctx.items) - 1] in
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

For a more advanced, end-to-end agent built from the same building
blocks, see the
[General Assistant – agent workflow](docs-src/guide/general-agent-workflow.md). This is the agent that was used to generate the example
recording at the top of this readme. Other examples located in [prompt-examples](prompt-examples/readme.md)

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

Or run a non‑interactive chat completion over a ChatMD prompt as a smoke test:

```sh
ochat chat-completion \
  -prompt-file prompts/hello.md \
  -output-file .chatmd/smoke.md
```

For more on `ochat chat-completion` (flags, exit codes, ephemeral runs), see
[`docs-src/cli/chat-completion.md`](docs-src/cli/chat-completion.md).

---

## [Core concepts](#concepts)

- **ChatMarkdown (ChatMD)**  \
  A Markdown + XML dialect that stores model config, tool declarations and the full conversation (including tool calls, reasoning traces and imported artefacts) in a single `.md` file. Because prompts are plain text files you can review, diff and refactor them like code, and the runtime guarantees that what the model sees is exactly what is in the document. See the [language reference](docs-src/overview/chatmd-language.md).

- **Tools**  \
  Functions the model can call, described by explicit JSON schemas. They can be built‑ins (e.g. `apply_patch`, `read_dir`, `read_file` *(alias: `get_contents`)*, `webpage_to_markdown`, `import_image`), shell wrappers around commands like `rg` or `git`, other ChatMD agents (prompt‑as‑tool), or remote MCP tools discovered from another server. (When embedding Ochat, you can also expose custom functions from your host application.) See [Tools – built‑ins, custom helpers & MCP](docs-src/overview/tools.md).

- **chat_tui**  \
  A Notty‑based terminal UI for editing and running `.md` files. It turns each prompt into a **terminal application**: live streaming of model output and tool responses, Vim‑style navigation, context compaction for long histories, and persistent sessions that you can resume or branch. You can think of `chat_tui` as the “host” and `.md` files as pluggable apps. See the [chat_tui guide](docs-src/guide/chat_tui.md).

- **CLI and helpers**  \
  Binaries like `ochat` and `md-index` / `md-search` provide script‑friendly entry points for running prompts, building indexes and querying them from the shell. (If you’re in OCaml, `odoc-index` / `odoc-search` can also index generated API docs.) See the [`ochat chat-completion` CLI](docs-src/cli/chat-completion.md) for non‑interactive runs; other commands are documented under `docs-src/cli/` and the generated odoc docs.

- **MCP server**  \
  `mcp_server` turns `.md` files and selected tools into MCP resources and tools that other applications can list and call over stdio or HTTP/SSE. See the [mcp_server binary doc](docs-src/bin/mcp_server.doc.md).

- **Search & indexing**  \
  Modules and binaries that build vector indexes over markdown docs and source code, powering tools like `markdown_search` and `query_vector_db`. (If you’re in OCaml, you can also index generated API docs.) See [Search, indexing & code-intelligence](docs-src/guide/search-and-indexing.md).

- **Meta-prompting**  \
  A library and CLI (`mp-refine-run`) for generating, scoring and refining prompts in a loop, so prompt engineering itself can be versioned and automated. See the [`Meta_prompting` overview](docs-src/lib/meta_prompting.doc.md).

> Each bullet links to a deeper reference under `docs-src/`.

---

## Documentation

Deep-dive docs live under `docs-src/`. Key entry points:

- [ChatMarkdown language reference](docs-src/overview/chatmd-language.md) – element tags, inline helpers, and prompt‑writing guidelines.
- [Built-in tools & custom tools](docs-src/overview/tools.md) – built‑in toolbox, shell wrappers, custom tools, and MCP tool import.
- [chat_tui guide & key bindings](docs-src/guide/chat_tui.md) – quick-start + muscle-memory cheat sheet, modes (including the ESC/cancel/quit behavior), editing + message selection workflows, quitting/export rules, sessions, context compaction, and troubleshooting.
- [`ochat chat-completion` CLI](docs-src/cli/chat-completion.md) – non‑interactive runs, flags, exit codes and ephemeral runs.
- [MCP server & protocol details](docs-src/bin/mcp_server.doc.md) – how `mcp_server` exposes prompts and tools over stdio or HTTP/SSE.
- [Search, indexing & code intelligence](docs-src/guide/search-and-indexing.md) – indexers, searchers and prompt patterns for hybrid retrieval.
- [Meta-prompting & Prompt Factory](docs-src/lib/meta_prompting.doc.md) – generators, evaluators, refinement loops and prompt packs.
- [Real-world example session: updating the tools docs](real-world-example-session/update-tool-docs/readme.md) – a non-trivial end-to-end ochat run (full transcript + compacted version using the chat compaction feature).

---

## Binaries

| Binary | Purpose | Example |
|--------|---------|---------|
| `chat_tui` (`chat-tui`) | interactive TUI | `chat_tui -file notes.md` |
| `ochat`    | misc CLI (index, query, tokenise …) | `ochat query -vector-db-folder _index -query-text "tail-rec map"` |
| `mcp_server` | serve prompts & tools over JSON-RPC / SSE | `mcp_server --http 8080` |
| `mp-refine-run` | refine prompts via *recursive meta-prompting* | `mp-refine-run -task-file task.md -input-file draft.md` |
| `md-index` / `md-search` | Markdown → index / search | `md-index --root docs`; `md-search --query "streams"` |
| `odoc-index` / `odoc-search` | (OCaml) odoc HTML → index / search | `odoc-index --root _doc/_html` |

Run any binary with `-help` for details.

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

## OCaml integration

Ochat is implemented in OCaml. Ochat intends to be language agnostic and the *workflows* can be used in any setup (tools exchange JSON; prompts are plain files), but being implemented in OCaml it has first class support for ocaml development. these entry points are OCaml-specific:

- **OCaml development environment guide**: see [`DEVELOPMENT.md`](DEVELOPMENT.md) for a dedicated walkthrough that sets up local OCaml documentation, search indexes, and related workflows that are useful for OCaml-focused agents.
- **OCaml API doc search**: `odoc-index` / `odoc-search` index and search generated odoc HTML.
- **Embedding as a library**: use the OCaml libraries directly (see [Embedding Ochat in OCaml](docs-src/lib/embedding.md)).
- **Ocaml indexing & code intelligence**: provides parsing and indexing of OCaml source files directly (no LSP dependency) to build precise indexes for code search and code-aware agents.

### Using builds/tests as an LLM feedback loop

When you run Ochat against an OCaml repository, the usual `dune build` / `dune runtest` loop becomes a high-signal feedback channel for LLM-generated edits: let an agent propose `apply_patch` diffs, run the build and tests, then feed compiler errors or failing expect tests back into the next turn.

### ChatML (experimental, but integrated for moderation)

The repository ships an experimental language called *ChatML*: a small,
expression-oriented ML dialect with Hindley–Milner type inference (Algorithm W)
extended with row polymorphism for records and variants (see
[language-spec](docs-src/guide/chatml-language-spec.md)).

The parser, type-checker and runtime live under the `Chatml` modules and are
documented under `docs-src/lib/chatml/` and `docs-src/guide/` (see
[`language-spec`](docs-src/guide/chatml-language-spec.md),
[`match-semantics`](docs-src/guide/chatml-match-semantics.md),
[`chatml_lang`](docs-src/lib/chatml/chatml_lang.doc.md),
[`chatml_parser`](docs-src/lib/chatml/chatml_parser.doc.md) and
[`chatml_resolver`](docs-src/lib/chatml/chatml_resolver.doc.md)).

Today ChatML is integrated as the host-managed moderation layer for ChatMD:

- a prompt may declare one `<script language="chatml" kind="moderator" ...>`,
- the script runs through the shared moderation manager used by `chat_tui`,
  file-backed drivers, nested agents, and MCP prompt-agent wrappers,
- moderator scripts receive `ctx.items` where each item is `{ id; value : json }`,
- the moderator surface includes `Item.*` helpers plus `Turn.append_item`,
  `Turn.replace_item`, and `Turn.delete_item` aliases,
- moderator scripts can request another ordinary turn after `turn_end` via
  `Runtime.request_turn()`,
- moderator scripts can call host-registered model recipes via `Model.call` and
  start background jobs via `Model.spawn`,
- spawned model jobs deliver completion back to the moderator as internal events:
  `Model_job_succeeded(job_id, recipe_name, result_json)` and
  `Model_job_failed(job_id, recipe_name, message)`,
- spawned job tracking is currently in-memory only (in-flight jobs are not
  durably persisted across process restarts),
- the host persists only serializable moderator state and queued internal
  events,
- prompts without a script still follow the baseline non-moderated path.

Outside that moderation integration, ChatML also remains available through the
experimental `dsl_script` binary and the `Chatml_*` library modules.

If you want “real code” examples (including expected types and evaluation
results) see [examples](docs-src/guide/chatml-language-spec.md#207-tiny-workflow-engine);
the tests are also a good starting point:
[`test/chatml_typechecker_test.ml`](test/chatml_typechecker_test.ml),
[`test/chatml_runtime_test.ml`](test/chatml_runtime_test.ml), and the
moderation-oriented tests under [`test/chat_response_*`](test/).

---

## Future directions

Ochat is intentionally **agent-first**: the roadmap focuses on making ChatMD, the runtime and `chat_tui` more expressive for building and operating fleets of custom agents, and on giving you better tools for observing and controlling how those agents behave.

Planned and experimental directions include:

- **Explicit control-flow & policy in ChatMD**  \
  The design note in [`control-flow-chatmd.md`](control-flow-chatmd.md)
  sketches a rules layer on top of ChatMD: you describe *events* (e.g.
  `pre_tool_call`, `turn_end`), *guards* over the transcript, and *actions*
  that materialise as normal ChatMD blocks (`<insert>`, `<deny>`, `<compact>`,
  `<agent>` …). The goal is to let you express things like “auto‑compact when
  the context grows too large” or “never call this tool without validating its
  inputs first” without hiding any logic from the transcript. This rules layer
  is not implemented yet; the document is a design sketch for future
  iterations.

- **Richer session tracking, branching and evaluation**  \
  Today `chat_tui` already persists sessions and lets you resume them.
  Future work focuses on making **branching conversations**, long‑term
  archives and agent evaluation runs first‑class so you can compare different
  agents on the same task, fork past sessions, and keep an auditable trail of
  how an agent evolved over time.

- **Session data (roadmap): per-session state + filesystem, backed by Irmin**  \
  Today sessions are persisted as on-disk snapshots (see the `Session_store` docs). A planned next step is to give agents a first-class way to store and retrieve session-specific data:

  - a simple key/value store API, scoped to the current conversation/session,
  - session-scoped file read/write (a “session filesystem”),
  - isolation by default (no accidental cross-session leakage),
  - tool-called agents inherit the parent session store (so helpers can share state without inventing ad-hoc protocols).

  The intent is for this to be backed by an Irmin database so session state can be versioned, merged, and synced in a principled way. This is not implemented yet; Irmin is currently only used by an auxiliary `sync` binary.

- **Additional LLM providers (roadmap)**  \
  Today the runtime integrates with OpenAI for chat execution and embeddings. A planned direction is to factor provider-specific details behind a stable interface so Ochat can target additional backends (for example: Anthropic/Claude, Google, local models) while keeping ChatMD files and tool contracts the same.

- **ChatML – a small, typed scripting language**  \
  The repository ships an experimental language called *ChatML*: a small,
  expression‑oriented ML dialect with Hindley–Milner type inference (Algorithm
  W) extended with row polymorphism for records and variants. The parser,
  type‑checker and runtime live under the `Chatml` modules and are documented
  under `docs-src/lib/chatml/` (see [`chatml_lang`](docs-src/lib/chatml/chatml_lang.doc.md),
  [`chatml_parser`](docs-src/lib/chatml/chatml_parser.doc.md) and
  [`chatml_resolver`](docs-src/lib/chatml/chatml_resolver.doc.md)). For examples of the language in action, see [`test/chatml_typechecker_test.ml`](test/chatml_typechecker_test.ml), [`test/chatml_runtime_test.ml`](test/chatml_runtime_test.ml), and the moderation-oriented tests under `test/chat_response_*`. Today ChatML is already wired into ChatMD prompts as the host-managed moderation layer via a single `<script language="chatml" kind="moderator" ...>` declaration, while still also being available through the experimental `dsl_script` binary and the `Chatml_*` library modules. The longer-term plan is to broaden that scripting role without sacrificing safety or auditability.

- **Custom Ocaml functions as tools via Dune plugins**  \
  A planned direction is to expose custom OCaml
  functions as tools via [Dune plugins](https://dune.readthedocs.io/en/stable/sites.html#plugins).

All of these directions share the same goal: make agents more reliable, 
composable, and expressive **without** sacrificing the “everything is a text file” property
that makes ChatMD workflows easy to debug and version‑control.

---

## Project status – expect rapid change

Ochat is a *Fast-moving project; core file format is designed to stay stable.  APIs,
tool schemas, and even high-level design choices may change as
we explore what works and what does not.

Despite the experimental label, **you can build real value today** – the
repository already enables powerful custom agent workflows.  I use it daily
with custom agents for everything from developing and documentation
generation, to writing emails and automating mundane tasks. Plus the workflow artifacts are plain text; 
even when internals change, your prompts remain portable

Please budget time for occasional refactors and breaking changes.
Bug reports, feature requests, and PRs are welcome and encouraged actually – just keep in mind the ground may still be
moving beneath your feet.

Documentation is a work in progress too – I try to keep things as updated as possible but some features may have outpaced the docs. If you find gaps or rough edges, please open issues or PRs to help improve it.

---

## License

All original source code is licensed under the terms stated in `LICENSE.txt`.
