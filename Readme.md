
# Ochat – toolkit for building custom AI agents, scripted LLM pipelines & vector search

*Everything you need to prototype and run modern LLM workflows as plain files (implemented in OCaml).*

<div>
 <img src="assets/demo.gif" alt="chat_tui demo" height="700" width="900"/>
</div>


## What is Ochat?

Ochat is a toolkit for building **agent workflows and orchestrations as static files**.

If you like tools like Claude Code or Codex, Ochat is a more fundamental set of building blocks: instead of hard-coding the “agent application” into a single UI, you can implement something Claude Code‑like by shipping a *prompt pack* (a set of `.md` files) plus tools and running the agent using the terminal UI that the project provides.

In Ochat, an agent is a `.md` file written in a Markdown + XML dialect called **ChatMarkdown (ChatMD)**. A single file is the whole program:

- the model and generation parameters,
- which tools the assistant is allowed to call,
- the full conversation history (including tool calls and their results),
- imported artefacts (documents/images) when needed.

The runtime does **not** depend on file extensions: any filename can contain ChatMD. We use `.md` by convention so editors render Markdown nicely and you get syntax highlighting.

Because everything is captured in text files, workflows are:

- **reproducible** – the exact config and transcript are version‑controlled,
- **diff‑able** – reviews show exactly what changed and what the model did,
- **composable** – workflows can call other workflows (prompt‑as‑tool),
- **portable** – prompts are plain text; tools exchange JSON.

The same `.md` definition can be executed in multiple hosts:

- the **terminal UI** (`chat_tui`) for interactive work,
- **scripts and CI** via `ochat chat-completion`, and
- a **remote MCP server** via `mcp_server`, so IDEs or other applications can call agents over stdio or HTTP/SSE.

The chatmd language provides a rich set of features for prompt engineering in a modular way supporting all levels of complexity.

Ochat is implemented in OCaml, and provides tools for ocaml development, but the workflows themselves are **language‑agnostic** and ochat makes no assumptions about the types of applications the workflows target: you can use ochat to build workflows for any use case that benefits from LLMs + tools, and it puts no contraints on how simple or complex those workflows are.

**LLM provider support (today): OpenAI only.** Ochat currently integrates with OpenAI for chat execution and embeddings. The architecture is intended to support additional providers, but those integrations are not implemented yet. 

For details on the current OpenAI surface, see `docs-src/lib/openai/` (for example: [`responses`](docs-src/lib/openai/responses.doc.md)).

If you want the OCaml-specific entry points (embedding as a library, OCaml API doc search, `opam`/`dune` workflows), see the **OCaml integration** section below.

---

## What can I do with Ochat?

- **Author agent workflows as static files**  
  Write agents as `.md` files (ChatMarkdown). Each file is both the prompt *and* the execution log: model config, tool permissions, tool calls/results, and the full transcript.

- **Compose unique agents via composition of tools (built-ins + your tools) and chat messages inputs via chatmd prompts**  

  You can mix:

  - **built-in tools** for common building blocks:
    - repo-safe editing: `apply_patch`
    - filesystem reads: `read_dir` (directory listing), `read_file` *(alias: `get_contents`)*
    - web ingestion: `webpage_to_markdown` (HTML → Markdown + GitHub blob fast-path)
    - local semantic search over docs: `index_markdown_docs` + `markdown_search`, and `odoc_search`
    - hybrid retrieval over code: `index_ocaml_code` + `query_vector_db`
    - vision inputs: `import_image` (bring local screenshots/diagrams into the model)
  - **custom shell tools** to wrap any command you already trust (`git`, `rg`, linters, internal CLIs…), and
  - **remote MCP tools** to import capabilities from other servers (or to export your own prompt pack as tools) like this:
  
    ```xml
    <tool mcp_server="stdio:npx -y brave-search-mcp" />
    ```

  - **agent-as-tool**: mount other `.md` files as tools inside a prompt.
  
  See [Tools – built-ins, custom helpers & MCP](docs-src/overview/tools.md).

- **Tools & capabilities (quick tour)**  
  These are the features most users care about on day 1—each with a minimal example.

  **1) Atomic repo edits with `apply_patch`**

  Declare:
  ```xml
  <tool name="apply_patch"/>
  ```
  Tool calls pass a single patch string (V4A format):
  ```text
  {
    "patch": "*** Begin Patch\n*** Update File: path/to/file\n...\n*** End Patch"
  }
  ```
  Why it’s great: you get **reviewable, multi-file, atomic** edits instead of ad-hoc mutations.

  **2) Read files safely with `read_file` (alias: `get_contents`)**

  Declare:
  ```xml
  <tool name="read_file"/>
  ```
  Notes: `read_file` refuses binary-ish content and truncates large files to keep context bounded.

  **3) Ingest web pages (and GitHub code slices) as Markdown with `webpage_to_markdown`**

  Declare:
  ```xml
  <tool name="webpage_to_markdown"/>
  ```
  Works especially well on GitHub blob URLs with line ranges, e.g.:
  - `https://github.com/owner/repo/blob/main/lib/foo.ml#L10-L80`

  Why it’s great: get clean, readable Markdown with code blocks instead of raw HTML. Much easier for the model to digest.

  **4) Prompt-as-tool: mount a `.chatmd` workflow as a callable tool**

  Declare:
  ```xml
  <tool name="triage" agent="prompts/triage.chatmd" local/>
  ```
  Why it’s great: build *small specialized agents* (triage, planner, doc-writer) and compose them.

  **5) “Docs RAG” over your project Markdown**

  Declare:
  ```xml
  <tool name="index_markdown_docs"/>
  <tool name="markdown_search"/>
  ```
  Typical flow: index once (per repo), then query in natural language to pull high-signal snippets from your docs.

  **6) Bring screenshots/diagrams into the model with `import_image`**

  Declare:
  ```xml
  <tool name="import_image"/>
  ```
  Example payload:
  ```json
  { "path": "assets/screenshot.png" }
  ```

  **7) Import tools from elsewhere via MCP**

  Declare:
  ```xml
  <tool mcp_server="https://tools.acme.dev" includes="weather,stock_ticker"/>
  ```
  Why it’s great: share tool catalogs across environments (local, container, CI) without changing prompts.

- **Build Claude Code/Codex-style agentic applications via custom “prompt packs”**  
  You can implement this as a set of specialized agents (planning agent, coding agent, test agent, doc agent…) and wire them together in an orchestration agent via agent-as-tool. The “application” is just a set of ChatMD files and you can run it via the terminal ui (`chat_tui`) or via the chat-completion CLI (`ochat chat-completion`).

- **Run the same workflows in different hosts**  
  Use `chat_tui` for interactive sessions, `ochat chat-completion` for scripts/CI/cron, and `mcp_server` to expose prompts as tools to IDEs and other hosts.

- **Ground agents in your own corpus**  
  Build indexes for docs/source trees and query them from within prompts so the agent can cite and follow project conventions rather than guessing. See [Search, indexing & code intelligence](docs-src/guide/search-and-indexing.md).

- **Continuously improve prompts**  
  Use the `mp-refine-run` binary to iteratively refine prompts and tool descriptions using evaluators, treating prompt design as a versioned, testable artifact.

---

## Example ChatMD prompts


### Example: interactive refactor agent

Turn a `.md` file into a refactoring bot that reads files and applies patches under your control.

1. Create `prompts/refactor.md`:

```xml
<config model="gpt-4o" temperature="0"/>

<tool name="read_dir"/>
<tool name="read_file"/>
<tool name="apply_patch"/>

<system>
You are a careful refactoring assistant. Work in small, reversible steps.
Before calling apply_patch, explain the change you want to make and wait for
confirmation from the user.
</system>

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
code, then generate `apply_patch` diffs and apply them, with every tool call
and patch recorded in the `.md` file.

### Example: publish a prompt as an MCP tool

Export a `.md` file as a remote tool that other MCP‑compatible clients can call.

1. Create `prompts/hello.md`:

```xml
<config model="gpt-4o" temperature="0"/>

<tool name="read_dir"/>
<tool name="read_file"/>

<system>You are a documentation assistant.</system>

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

For a more advanced, end-to-end research agent built from the same building
blocks, see the
[Discovery bot – research agent workflow](docs-src/guide/discovery-bot-workflow.md).

---

## Build from source (OCaml)

Install dependencies, build, and run tests:

```sh
opam switch create .
opam install . --deps-only

dune build
dune runtest

# Optional – build API docs when the dune-project declares a (documentation ...) stanza
dune build @doc
```

> The `@doc` alias is generated only when the project’s `dune-project` file
> contains a `(documentation ...)` stanza. If the command above fails, add the
> stanza or skip the step.

> On Apple Silicon (macOS arm64), Owl's OpenBLAS dependency can sometimes fail
> to build during `opam install`. If you see BLAS/OpenBLAS errors while
> installing dependencies or running `dune build`, see
> [Build & installation troubleshooting](docs-src/guide/build-troubleshooting.md#owl--openblas-on-apple-silicon-macos-arm64)
> for a proven workaround.

Run a quick interactive session with the terminal UI:

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

## Core concepts in one page

- **ChatMarkdown (ChatMD)**  \
  A Markdown + XML dialect that stores model config, tool declarations and the full conversation (including tool calls, reasoning traces and imported artefacts) in a single `.md` file. Because prompts are plain text files you can review, diff and refactor them like code, and the runtime guarantees that what the model sees is exactly what is in the document. See the [language reference](docs-src/overview/chatmd-language.md).

- **Tools**  \
  Functions the model can call, described by explicit JSON schemas. They can be built‑ins (e.g. `apply_patch`, `read_dir`, `read_file` *(alias: `get_contents`)*, `webpage_to_markdown`, `import_image`), shell wrappers around commands like `rg` or `git`, other ChatMD agents (prompt‑as‑tool), or remote MCP tools discovered from another server. (When embedding Ochat, you can also expose custom functions from your host application.) See [Tools – built‑ins, custom helpers & MCP](docs-src/overview/tools.md).

- **Agents & prompt‑as‑tool**  \
  Any `.md` file can be treated as an agent. Locally you can call it via `<agent>` blocks or mount it as a tool inside another prompt; remotely, `mcp_server` exposes it as a `tools/call::<name>` endpoint that IDEs or other hosts can invoke. Complex workflows become graphs of small, composable agents rather than monolithic prompts.

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

- [Real-world example session: updating the tools docs](real-world-example-session/update-tool-docs/readme.md) – a non-trivial end-to-end ochat run (full transcript + compacted version).
- [ChatMarkdown language reference](docs-src/overview/chatmd-language.md) – element tags, inline helpers, and prompt‑writing guidelines.
- [Built-in tools & custom tools](docs-src/overview/tools.md) – built‑in toolbox, shell wrappers, custom tools, and MCP tool import.
- [chat_tui guide & key bindings](docs-src/guide/chat_tui.md) – quick-start + muscle-memory cheat sheet, modes (including the ESC/cancel/quit behavior), editing + message selection workflows, quitting/export rules, sessions, context compaction, and troubleshooting.
- [`ochat chat-completion` CLI](docs-src/cli/chat-completion.md) – non‑interactive runs, flags, exit codes and ephemeral runs.
- [MCP server & protocol details](docs-src/bin/mcp_server.doc.md) – how `mcp_server` exposes prompts and tools over stdio or HTTP/SSE.
- [Search, indexing & code intelligence](docs-src/guide/search-and-indexing.md) – indexers, searchers and prompt patterns for hybrid retrieval.
- [Meta-prompting & Prompt Factory](docs-src/lib/meta_prompting.doc.md) – generators, evaluators, refinement loops and prompt packs.

OCaml integration and internals:

- [Embedding Ochat in OCaml](docs-src/lib/embedding.md) – reusing the libraries and caching patterns.
- [ChatML language & runtime](docs-src/lib/chatml/chatml_lang.doc.md) – experimental typed scripting language; see also the parser and resolver docs under `docs-src/lib/chatml/`.

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

### ChatML (experimental)

The repository ships an experimental language called *ChatML*: a small, expression‑oriented ML dialect with Hindley–Milner type inference (Algorithm W) extended with row polymorphism for records and variants.

The parser, type‑checker and runtime live under the `Chatml` modules and are documented under `docs-src/lib/chatml/` (see [`chatml_lang`](docs-src/lib/chatml/chatml_lang.doc.md), [`chatml_parser`](docs-src/lib/chatml/chatml_parser.doc.md) and [`chatml_resolver`](docs-src/lib/chatml/chatml_resolver.doc.md)). Today it is exposed primarily via the experimental `dsl_script` binary and the `Chatml_*` library modules; it is not yet wired into ChatMD prompts or the main CLIs.

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
  [`chatml_resolver`](docs-src/lib/chatml/chatml_resolver.doc.md)). Today it is
  exposed primarily via the experimental `dsl_script` binary and the
  `Chatml_*` library modules; it is not yet wired into ChatMD prompts or the
  main CLIs. The long‑term plan is to use ChatML as a safe scripting language
  that agents can write and execute via a tool call, and can be embed inside ChatMD files for small,
  deterministic pieces of logic. Since it is strongly typed with full type inference, it provides a simple way to express logic without sacrificing safety or auditability. You can provide code execution capablities with high confidence, and provide a powerful tool for agents to express complex logic 

- **Custom Ocaml functions as tools via Dune plugins**  \
  A planned direction is to expose custom OCaml
  functions as tools via [Dune plugins](https://dune.readthedocs.io/en/stable/sites.html#plugins).

All of these directions share the same goal: make agents more reliable, 
composable, and expressive **without** sacrificing the “everything is a text file” property
that makes ChatMD workflows easy to debug and version‑control.

---

## Project status – expect rapid change

Ochat is a **research-grade** project that is evolving very rapidly.  APIs,
tool schemas, file formats and even high-level design choices may change as
we explore what works and what does not. If you intend to build something on
top of Ochat, please be prepared to:

* pin a specific commit or tag,
* re-run the tests after every `git pull`, and
* embrace breaking changes as part of the fun.

Despite the experimental label, **you can build real value today** – the
repository already enables powerful custom agent workflows.  I use it daily
with custom agents for everything from developing and documentation
generation, to writing emails and automating mundane tasks.

Please budget time for occasional refactors and breaking changes.
Bug reports, feature requests, and PRs are welcome and encouraged actually – just keep in mind the ground may still be
moving beneath your feet.

---

## License

All original source code is licensed under the terms stated in `LICENSE.txt`.
