# Project README – Skeleton (Auto-generated)

> _This file is a **template** produced by the research tasks.  Replace
> the `TODO:` markers with the final content when assembling the public
> README._

---

## 0  Badge strip

TODO: CI / coverage / version badges

---

## 1  Introduction

**ChatGPT Toolkit** is a comprehensive OCaml framework that allows you to **script, automate and embed ChatGPT-class agents** inside your own applications.  It ships with:

* **Domain-specific languages** –  ✨  *ChatMD* for Markdown-flavoured prompt flows and 🧩  *ChatML* for embedded scripting and control-flow.
* **Turn-key binaries** – a one-stop `gpt` CLI, a full-screen `chat-tui`, HTTP & STDIO bridges (MCP) and an extensible set of indexers and search helpers.
* **Self-contained infrastructure** – local vector database (dense + BM25), incremental indexers for Markdown / odoc / web pages, an OAuth2 helper stack and resilient OpenAI streaming bindings.

The project was born to scratch an internal itch: **drastically lower the barrier to experiment with prompt-engineering, agent orchestration and semantic search** from the comfort of OCaml.  It targets power-users who want reproducible chat sessions, rich tool-calling and first-class integration with the OCaml build ecosystem.

Typical use-cases include:

* Automating code-reviews or refactors via ChatMD scripts checked into the repo.
* Building knowledge-base chatbots that run fully offline by leveraging the vector DB + BM25 hybrid search.
* Driving ChatGPT from CI pipelines (quality gates, changelog generation, release notes, …).
* Hacking interactive tools and demos without leaving the OCaml universe.

## 2  Quick-start

```sh
# 0.  Prerequisites — OCaml ≥ 5.1  +  latest opam

# 1.  Clone & initialise the local switch (≈ first-time only)
git clone https://github.com/your-org/chatgpt-toolkit.git && cd chatgpt-toolkit
opam switch create .  # picks up the repo-local `chatgpt.opam`

# 2.  Build everything
dune build

# 3.  Run the CLI – you should see the global help
dune exec gpt -- --help

# 4.  (Optional) run the text-UI
dune exec chat-tui

# 5.  (Optional) index the docs folder and perform a search
dune exec md-index -- docs
dune exec md-search -- "vector db"
```

The project defaults to **offline / dry-run mode** when the `OPENAI_API_KEY` environment variable is missing, so you can safely explore the commands without network access.  Export the variable when you are ready to talk to the OpenAI API:

```sh
export OPENAI_API_KEY="sk-..."
```

For a guided tour run:

```sh
# reproduces the examples used in the docs
make tour
```

## 3  Architecture overview

> *If you prefer pictures:* see `docs/architecture.mmd` for an interactive
> Mermaid diagram or the pre-rendered `docs/architecture.svg`.

```
graph LR
  subgraph Front-ends
    CLI[gpt / chat-tui]
    HTTP[MCP HTTP server]
  end

  subgraph Domain-layer
    ChatMD[ChatMD parser & runtime]
    ChatML[ChatML interpreter]
  end

  subgraph Core Runtime
    OpenAI[OpenAI bindings]
    Search[(Vector DB + BM25)]
    OAuth[OAuth2 stack]
  end

  CLI --> ChatMD
  HTTP --> ChatMD
  ChatMD -->|tool calls| Search
  ChatMD -->|stream| OpenAI
  ChatML --> ChatMD
  OpenAI -.-> OAuth
```

At a glance the system is organised in three concentric rings:

1. **Core runtime** – handles external side-effects: OpenAI REST calls, dense & sparse search, token caching and OAuth2 credentials.
2. **Domain layer** – defines the prompt DSLs (*ChatMD*, *ChatML*) and their dialogue/runtime helpers.
3. **Front-ends** – expose the features via command-line, terminal UI and HTTP/MCP transports.

Each layer only depends *inwards* which keeps the build graph simple and allows
you to embed just the lower rings inside your own code when you don’t need the
full CLI.

## 4  CLI Tools

| Command | Purpose | Help output |
|---------|---------|-------------|
| `gpt` | Swiss-army knife CLI (~10 sub-commands) for chat completions, embedding index management and misc utilities. | [`out/help/gpt.txt`](../../out/help/gpt.txt) |
| `chat-tui` | Full-screen TUI client for ChatMD conversations. | [`out/help/chat-tui.txt`](../../out/help/chat-tui.txt) |
| `md-index` | Crawl Markdown trees and build a vector/BM25 index. | [`out/help/md-index.txt`](../../out/help/md-index.txt) |
| `md-search` | Query one or more Markdown indices. | [`out/help/md-search.txt`](../../out/help/md-search.txt) |
| `odoc-index` | Index compiled `.odoc` documentation. | [`out/help/odoc-index.txt`](../../out/help/odoc-index.txt) |
| `odoc-search` | Semantic search over odoc indices. | [`out/help/odoc-search.txt`](../../out/help/odoc-search.txt) |
| `mcp_server` | Machine-Control-Protocol JSON/STDIO bridge (HTTP & pipes). | [`out/help/mcp_server.txt`](../../out/help/mcp_server.txt) |
| `dsl_script` | Execute ChatML `.cml` scripts from disk or stdin. | [`out/help/dsl_script.txt`](../../out/help/dsl_script.txt) |
| `key-dump` | Inspect OpenAI API key & model quotas. | [`out/help/key-dump.txt`](../../out/help/key-dump.txt) |
| `terminal_render` | Render markdown+ANSI blobs to a terminal buffer (demo). | [`out/help/terminal_render.txt`](../../out/help/terminal_render.txt) |

## 5  ChatMD language

See `docs/chatmd_syntax_reference.md` and examples under
`examples/chatmd/`.

## 6  ChatML language (experimental)

See `docs/chatml_overview.md`.

## 7  Embedding & search stack

Flow diagram: `docs/seq_embedding.md`  
Performance numbers: `docs/vector_db_indexing.md`

## 8  OAuth2 helper stack

Overview: `docs/oauth2_overview.md`

## 9  MCP protocol

Overview: `docs/mcp_protocol.md`

## 10  Contributing

TODO: coding style, how to run tests, generate docs.

## 11  License

MIT – see `LICENSE.txt`.

---

*Last updated: 2025-07-23*

