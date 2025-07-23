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

![Architecture diagram](docs/architecture.svg)

High-level description:

* Core runtime – embeddings, OAuth, MCP, search.
* Domain layer – ChatMD & ChatML.
* Front-ends – CLI, TUI, HTTP server.

## 4  CLI Tools

| Command | Purpose | Help output |
|---------|---------|-------------|
| `gpt chat-completion` | Run a chatmd session | `out/help/chat-completion.txt` |
| … | … | … |

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

