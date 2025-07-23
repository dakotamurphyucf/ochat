 # README Research & Generation – Implementation Plan

 This document tracks **how** we will research the code-base and produce a *brand-new* comprehensive `README.md` that fulfils all the user requirements.

 --------------------------------------------------------------------------------
 ## 1. Goals recapped

 1. Provide a high-level architecture overview of the whole project.
 2. Catalogue **all** libraries, modules and executables shipped in the repo.
 3. Explain how the major subsystems interact.
 4. Give special attention to the **ChatMD** language: syntax, semantics, runtime, examples and future roadmap.
 5. Collect real-world usage snippets / CLI invocations for every executable.
 6. Aggregate the above into a well-structured, fully-cross-linked `README.md`.

 > NOTE  The outcome of the *current* ticket is **not** the README itself but a *research report* that will make writing the README a mechanical task later on.

 --------------------------------------------------------------------------------
 ## 2. High-level architecture (initial draft)

 Below is a first orienting map derived from an initial scan of `dune` stanzas and existing docs. It will be refined during the research tasks.

 *Languages & DSLs*
 • **ChatMD** (Markdown-flavoured prompt language)  ↔  parsed by `lib/chatmd`  ↔  executed by ChatGPT via `chat_response`.
 • **ChatML** (embedded scripting)  ↔  front-end in `lib/chatml`, resolver & type-checker compile to an interpreter (`chatml_lang`).

 *End-user interfaces*
 • **CLI `gpt`** (`bin/main.ml`) – Swiss-army knife wrapper around all services.
 • **TUI `chat-tui`** – full-screen Notty UI for Chat sessions powered by ChatMD.
 • **`mcp_server` / `mcp_client`** – *Machine Control Protocol* JSON/STDIO bridge enabling external tools to drive chat agents.
 • Utility binaries: `md-index`, `md-search`, `odoc-index`, `odoc-search`, `dsl_script`, `key-dump`, `terminal_render`, sample HTTP clients (`eio_get`, `piaf_example`).

 *Core libraries*
 • `chat_response`, `chat_completion`, `openai`, `io` – thin wrappers over the OpenAI REST API with streaming support (Eio).
 • `vector_db`, `bm25`, `embed_service` – local semantic-search engine backed by Owl, BM25 and binary-prot persistence.
 • `markdown_indexer`, `odoc_indexer` – crawl docs and populate `vector_db`.
 • `oauth2` – self-contained OAuth2 helper stack used by MCP HTTP transport.

 *Infrastructure helpers*
 • `apply_patch`, `template`, `dune_describe`, `git`, … – developer tooling.

 The architecture can be visualised as three concentric rings:
 1. **Core runtime** – OpenAI bindings, embeddings, search, OAuth.
 2. **Domain layer** – ChatMD / ChatML languages + prompt tooling.
 3. **Front-ends** – CLI, TUI, MCP services, examples.

 --------------------------------------------------------------------------------
 ## 3. Research work-streams

 We break the work into 6 parallel but inter-dependent streams:

 A. *Inventory & Mapping* – scrape every `dune` file, build a canonical list of libraries, modules, executable names, public CLI commands and opam deps.

 B. *Code-level Documentation Mining* – reuse existing generated docs in `docs-src` and odoc/markdown indices, highlight missing areas.

 C. *Runtime Exploration* – run the main binaries with `--help` (or equivalent) and capture CLI usage; run smoke test sessions in ChatMD/TUI.

 D. *ChatMD Deep-dive* – document syntax, lexer/parser pipeline, AST, runtime execution path; prepare runnable examples.

 E. *Subsystem Walkthroughs* – vector DB, indexers, OAuth2 stack, MCP protocol.

 F. *README Skeleton* – outline sections, decide ordering, cross-link strategy and style-guide alignment.

 --------------------------------------------------------------------------------
 ## 4. Implementation steps

 1. **Environment bootstrap**
    • Ensure `opam install . --deps-only` succeeds.
    • `dune build` + `dune runtest --diff-command=diff -u` – green baseline.

 2. **Generate fresh dependency graph**
    • `dune describe` JSON → convert to mermaid diagram.
    • Cross-check against `dune-project` declared libraries.

 3. **Module catalogue**
    • Auto-extract `*.ml` & `*.mli` paths via `git ls-files`.
    • Group by library/exe; export CSV for later table importing.

 4. **Executable survey**
    • Run each public exe with `--help` or equivalent.
    • Capture output and jot down primary use-cases & flags.

 5. **ChatMD research**
    • Study `chatmd_lexer`, `chatmd_parser`, `prompt.ml`.
    • Build minimal prompt, run via `bin/main.ml` or TUI to inspect flow.
    • Document lifecycle: parsing → `chat_response` → OpenAI streaming.

 6. **ChatML research** (lighter weight – still experimental)
    • Inspect type-checker and built-ins.
    • Showcase sample program executed via `dsl_script`.

 7. **Vector DB & indexing**
    • Read `vector_db.ml`, `embed_service.ml`, `bm25.ml`.
    • Trace code-path in `markdown_indexer` & `odoc_indexer`.

 8. **OAuth2 stack**
    • Catalogue modules under `lib/oauth`.
    • Record supported grant types and flows.

 9. **MCP protocol**
    • Detail message schema (`mcp_types`) & transport options.

 10. **Draft README outline**
     • Intro, quick-start, architecture, CLI tools, ChatMD tutorial, advanced topics, contribution guide.

 11. **Populate sections with gathered material** (to be done in a later ticket).

 --------------------------------------------------------------------------------
 ## 5. TODO Table

 | Task | State | Description | Dependencies | Notes |
 |------|-------|-------------|--------------|-------|
 | Bootstrap env | pending | Install all opam deps, confirm `dune build` + tests pass. | – | Prepare reproducible dev-shell. |
 | Generate dep graph | pending | Run `dune describe`, create mermaid diagram of libs & exes. | Bootstrap env | Use `dune describe --eval` for workspace-wide view. |
 | Catalogue modules | pending | Script to list every ML(i) file grouped by library/exe. | Bootstrap env | Could reuse existing `dune describe` JSON. |
 | Executable survey | pending | Run each public binary with `--help`, record usage & examples. | Bootstrap env | Attach captured output as artefacts. |
 | ChatMD deep-dive | pending | Analyse lexer/parser, produce syntax reference & examples. | Catalogue modules | Also inspect docs-src snippets. |
 | ChatML deep-dive | pending | Summarise language, type system, built-ins, future plans. | Catalogue modules | Less critical but valuable. |
 | Vector DB & indexing notes | pending | Explain embedding flow, BM25 scoring, markdown & odoc crawlers. | Catalogue modules | May include performance numbers. |
 | OAuth2 stack notes | pending | Document grant types, storage abstractions, client/server helpers. | Catalogue modules | Cross-reference MCP HTTP transport. |
 | MCP protocol write-up | pending | Diagram of message flow, transport variants, sample JSON. | Vector DB & indexing notes |  |
 | Draft README skeleton | pending | Produce outline with placeholders for each section. | All research tasks | Align with existing style-guides. |

 --------------------------------------------------------------------------------
 ## 6. Risk & Mitigation

 *Large surface area* → strictly enforce small tasks & frequent commits.

 *Stale information* → automate extraction where possible (e.g. introspect `dune` rather than manual lists).

 *Tooling drift* → pin opam switch in `.ocaml-env` file.

 --------------------------------------------------------------------------------
 ## 7. Next action

 Move **Bootstrap env** to `in_progress` once this plan is accepted.
