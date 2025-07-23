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
 • `chat_response`, `chat_completion`, **`functions`**, **`definitions`**, `openai`, `io` – layers that transform ChatMD prompts into OpenAI REST requests, stream responses, and post-process tool calls.
 • `vector_db`, `bm25`, `embed_service` – local semantic-search engine (Owl backed dense vectors + BM25 sparse index) with bin_prot persistence and Eio streaming helpers.
 • `markdown_indexer`, `odoc_indexer`, **`package_index`**, **`webpage_markdown`** – ingestion pipelines that feed content into `vector_db`.
 • `oauth2` – self-contained OAuth2 helper stack (client credentials & PKCE, plus in-memory/lightweight server stubs) used by the MCP HTTP transport.

 *Infrastructure helpers*
 • `apply_patch`, `template`, `dune_describe`, `git`, … – developer tooling.

--------------------------------------------------------------------------------
### 2.a Tooling conventions for OCaml helpers

When writing **new OCaml automation scripts** (e.g., research utilities under
`script/`):

1. **Start with semantic search**
   • Use `markdown_search` to query the repository’s Markdown docs in natural
     language.
   • Use `odoc_search` for APIs in both *opam* libraries and in-repo modules.

   Example: `odoc_search {query = ": Eio.Switch.t"; package = "all"}` quickly
   surfaces constructors/consumers of `Eio.Switch.t` without browsing the web.

2. **Complement with Sherlodoc** when you need fuzzy type or name matching
   across *all* opam packages (`sherlodoc search "List concat"`).

3. **Always add a dune stanza** for each script so that `dune exec
   <script_name>` works in CI.  Place the file in `script/`.

4. **Prefer Base & Eio**  – follow Jane-Street conventions and default to Eio
   for non-blocking IO or subprocess management.

5. **Update the TODO table live** – mark the current task `in_progress` →
   `completed` *immediately* when the script is functional and committed.

These points are now treated as **hard requirements** for future tasks.

 The architecture can be visualised as three concentric rings:
 1. **Core runtime** – OpenAI bindings, embeddings, search, OAuth.
 2. **Domain layer** – ChatMD / ChatML languages + prompt tooling.
 3. **Front-ends** – CLI, TUI, MCP services, examples.

 --------------------------------------------------------------------------------
 ## 3. Research work-streams

 We break the work into 6 parallel but inter-dependent streams:

A. *Inventory & Mapping* – crawl every `dune` stanza, produce machine-readable catalogues of:
   • libraries (public & private),
   • modules within each library / executable,
   • public executables and their install-names,
   • opam dependency graph.

B. *Code-level Documentation Mining* – aggregate existing docs using `markdown_search` & `odoc_search`; tag uncovered areas.

C. *Runtime Exploration* – run main binaries with `--help` (or equivalent) **under Eio test harness** to avoid blocking; record usage, options, sample sessions.

D. *ChatMD Deep-dive* – reverse-engineer language spec from lexer, parser, AST + unit-tests.  Produce runnable `examples/*.chatmd` scripts.

E. *Subsystem Walkthroughs* – vector DB (dense + sparse), indexing pipelines, OAuth2 flows, MCP protocol, OpenAI streaming path.

F. *README Skeleton* – draft structure, call-outs, cross-links; always kept in sync with completed research.

 --------------------------------------------------------------------------------
## 4. Implementation steps (revised)

1. **Environment bootstrap (already provisioned)**
   The development environment is assumed to be fully configured:
   • All opam dependencies are installed.
   • `OPENAI_API_KEY` (and any other required secrets) are set.
   • `dune build` and `dune runtest` pass.
   Therefore this step is marked *completed*—no further action required.

2. **Generate fresh dependency graph**
   • Run `dune exec dep_graph_generator` (script in `script/dep_graph_generator.ml`).
   • Artefacts are placed in the `plan/` folder:
     – `deps.sexp`  raw `dune describe workspace` snapshot.
     – `deps.json`  JSON wrapper of the same data.
     – `deps.mmd`   Mermaid diagram listing every library node (visualise via mermaid.live).
     – `deps-check.log` placeholder for public-name consistency checks.
   • The OCaml script replaces the old shell+jq pipeline and conforms to repo rules (pure OCaml, only core libs).

3. **Module catalogue**
   • `git ls-files '*.ml' '*.mli'` → feed into a small OCaml script that queries `dune describe` to know which lib/exe owns each file.
   • Emit `out/modules.csv` with columns: file, module, library, public_name?, interface?, path.

4. **Executable survey**
   • Iterate over `bin/dune` `(public_name ...)` stanzas; for each generated exe run `<exe> --help || true` inside an Eio_subprocess capturing stdout/stderr.
   • Store captured help text in `out/help/<exe>.txt`.
   • Attempt minimal run examples (e.g., `odoc-index --help`, `md-search "hello"`). Store transcripts.

5. **ChatMD research**
    • Use `odoc_search` for `Chatmd_*` modules and `markdown_search` for docs.
    • Extract grammar from `chatmd_parser.mly` comments; render EBNF table in `docs/chatmd_spec.md`.
    • Build sample prompts in `examples/chatmd/{hello,tools}.chatmd`.
    • Run: `dune exec bin/main.exe -- --prompt-file examples/chatmd/hello.chatmd --dry-run` to inspect generated OpenAI JSON (no network).
    • Trace runtime pipeline: AST → Prompt.t → Chat_response request → streaming loop.

6. **ChatML research** (experimental)
    • Read `chatml_typechecker_test.ml` for coverage.
    • Compile sample `examples/chatml/fibonacci.cml` & run via `dune exec bin/dsl_script.exe -- examples/chatml/fibonacci.cml`.
    • Summarise core language features and future roadmap in `docs/chatml_overview.md`.

7. **Embedding & search layer**
    • Inspect `embed_service` for batching & caching logic; note OpenAI requests.
    • Create sequence diagram (`docs/seq_embedding.svg`) with Mermaid showing flow from crawler → embed_service → vector_db.
    • Measure indexing throughput using `time dune exec bin/md_index.exe -- examples`. Log results.

8. **OAuth2 & Authentication stack**
    • Build table of flows (Client Credentials, PKCE) with involved modules (`oauth2_manager`, `oauth2_pkce_flow`).
    • Produce `docs/oauth2_overview.md` outlining how the minimal server stub is composed.

9. **MCP protocol**
    • Summarise JSON schemas from `mcp_types.ml` using `ppx_jsonaf_conv` generated conv functions.
    • Record example session (`test/mcp_server_integration_test.ml`).

 10. **Draft README outline**
     • Intro, quick-start, architecture, CLI tools, ChatMD tutorial, advanced topics, contribution guide.

 11. **Populate sections with gathered material** (to be done in a later ticket).

12. **Continuous validation**
    • After each major research task, update the README skeleton to ensure coverage (prevent last-minute surprises).

 --------------------------------------------------------------------------------
 ## 5. Risk & Mitigation

 *Large surface area* → strictly enforce small tasks & frequent commits.

 *Stale information* → automate extraction where possible (e.g. introspect `dune` rather than manual lists).

--------------------------------------------------------------------------------

 ## 6. TODO Table

 | Task | State | Description | Dependencies | Notes |
 |------|-------|-------------|--------------|-------|
 | Bootstrap env | completed | Environment already set-up and verified (`dune runtest` green). | – |  |
| Generate dep graph | completed | Outputs stored under `plan/` (`deps.sexp`, `deps.json`, `deps.mmd`, `deps-check.log`) generated by OCaml executable `dep_graph_generator`. | Environment ready | Run `dune exec dep_graph_generator` to refresh. |
| Catalogue modules | completed | Script generated `out/modules.csv` via new `module_catalogue` executable. | Environment ready | Run `dune exec module_catalogue` to refresh. |
| Documentation mining | completed | Aggregated docs using new `doc_mining` script – outputs in `out/mined_docs/`; missing docs list generated. | Catalogue modules | Run `dune exec doc_mining` to refresh. |
|  └─ Doc coverage report | completed | Implement `script/doc_coverage.ml` to generate `out/docs_coverage.csv` listing modules with/without docs. | Documentation mining | Scans `out/modules.csv` & `docs-src`. |
| Executable survey | completed | Run each public binary with `--help`, record usage & examples. | Environment ready | Artefacts committed. |
|  ├─ List binaries | completed | Enumerate all (public_name ...) stanzas under `bin/` and similar. Produce `out/binaries.csv`. | Executable survey | |
|  ├─ Implement `script/exe_survey.ml` | completed | OCaml script `script/exe_survey.ml` captures `--help` output for each binary into `out/help/`, utilises Eio.Process. | List binaries | `dune exec exe_survey` to refresh. |
|  ├─ Capture minimal run examples | completed | Extend script to run sample invocations for selected binaries (`md-search "hello"`, etc.). Logs in `out/examples/<exe>.txt`. | Implement `script/exe_survey.ml` | |
|  ├─ Commit artefacts | completed | Stage and commit generated help and example transcripts. | Capture minimal run examples | |
|  └─ Close task | completed | Review outputs, update TODO statuses (`completed`). | Commit artefacts | |
| ChatMD spec (EBNF) | completed | Extract grammar from `chatmd_parser.mly`, render to markdown & embed diagrams. | ChatMD deep-dive | Use Menhir `--list-errors` to help. |
|  ├─ Extract grammar rules | completed | Review `chatmd_parser.mly` & transpose to concise EBNF notation. | ChatMD spec (EBNF) |  |
|  ├─ Write markdown section | completed | Replace section 6 of `docs/chatmd_syntax_reference.md` with formal EBNF & description. | Extract grammar rules |  |
|  ├─ Add Mermaid diagram | completed | Embed a simple state diagram illustrating element nesting. | Write markdown section |  |
|  └─ Update last-updated timestamp | completed | Insert current date in markdown footer. | Write markdown section |  |
| Embedding flow diagram | completed | Create Mermaid sequence diagram for embedding pipeline. | Vector DB & indexing notes |  |
| OAuth2 overview doc | completed | Write `docs/oauth2_overview.md` summarising flows. | OAuth2 stack notes |  |
| ChatMD deep-dive | completed | Analyse lexer/parser, produce syntax reference & examples. | Catalogue modules | Also inspect docs-src snippets. |
|  ├─ Inspect lexer & parser | completed | Reviewed chatmd_lexer.mll & chatmd_parser.mly, wrote summary in `docs/chatmd_lexer_parser_notes.md`. | ChatMD deep-dive |  |
|  ├─ Draft syntax reference | completed | Draft syntax reference outline in `docs/chatmd_syntax_reference.md`. | Inspect lexer & parser |  |
|  ├─ Create sample prompts | completed | Sample prompts added under `examples/chatmd/`. | Inspect lexer & parser |  |
|  ├─ Capture dry-run JSON | completed | Generated JSON using `chatmd_to_json` script; saved to `out/chatmd/hello.json`. | Create sample prompts |  |
|  └─ Pipeline trace notes | completed | Document AST-to-OpenAI request pipeline in docs/chatmd_pipeline.md. | Capture dry-run JSON |  |
| ChatML deep-dive | completed | Summarise language, type system, built-ins, future plans. | Catalogue modules | Less critical but valuable. |
| Vector DB & indexing notes | completed | Explain embedding flow, BM25 scoring, markdown & odoc crawlers. | Catalogue modules | Completed in docs/vector_db_indexing.md with throughput table. |
| OAuth2 stack notes | completed | Document grant types, storage abstractions, client/server helpers. | Catalogue modules | Cross-reference MCP HTTP transport. |
| MCP protocol write-up | completed | Diagram of message flow, transport variants, sample JSON. | Vector DB & indexing notes |  |
| Draft README skeleton | completed | Produce outline with placeholders for each section. | All research tasks | Align with existing style-guides. |
| Continuous validation | completed | Keep README skeleton up-to-date as tasks complete. Validation script `readme_validation` added. | Draft README skeleton | Prevent scope creep. |
| Populate README – Introduction & Quick-start | completed | Fill sections 1 & 2 of the README using gathered research artefacts. | Draft README skeleton | Ensure commands verified via `dune exec`. |
| Populate README – Architecture & CLI tools | pending | Complete sections 3 & 4 incl. architecture diagram link and auto-generated CLI table. | Populate README – Introduction & Quick-start | Use `out/help/*.txt` for table content. |
| Populate README – Domain languages | pending | Finalise ChatMD (sec 5) and ChatML (sec 6) sections with links to detailed docs and examples. | Populate README – Architecture & CLI tools |  |
| Populate README – Subsystems | pending | Write embedding/search, OAuth2, MCP sections (7-9) referencing dedicated docs. | Populate README – Domain languages |  |
| Populate README – Contributing & polish | pending | Fill contributing guidelines, badges, license section and perform final proofreading. | Populate README – Subsystems | Run `readme_validation` script. |

> Follow the Task States & Management rules when updating this table during implementation.
