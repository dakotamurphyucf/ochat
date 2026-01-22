*Ochat — A File-Native Agent/Workflow Runtime for Auditable, Composable LLM Systems*

**Author’s note on scope:** This paper describes Ochat as it exists today *and* clearly marks roadmap/proposed features. All citations point to primary docs in the repo or the referenced upstream projects/specs.

---

### Abstract
Ochat is an OCaml-based **agent/workflow runtime** where the unit of composition is a **plain-text file** written in a Markdown + XML dialect called **ChatMarkdown (ChatMD)**. A ChatMD file is simultaneously:

1) the **program** (model configuration, tool permissions, policy/instructions), and  
2) the **execution artifact** (full transcript including tool calls and tool outputs).

This single choice—treating “prompt-as-program” and “transcript-as-artifact” as the core primitive—creates leverage in **auditability, reproducibility, portability, composition, and long-horizon state management** compared to many agent frameworks and consumer “agent apps”. Ochat ships multiple execution “hosts” (TUI, batch CLI, MCP server), an explicit allowlisted tool system, local indexing/retrieval building blocks, a meta-prompting/refinement library, and an experimental typed DSL (ChatML) that signals a path toward **safe embedded scripting**.  
Primary source: Ochat README and docs (https://github.com/dakotamurphyucf/ochat, plus the doc links cited throughout).

---

### 1. Problem: Agent workflows are hard to trust, hard to reproduce, and hard to evolve
Most LLM agent systems break down on three practical dimensions:

1) **Hidden truth:** critical behavior is split across framework code, runtime logs, UI-only state, vendor defaults, and implicit tool policies.  
2) **Poor reproducibility:** it’s difficult to answer “what exactly did the agent do?” in a way that’s diffable and reviewable.  
3) **State collapse over time:** long-running tasks exceed context windows, lose intent, repeat work, and forget why decisions were made.

Ochat targets these failures by making the workflow and its execution trace first-class, textual, and explicit.

---

### 2. Ochat’s central primitive: *a ChatMD file is both the workflow definition and the execution log*
Ochat’s differentiator isn’t “more integrations than framework X.” It’s that Ochat chooses **different primitives**.

A **single ChatMD file** contains (at minimum):

- model configuration (`<config .../>`)
- explicit tool allowlist (`<tool .../>`)
- role-tagged transcript (`<system>`, `<developer>`, `<user>`, `<assistant>`)
- tool call trace (`<tool_call ...>`, `<tool_response ...>`)
- optional inline/imported artifacts (`<doc .../>`, `<img .../>`, `<import .../>`, `<agent ...>...</agent>`)

This is documented in the ChatMD language reference (https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/overview/chatmd-language.md) and summarized in the README (https://github.com/dakotamurphyucf/ochat/blob/main/README.md).

#### 2.1 Consequence: “Where is the truth?” becomes a solved problem
When the program and log are unified into a single structured file:

- **PR review becomes possible** for agent behavior: diffs show prompt changes *and* tool usage and outputs.
- Work can be **forked/branched** by copying/exporting the file or session, then resuming from that artifact (Ochat README + `chat_tui` session/export concepts; `chat_tui` guide: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/guide/chat_tui.md).
- Workflows become **composable** because a ChatMD prompt can be mounted as a tool (“agent-as-tool”) in another ChatMD workflow (tools doc: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/overview/tools.md).

---

### 3. ChatMD: a constrained, closed vocabulary that trades “magic” for reliability
ChatMD is intentionally **not general XML**. It is a *closed set* of tags embedded in Markdown with strict parsing rules (ChatMD reference: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/overview/chatmd-language.md):

- Top-level must be recognized ChatMD elements (no free text at the top level).
- RAW blocks exist as an escape hatch for literal JSON/code: `RAW| ... |RAW`.
- Some deterministic transforms exist (e.g., stripping HTML comments; `<import/>` expansion under defined circumstances).

This “closed vocabulary” is a major design choice: it reduces ambiguity, avoids “best effort parsing,” and keeps workflows human-reviewable while still being machine-checked.

#### 3.1 Tool payload persistence: readability *and* auditability
ChatMD supports persisting tool payloads either:
- inline (if configured), or
- externalized to `./.chatmd/*.json` to keep transcripts readable,

as described in the tool call persistence section of the ChatMD reference (https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/overview/chatmd-language.md).

---

### 4. The tool system: explicit allowlisting and multiple extension modes
Ochat treats tools as **opt-in**. The model can call only tools declared via `<tool .../>` (tools overview: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/overview/tools.md).

#### 4.1 Built-in tools (selected highlights)
From the tools catalog (same doc):

- `apply_patch` (structured atomic multi-file edits)
- file ops: `read_dir` / `read_directory`, `read_file` / `get_contents`
- `webpage_to_markdown` (includes a GitHub blob fast path)
- retrieval/indexing:
  - `index_markdown_docs` + `markdown_search`
  - `index_ocaml_code` + `query_vector_db`
  - `odoc_search`
- `meta_refine` (prompt refinement)
- `import_image` (vision ingestion)

Source: tools overview (https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/overview/tools.md).

#### 4.2 Extension mechanisms: shell, agent-backed, and MCP
Ochat extends tools in three practical ways (tools overview: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/overview/tools.md):

1) **Shell wrappers** (`<tool name="rg" command="rg" .../>`)  
2) **Agent-backed tools** (`<tool name="triage" agent="prompts/triage.chatmd" local/>`)  
3) **MCP tools** (mount remote tool catalogs via MCP)

This yields a “least authority” workflow style: you give the agent exactly the tools needed for the job, no more.

---

### 5. Multiple “hosts” run the same workflow artifact (ChatMD)
Ochat ships three primary execution hosts (README: https://github.com/dakotamurphyucf/ochat/blob/main/README.md):

1) **`chat_tui`** — interactive terminal UI  
   - streaming output + tool traces
   - modal UX (Insert/Normal/Cmdline)
   - message selection, edit/resubmit
   - context compaction (`:compact`)
   - persistent sessions under `~/.ochat/sessions/<id>/...`  
   Sources:
   - `chat_tui` guide: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/guide/chat_tui.md  
   - CLI reference: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/bin/chat_tui.doc.md

2) **`ochat chat-completion`** — non-interactive CLI (scripts/CI)  
   - `-prompt-file` prepended once
   - `-output-file` appended transcript (or `/dev/stdout`)  
   Source: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/cli/chat-completion.md

3) **`mcp_server`** — expose prompts/tools over MCP (stdio or HTTP/SSE)  
   - registers selected built-in tools (echo/apply_patch/read_dir/get_contents/webpage_to_markdown/meta_refine)
   - scans a prompts directory and registers each `*.chatmd` as a prompt *and* tool
   - supports stdio JSON-RPC or HTTP/SSE; HTTP mode enforces OAuth bearer tokens (per docs)  
   Source: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/bin/mcp_server.doc.md

#### 5.1 Why this matters: portability of workflows across contexts
If the artifact is stable (ChatMD), you can move between:
- interactive development (TUI),
- automated runs (CI),
- remote invocation by IDEs/apps (MCP),

without rewriting the workflow in a framework-specific API.

---

### 6. Local grounding and retrieval: indexing/search as first-class building blocks
Ochat includes local indexing utilities so agents can retrieve *snippets* rather than stuffing entire corpora into context (search/indexing guide: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/guide/search-and-indexing.md):

- Markdown semantic search: `md-index` / `md-search` and tools `index_markdown_docs` / `markdown_search`
- OCaml source indexing + hybrid retrieval (dense shortlist + BM25 rerank): `ochat index` / `ochat query` and tools `index_ocaml_code` / `query_vector_db`
- odoc HTML semantic search: `odoc-index` / `odoc-search` and tool `odoc_search`

Notable operational details (same guide):
- hybrid retrieval design (dense shortlist + BM25 rerank)
- beta differences between CLI vs tool can change rankings
- OpenAI embeddings required (`OPENAI_API_KEY`)

---

### 7. Meta-prompting: prompt refinement as a library/tool, not a manual ritual
Ochat ships a Meta_prompting library + CLI + `meta_refine` tool to:
- generate prompts from typed task records,
- iteratively refine prompts using evaluator/judge strategies,
- optionally retrieve context from vector DBs during refinement.  
Source: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/lib/meta_prompting.doc.md

This is significant because it frames “prompt engineering” as an iterative, testable workflow—closer to how we treat software artifacts.

---

### 8. Long-horizon reliability: compaction as a first-class operation with rolling summaries
Long tasks fail because **context management fails**. Ochat explicitly supports compaction in `chat_tui`:

- `:compact` (aliases `:c`, `:cmp`) triggers summarization to stay within budget.
- Crucially, Ochat keeps prior compactions: it preserves system/developer messages and any previous `<system-reminder>` blocks, then appends a new `<system-reminder>...</system-reminder>` summary.  
Source: `chat_tui` guide section on compaction: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/guide/chat_tui.md

#### 8.1 Why rolling `<system-reminder>` retention is a force multiplier
Most systems treat summarization as destructive compression. Ochat’s retention semantics enable a **rolling ledger of compaction epochs**, which better preserves:
- temporal continuity (“what we tried and why”),
- anchor details (file paths, errors, decisions),
- drift detection (“we already tried that two summaries ago”).

Your review also notes that Ochat’s summarizer prompt is visible in the source (https://github.com/dakotamurphyucf/ochat/blob/dcef230d670df2225a3098adc1fe99a723293ece/lib/context_compaction/summarizer.ml#L7), reinforcing the “no hidden state” ethos: even compaction policy can be audited and improved like a code artifact.

---

### 9. Roadmap power multipliers: declarative control flow + session state + typed scripting
Ochat becomes more than “a good CLI” if its roadmap lands, because the file-based paradigm can absorb higher-level orchestration without jumping into an external framework.

#### 9.1 Declarative control flow and policy (design note)
The repo contains a design note proposing `<rules>` and `<policy>` blocks with an event → guard → action structure (not implemented yet):  
Source: https://github.com/dakotamurphyucf/ochat/blob/main/control-flow-chatmd.md

If implemented, this could enable:
- explicit tool guardrails (“never call tool X unless condition Y”),
- event-driven remediation (“on tool failure, insert step Z”),
- automated compaction triggers, quotas, cooldowns,
- all within ChatMD, staying transcript-visible and diffable.

#### 9.2 Session state as first-class durable memory (roadmap)
Ochat already has persistent sessions in `chat_tui` (session directory described in `chat_tui` guide: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/guide/chat_tui.md).

The README describes a future direction for **session-scoped state**:
- key/value store,
- session filesystem read/write,
- isolation by default,
- inheritance across tool-called agents,
- intended backing store: **Irmin**.  
Source: README “Future directions”: https://github.com/dakotamurphyucf/ochat/blob/main/README.md

This addresses a core weakness of chat-based agents: “conversation as state” is fragile. With durable session state:
- narrative continuity can live in rolling summaries, while
- operational truth (tasks, artifacts, failures, decisions) lives in structured storage.

#### 9.3 ChatML (experimental): a path to safe embedded computation
Ochat includes an experimental typed DSL called ChatML:
- Hindley–Milner type inference (Algorithm W) + row polymorphism (per your notes/README),
- modules for parsing/resolving/typechecking/evaluation,
- a demo binary `dsl_script` (hard-coded program).  
Sources:
- ChatML doc: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/lib/chatml/chatml_lang.doc.md  
- demo binary doc: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/bin/dsl_script.doc.md

It is not yet wired into ChatMD prompts (as noted in your review), but strategically it signals a direction: instead of “give the model bash,” offer a **typed, auditable, constrained execution substrate**.

---

### 10. Why OCaml is a strategic fit (without tying the format to OCaml)
Your review makes two key points that are worth separating:

1) **OCaml is a good implementation language** for a strict, safe runtime:
   - strong types for protocol/tool/parsing invariants,
   - good performance for indexing/TUI responsiveness,
   - OCaml 5 concurrency suitable for I/O-heavy runtimes.  
   Dependency signals appear in `ochat.opam` / `dune-project` (source links from your review: https://github.com/dakotamurphyucf/ochat/blob/main/ochat.opam and https://github.com/dakotamurphyucf/ochat/blob/main/dune-project).

2) **ChatMD is language-agnostic**:
   - workflows are plain text with a closed syntax,
   - alternative runtimes could be built in other languages without changing workflow artifacts.

This mirrors a “bytecode + runtimes” idea: stable IR + multiple executors.

---

### 11. Comparative positioning (precise category differences)
Ochat sits in a different category than many things people compare it to. It overlaps, but the core abstraction differs.

#### 11.1 Versus agent frameworks (LangChain / LlamaIndex / Haystack)
- LangChain: https://github.com/langchain-ai/langchain  
- LlamaIndex: https://github.com/run-llama/llama_index  
- Haystack: https://github.com/deepset-ai/haystack  

Frameworks model workflows primarily as code graphs/objects/pipelines. Ochat models workflows primarily as **inspectable file artifacts** that embed both policy and logs. In exchange, Ochat has a smaller integration surface today, but lower “magic,” higher auditability, and a tighter loop for prompt-pack evolution.

#### 11.2 Versus agentic coding apps (Claude Code / Aider)
- Claude Code repo: https://github.com/anthropics/claude-code  
- Claude Code practices article: https://www.anthropic.com/engineering/claude-code-best-practices  
- Aider: https://github.com/Aider-AI/aider  

Claude Code/Aider are end-user products with strong UX tuned for coding tasks. Ochat is closer to a **toolkit/runtime** for building many specialized agents as version-controlled prompt packs, with multiple hosts (TUI/CLI/MCP). You can approximate “Claude Code-like” behavior by composing specialized ChatMD agents under constrained tools, but Ochat’s emphasis is on artifacts and composability rather than a single monolithic UX.

#### 11.3 Versus MCP-first ecosystems
- MCP spec: https://modelcontextprotocol.io/specification/2025-06-18  
- MCP repo: https://github.com/modelcontextprotocol/modelcontextprotocol  

Ochat can **consume** MCP tools and also **serve** tools/prompts via its own `mcp_server` (doc: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/bin/mcp_server.doc.md). This positions Ochat well as a “prompt-pack server” in an MCP world.

#### 11.4 Versus OCaml OpenAI clients
Examples:
- https://github.com/XFFS/oopenai  
- https://github.com/Nymphium/openai-ocaml  

These are API bindings; Ochat is a full workflow runtime with prompt representation, tool calling, indexing, TUI, MCP server, etc.

---

### 12. Practical adoption: what Ochat is best for (and what it is not yet)
#### Best-fit today (from your review)
1) **Version-controlled agent workflows (“prompt packs”)**  
2) **Terminal-first agentic work** (auditable file-based operations; patch/review loops)  
3) **Composable multi-agent orchestration** (agent-as-tool)  
4) **Local-first grounding** (docs/code/odoc indexing)  
5) **Exporting workflows to IDEs/apps via MCP**  

#### Less ideal today
- Multi-provider support: currently **OpenAI only** for chat + embeddings (README per your review: https://github.com/dakotamurphyucf/ochat/blob/main/README.md).
- “Enterprise platform” concerns (hosted observability, dashboards, managed deployment patterns): Ochat is closer to a powerful toolkit than a turnkey platform.
- Research-grade warning: expect breaking changes; pin commits (README).

---

### 13. Installation and build reality (so adopters don’t bounce)
- OCaml >= 5.1.0 (metadata via opam/dune, cited in your review)
- Standard dune/opam build flow shown in README: https://github.com/dakotamurphyucf/ochat/blob/main/README.md
- Apple Silicon/OpenBLAS workaround for Owl is documented: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/guide/build-troubleshooting.md
- License: MIT (https://github.com/dakotamurphyucf/ochat/blob/main/LICENSE.txt)

---

### 14. The core thesis: why Ochat can be “more powerful” over time
Ochat’s “power advantage” is structural, not just a snapshot of features:

1) **Workflows are stable, diffable artifacts** (ChatMD) rather than scattered state.  
2) **Tool governance is explicit** (allowlist + persisted traces).  
3) **Composition is first-class** (agent-as-tool + MCP import/export).  
4) **Long-horizon operation is treated seriously** (compaction with rolling retention).  
5) **Roadmap aims at reliability primitives** (policy/rules, session state, typed scripting) without abandoning the file-native model.

This combination makes Ochat a plausible substrate for building *families* of specialized agents that evolve like software: reviewed, versioned, regression-tested, and shared.

---

### A) Compaction & Summarization (expanded, more comprehensive)

#### A.1 Compaction is a first-class workflow operation (not a hidden UI feature)
Ochat’s `chat_tui` includes built-in context compaction via `:compact` (aliases `:c`, `:cmp`). Compaction is explicitly treated as an operational step in long-running work, not a behind-the-scenes trick: it is invoked intentionally when a conversation grows large and must be reduced to stay within context limits.  
Source: `chat_tui` guide (“Context compaction (`:compact`) — current behavior”)  
https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/guide/chat_tui.md

This matters because long-horizon agent workflows fail disproportionately due to *state management collapse*: overflowing context windows cause the agent to lose intent, repeat work, or forget why decisions were made. By making compaction an explicit, visible action in the primary interactive host, Ochat acknowledges this as a core runtime concern.

#### A.2 The key semantic: rolling retention via `<system-reminder>` blocks
Ochat’s most strategically important compaction detail is its retention behavior:

- The compactor keeps **system and developer messages** from the existing history,
- it also keeps **any previous `<system-reminder>` blocks**,
- and then it appends a **new summary inside a `<system-reminder>...</system-reminder>` block**.  
Source: `chat_tui` guide  
https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/guide/chat_tui.md

This yields a *rolling summary ledger* instead of a single overwritten recap. Over time, the conversation accumulates a sequence of compaction epochs that preserve the “story of work” in a time-indexed way.

**Why this is a force multiplier:** most summarization systems optimize for human readability (“short recap”) and overwrite previous summaries. Ochat’s rolling retention is more compatible with agent operability because it:
- preserves temporal continuity (what was tried, what failed, what changed),
- reduces drift (“we already attempted that two compactions ago”),
- provides anchors for later retrieval/tool use (files, errors, commands, decisions).

#### A.3 Compaction output is stored *in the transcript* (auditable, diffable, improvable)
Compaction results are written into the same ChatMD transcript as a structured `<system-reminder>` block, rather than being hidden inside opaque runtime state.  
Source: `chat_tui` guide  
https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/guide/chat_tui.md

This aligns with Ochat’s “transcript-as-artifact” philosophy: key runtime transformations remain inspectable. It also means compaction can be iterated on like any other workflow artifact (reviewed in PRs, compared across runs, refined over time).

Additionally, your review notes that the compaction behavior is driven by a visible summarizer prompt in the codebase—i.e., there is an inspectable “policy” for how summarization is done, reinforcing that it is not magic.  
Source: summarizer prompt in repo  
https://github.com/dakotamurphyucf/ochat/blob/dcef230d670df2225a3098adc1fe99a723293ece/lib/context_compaction/summarizer.ml#L7

#### A.4 A compaction *format* designed for reconstruction (not just summarization)
Your review proposes a structured compaction format that is explicitly aimed at future reconstruction—i.e., a future model should be able to recover crucial operational detail with tools even if raw history is gone. In this view, compaction should produce:

**Part A: structured chronological analysis**
- message-by-message / section-by-section narrative
- extracted signals: decisions, intent changes, watch-outs
- concrete anchors: files touched, errors encountered, commands run, unresolved issues

**Part B: structured summary for future reconstruction**
- preserve identifiers: filenames/paths, symbols, error strings, commands, artifacts
- clear solved vs unsolved lists
- a chronological one-sentence list of all user/assistant messages
- current work + optional next steps

**Rolling retention across compactions**
- keep prior compaction summaries (which Ochat already does)
- so the agent retains an evolving, time-indexed narrative across long tasks  
Source for rolling retention semantics: `chat_tui` guide  
https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/guide/chat_tui.md

The key whitepaper claim here (grounded in the above semantics) is: **Ochat’s compaction mechanism is unusually compatible with long-running, tool-using agent loops** because the runtime preserves a durable narrative spine (rolling `<system-reminder>` blocks) that later turns can build upon and use as an index for targeted retrieval.

---

### B) ChatML (expanded: what it is, what it enables, what it is not yet)

#### B.1 What ChatML is in Ochat today
The repository includes an experimental typed scripting language called **ChatML**. It is described as using:
- Hindley–Milner type inference (Algorithm W)
- row polymorphism
- a pipeline of parsing → resolving → typechecking → evaluation  
Source: ChatML interpreter core documentation  
https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/lib/chatml/chatml_lang.doc.md

There is also a demo/smoke-test binary, `dsl_script`, that runs a hard-coded ChatML program.  
Source: demo binary documentation  
https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/bin/dsl_script.doc.md

Crucially, per your review: **ChatML is not yet wired into ChatMD prompts**; it exists as an experimental subsystem / future direction (i.e., do not claim it is a core user-facing workflow primitive today).

#### B.2 Why ChatML matters strategically: safe embedded computation
ChatML’s significance is not “a new language for its own sake,” but its role as a plausible path to **safe scripting inside agent workflows**.

Many agent systems end up choosing between:
- brittle prompt-only logic (hard to validate), or
- unrestricted shell execution (powerful but unsafe / difficult to govern).

A typed DSL suggests a third option:
- embed deterministic computations (validation, parsing, transformation, simple orchestration) inside workflows,
- with *strong static guarantees* (type inference reduces author burden while still enforcing invariants),
- and without giving the model “arbitrary code execution.”

This dovetails with Ochat’s broader emphasis on explicitness, auditability, and governance: a typed scripting substrate can be treated as another inspectable artifact rather than hidden runtime glue.

#### B.3 How ChatML fits the whitepaper thesis
ChatML strengthens the “workflow-as-file” thesis by offering a credible route to:
- richer behavior without migrating orchestration into opaque framework code,
- safer internal logic without relying on `bash`,
- and more predictable agent behavior as workflows become more complex.

Even as an experimental module, it signals that Ochat is thinking in “runtime primitives” (parsing, types, evaluation) rather than only “prompt UX.”

---

### C) Why OCaml (expanded: technical advantages tied to Ochat’s goals)

#### C.1 The fit: strict parsing + protocol correctness + concurrency
Ochat is built around strict, structured artifacts (ChatMD) and protocol surfaces (tool calling, MCP server). These are domains where static typing meaningfully reduces failure modes:

- **ChatMD’s closed vocabulary and strict rules** benefit from an implementation that can make parsing/validation airtight.  
  Source: ChatMD language reference  
  https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/overview/chatmd-language.md

- **MCP serving and transport concerns** (stdio JSON-RPC, HTTP/SSE, auth) benefit from correctness-oriented engineering.  
  Source: MCP server documentation  
  https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/bin/mcp_server.doc.md

#### C.2 Performance and system-level ergonomics for indexing + TUI
Ochat includes local indexing and hybrid retrieval (dense shortlist + BM25 rerank) for code and docs, as well as an interactive terminal UI. These workloads benefit from a compiled runtime with predictable performance characteristics:

- Indexing/search building blocks (markdown, code, odoc):  
  https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/guide/search-and-indexing.md

- Interactive TUI host:  
  https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/guide/chat_tui.md

Your review also highlights that the dependency stack reflects this “systems runtime” orientation (Eio-based networking/concurrency, Notty UI, etc.).  
Sources:
- opam file: https://github.com/dakotamurphyucf/ochat/blob/main/ochat.opam  
- dune project: https://github.com/dakotamurphyucf/ochat/blob/main/dune-project

#### C.3 OCaml 5 concurrency matches tool-heavy agent runtimes
Agent runtimes are I/O-bound: model calls, filesystem operations, web ingestion, indexing queries, tool execution. Ochat’s runtime is designed for these environments (as suggested by its Eio-oriented stack in opam/dune metadata).  
Sources:
- https://github.com/dakotamurphyucf/ochat/blob/main/ochat.opam  
- https://github.com/dakotamurphyucf/ochat/blob/main/dune-project

#### C.4 Important nuance: ChatMD is *not tied* to OCaml
Your review makes a key architectural point: **ChatMD files are plain text** with a closed XML-like syntax. That makes them language-agnostic artifacts. Even if OCaml is an excellent fit for building the reference runtime, alternative hosts could be implemented in other languages without changing the workflow files.

This strengthens the argument that Ochat’s main innovation is the artifact + semantics, not the implementation language alone.

---

### D) Declarative Control Flow / Policy (expanded: what exists, what it unlocks, why it matters)

#### D.1 Current status: design note (not implemented yet)
The repo includes a design note proposing declarative orchestration primitives inside ChatMD:
- `<rules>` and `<policy>` blocks
- event → guard → action structure
- actions like deny/insert/call/compact/set (as described in the note)  
Source: `control-flow-chatmd.md`  
https://github.com/dakotamurphyucf/ochat/blob/main/control-flow-chatmd.md

This should be described as a **roadmap signal**, not current functionality.

#### D.2 Why policy/control flow inside ChatMD is a big deal
Most agent stacks force a split:
- prompting lives in text,
- but reliability features (policies, orchestration, guardrails) live in external framework code.

Ochat’s proposed direction aims to keep orchestration **declarative and transcript-visible**—meaning:
- the “governance layer” becomes diffable,
- enforcement rules can be reviewed like code,
- and the workflow remains portable across hosts.

If implemented, this could enable:
- tool-call gating (“never call X unless Y”),
- automated remediation on tool failure,
- automatic compaction triggers,
- structured safety constraints without relying on hidden system prompts.

This aligns directly with Ochat’s broader “humans in the loop and in control” philosophy expressed in your review: allow only what is explicitly permitted, expressed in a form humans can understand and drive.

---

### E) Vendor Lock‑in & Portability (expanded: file-native workflows + MCP + open source; plus current limits)

#### E.1 The lock-in problem in agent tooling
Many consumer “agent apps” and vendor CLIs bundle together:
- hidden system prompts,
- proprietary tool semantics,
- provider-specific orchestration,
- and non-exportable runtime state.

This creates lock-in at the workflow level: even if you can export text, you often can’t export the *actual program* (tools/policy/transcript trace) in a reproducible way.

#### E.2 Ochat’s answer: portable workflow artifacts + multiple hosts
Ochat’s design centers on **portable files** (ChatMD) that can be executed across:
- interactive TUI (`chat_tui`)
- batch CLI (`ochat chat-completion`)
- remote MCP server (`mcp_server`)  
Source: README and host docs  
https://github.com/dakotamurphyucf/ochat/blob/main/README.md  
https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/guide/chat_tui.md  
https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/cli/chat-completion.md  
https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/bin/mcp_server.doc.md

This is a direct mitigation against vendor lock-in: the workflow is not trapped inside a single UI or hosted platform.

#### E.3 MCP: portability across clients/tool ecosystems
Ochat both:
- **consumes** MCP tools (mount remote tool catalogs into prompts), and
- **serves** MCP tools/prompts (turn prompt packs into callable tools via `mcp_server`).  
Sources:
- MCP server doc: https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/bin/mcp_server.doc.md  
- MCP spec: https://modelcontextprotocol.io/specification/2025-06-18  
- MCP repo: https://github.com/modelcontextprotocol/modelcontextprotocol

This is a practical anti-lock-in story: even if a team uses different IDEs/clients, MCP offers a shared protocol surface for invoking the same prompt packs and tools.

#### E.4 Open source licensing reduces platform dependence
Ochat is MIT licensed, which materially changes the risk profile for adoption and long-term workflow investment.  
Source: LICENSE  
https://github.com/dakotamurphyucf/ochat/blob/main/LICENSE.txt

#### E.5 Important current limitation: provider support is OpenAI-only today
To keep the whitepaper technically honest: Ochat currently supports **OpenAI only** for chat + embeddings (as noted in your review, from the README).  
Source: README  
https://github.com/dakotamurphyucf/ochat/blob/main/README.md

However, the anti-lock-in argument remains strong at the *workflow representation* level: ChatMD artifacts are stable and could be executed by alternative runtimes/providers in the future, and the MCP integration already positions Ochat within a broader, vendor-neutral tool ecosystem.


### F) Constraining models so humans stay in the driver’s seat

#### F.1 The core premise: capability without control is not “power”
As LLMs become more capable, the limiting factor in real deployments is rarely “can the model do it at all?” and more often:

- **Can we predict what it will do?**
- **Can we explain why it did it?**
- **Can we bound the blast radius if it behaves incorrectly?**
- **Can we audit and reproduce the work later?**

A system that produces impressive outcomes but cannot be understood, reviewed, or constrained is not truly useful for serious work—because it cannot be trusted or improved systematically. In that world, the human is no longer operating a tool; they are *hoping* an opaque process does what they meant.

Ochat’s design choices consistently push toward the opposite: keep humans in the driver’s seat by ensuring model behavior is expressed in forms humans can inspect and govern.

#### F.2 “Constrained agency” as a design goal: do only what we explicitly permit
Ochat’s philosophy can be summarized as:

> Give models power through tools and workflows, but constrain them to operate **only within explicitly declared capabilities**, and record what happened in a form humans can understand.

This shows up structurally in multiple places:

1) **Closed, strict workflow language (ChatMD)**
   - ChatMD uses a closed set of XML-like tags embedded in Markdown, with strict rules (e.g., no free top-level free text; RAW blocks as an explicit escape hatch).  
   Source: ChatMD language reference  
   https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/overview/chatmd-language.md

   Why this matters: when the “program” format is constrained and machine-parseable, you can build reliable tooling around it (validation, transforms, policy enforcement). You avoid “mystery formatting” and hidden semantics.

2) **Explicit tool allowlisting**
   - The model can only call tools you declare via `<tool .../>`.  
   Source: tools overview  
   https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/overview/tools.md

   This is the practical embodiment of “least authority”: you do not grant broad capabilities by default; you grant narrowly scoped tools needed for the job.

3) **Transcript-as-artifact (auditability by default)**
   - ChatMD is not only instructions; it also records the execution trace, including tool calls and tool outputs (inline or externalized).  
   Source: ChatMD reference (tool call persistence)  
   https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/overview/chatmd-language.md

   This ensures the human can review what happened after the fact (and in many workflows, during the fact).

Taken together, these are not “extra knobs.” They are a coherent governance posture: constrain what the model can do, and preserve enough structured evidence to understand and reproduce behavior.

#### F.3 Understandability is a systems property, not a model property
A common mistake in agent design is to treat “understandability” as something you get from the model (e.g., “just ask it to explain itself”). In practice, the system must *force* legibility:

- **Tools must be explicit** (what can be done)
- **Calls must be logged** (what was done)
- **Artifacts must be inspectable** (what changed)
- **Policies must be reviewable** (what should be done)
- **State must be managed intentionally** (what is remembered)

Ochat’s file-native approach—where the workflow file contains tool policy, messages, and trace—makes legibility a default outcome rather than an optional discipline.

#### F.4 Compaction, but not amnesia: preserving the “story of work” for human review
Long-running workflows require summarization, but summarization often becomes a source of hidden behavior: the system silently discards history and the human loses the ability to reconstruct how a conclusion was reached.

In `chat_tui`, compaction is explicit (`:compact`) and preserves prior compactions by retaining previous `<system-reminder>` blocks and appending a new one.  
Source: `chat_tui` guide  
https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/guide/chat_tui.md

This design supports a philosophy of constrained agency in two ways:
- It keeps the model operating within resource limits (context windows) without silently rewriting history.
- It preserves a time-indexed narrative ledger so both humans and future turns can re-understand decisions, failures, and intent shifts.

#### F.5 The roadmap reinforces the philosophy: policy, state, and typed computation
Ochat’s forward-looking design directions are especially aligned with “humans remain the operators”:

1) **Declarative rules/policy in ChatMD** (design note)
   - Proposed `<rules>` / `<policy>` blocks with event → guard → action structure.  
   Source: `control-flow-chatmd.md`  
   https://github.com/dakotamurphyucf/ochat/blob/main/control-flow-chatmd.md

   This is effectively a move toward *machine-enforceable* constraints on agent behavior—constraints humans can inspect and reason about because they live in the same textual artifact ecosystem.

2) **First-class session state (planned)**
   - Key/value store + session filesystem + isolation/inheritance, Irmin-backed (per README future directions).  
   Source: README  
   https://github.com/dakotamurphyucf/ochat/blob/main/README.md

   This reduces the need to encode critical state in ambiguous natural language, which is both harder to govern and harder to audit.

3) **ChatML typed DSL (experimental)**
   - A path away from “give the model a shell” toward constrained, typed, auditable computation.  
   Source: ChatML docs  
   https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/lib/chatml/chatml_lang.doc.md

#### F.6 Practical implication: “driver’s seat” means the human can intervene at every layer
Ochat’s architecture supports meaningful human control because the human can intervene by editing plain text artifacts:

- adjust system/developer instructions in the ChatMD file
- change the tool allowlist (tighten or expand authority)
- review the logged tool calls and outputs
- fork/branch workflows by copying/exporting files/sessions
- refine prompts via meta-prompting tooling (where used)  
  Source: Meta_prompting library/tool doc  
  https://github.com/dakotamurphyucf/ochat/blob/main/docs-src/lib/meta_prompting.doc.md

This is the opposite of “the agent is a black box.” It’s closer to how we manage other powerful systems: explicit config, explicit permissions, explicit logs, and artifacts that can be reviewed and versioned.

**Ochat philosophy: constrained agency, human legibility.**  
Agents should be **programmable, not magical**: we should grant models only the capabilities we explicitly understand and intend (tool allowlists), and we should record what happened in artifacts humans can inspect (diffable transcripts + tool traces). The goal isn’t to limit usefulness—it’s to make usefulness **reliable**: if we can’t understand a system at some level, we can’t safely steer it, debug it, or improve it. Ochat is built to keep humans in the driver’s seat while still scaling up to complex, tool-using workflows.