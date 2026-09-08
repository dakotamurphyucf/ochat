# Ochat public website: research and implementation specification

**Status:** Implementation in progress. Foundation and representative website preview are being built; phase gates remain open unless checked below. See `scratch/ochat-website-implementation-notes.md` for current evidence.

**Prepared:** 2026-09-06.

**Repository baseline:** `bc76b6c72a280b4bad48a793a63373e6db26d2b4`.

**In-depth review:** 2026-09-06, same repository revision. See Section 25 for additional code-backed findings, requirements added, and the actual offline verification result.

**Intended deliverable:** A distinctive public website that introduces Ochat, teaches people to build agents, and publishes maintainable, searchable documentation on a custom domain.

**Recommended foundation:** Astro, Starlight, repository-owned Markdown, and Cloudflare Workers Static Assets.

**How to read this document:** Repository observations describe the inspected checkout. External platform facts have primary-source links. Design decisions, budgets, routes, schemas, schedules, and acceptance criteria are proposals for this project. They are not claims that the website or tooling already exists. Recheck package compatibility and hosting terms when implementation begins.

**Tracked snapshot:** Keep requirement and task changes synchronized with `scratch/ochat-website-research-spec.md`. The scratch memory file is deliberately not published.

## Contents

1. [Outcome and scope](#1-outcome-and-scope)
2. [Repository findings](#2-repository-findings)
3. [Technology research and decisions](#3-technology-research-and-decisions)
4. [Audience and user journeys](#4-audience-and-user-journeys)
5. [Information architecture](#5-information-architecture)
6. [Brand and visual direction](#6-brand-and-visual-direction)
7. [Homepage specification](#7-homepage-specification)
8. [Documentation interface](#8-documentation-interface)
9. [Tutorial and example specification](#9-tutorial-and-example-specification)
10. [Content architecture and migration](#10-content-architecture-and-migration)
11. [Search](#11-search)
12. [Code, diagrams, and demonstrations](#12-code-diagrams-and-demonstrations)
13. [Frontend architecture](#13-frontend-architecture)
14. [Accessibility and responsive behavior](#14-accessibility-and-responsive-behavior)
15. [Performance](#15-performance)
16. [Discovery, metadata, and GitHub](#16-discovery-metadata-and-github)
17. [Domain and deployment](#17-domain-and-deployment)
18. [Testing and release evidence](#18-testing-and-release-evidence)
19. [Implementation phases and work packages](#19-implementation-phases-and-work-packages)
20. [Operations and maintenance](#20-operations-and-maintenance)
21. [Risks and open decisions](#21-risks-and-open-decisions)
22. [Launch acceptance checklist](#22-launch-acceptance-checklist)
23. [Research sources](#23-research-sources)
24. [Complete source inventory](#24-complete-source-inventory)
25. [Codebase review and requirements audit](#25-codebase-review-and-requirements-audit)

## 1. Outcome and scope

### 1.1 Product objective

Make Ochat understandable in one visit and useful in the next action. A visitor should recognize that Ochat defines agents in editable text files, see a real example, and find a practical route to running an agent locally. A returning user should reach a specific tool, declaration, command, or host behavior quickly.

The website should communicate the depth of the framework through progressive disclosure: instructions and tools first; composition next; optional orchestration and durable hosting after that. Its visual identity should emerge from the actual product: source files, terminal interaction, explicit tools, and inspectable workflows.

The central homepage message is:

> Build your own AI agents in text files.

This is existing project positioning from [Readme.md](../../Readme.md), not a new promise of product capability.

### 1.2 Required outcomes

- A custom landing page with recognizable Ochat branding and a concise product explanation.
- A clear first-agent tutorial, with installation requirements visible before commands.
- Searchable documentation generated from repository Markdown.
- Clean permanent URLs that do not expose `.doc.md` or `README.md` naming conventions.
- Distinct navigation for tutorials, concepts, operational guides, and reference.
- Carefully labeled library prose and historical material.
- An attractive, readable desktop and mobile experience in light and dark themes.
- A reproducible static build, automated validation, and a documented deployment path.
- A custom domain and reciprocal links between the website and GitHub at launch.
- A maintenance process that keeps code changes and documentation changes together.

### 1.3 Scope of the first public release

The first release includes the homepage, a curated documentation home, the first-agent path, at least three follow-on tutorials, current topic introductions, primary command/language/host references, current library sidecars that pass review, search, source links, and a deployment runbook.

Every existing Markdown file must receive a recorded publication disposition. A first release does not need to render all historical research and audit documents into the main site. Repository-only documents remain accessible through explicit GitHub links where useful.

The generated OCaml API site is a separate work package. Ship it under `/api/` only once a fresh build has been verified. Before that, provide an honest OCaml integration landing page under `/docs/integrations/ocaml/` with links to current interfaces and library prose; omit `/api/` from global navigation until it resolves to a verified artifact.

### 1.4 Deliberate later additions

The following are outside the first release: an authenticated agent dashboard, browser-hosted agent execution, API-key entry, a paid service, a CMS, user accounts, a blog without an editorial owner, multilingual content, multi-version documentation, and an AI documentation assistant.

A future browser interface for running Ochat agents would be a separate product specification with its own execution, identity, permissions, and persistence requirements. Nothing in the documentation site should imply that such an interface already exists.

### 1.5 Definition of a successful first visit

In a short moderated usability session, a new developer can:

1. Explain what Ochat is without calling it only a chat application.
2. Identify that agent definitions live in text files.
3. Find installation requirements and the local first-agent tutorial.
4. Understand that creating an agent does not require writing OCaml.
5. Understand that a daemon and ChatML are optional starting choices.

After installation, the tutorial should lead to a visible response from an agent. Do not advertise a fixed time such as “working in five minutes” until measured with representative fresh installations.

## 2. Repository findings

### 2.1 Inspected material

The research inspected the root README; documentation and topic indexes; project overview; ChatMD introduction; quickstart and local tutorial; library index; coverage ledger excerpts; development instructions; package metadata; the existing TUI screenshot; forwarding-page structure; and documentation-check declarations. It also inventoried all Markdown files under `docs-src/`.

This is sufficient to define the website architecture and migration strategy. It is not a line-by-line technical audit of every existing example or library explanation.

### 2.2 Quantitative baseline

| Observation | Inspected checkout |
|---|---:|
| Tracked files under `docs-src/` | 306 |
| Markdown documents under `docs-src/` | 297 |
| Markdown documents with leading YAML frontmatter | 0 |
| Markdown documents under `docs-src/lib/` | 183 |
| Markdown documents under `docs-src/agent-server/` | 24 |
| Markdown documents under `docs-src/guide/` | 19 |
| Markdown documents under `docs-src/bin/` | 18 |
| Older forwarding documents under `docs-src/chat_tui/` | 9 |
| Documents under `docs-src/development/` | 8 |
| Existing files under local generated `docs/` | 441 |
| Existing generated `docs/` size | 5,694,570 bytes |
| Existing `.github/` directory | Absent |

The generated `docs/` figures describe local files, not a verified release artifact or a claim that they are tracked. Recompute inventory at implementation time.

### 2.3 Source characteristics that affect implementation

- Source Markdown uses repository-relative links, including links outside `docs-src/`.
- Pages generally supply a first-level heading rather than website metadata.
- Source filenames include `.doc.md`, underscores, capitalized module names such as `Io`, and `README.md` indexes.
- Content includes both current behavior and historical design/research material.
- Older TUI pages preserve headings and forward to canonical library pages. Some old fragments map to differently named new fragments.
- Some references are long: the ChatML language specification is approximately 74 KB of source and protocol types approximately 59 KB. The implementation specification and coverage ledger are larger still.
- Fenced blocks include OCaml, XML, shell, console, JSON, ChatML, Lisp, Mermaid, and other languages.
- A simple fence inventory found 670 OCaml, 116 XML, 85 text, 73 shell, 57 console, 39 JSON, 13 ChatML, and 3 Mermaid opening fences. Counts are inventory hints, not parser conformance results.
- The repository's `.gitignore` includes a broad `src/` rule. A new `website/src/` tree needs explicit tracking verification and, if needed, a narrowly scoped unignore rule.
- `.rgignore` excludes Markdown broadly. Inventory and migration tooling must not depend on default `rg` visibility. Use Git's tracked-file list or an explicit filesystem traversal with an allowlist.
- The existing screenshot at [assets/tui-snapshot.png](../../assets/tui-snapshot.png) is a tall code-heavy TUI capture. It is useful as product context, but a shorter task-oriented capture would make a clearer homepage demonstration.

### 2.4 Product facts the site must preserve

| Fact | Website implication | Local evidence |
|---|---|---|
| Agent definitions use ChatMD | Show a readable source example prominently | [README](../../Readme.md), [ChatMD introduction](../../docs-src/chatmd/README.md) |
| ChatML orchestration is optional and experimental | Introduce it after a working prompt; label status accurately | [Project overview](../../docs-src/overview/project.md), [ChatML index](../../docs-src/chatml/README.md) |
| Agents can be used without writing OCaml | Explain language independence early | [README](../../Readme.md) |
| Building currently requires OCaml/opam and native dependencies | Do not invent a one-line binary installer | [Quickstart](../../docs-src/agent-server/quickstart.md), [dune-project](../../dune-project) |
| Native local TUI state is transient and process-bound | State persistence behavior in the first tutorial | [Local tutorial](../../docs-src/agent-server/tutorials/local-tui.md) |
| Durable daemon hosting is implemented | Present it as an available advanced capability | [Agent-server index](../../docs-src/agent-server/README.md) |
| Legacy and native host flags have different contracts | Keep tutorial mode selection explicit | [Local tutorial](../../docs-src/agent-server/tutorials/local-tui.md) |
| MCP-backed tools remain maintained | Do not label all MCP functionality deprecated | [Tools index](../../docs-src/tools/README.md) |
| The older MCP prompt server is compatibility functionality | Isolate its page from primary onboarding | [Documentation index](../../docs-src/README.md) |
| Markdown sidecars and odoc are separate | Publish prose and generated API artifacts deliberately | [Library index](../../docs-src/lib/README.md), [DEVELOPMENT.md](../../DEVELOPMENT.md) |
| Model/proxy support is constrained by current runtime behavior | Avoid broad provider-agnostic claims | [Project overview](../../docs-src/overview/project.md) |

### 2.5 Existing validation to retain

`dune build @agent-docs-check` is defined by [test/agent_docs/dune](../../test/agent_docs/dune). It provides offline documentation validation and uses tracked example executables. It is separate from normal `dune runtest` and does not establish that every historical example was live-tested.

The website needs additional checks for generated routes, fragments, rendering, and search. Do not replace the existing semantic documentation checks with a successful Astro build.

The in-depth follow-up ran `dune build --force @agent-docs-check` successfully: 297 documentation pages, 38 protocol methods, and no live provider calls. This establishes the current repository's offline documentation baseline. It does not validate a future Astro renderer, website deployment, or every command on every platform.

## 3. Technology research and decisions

### 3.1 Recommended stack

| Layer | Choice | Reason for this project |
|---|---|---|
| Page generation | Astro in static-output mode | A content-oriented site with small interactive elements |
| Documentation shell | Starlight | Established documentation navigation and Markdown integration |
| Homepage | Custom Astro page | Freedom to establish Ochat's visual identity |
| Styling | CSS variables and component styles | A small, explicit design system shared across homepage and docs |
| Documentation content | Existing Markdown plus a metadata manifest | Preserve repository readability and one authoritative source |
| Rich presentation | Astro components; limited MDX only where useful | Avoid converting the existing corpus into JSX |
| Search | Starlight's Pagefind integration | Build-time indexing and client-side retrieval |
| Code presentation | Starlight/Expressive Code configuration | Consistent highlighting, frames, and copy behavior |
| Hosting | Cloudflare Workers Static Assets | Static deployment with domain and Git integration |
| Regression checks | Content validation and Playwright | Test migration behavior and real reader interactions |
| API reference | Fresh odoc artifact, separately verified | Preserve OCaml-specific API navigation |

Starlight supports Markdown/MDX, documentation navigation, highlighting, and theming. Its custom-page support permits a separate homepage within the same Astro project. These platform capabilities support the recommendation; the design and content structure in this document are Ochat-specific decisions. [Starlight](https://starlight.astro.build/), [pages](https://starlight.astro.build/guides/pages/).

### 3.2 Alternatives and decision boundaries

| Option | Assessment | Reconsider when |
|---|---|---|
| Astro + Starlight | Recommended balance of custom presentation and existing Markdown | Baseline choice |
| Next.js + Fumadocs | Credible alternative if React application development becomes central | The same site must grow substantial authenticated application features |
| A bespoke documentation renderer | Maximum control, larger maintenance responsibility | A demonstrated requirement cannot be served through supported Starlight extensions |
| A hosted documentation service | Can reduce infrastructure ownership, but introduces another publishing system | The owner explicitly prioritizes managed editorial workflows over repository-owned presentation |
| Existing odoc output as the entire site | Useful API material, insufficient as the proposed product introduction and tutorial experience | Never the primary marketing/tutorial shell for this scope |

Fumadocs supplies documentation UI and content/search primitives, which makes it a legitimate alternative. The choice of Starlight here is a fit judgment based on this repository, not a claim that Fumadocs cannot generate static documentation. [Fumadocs introduction](https://www.fumadocs.dev/docs).

### 3.3 Why Cloudflare Workers rather than a new Pages project

Astro's current Cloudflare deployment guide states that Cloudflare recommends Workers for new projects and documents a static-assets configuration. Use that current path for a new Ochat website. This recommendation does not imply that existing Cloudflare Pages sites stop working. [Astro deployment guide](https://docs.astro.build/en/guides/deploy/cloudflare/).

### 3.4 Version policy

Do not copy an assumed Astro/Starlight major version from this specification. During the initial technical spike:

1. Resolve a supported Astro, Starlight, Node, and package-manager combination.
2. Confirm compatibility with Expressive Code, Pagefind, and any Markdown plugins.
3. Record exact versions in `website/package.json`, the lockfile, and the website development guide.
4. Pin the deployment CLI and Node runtime used by CI.
5. Prove a clean installation and production build on Linux as well as the local development machine.
6. Record the chosen compatibility date in Wrangler configuration.

Use one package manager. Default to npm and a committed `package-lock.json` unless an implementation-time repository convention dictates otherwise.

### 3.5 Technical spike exit criteria

Before a full migration, render six representative pages: a simple tutorial, the ChatMD reference, the ChatML reference, a library sidecar, a forwarding page, and a page with Mermaid or raw HTML. Confirm custom homepage coexistence, `/docs/` route ownership, frontmatter generation, exact code-copy behavior, search indexing, and a working static deployment preview.

This spike should resolve framework integration details without redesigning the source documentation prematurely.

## 4. Audience and user journeys

### 4.1 Primary audiences

| Audience | Immediate question | Primary destination |
|---|---|---|
| Developer evaluating Ochat | What makes this useful for my project? | Homepage and first-agent tutorial |
| Agent author | How do I add instructions, tools, or a specialist? | ChatMD, tools, and composition tutorials |
| Workflow author | How do I control multi-step or background work? | ChatML and background-agent guides |
| Integrator/operator | How do I host it and connect a client? | Host concepts, daemon, stdio, and HTTP |
| OCaml contributor | Where are the libraries and contracts? | Integration guide, library prose, API reference |

Avoid presenting the same long list of module names to every audience.

### 4.2 Journey A: discover and run

Entry: homepage or GitHub README. The user reads the headline, sees a complete agent file, opens the first-agent tutorial, installs dependencies, configures the provider privately, saves a prompt, opens the local TUI, and submits a first request.

Success: an agent response appears and the user knows which file to edit next. The tutorial explains how to quit and what state persists.

Potential abandonment points: native dependency installation, unclear working directory, missing provider setup, terminal keybindings, and assuming a server is required. Address these in context with short troubleshooting links.

### 4.3 Journey B: add useful capabilities

Entry: first-agent completion panel. The user chooses a concrete task, adds a built-in tool, understands its declared access, and sees a result. The next tutorial introduces a specialist agent as a callable tool.

Success: the user can explain the difference between changing instructions and changing tool capabilities.

### 4.4 Journey C: look up an exact declaration

Entry: search, bookmark, or external search engine. The user searches for `shell_access`, `${workspace}`, `read_file`, `--local`, or a module name.

Success: a current reference section is reachable in one selection, with its hierarchy and source link visible. Search results distinguish concepts from references and compatibility notes.

### 4.5 Journey D: host a durable agent

Entry: background-work capability or operational guide. The user learns the lifecycle difference, follows a daemon setup tutorial, connects a client, detaches, and reconnects.

Success: the user understands the difference between client disconnection and session termination, and can find backup/recovery information.

### 4.6 Journey E: contribute a documentation change

Entry: “Edit this page.” The contributor reaches the actual source Markdown, updates it, runs the website checks and relevant documentation validation, and previews the change.

Success: one source edit updates GitHub-readable documentation and the generated website. No generated copy needs manual editing.

## 5. Information architecture

### 5.1 Global navigation

Desktop header: Ochat wordmark, Docs, Tutorials, Examples, GitHub, search, and theme selection. Keep the homepage's primary “Build your first agent” action visually prominent without duplicating too many buttons.

“Tutorials” and “Examples” link into the documentation hierarchy. They do not require separate applications or publishing systems. Do not show empty top-level destinations.

### 5.2 Proposed route tree

```text
/
/docs/
  start/
    installation/
    first-agent/
    troubleshooting/
  tutorials/
    add-tools/
    specialist-agents/
    headless-workflow/
    chatml-workflow/
    shell-agent/
    background-agent/
    unix-daemon/
    stdio-client/
    http-client/
  concepts/
    chatmd/
    chatml/
    tools/
    shell-access/
    hosts/
    sessions-and-workspaces/
  guides/
    tui/
    search-and-indexing/
    ...
  examples/
  reference/
    chatmd/
    chatml/
    tools/
    commands/
    agent-server/
    shell/
  integrations/
    ocaml/
  operations/
  library/
  contributing/
  compatibility/
/api/                         # Only once a verified odoc artifact is ready
/404.html                     # Error document, not a navigation item
/sitemap-index.xml            # Or generator-equivalent sitemap entry point
/robots.txt
```

These are proposed public routes, not a request to rename all corresponding source files. The content manifest owns the mapping.

### 5.3 Navigation principles

- Organize beginner navigation by reader task.
- Organize exact reference navigation by product subsystem.
- Expand the current sidebar group; keep unrelated large groups collapsed.
- Keep tutorials in an intentional sequence rather than alphabetical order.
- Use visible names such as “Command reference,” not `bin`.
- Make module details available under Library without crowding the first viewport.
- Give each page one canonical location even when several hubs link to it.
- Show compatibility or historical status before examples on affected pages.
- A page excluded from the sidebar is not automatically private or excluded from search.

### 5.4 Initial source-to-route mapping

| Source | Proposed canonical route | Action |
|---|---|---|
| `docs-src/README.md` | `/docs/` | Adapt navigation into a documentation home |
| `Readme.md` installation sections | `/docs/start/installation/` | Extract into a canonical source page, then shorten README by linking |
| `docs-src/agent-server/tutorials/local-tui.md` | `/docs/start/first-agent/` | Expand into complete beginner walkthrough |
| `docs-src/guide/build-troubleshooting.md` | `/docs/start/build-troubleshooting/` | Preserve platform-specific guidance; avoid the existing host troubleshooting route |
| `docs-src/chatmd/README.md` | `/docs/concepts/chatmd/` | Retain concise introduction |
| `docs-src/chatml/README.md` | `/docs/concepts/chatml/` | Preserve experimental status |
| `docs-src/tools/README.md` | `/docs/concepts/tools/` | Introduce capability choices |
| `docs-src/shell/README.md` | `/docs/concepts/shell-access/` | Explain host authorization and policy |
| `docs-src/agent-server/concepts.md` | `/docs/concepts/hosts/` | Explain host modes and lifecycle |
| `docs-src/agent-server/sessions-and-workspaces.md` | `/docs/concepts/sessions-and-workspaces/` | Preserve exact distinctions |
| `docs-src/overview/chatmd-language.md` | `/docs/reference/chatmd/` | Preserve declaration anchors |
| `docs-src/guide/chatml-language-spec.md` | `/docs/reference/chatml/` | Preserve language anchors and long-form structure initially |
| `docs-src/overview/tools.md` | `/docs/reference/tools/` | Preserve tool catalog anchors |
| `docs-src/bin/README.md` | `/docs/reference/commands/` | Build a readable command index |
| `docs-src/guide/chat_tui.md` | `/docs/guides/tui/` | Preserve keyboard and host qualifications |
| `docs-src/guide/search-and-indexing.md` | `/docs/guides/search-and-indexing/` | Retain setup requirements |
| `docs-src/guide/general-agent-workflow.md` | `/docs/guides/agent-workflows/` | Curate as an advanced composition guide |
| `docs-src/cli/chat-completion.md` | `/docs/reference/commands/chat-completion/` | Reference backing the headless tutorial |
| `docs-src/cli/shell-runtime-management.md` | `/docs/reference/commands/shell-management/` | Keep inspection/authorization distinctions |
| `docs-src/agent-server/tutorials/unix-daemon.md` | `/docs/tutorials/unix-daemon/` | Publish complete hosting tutorial |
| `docs-src/agent-server/tutorials/stdio-client.md` | `/docs/tutorials/stdio-client/` | Publish client tutorial |
| `docs-src/agent-server/tutorials/http-client.md` | `/docs/tutorials/http-client/` | Publish authenticated client tutorial |
| `docs-src/agent-server/tutorials/shell-agent.md` | `/docs/tutorials/shell-agent/` | Retain mode-specific authorization |
| `docs-src/agent-server/tutorials/background-agent.md` | `/docs/tutorials/background-agent/` | Preserve timer/background semantics |
| `docs-src/agent-server/protocol.md` | `/docs/reference/agent-server/protocol/` | Preserve protocol reference |
| `docs-src/agent-server/protocol-types.md` | `/docs/reference/agent-server/protocol-types/` | Audit table and long-code rendering |
| `docs-src/agent-server/configuration.md` | `/docs/reference/agent-server/configuration/` | Keep exact configuration ownership |
| `docs-src/agent-server/environment.md` | `/docs/reference/agent-server/environment/` | Keep provider/host environment distinctions |
| `docs-src/agent-server/operations.md` | `/docs/operations/` | Operational landing/reference |
| `docs-src/agent-server/troubleshooting.md` | `/docs/start/troubleshooting/` | Preserve the existing URL; label Host troubleshooting under Agent hosting |
| `docs-src/agent-server/embedding.md` | `/docs/integrations/ocaml/` | Main entry for OCaml users |
| `docs-src/examples/README.md` | `/docs/examples/` | Curate example discovery |
| `docs-src/examples/prompt-patterns.md` | `/docs/examples/prompt-patterns/` | Label templates and assumptions |
| `docs-src/overview/project.md` | `/docs/concepts/project/` | Explain principles, architecture, and status |
| `docs-src/lib/README.md` | `/docs/library/` | Explain prose/API distinction |
| `docs-src/lib/**/*.doc.md` | `/docs/library/.../` | Explicit normalized mapping after review |
| `docs-src/chat_tui/*.doc.md` | Compatibility mapping | Preserve old fragments or use bridge pages |
| `docs-src/bin/mcp_server.doc.md` | `/docs/compatibility/mcp-prompt-server/` | Preserve deprecation scope |
| `docs-src/development/**` | Usually repository-only | Review individually; do not mass-publish audit ledgers |
| `docs-src/design/**` | Usually repository-only | Link as design background with status context |

New tutorials such as add-tools and specialist-agents should have canonical Markdown sources under a deliberate `docs-src/tutorials/` directory. Do not build them by maintaining copied prose inside Astro components.

### 5.5 Documentation home

The documentation home should answer “Where do I start?” with one primary route and a small number of alternatives:

1. Build your first local agent.
2. Add tools and specialist agents.
3. Control a workflow with ChatML.
4. Host an agent or build a client.
5. Look up a language, command, or tool.

Below those routes, include compact topic groups and an OCaml/contributor entry. Avoid reproducing the entire 297-document tree on the landing page.

### 5.6 Capability coverage beyond the beginner path

The main route map is not the entire content specification. The following capabilities must have discoverable destinations or explicit deferrals so that the website represents the framework's breadth. These are documentation requirements; they do not require new runtime features or a tutorial for every library.

| Capability | Required reader entry | Existing source | Qualification to preserve |
|---|---|---|---|
| Prompt imports, source paths, and reusable declarations | ChatMD reference and composition links | [ChatMD language](../../docs-src/overview/chatmd-language.md), [source loader](../../lib/chatmd/source_loader.mli) | Imported source paths and captured source closures have exact semantics |
| Built-in tool names and access | Tool catalog, searchable by declaration and model-visible names | [Tool reference](../../docs-src/overview/tools.md) | `read_dir` and model-visible `read_directory` are distinct names |
| Maintained MCP tools and authentication | Tools/integrations entry | [Tools introduction](../../docs-src/tools/README.md), [MCP libraries](../../docs-src/lib/mcp/mcp_client.doc.md) | Distinguish tool consumption from deprecated prompt serving |
| Custom OCaml tools, progress, and traces | OCaml integration and example catalog | [Tool registration](../../docs-src/lib/gpt_function.doc.md), [compiled example](../../docs-src/examples/tools/custom_tool.ml) | A source example may need companion Dune dependencies |
| Retrieval and indexing | Search/indexing guide | [Indexing guide](../../docs-src/guide/search-and-indexing.md) | Embedding-backed Ochat search is separate from website Pagefind search; stubs are not semantic retrieval |
| Prompt refinement and evaluation | Advanced guide or library hub entry | [Meta-prompting](../../docs-src/lib/meta_prompting.doc.md), [refinement CLI](../../docs-src/bin/mp_refine_run.doc.md) | Preserve model-work costs, setup, and example status |
| Context compaction and conversation history | TUI guide, operations, and library links | [TUI guide](../../docs-src/guide/chat_tui.md), [compaction](../../docs-src/context_compaction/compactor.doc.md) | Visible history, canonical history, and exports are not interchangeable backups |
| ChatML host capabilities and budgets | Runtime guide and related contracts | [Runtime guide](../../docs-src/guide/chatml-moderator-runtime.md), [budget policy](../../docs-src/chatml-budget-policy.md), [UI capabilities](../../docs-src/chatml-ui-host-capabilities.md) | Capability availability depends on the host; turn limits are not dollar caps |
| Background jobs, timers, and restart behavior | Background tutorial and orchestration reference | [Orchestration](../../docs-src/agent-server/chatml-orchestration.md) | Recovery does not resume arbitrary continuations or guarantee exactly-once external effects |
| TUI attachments, exports, keybindings, and draft suggestions | TUI guide with direct section links | [TUI guide](../../docs-src/guide/chat_tui.md) | Optional draft suggestions can send unsent text and incur provider work |
| Shell inspection, grants, audit, and interruption | Shell and operations hubs | [Host integration](../../docs-src/guide/chatmd-shell-host-integration.md), [management CLI](../../docs-src/cli/shell-runtime-management.md) | Legacy session commands are not daemon administration commands |
| Protocol events, reconnect, and projection | Client tutorials and protocol reference | [Protocol](../../docs-src/agent-server/protocol.md), [permissions](../../docs-src/agent-server/permissions-and-security.md) | Protocol 1.0 does not promise replay of live deltas |
| Current and older embedding APIs | Distinct OCaml integration/library routes | [Agent-core embedding](../../docs-src/agent-server/embedding.md), [other embedding](../../docs-src/lib/embedding.md) | Avoid substituting legacy store ownership for the current actor-owned host |
| Executables and utility subcommands | Command index | [Command index](../../docs-src/bin/README.md), [binary declarations](../../bin/dune), [main CLI](../../bin/main.ml), [installed scripts](../../scripts/dune) | Preserve actual executable spelling, installation status, and source-only utilities |

Record coverage of these rows during P05. Add related links and search metadata where an existing reference is sufficient. Do not bury all advanced capabilities behind an undifferentiated library list or promote historical algorithms as maintained features without review.

## 6. Brand and visual direction

### 6.1 Proposed visual character

The design should feel precise, calm, and crafted for people who work with source code. Use a strong typographic hierarchy, ample whitespace, subtle surfaces, crisp rules, and real code examples. The site should remain visually coherent without decorative images.

Use lowercase `ochat` for the proposed wordmark, “Ochat” in prose, and exact case for technical identifiers. This is a proposed brand convention; existing technical names remain unchanged.

### 6.2 Logo and identity assets

Begin with a typographic wordmark and a simple vector mark that remains recognizable at favicon size. A restrained circular or bracket-inspired mark can reference the name and source-file interaction. Evaluate at 16, 24, and 32 pixels before polishing larger uses.

Deliver SVG wordmark and symbol, favicon assets, light/dark variants where required, and a social-preview composition. Keep shape assets native to SVG/CSS. Record any external font or icon licensing in the website asset notes.

### 6.3 Proposed color tokens

These values are design starting points. Contrast must be measured for actual foreground/background combinations before acceptance.

| Semantic token | Light proposal | Dark proposal | Use |
|---|---|---|---|
| Page background | `#FAFAF7` | `#101211` | Main canvas |
| Raised surface | `#FFFFFF` | `#191D1B` | Cards and menus |
| Subtle surface | `#F0F2ED` | `#222824` | Code tabs and secondary regions |
| Main text | `#18231E` | `#EFF4EE` | Headings and prose |
| Secondary text | `#526058` | `#ABB9AE` | Metadata and supporting copy |
| Accent | `#286B4B` | `#9BDDAB` | Links, active state, primary controls |
| Decorative border | `#D8DED7` | `#354139` | Surface separation |
| Focus indicator | `#175CA8` | `#8EBEFF` | Keyboard focus |

Do not use the decorative border token as the sole required boundary for an interactive control until its contrast passes. Status colors need accompanying text/icons. Syntax colors need independent light/dark testing.

### 6.4 Typography

- Use one readable sans-serif family for interface and body text, plus one monospace family for source and terminal content.
- Start the visual prototype with system stacks; evaluate a licensed self-hosted font pair during design polish.
- Prefer no more than two font families and a small number of weights.
- Body text: proposed `1rem` to `1.0625rem`, line-height about `1.65`.
- Article line length: approximately `68–76ch`, with deliberate exceptions for tables and code.
- Homepage hero: proposed fluid scale from roughly `2.5rem` to `4.75rem`.
- Documentation title: proposed `2rem` to `2.75rem` depending on viewport.
- Code: readable at normal zoom, approximately `0.875rem` to `0.9375rem`, with sufficient line-height.
- Never shrink an entire terminal recording until its text becomes illegible to fit a decorative frame.

### 6.5 Layout and spacing

Use a spacing scale based on 4 pixels: 4, 8, 12, 16, 24, 32, 48, 64, 96. Most cards use 20–28 pixels of internal padding. Major homepage sections use 64–112 pixels of separation on wide screens and 40–64 pixels on smaller screens.

Set the marketing content width around 1200 pixels. Documentation may use a wider total shell while keeping the article measure constrained. Border radius should be modest: approximately 8–12 pixels for panels and smaller values for controls.

### 6.6 Motion

Limit motion to useful feedback: menu transitions, copy confirmation, and a modest active-state transition. A homepage example may change on explicit user selection. Avoid continuous ambient motion, scroll hijacking, cursor followers, or automatic typing effects that delay reading.

Reduced-motion preferences disable nonessential transitions and any automatic demo movement. A paused demonstration should still make sense as a static composition.

### 6.7 Visual quality review

Evaluate the homepage and one dense reference page together. A beautiful landing page does not compensate for cramped code, weak contrast, or confusing documentation navigation.

Review at least these compositions: desktop light homepage, desktop dark homepage, mobile homepage, desktop tutorial, mobile tutorial, long reference, search dialog, and 404 page. Confirm that the identity survives all of them.

## 7. Homepage specification

### 7.1 Section sequence

1. Header.
2. Hero with product explanation and a complete agent example.
3. Short real demonstration.
4. Three-step explanation: define, compose, control.
5. Concrete use cases.
6. Execution choices.
7. Learning paths and final call to action.
8. Footer.

The hero and demonstration can share a composition on wide screens, but the reading order must remain coherent on mobile.

### 7.2 Hero copy proposal

**Eyebrow:** Ochat · agent workflows you define.

**Headline:** Build your own AI agents in text files.

**Supporting copy:** Define instructions, tools, and reusable specialists in ChatMD. Run your agent in the terminal, use it from scripts, or add ChatML to coordinate a longer workflow.

**Primary action:** Build your first agent.

**Secondary action:** View on GitHub.

**Supporting fact:** Written in OCaml. Agents for projects in any language.

Final copy must be checked against current source behavior. Keep the runtime maturity and installation qualifications accessible in the first tutorial rather than packing the hero with implementation detail.

### 7.3 Hero example behavior

Use a complete, small ChatMD definition drawn from a maintained example. Display the filename, readable source, and a copy button. Provide the launch command in a separate terminal block. Do not combine a source file and shell command into one ambiguous clipboard action.

The hero can use a tool-free prompt to keep the definition short. If a read-oriented example is used, retain the complete tool declaration and explain `${workspace}` in the tutorial. Do not invent abbreviated syntax that users will copy and fail to run.

The headline, explanation, source code, and primary link must exist in rendered HTML before JavaScript executes.

### 7.4 Demonstration specification

Show one meaningful task: launch a project explorer, ask about a named file, observe a file-tool event, and read a concise answer. The sequence should be short enough to understand without watching a lengthy coding session.

Use a new recording verified against the documented launch command, or a clearly labeled static walkthrough while that recording is being prepared. The existing historical recording can remain as a secondary project artifact.

Required demonstration metadata: source commit, recording date, selected host mode, prompt path, whether responses are a real run or illustrative, and the corresponding tutorial. Keep this metadata in the asset manifest; show only reader-useful portions next to the media.

### 7.5 Three product explanations

| Section | Reader takeaway | Supporting visual |
|---|---|---|
| Define your agent | Instructions and selected capabilities live in a file | A short annotated ChatMD block |
| Compose useful tools and specialists | Reuse an agent as a callable specialist | Main agent connected to a documentation reviewer |
| Add workflow control | Optional ChatML reacts to runtime events | Small event/action diagram with source link |

All diagrams must be grounded in maintained examples. Do not suggest automatic orchestration semantics that the runtime does not provide.

### 7.6 Use-case cards

Use four to six concrete cards: understand a repository, review documentation, build a project assistant, search local knowledge, coordinate specialists, and run background work. Each card links to a real tutorial or guide. Cards should not depend on hover to reveal their purpose.

### 7.7 Execution choices

Present three choices in plain language:

- **Terminal:** work interactively with a local agent.
- **Scripts:** run a request and write its conversation output.
- **Agent server:** keep supported work independent of a terminal client and connect clients.

Follow with links to precise host contracts. Do not summarize all persistence modes as interchangeable or imply that quitting the native local TUI saves a resumable session.

### 7.8 Homepage acceptance

- First viewport communicates the product without waiting for a demo.
- Primary action reaches the maintained first-agent page.
- Example source is complete and copyable.
- Mobile layout preserves source readability and logical reading order.
- Product claims are traceable to current docs.
- No invented star counts, testimonials, partner logos, benchmark claims, or support promises.
- Footer has working documentation, GitHub, license, and contribution links.

## 8. Documentation interface

### 8.1 Desktop shell

Use three regions when space permits: left section navigation, central article, and right “On this page” contents. Proposed dimensions are a 248–280 pixel sidebar, a constrained article, and a 192–224 pixel contents rail. These are prototype targets, not fixed widths that override reflow.

The header remains available without consuming excessive vertical space. Anchor navigation must account for its height. The article should be the main landmark and the first useful destination of a skip link.

### 8.2 Article anatomy

1. Breadcrumb or compact section context.
2. Page title.
3. One-sentence description when useful.
4. Relevant status or host-scope label.
5. Main content.
6. Related next steps.
7. Edit-source link and factual update/verification metadata.
8. Previous/next links within an intentional sequence.

Do not show a build timestamp as “last updated” on every page. Derive content-change dates from Git where reliable. A separate “last verified” field means someone actually checked the specified behavior, not that the file was regenerated.

### 8.3 Sidebar behavior

- Current page has a clear active treatment and `aria-current` where applicable.
- Group toggles are buttons with expanded state.
- Current group opens automatically without collapsing the article on navigation.
- Labels remain understandable without reading the filesystem path.
- Large library trees are grouped by subsystem.
- Keyboard navigation uses standard links and buttons; avoid a custom tree widget unless its full interaction model is necessary.
- If saved expansion state is implemented, it must never hide the current page or break when storage is unavailable.

### 8.4 Table of contents

Include meaningful second- and selected third-level headings. Very dense API/reference pages may limit the visible depth to keep the rail usable. Every heading remains directly linkable even if it is omitted from the rail.

An optional active-section indicator must not continuously rewrite browser history. Clicking a heading link should create normal browser navigation behavior. Test direct fragment loads with encoded symbols and legacy anchors.

### 8.5 Callouts

Use short callouts for prerequisites, host-specific behavior, compatibility, and material execution consequences. Keep the main action readable. Do not turn every paragraph into an admonition.

Existing plain Markdown should remain understandable on GitHub. Add web-specific presentation only where the source remains useful, or through structured metadata and build transformations.

### 8.6 Tables and long code

Tables need proper headers and contextual introductions. Wide tables may scroll within their own region, with a visible hint on small screens. Never force the entire page to overflow horizontally.

Code keeps whitespace intact. Prefer horizontal scrolling to silently altering commands. An optional visual wrap control must not change copied bytes. Large reference blocks can have an explicit expand control only if content remains accessible and deep links still work.

### 8.7 Mobile shell

The documentation navigation becomes a labeled menu button. “On this page” becomes an in-flow disclosure near the title. Search remains easy to reach. The article uses the full available width minus comfortable gutters.

If navigation uses a modal drawer, opening it moves focus inside; closing restores focus; background interaction is disabled. Avoid simultaneously opening two overlapping modal layers for menu and search.

### 8.8 Error and empty states

The 404 page should identify the missing page, provide a documentation-home link, and offer search. It must be served with a real 404 status rather than returning the homepage with 200.

Search needs distinct initial, loading, no-results, and load-error states. A missing demo asset should leave a useful poster/transcript link rather than an empty box. Clipboard failure should leave source selectable and give a concise fallback message.

## 9. Tutorial and example specification

### 9.1 Required tutorial structure

Every tutorial should contain:

1. A concrete result the reader will achieve.
2. Prerequisites and the relevant host mode.
3. Working-directory assumptions.
4. Required files and complete starting content.
5. Numbered actions with expected observations.
6. A checkpoint that confirms success.
7. Common failures connected to the step where they occur.
8. Cleanup, exit, or persistence behavior.
9. One or two next steps.

Reading time and execution time are different. Show estimates only after measurement, and account separately for first-time installation.

### 9.2 Tutorial curriculum

| ID | Tutorial | Starting state | Expected outcome | Source basis |
|---|---|---|---|---|
| T01 | Build your first local agent | Fresh checkout and supported environment | A local agent answers one request | README, quickstart, local-TUI tutorial |
| T02 | Give an agent a file tool | T01 complete | Agent reads a named project file | README explorer and tools reference |
| T03 | Add a specialist reviewer | T02 complete | Main agent calls another prompt as a tool | README specialist example |
| T04 | Run a request from a script | Installed CLI and a prompt | Inspect a conversation output file | Completion CLI guide |
| T05 | Add ChatML workflow logic | Working prompt | Observe a bounded event-driven behavior | README moderator and ChatML guides |
| T06 | Give an agent a narrow shell command | Appropriate host/platform setup | Inspect, authorize, and run the declared command | Shell-agent tutorial |
| T07 | Run a durable daemon session | Configured provider and private demo setup | Detach and reconnect to a session | Unix-daemon tutorial |
| T08 | Respond to a background event | Appropriate durable host setup | Observe a timer/background workflow | Background-agent tutorial |
| T09 | Connect a stdio client | Installed host/client tooling | Exchange protocol messages | Stdio-client tutorial |
| T10 | Connect an HTTP client | Configured authenticated endpoint | Receive a response and live updates | HTTP-client tutorial |

Launch minimum: T01 through T04 polished to beginner quality, with maintained existing advanced tutorials published and clearly scoped. T05–T10 must not be advertised as newly verified unless their verification evidence exists.

### 9.3 First-agent tutorial detail

The first-agent page should explicitly separate installation from day-to-day use. Provide an installation link for readers who already have Ochat and full prerequisite context for readers who do not.

Use the repository's supported installation process, including opam environment activation, dependency installation, build, and install. Commands must be validated against the actual target revision before publication. Do not introduce `curl | sh`, package-manager packages, or download binaries that the project has not shipped.

Provider setup belongs before the first model request. Explain where the key is read from and link to environment details without exposing a value in screenshots or examples. Retain the documented `API_URL` semantics; do not silently add `/v1`.

Create or open a complete prompt, show the exact launch directory, and use the documented native-local invocation. Explain the small set of necessary keys, including a documented alternate submit path if the terminal does not transmit Alt/Option+Enter.

Finish with the expected response, quit instructions, and the fact that native local mode ends with the process and does not automatically create a resumable daemon session.

### 9.4 Example data model

Examples should have a stable ID, title, summary, source path, relevant tutorial, host mode, required capabilities, and verification status. A capability badge such as “file read” or “shell command” should help the reader choose an example; it should not replace the detailed permissions documentation.

Distinguish complete runnable examples, configurable templates, and illustrative output. Do not place a “Run” action on a static example unless it performs a real, documented action.

### 9.5 Inline example source and downloads

User requirement added after P06: readers must be able to inspect connected ChatMD examples inside the website without downloading files. Provide the same inline source reader on tutorial pages and catalog entries. Display the complete approved entrypoint and expose every selected companion prompt, ChatML script, data/build file, and notice by its relative filename. Show the first tutorial entrypoint immediately and provide an explicit View source control in catalog cards. Preserve source text and visible tags, use theme-aware syntax highlighting (OCaml for ChatML source files and Markdown `chatml` fences, per the user’s explicit preference), support keyboard access and narrow screens, and keep basic reading functional without JavaScript. Generate displayed text from the same approved source bytes as downloads; never maintain a separate excerpt or execute source. Keep optional individual/bundle downloads and existing provenance/verification. Test source parity, companion navigation, no-JavaScript access, accessibility, and both themes.

Allow downloading selected tracked `.chatmd` and `.chatml` examples as source files. Publish only explicit manifest entries. Downloads must preserve the file bytes used by the tutorial, use a useful filename, and display the corresponding source link and host assumptions.

If an example is generated from a canonical snippet, record its origin and verify consistency. Never build downloads by scraping formatted HTML or copy-button labels.

### 9.6 Verification record

For each first-class tutorial record: tested commit, operating system, toolchain, host mode, example hashes, commands exercised, whether a live provider was used, observed result, and known limitations. A successful offline parse is useful evidence but is not a successful live run.

### 9.7 Runtime limitations that must survive editorial migration

The following are observed repository conditions, not hypothetical caveats to add indiscriminately to the homepage:

- **Local stdio startup:** The current standalone local stdio path can allocate a transient root before initializing the default cryptographic RNG. The maintained stdio tutorial uses an explicit private `--data-root` to avoid that path. Preserve that command and its explanation until a separately verified runtime fix changes the contract. A test harness that initializes the RNG globally is insufficient evidence that the standalone binary works without the flag. See [stdio tutorial](../../docs-src/agent-server/tutorials/stdio-client.md), [troubleshooting](../../docs-src/agent-server/troubleshooting.md#local-stdio-rng-initialization), [binary](../../bin/ochat_agent_stdio.ml), and [embedded host](../../lib/agent_server/embedded.ml).
- **Provider TLS:** `lib/io.ml` currently contains permissive TLS authenticators, and the existing permissions guide documents the outbound-provider limitation. Preserve a clear link to that guidance in relevant provider/deployment tutorials. HTTPS on the static documentation domain does not change Ochat's provider transport. Do not market the runtime as having verified provider TLS because the website has a valid certificate. See [transport implementation](../../lib/io.ml) and [documented boundary](../../docs-src/agent-server/permissions-and-security.md).
- **Shell bootstrap:** Native local, legacy local, stdio, and daemon paths differ. `--authorize-shell-manifest` must not be combined with native `--local`. The generic embedded profile requires grants and defaults to asking with denial fallback; a UI label must not imply that adding a declaration grants authority. See [CLI normalization](../../bin/chat_tui.ml), [embedded default](../../lib/agent_server/embedded.ml), and [shell host guide](../../docs-src/guide/chatmd-shell-host-integration.md).
- **Host-dependent scripting:** UI-only capabilities, approval suspension, persistence, and background lifecycle differ by host. Preserve the host qualification before examples; an experimental-language badge alone does not explain these differences.
- **Platform setup:** Keep Apple Silicon/OpenBLAS troubleshooting discoverable. Validate the exact toolchain and sandbox backend used in demonstrations. Do not infer native Windows support or identical shell confinement from the fact that the website itself works in a Windows browser.

Add a verification-state field with values such as `offline-checked`, `live-checked`, `known-limitation`, or `not-checked` to tutorial/example records. Allow a concise limitation and its source link. These fields supplement product maturity status; they are not interchangeable with `current` or `experimental`.

### 9.8 Example dependency closure and command context

An example download must include or clearly link all required local companion files: imported ChatMD, external ChatML scripts, specialist prompts, selected assets, and build files for OCaml examples. Preserve their relative layout. Downloading a parent prompt alone must not be labeled a complete runnable example when its imports are missing.

Resolve declared dependencies from the maintained example and verified source semantics, not from a generic text search for filenames. In particular, relative local agent references resolve at their declaration source; they do not gain an arbitrary process-working-directory fallback. Captured source loaders do not read uncaptured paths. [Source-loader contract](../../lib/chatmd/source_loader.mli).

Represent each command example's context explicitly: working directory, installed command versus `dune exec`, host mode, required services, and whether the command performs live model work. Show the context near the block, but keep display labels and shell prompt markers out of copied commands. Do not silently turn source-only development commands into installed commands.

## 10. Content architecture and migration

### 10.1 Authoritative sources

Keep long-form user documentation under `docs-src/`. Keep homepage composition and presentation under `website/`. Keep page metadata and route ownership in a versioned manifest at `website/config/docs-manifest.json`.

The build generates a Starlight-compatible content tree into an ignored directory that the chosen content integration reads. The P02 spike selected `website/.generated/docs/`, loaded through Astro glob with Starlight schema and processed-directory configuration. Docs, public assets, homepage data and reports are replaced together as one ignored `.generated/` snapshot. Hand-authored rich content must live elsewhere and be explicitly incorporated, so cleaning generated files cannot delete authored work.

Do not use ad hoc symlinks as the only migration mechanism: they do not solve frontmatter, canonical routes, exclusions, or link transformation. Astro content collections provide loading and schema mechanisms, but this project should use the simplest supported integration proven compatible with Starlight at the selected versions. [Astro content collections](https://docs.astro.build/en/guides/content-collections/).

### 10.2 Publication dispositions

Each source file has one of these dispositions:

| Disposition | Meaning | Search | Sitemap |
|---|---|---|---|
| `publish` | Current public page with a canonical route | Yes, unless specifically excluded | Yes |
| `compatibility` | Published for existing users with explicit status | Separately classified or excluded | Decide per page |
| `bridge` | Preserves older fragment locations and links forward | No | No |
| `repository-only` | Accessible on GitHub, not rendered as a site page | No | No |
| `deferred` | Needs a recorded review before publication | No | No |

Navigation membership is a separate field. A published library reference can be searchable without appearing in the beginner sidebar. A file cannot be both silently omitted and counted as migrated.

### 10.3 Proposed manifest schema

This is an application-owned schema, not a claim about Starlight's native configuration API:

```ts
type DocumentationEntry = {
  id: string;
  source: string;                 // Repository-relative, case-exact path
  disposition: 'publish' | 'compatibility' | 'bridge'
    | 'repository-only' | 'deferred';
  route?: string;                 // Absolute site path with trailing slash
  title: string;
  description?: string;
  section?: string;
  order?: number;
  audience?: Array<'author' | 'operator' | 'integrator' | 'contributor'>;
  kind?: 'tutorial' | 'concept' | 'guide' | 'reference' | 'index';
  status?: 'current' | 'experimental' | 'compatibility' | 'historical';
  navigation?: boolean;
  search?: boolean;
  sitemap?: boolean;
  aliases?: string[];
  fragmentAliases?: Record<string, string>;
  related?: string[];             // Other stable entry IDs
  verifiedAt?: string;
  verifiedCommit?: string;
  reviewNote?: string;
  provenance?: 'authored' | 'generated-from-code';
  generatedBy?: string;           // Repository generator, if applicable
  sourceCommit?: string;          // Revision supporting source/API links
  noindex?: boolean;              // Independent of navigation/search/sitemap
  verification?: 'offline-checked' | 'live-checked'
    | 'known-limitation' | 'not-checked';
  limitationSource?: string;
};
```

Require `route` for rendered pages and forbid it where the disposition should not create a public route. Verify that related IDs resolve and that referenced files exist in a clean checkout.

### 10.4 Frontmatter generation

Starlight uses page frontmatter and requires a title. Generate it from the manifest, using the source heading as a fallback only during migration. Set descriptions deliberately for main entry pages. [Starlight authoring](https://starlight.astro.build/guides/authoring-content/), [frontmatter reference](https://starlight.astro.build/reference/frontmatter/).

Avoid duplicate article titles: if the source's first H1 becomes the page title, remove only that title node from the generated body. Preserve any needed anchor alias. Do not strip headings with a regex that might match a code fence.

Preserve frontmatter if it is later introduced in a source document, but define precedence explicitly: source technical metadata should not unexpectedly override manifest route ownership or publication disposition. Schema validation should fail on conflicting declarations.

### 10.5 Build pipeline

```text
Tracked source inventory + publication manifest
                   |
                   v
Validate paths, disposition coverage, routes, and metadata
                   |
                   v
Parse Markdown -> transform links/heading aliases -> generate page metadata
                   |
                   v
Write generated Starlight content and selected example assets
                   |
                   v
Astro/Starlight static build -> optional verified odoc assembly
                   |
                   v
Search-index finalization -> HTML/link/asset checks -> deployable dist/
```

The actual Pagefind execution order must be verified against Starlight's integration. Do not index twice by accident. If odoc is excluded from Pagefind, document that boundary; adding odoc later may require an explicit final indexing stage.

### 10.6 Deterministic route ownership

Build an explicit source-path-to-route map before rewriting links. Normalize public routes to lowercase and kebab case where appropriate, but retain exact source paths separately. Strip `.doc.md` and index basenames through mapping rules only after collision checks.

Treat `Io.doc.md`, underscore variants, duplicate index names, and platform case sensitivity as real migration cases. A collision must fail the build with both sources listed. Never let the last discovered file win.

Reserve `/`, `/docs/`, `/api/`, asset paths, search-index paths, and error routes before assigning document routes. Verify that a custom Astro page and generated Starlight document do not claim the same URL.

### 10.7 Link transformation contract

Use a Markdown AST and, where raw HTML links are supported, an HTML parser. Do not replace `.md` globally in raw strings: that can corrupt examples, shell commands, query parameters, or prose.

| Source link type | Required output behavior |
|---|---|
| Link to published Markdown | Resolve relative to original source, then map to canonical site route |
| Fragment-only link | Preserve or map against the rendered heading table |
| Link to repository-only Markdown | Generate an explicit repository URL |
| Link to `.ml`, `.mli`, `dune`, or package file | Generate a repository source URL unless intentionally published as a download |
| Link to selected example source | Prefer example viewer/download route only when manifest declares one |
| Relative image | Copy/transform only an allowlisted asset and rewrite its output path |
| External HTTP(S) link | Preserve destination; validate separately |
| Mail link | Preserve valid mail destination |
| Unsupported executable URL scheme | Fail or reject according to the renderer policy |
| Unknown local destination | Fail with source file and link location |

Preserve query strings and fragments. Decode URL escapes only as required for resolution, then encode output segments correctly. Support reference-style links, autolinks, image links, and nested relative paths. Do not transform literal examples inside code fences or inline code.

Links outside the repository root must never become accidental file reads or copied assets. A GitHub fallback is valid only for a real tracked repository target or an explicit reviewed external URL.

### 10.8 Heading and fragment preservation

Create a heading inventory from the actual renderer, including punctuation, code spans, duplicate headings, Unicode, and setext headings. Compare source-fragment targets with generated IDs. Add explicit alias anchors when necessary.

Older forwarding pages require special care. A server redirect does not receive the browser's fragment, so it cannot generally translate `#old-heading` into a differently named new fragment. Options are:

1. Add old fragment aliases to the new canonical page when there are no collisions, then redirect the old path.
2. Keep a small bridge page with the original headings and explicit links to new sections.

Default to bridge pages for complex existing TUI forwarding documents until a complete fragment mapping is verified. Exclude bridge pages from search and the sitemap.

### 10.9 Markdown and embedded markup

Preserve plain Markdown as the source format. ChatMD tags must remain visible as code when they are examples. Converting all pages to MDX would risk interpreting XML-like syntax or braces as JSX expressions, so it is not the default migration.

Audit raw HTML and support only the required presentation structures, such as `details`, `summary`, links, and images. Treat site-authored interactive components as trusted code reviewed with the website. Do not execute arbitrary scripts found in imported prose.

### 10.10 Source-edit links

Every published source page should link to its original Markdown path on the configured repository branch. The label should be “Edit this page” or “View source,” according to the destination. Never point to generated content.

Configure repository URL and default branch centrally. Detect them during implementation, validate they are public and intended, and avoid guessing a repository slug from the project name. Store no embedded credentials in generated links.

### 10.11 Development refresh behavior

`npm run dev` should regenerate content once, start the preview server, and watch the source Markdown plus manifest. Edits should update without requiring contributors to copy files manually.

A source deletion should remove the generated page and trigger a disposition/link error if still referenced. Watch mode must not recursively watch and regenerate its own output forever. Clean only the owned generated directory and reject paths outside it.

### 10.12 Migration report

Generate a machine-readable report and a short human summary with counts for every disposition, new/removed routes, broken links, fragment aliases, unsupported fences, copied assets, and review-required entries. Do not commit volatile timestamps into reproducibility-sensitive outputs.

The sum of dispositions must equal the tracked Markdown inventory, plus any explicitly included root documents. This is the evidence that the migration has accounted for the corpus.

### 10.13 Generated API documentation

Do not publish the current local `docs/` directory merely because it exists. Build odoc from the chosen release/revision using documented toolchain steps, capture artifact provenance, and verify module links and supporting resources after mounting beneath `/api/`.

Publish only intended project API material; inspect whether the generator includes dependency documentation. Keep fonts, scripts, indexes, and CSS paths intact. Add a return link to Ochat documentation where supported without fragile string replacement across generated pages.

Initially keep odoc's specialized search distinct from main prose search. Label the boundary clearly on the API landing page. Merging the indexes is optional future work with separate relevance and payload measurements. Odoc is the OCaml documentation generator; its native artifact structure deserves its own integration check. [Odoc documentation](https://ocaml.github.io/odoc/odoc/index.html).

### 10.14 Source restructuring policy

The first migration should change source content only where it improves accuracy or onboarding. Do not rename hundreds of source files solely to make website URLs attractive.

When extracting installation or tutorial material from the README, choose one canonical source, update the README to link to it, and preserve a useful short start path for GitHub visitors. Run both repository documentation checks and generated-site link checks after the edit.

Preserve useful README heading entry points as concise sections linking to the canonical tutorial. Existing links to GitHub README fragments cannot be repaired by website redirects alone. The website importer must leave repository source links unchanged in the source files; only generated destinations become website URLs.

### 10.15 Generated Markdown ownership and exact-source tests

Some files in `docs-src/` are themselves generated artifacts with tracked output. They require a separate ownership layer from the website's generated content tree:

| Tracked Markdown | Generator/contract | Editing rule |
|---|---|---|
| `agent-server/protocol-types.md` | `Docs_inventory.protocol_types`, current protocol interfaces | Regenerate from code; do not hand-edit contract excerpts |
| `agent-server/operator-contracts.md` | `Docs_inventory.contracts`, configuration/scopes/HTTP/CLI source inventory | Regenerate; checker compares full generated content |
| `development/documentation-coverage.md` | `Docs_inventory.coverage`, documentation and interface inventory | Treat as a generated maintainer ledger |

The refresh implementation is in [docs_inventory.ml](../../test/agent_docs/docs_inventory.ml). A normal website build reads these tracked outputs and does not run an OCaml generator or refresh source files. If runtime source changes make them stale, regenerate and review them through the repository workflow before publication.

Preserve these existing validation contracts:

- [docs_check.ml](../../test/agent_docs/docs_check.ml) checks protocol excerpts and operator-contract equality. Pages declaring a current callable contract must retain the exact relevant `.mli` excerpt and unambiguous interface source link in canonical Markdown.
- [docs_chatml.ml](../../test/agent_docs/docs_chatml.ml) finds the exact `### 21.5 Moderator script contract` heading and compares the following `ocaml` fence with `docs-src/examples/chatml/moderator.chatml` before executing it offline.
- [docs_examples.ml](../../test/agent_docs/docs_examples.ml) compares five library examples against compiled test-source bytes.
- [docs_smoke.ml](../../test/agent_docs/docs_smoke.ml) recognizes selected XML fences and shell action declarations.

Website title removal, display-only language labels, and highlighted presentation operate on generated output. Changing canonical headings, fence languages, example bytes, or source paths requires a deliberate corresponding update to these checks. Do not disable checks merely to accommodate presentation changes.

When adding new canonical tutorial/example locations, inspect [test/agent_docs/dune](../../test/agent_docs/dune) and update explicit dependencies where needed. The current protocol-JSON check targets authored pages under `docs-src/agent-server/`; moving content into `docs-src/tutorials/` does not automatically retain that semantic check. Extend its selection or add an equivalent targeted check for relocated protocol examples.

### 10.16 Real Markdown edge cases and source corrections

The follow-up structural audit found 61 explicit anchor declarations across 12 documentation files. Preserve required `id`/`name` anchors through any raw-HTML handling, sanitization, heading extraction, or link transformation. Also support reader-relevant `kbd`, `br`, and inline `code` markup found in the corpus.

Two concrete rendering hazards need fixture coverage and reviewed source correction during migration:

1. [ChatMD parser notes](../../docs-src/lib/chatmd/chatmd_parser.doc.md) include an error-message table cell with XML-like tags outside code spans. Escape or code-format literal tags in the canonical source so the message remains visible instead of becoming markup.
2. [Markdown renderer notes](../../docs-src/lib/webpage_markdown/md_render.doc.md) demonstrate a fenced block inside another fence using the same delimiter length. Use a longer outer fence or another valid representation; confirm that the displayed/copyable inner example is unchanged.

The existing repository link checker is intentionally narrower than a full Markdown parser. A passing docs gate does not prove correct nested-fence rendering, safe markup treatment, or preservation of inline literal tags. Add real-page fixtures to the website checks. Do not interpret all angle brackets as dangerous HTML or all backslashes as math syntax: the evaluator prose's `\[0, 1\]` is an escaped interval, not evidence that a math-rendering dependency is required.

### 10.17 Supplemental content and publication boundaries

The 297-file inventory is limited to `docs-src/`. Add an explicit supplemental-source manifest for useful tracked material outside that directory:

- `Readme.md` and `DEVELOPMENT.md`: onboarding/contributor source or repository-link destinations.
- `prompt-examples/`: examples to review as source files, including `.md` files that contain ChatMD rather than ordinary prose.
- `real-world-example-session/update-tool-docs/`: historical session artifacts, repository-only by default; do not automatically promote complete transcripts into website search.
- `assets/tui-snapshot.png`: reviewed visual input, not proof of current runtime behavior.
- Referenced interfaces, executable sources, and build files: source links or explicit example dependencies.

Other tracked root Markdown, including older guides and history, does not become public website content merely because it is tracked. The ignored `scratch/` spec remains a local planning artifact unless deliberately moved into a tracked planning location during implementation. Record its durable implementation home so subsequent contributors can find the requirements.

Importers may read explicitly selected new files during local authoring, but CI and release builds must reject manifest entries missing from the committed checkout. Apply real-path containment and symlink checks to copied assets and dependency closures; lexical normalization alone does not prevent a symlink from escaping an allowlisted directory.

### 10.18 Map publication intent to actual framework behavior

The manifest fields are application-owned. Each needs an implemented adapter and an observable output check:

| Manifest intent | Implementation responsibility | Output assertion |
|---|---|---|
| `repository-only` / `deferred` | Do not emit a production content page | No HTML, search entry, sitemap URL, or accidental source download |
| `search: false` | Set supported Pagefind exclusion behavior | Page absent from produced search index |
| `navigation: false` | Filter configured sidebar and/or supported auto-sidebar metadata | No unwanted sidebar item; page may still be intentionally public |
| `sitemap: false` | Filter canonical URLs through the configured sitemap generator | Page absent from generated sitemap files |
| `noindex: true` | Emit applicable meta/header behavior | Deployed response/page contains noindex |
| `bridge` | Emit required compatibility headings and forward links | Old fragments remain usable; search/sitemap/noindex policy verified |

Starlight distinguishes page search exclusion, auto-sidebar hiding, and production drafts. These controls are not substitutes for each other. [Frontmatter reference](https://starlight.astro.build/reference/frontmatter/). Astro's sitemap integration exposes URL filtering; wire the manifest to that filtering rather than assuming a custom `sitemap` field automatically takes effect. [Sitemap filtering](https://docs.astro.build/en/guides/integrations-guide/sitemap/).

Default bridge pages to `search: false`, `sitemap: false`, and `noindex: true`. Preserve their content for existing links. A `repository-only` document is not secret: the classification means it stays available on the public repository without a separate website page.

### 10.19 Provenance, dates, and failure-safe generation

Use the build revision for immutable “View source” links on generated contract/API content; use the maintained branch for “Edit this page.” A mutable branch link alone cannot demonstrate that the code matches a previously deployed excerpt.

Compute last-changed metadata from original source paths and available Git history. Generated-file mtimes are not content-change dates. If a shallow checkout lacks the necessary history, fetch the required history deliberately or omit the date; do not display an invented fallback as verified freshness.

Stage generated content and route reports in an owned temporary directory, validate them, and then replace the prior generated tree. On a failed regeneration, fail the production build and show a clear development error; never report a successful refresh while serving a mixture of old and new routes. Test source deletion, manifest changes, interrupted generation, and paths containing spaces or Unicode.

## 11. Search

### 11.1 Baseline

Use Starlight's included Pagefind search initially. It indexes a built static site and provides full-text retrieval without a separately operated search backend. Validate it against the production build, not only the development server. [Starlight search](https://starlight.astro.build/guides/site-search/).

Do not couple website search to Ochat's embedding indexes or require a model API key to build or browse the documentation. The website search has a separate purpose and infrastructure contract.

### 11.2 Search interaction

- Provide a visible search control in the global header.
- Support the documented keyboard shortcut and ensure it does not interfere with focused text inputs or assistive technology.
- Focus the query input when the search dialog opens.
- Support keyboard traversal, Enter to open a result, Escape to close, and focus restoration.
- Show title, useful excerpt, and section context for each result.
- Preserve enough query state when returning from a result to continue searching, if supported without a complex custom state system.
- Keep the search dialog usable with a mobile software keyboard and at increased text zoom.
- Show a useful no-results message linking to topic navigation.

Reuse the established search implementation where possible. A custom search interface is justified only by a demonstrated usability problem, and must preserve accessible dialog behavior.

### 11.3 Corpus and relevance

Index current tutorials, concepts, guides, command/language references, and approved library prose. Exclude navigation chrome, footer repetition, bridge pages, audit ledgers, and historical research unless deliberately exposed as a separate scope.

Start with default relevance and measure representative queries. Pagefind supports explicit content weighting and filter metadata; use those mechanisms only where measured results justify them. Do not promise arbitrary synonym expansion or exact identifier behavior without testing it. [Pagefind weighting](https://pagefind.app/docs/weighting/), [filters](https://pagefind.app/docs/filtering/).

If category filters are added, use a small set: All docs, Tutorials, Reference, and Library. Filter controls are an enhancement, not a reason to delay working baseline search.

### 11.4 Search evaluation set

| Query | Expected destination or result family |
|---|---|
| install | Installation and build troubleshooting |
| first agent | Local first-agent tutorial |
| read_file | Tool declaration/catalog section |
| workspace | Workspace/path semantics and session concepts |
| `${workspace}` | ChatMD path-variable reference |
| ChatMD | Concept introduction and language reference |
| ChatML | Concept introduction and language reference |
| specialist | Specialist-agent tutorial |
| --local | TUI/local-host documentation |
| save session | Accurate persistence/host guidance |
| daemon reconnect | Unix-daemon tutorial |
| shell_access | Shell declaration and authorization guidance |
| MCP | Maintained tool integration before legacy prompt serving |
| stdio | Client tutorial and transport reference |
| HTTP authentication | HTTP tutorial and permissions reference |
| compaction | Current context-compaction material |
| Prompt_session | Relevant library prose |
| apply_patch | Current tool or library reference |

For each query, record whether the expected page appears in the top five and whether the excerpt helps distinguish it. A reasonable launch target is at least 90% of this curated set in the top five, with no misleading historical result ahead of the current first-agent or installation page. This is a proposed project benchmark, not a property guaranteed by Pagefind.

### 11.5 Search loading and failure

Load index data on demand. Do not fetch the full documentation corpus during homepage rendering. If the search bundle fails, the dialog should explain the problem and retain a link to documentation navigation. Search results and snippets must be rendered safely as text or through the library's supported sanitized rendering.

### 11.6 P07 implementation decisions

Measured baseline failures justify the shared native-dialog frontend: the default
widget lacked homepage integration and left failed loading without useful fallback.
The frontend retains Pagefind's normal ranking and supported search/data APIs.
One owned `astro:build:done` integration indexes the final HTML; Starlight's built-in
Pagefind pass is disabled because it does not expose the required `${}` indexing
character option. Keep this single-owner arrangement when upgrading dependencies.

The approved index has 106 pages: 102 current published routes and four deliberately
searchable, visibly labeled compatibility routes. All nine bridges are excluded.
Measured weighting reduces compatibility content and emphasizes authored current
descriptions. The canonical save/resume introduction and `${workspace}` heading
clarify actual host/path semantics. The 18-query benchmark remains the acceptance
gate; no query-specific reranking or synonym service was added.

Pagefind 1.5.2 can swallow failed index-chunk fetches and return an apparent empty
result. An isolated worker observes its own fetch failures around the supported
API, reports a recoverable error, and is replaced on retry. Preserve this boundary
until real fault-injection tests demonstrate that it is unnecessary. No page-global
fetch patch or generated vendor edit is permitted by this implementation decision.
Index/worker loading starts only after a nonempty query; output uses plain text,
local canonical URLs, and verified real headings. Query return state stays in local
browser history. An exact-phrase hint explains quoted searches without promising
arbitrary exact matching for unquoted input.

## 12. Code, diagrams, and demonstrations

### 12.1 Syntax-highlighting plan

| Fence type | Initial treatment | Verification |
|---|---|---|
| `ocaml` | Supported OCaml grammar | Types, variants, operators, strings, comments |
| `xml` / ChatMD examples | XML grammar with clear ChatMD labeling | Tags, attributes, embedded literal values |
| `chatml` | OCaml grammar, as explicitly requested, in Markdown and inline source readers | Display highlighting only; never claim full ChatML grammar support |
| `sh`, `bash` | Shell grammar | Multiline continuations and variable syntax |
| `console` | Terminal/output presentation | Prompt markers excluded from copy only when intentional |
| `json`, `jsonc` | Appropriate grammar | Comments handled only where permitted |
| `lisp` | Confirm supported grammar or map to a suitable display fallback | S-expression config readability |
| `mermaid` | P08: optional rendering on explicit request, with exact source always visible | Reserved scrollable viewport, failure fallback, labels, and theme contrast |
| Unknown language | Visible plain-text fallback plus build report | No dropped content or failed page rendering |

Starlight uses Expressive Code for code presentation. Configure its supported extension points rather than maintaining a second highlighting pipeline for the same blocks. [Code authoring and Expressive Code integration](https://starlight.astro.build/guides/authoring-content/).

The existing TUI grammar files may be useful reference material. Reuse requires checking licensing and format compatibility; their existence does not establish compatibility with the website highlighter.

### 12.2 Copy behavior

Copied code must match the intended source, excluding decorative line numbers, filenames, copy status, or surrounding prose. Preserve quotes, backslashes, dollar signs, angle brackets, and whitespace that matters. Test ChatMD and multiline shell commands explicitly.

Copy confirmation should be short and announced politely to assistive technology. A failed clipboard operation must not destroy the selection or obscure the code. Do not require clipboard permissions merely to read a tutorial.

### 12.3 Diagram policy

Use diagrams for relationships that are easier to understand spatially: ChatMD definition to host to tools, main agent to specialist, and local versus daemon lifecycle. Keep diagram claims at the same precision as surrounding prose.

Generate small diagrams as SVG at build time where practical. Provide an adjacent textual explanation. Diagrams should scale without clipping labels, use theme-safe colors, and avoid loading a large diagram engine into every page.

If source Mermaid is published, preserve its source separately from rendered SVG. Treat diagram tooling and its output as part of the build pipeline and asset review.

**P08 implementation decision:** Retain the labeled static homepage example and
illustrative composition, with equivalent prose and links to maintained instructions.
Mermaid rendering is an explicit opt-in enhancement using the exact canonical fence,
strict mode, a fixed scrollable viewport, and visible error/source fallback. Neutral
high-contrast diagram paper is intentional in both themes. No player or generated
model response is required. Publishing cards use deterministic local Satori/Sharp
rendering with pinned Manrope outlines; no AI bitmap generation or remote font fetch
is part of the build. Preview has no sitemap; production retains eligible routes.

### 12.4 Recording implementation

Choose between a self-hosted terminal player and a short video after measuring accessibility, readability, and payload. Default to a static poster plus explicit Play action. Do not autoplay audio, preload a large recording unnecessarily, or require an external embed to understand the page.

For a terminal recording, provide a readable transcript and test player controls. For video, provide captions if it contains speech and an equivalent explanation of the relevant visual actions. Use a sanitized demonstration workspace and check the capture for private paths, prompt contents, and credentials before publication.

### 12.5 Asset manifest

Track each published asset's source, output filename, dimensions where relevant, byte size, license/ownership, alt-text purpose, and associated page. Track demonstration provenance separately from image optimization settings.

Only copy manifest-approved assets into the deployment output. Do not copy the repository root, `scratch/`, private transcripts, local search indexes, or arbitrary generated files into `public/`.

Preserve required license/notice files when distributing project examples or reused third-party assets. The project has [LICENSE.txt](../../LICENSE.txt), and the bundled VS Code grammar assets have a separate [Microsoft license notice](../../lib/chat_tui/grammars/vscode/LICENSE.txt). Do not assume a single project-license link covers every copied grammar/font/icon asset, and do not invent a copyright owner to fill incomplete existing metadata.

## 13. Frontend architecture

### 13.1 Proposed directory structure

```text
website/
  package.json
  package-lock.json
  astro.config.mjs
  tsconfig.json
  wrangler.jsonc
  README.md
  content/
    docs-manifest.json
    examples-manifest.json
    assets-manifest.json
    redirects.json
  scripts/
    sync-docs.mjs
    validate-manifest.mjs
    check-built-site.mjs
    prepare-deployment.mjs
  src/
    content.config.ts
    content/docs/              # Generated only; ignored
    pages/
      index.astro
      404.astro                # Exact integration resolved in spike
    components/
      Brand.astro
      HeroExample.astro
      DemoPanel.astro
      FeatureSection.astro
      LearningPathCard.astro
      DocsStatus.astro
    styles/
      tokens.css
      global.css
      docs.css
    assets/
      brand/
      demo/
  public/
    favicon.svg
    robots.txt                # May be generated per environment instead
  tests/
    content/
    browser/
  .generated/
    route-map.json
    migration-report.json
  dist/                       # Build output; ignored
```

Names are proposed implementation structure. Do not create empty component abstractions merely to match this tree. Keep scripts small enough to understand, and split only when responsibilities justify it.

### 13.2 Integration boundaries

- Astro owns static page composition and asset output.
- Starlight owns the documentation shell and established accessibility behavior.
- The content pipeline owns publication disposition, route mapping, and source-link resolution.
- The manifest owns editorial ordering and page metadata.
- The deployment configuration owns domain/environment behavior.
- Odoc owns generated API pages and their specialized assets.

Do not make domain selection part of Markdown source. Do not make the documentation importer responsible for runtime deployment credentials.

### 13.3 Starlight customization strategy

Start with CSS tokens and supported configuration. Override components only where branding or reader behavior requires it. Keep overrides shallow, document their purpose, and test them when upgrading Starlight. Official component overrides are supported extension points; replacing the entire shell increases upgrade work. [Starlight overrides](https://starlight.astro.build/guides/overriding-components/).

Candidate customizations are branding, article status metadata, footer content, and a carefully integrated header. Do not replace navigation, search focus management, or code rendering solely to change their color.

### 13.4 Static-output contract

All first-release content routes render to HTML during the build. The website has no model-execution endpoint, session database, or server-side rendering requirement. Essential content must be available without browser JavaScript.

Interactive scripts should be limited to navigation controls, theme preference, search, copy actions, and optional media playback. Use framework components only when an interaction benefits from them; a React dependency is not required just to render cards.

### 13.5 Proposed local commands

These scripts are requirements for the future website package, not commands that exist today:

```sh
cd website
npm ci
npm run dev
npm run check
npm run build
npm run preview
npm run test:content
npm run test:browser
```

`build` should generate content automatically. Contributors must not need an undocumented prebuild step. `check` should include schema and type checks. `test:content` should exercise route/link/fragment behavior. `test:browser` should run against the production build with search available.

### 13.6 Configuration inputs

| Input | Purpose | Production rule |
|---|---|---|
| Site origin | Absolute canonical URLs and sitemap | Required, HTTPS, selected domain |
| Repository URL | Source and GitHub links | Required, public intended repository |
| Repository branch | Edit-source destination | Required, actual maintained branch |
| Deployment environment | Production or preview behavior | Explicit; never inferred solely from hostname guesses |
| Source revision | Artifact provenance | Required in build metadata |
| API artifact location | Optional verified odoc assembly | Omit API navigation if unavailable |

No model API keys are required for a normal website build. Domain and deployment credentials remain in the hosting/CI environment, never in public build variables.

### 13.7 Tracking and ignore rules

Verify `website/src/` files are tracked despite the existing broad ignore rule. Add narrowly scoped exceptions if needed and keep generated `website/.generated/docs/`, `node_modules/`, `dist/`, and cache/report directories ignored.

Use `git check-ignore -v` and a clean-checkout build as acceptance evidence. A website that runs only because of ignored local source files is not complete.

### 13.8 Build isolation

The normal website build should require the website toolchain and tracked content, not a local opam installation. Run Ochat semantic checks and odoc generation in dedicated jobs/environments when their inputs change.

The optional API artifact may be assembled from a verified CI artifact at the same source revision. Record that relationship explicitly; do not quietly combine current prose with an unrelated old API dump.

### 13.9 Dune and website coexistence

Keep JavaScript dependencies and generated website content out of Dune's normal directory scanning. Git ignore rules alone do not define Dune's build scope. The inspected checkout has no root `dune` or `dune-workspace` file establishing this boundary.

For an independently built `website/`, the proposed root Dune boundary is:

```lisp
(dirs :standard \ website)
```

Merge this into any root `dune` file present at implementation time rather than overwriting unrelated settings. Dune documents that excluded directories are not scanned for build rules; `data_only_dirs` is another supported choice when data-only treatment is specifically needed. Select the simplest appropriate boundary and verify it with the actual toolchain. [Dune directories](https://dune.readthedocs.io/en/stable/reference/dune/dirs.html), [data-only directories](https://dune.readthedocs.io/en/stable/reference/dune/data_only_dirs.html).

After the first npm installation and site generation, run a normal relevant Dune build and the documentation gate. Check that package fixtures or copied example `dune` files inside `website/` do not introduce projects, rules, formatting work, or duplicate targets into the OCaml build. Conversely, building the static website must not require a preexisting `_build/` tree.

Keep the website out of broad OCaml source-tree artifact assembly unless explicitly required. Test both the clean repository and the normal developer state containing `node_modules/` and generated website files.

## 14. Accessibility and responsive behavior

### 14.1 Accessibility target

Target WCAG 2.2 AA. Relevant requirements include keyboard access, text contrast, reflow, visible focus, focus not obscured, meaningful labels, and target sizing. Normal text generally needs 4.5:1 contrast; large text 3:1; applicable non-text controls need 3:1. WCAG's minimum target-size criterion includes exceptions; this project's larger touch-target preference below is a design choice. [WCAG 2.2](https://www.w3.org/TR/WCAG22/).

Automated checks support this target but do not establish full conformance. Keyboard, zoom, mobile, and screen-reader review remain necessary.

### 14.2 Project interaction requirements

| Interaction | Required behavior |
|---|---|
| Search/menu dialog | Focus enters on open, stays within modal, Escape closes, focus returns |
| Theme switch | Labeled control; selection understandable without color alone |
| Code copy | Keyboard reachable; result announced; source remains readable on failure |
| Sidebar disclosure | Expanded state exposed; standard button behavior |
| Table of contents | Real links to stable anchors |
| Demo playback | Explicit controls and equivalent text explanation |
| Tabs, if used | Labeled panels and a complete keyboard model |
| External links | Meaningful labels; no unexplained icon-only navigation |

The WAI-ARIA dialog pattern documents modal focus and labeling behavior. Prefer a proven implementation and verify it after customization. [Modal dialog pattern](https://www.w3.org/WAI/ARIA/apg/patterns/dialog-modal/).

### 14.3 Responsive layout matrix

| Viewport category | Proposed behavior |
|---|---|
| Narrow phone, around 320–479 px | Single-column content, compact header, in-flow contents disclosure |
| Phone/small tablet, around 480–767 px | Wider code area, stacked hero and cards |
| Tablet/small laptop, around 768–1099 px | Evaluate sidebar availability based on actual content width |
| Desktop, around 1100–1439 px | Sidebar and article; contents rail if article measure remains comfortable |
| Wide desktop, 1440 px and above | Full three-region docs shell within a maximum width |

Breakpoints should be chosen from content stress tests rather than device names alone. At 400% zoom, the layout must still behave like a narrow viewport without losing navigation or essential text.

### 14.4 Manual review checklist

**Launch-scope update (2026-09-07):** The user deferred manual accessibility review and explicitly removed it as a launch requirement. This checklist is retained for later review; automated accessibility tests remain required. No manual review or full conformance is claimed.

- Reach the main article through a skip link.
- Navigate the entire page using only the keyboard.
- Open and close menu/search controls and verify focus restoration.
- Increase text size to 200% and browser zoom to 400%.
- Read the page at a 320 CSS-pixel viewport.
- Check representative pages with VoiceOver/Safari and, where available, NVDA/Firefox.
- Check reduced-motion mode and both themes.
- Check high-contrast/forced-color behavior on important controls.
- Ensure sticky elements do not hide a focused control or linked heading.
- Use a mobile software keyboard while searching.
- Confirm image alternatives explain purpose rather than filenames.
- Confirm code and tables remain usable without a mouse.

### 14.5 Touch and pointer behavior

Prefer at least 44-pixel interactive areas for primary mobile controls, while balancing dense navigation appropriately. Do not require precise hover movement to follow a menu. Hover effects should have corresponding focus behavior and should not be the only signal that something is clickable.

## 15. Performance

### 15.1 Performance objectives

Core Web Vitals identify LCP, INP, and CLS as key user-experience metrics. Their good thresholds are LCP at or below 2.5 seconds, INP at or below 200 milliseconds, and CLS at or below 0.1, evaluated at the 75th percentile of visits. Use these as field targets once traffic supports measurement. [Web Vitals](https://web.dev/articles/vitals).

At launch, use repeatable lab checks as proxies. Do not claim field performance from Lighthouse alone.

### 15.2 Proposed asset budgets

| Surface | Initial budget | Measurement scope |
|---|---:|---|
| Homepage eager JavaScript | At most 100 KB gzip | Excludes user-triggered demo/search downloads |
| Typical documentation eager JavaScript | At most 150 KB gzip | Includes shell behavior, excludes deferred index shards |
| Shared CSS | At most 100 KB gzip | First-load CSS across the page |
| Initial font transfer | At most 150 KB total | Prefer fewer files and self-hosted subsets |
| Hero poster | At most 250 KB | Appropriate dimensions and modern format |
| Homepage initial transfer | At most 800 KB | Excludes on-demand recording and search |
| Layout shift | At most 0.1 | Reserve image/player dimensions |
| Normal content build | Target under 5 minutes | Reference CI runner; excludes fresh OCaml/odoc toolchain setup |

These are project budgets to validate during the spike. Record a justified exception when an inherited library or essential content exceeds a budget; do not silently remove functionality to satisfy a number.

### 15.3 Implementation techniques

Render article text and code at build time. Use responsive images, explicit dimensions, lazy loading below the fold, and deliberate font loading. Avoid third-party scripts on the critical path. Load recording players only when needed.

Do not add a global client-side router solely for transition effects. Browser-native navigation is sufficient for the initial documentation experience. If view transitions are introduced, test focus, anchors, search state, and script lifecycle carefully.

### 15.4 Long-document handling

Measure the ChatML reference and protocol-types page separately from short tutorials. Source length alone is not a reason to split a document, but excessive HTML, highlighting output, or navigation cost may justify a carefully planned split.

If a long page is split, maintain old anchors through compatibility handling and give the reader a clear index. Do not impose pagination on a language reference purely to make source files smaller.

## 16. Discovery, metadata, and GitHub

### 16.1 Metadata requirements

Each indexable page needs a unique, meaningful title, a concise description where useful, a canonical URL, and consistent site identity. Homepage title proposal: “Ochat — build your own AI agents in text files.” Documentation titles should put the topic first and append Ochat consistently.

Generate a sitemap from canonical indexable routes. Exclude bridge pages, previews, and unreviewed material. Use stable routes and redirect changed URLs deliberately. Validate absolute canonical URLs against the actual production origin before launch.

### 16.2 Social sharing

Create a shared social-preview template containing the Ochat wordmark, page/topic title, and restrained code-related visual styling. Homepage art can be bespoke; documentation cards should come from a deterministic template. Verify preview assets load publicly and have appropriate dimensions for common link previews.

Do not include changing claims such as star counts or performance numbers in static preview art.

### 16.3 Search-engine content

Use descriptive headings that match real reader questions: installation, first agent, ChatMD tools, ChatML workflows, daemon sessions, and OCaml integration. Avoid repeated keyword blocks or hidden search-engine-only copy.

Experimental and compatibility labels must be present in the visible page, not only metadata. Prefer current guidance in internal links so external search results lead visitors into a maintained route.

### 16.4 Preview indexing policy

Preview deployments should emit `noindex` through a response header or page metadata and should not publish a production sitemap as if they were canonical. `robots.txt` alone is not a reliable deindexing mechanism. Test the deployed preview response because local HTML checks do not verify hosting headers.

Use one explicit environment flag to control these differences. Production validation must fail if preview-only noindex behavior remains active.

### 16.5 GitHub integration

At launch:

1. Set the repository's About → Website field to the chosen canonical domain.
2. Add a prominent documentation link near the README introduction.
3. Preserve a compact GitHub-readable explanation and setup path.
4. Link the website header/footer back to the intended repository.
5. Point article edit links to original Markdown.
6. Add a contribution link and a factual website development guide.
7. Consider a social-preview image for the repository if the owner wants consistent presentation.

Updating GitHub settings and purchasing/attaching a domain are launch actions, not performed by writing this specification. No repository URL or available domain is assumed to be established by this document.

### 16.6 Machine-readable documentation

Optional later work: expose selected raw Markdown or an `llms.txt`-style index for agent consumption. Treat this as a convenience format whose usefulness should be validated, not as a search-ranking guarantee or a universal standard.

Any such export must use the same publication manifest and exclusions as the website. Do not publish repository-only audits, private artifacts, or duplicate outdated content through a second export path.

## 17. Domain and deployment

### 17.1 Domain shortlist

| Candidate | Naming rationale | Current status |
|---|---|---|
| `ochat.dev` | Short and aligned with a developer framework | Availability and price unverified |
| `useochat.com` | Action-oriented alternative | Availability and price unverified |
| `getochat.dev` | Clear fallback retaining the project name | Availability and price unverified |

The implementation should use a configurable origin until a domain is purchased. Do not put any candidate into production canonicals or documentation as an owned domain before ownership is established.

### 17.2 Registrar decision

Cloudflare Registrar is the default recommendation because it advertises registration/renewal without registrar markup and integrates with the proposed DNS/hosting setup. Compare the full registration and renewal cost at checkout. A different registrar remains possible. [Cloudflare Registrar](https://www.cloudflare.com/products/registrar/).

Cloudflare-registered domains must use Cloudflare nameservers while registered there. Record this tradeoff before purchase; it does not prevent hosting the website elsewhere with appropriate DNS records. Complete required registrant-email verification. [Registration requirements](https://developers.cloudflare.com/registrar/get-started/register-domain/).

Do not treat a DNS lookup, unavailable website, or search-engine result as proof that a domain can be registered. Confirm availability and total price in the registrar's purchase flow. Check renewal price, any premium classification, required term, and the account that will own the domain.

### 17.3 Domain layout

Use the chosen apex domain as the canonical homepage and `/docs/` for documentation. Redirect `www` to the canonical hostname if configured. A `docs.` subdomain is unnecessary for the initial single-site deployment.

If a future application needs a separate host, reserve its naming then. Do not make the current architecture depend on an unplanned application subdomain.

### 17.4 Static hosting contract

Deploy only the verified `website/dist/` output. Cloudflare Workers Static Assets currently provides free unlimited static-asset requests and no additional storage charge; Worker-script invocations have separate pricing. Build quotas and platform limits still need checking. [Static asset billing](https://developers.cloudflare.com/workers/static-assets/billing-and-limitations/).

The initial site should not require a Worker script. Preserve ordinary static delivery for HTML, CSS, JavaScript, fonts, downloads, and images. A future dynamic endpoint requires a separate decision and cost review.

### 17.5 Proposed Wrangler configuration

This is a starting sketch to validate with the pinned deployment CLI. It deliberately omits a domain until one has been selected:

```json
{
  "name": "ochat-website",
  "compatibility_date": "2026-09-06",
  "assets": {
    "directory": "./dist",
    "not_found_handling": "404-page",
    "html_handling": "auto-trailing-slash"
  }
}
```

Use the actual implementation/test date for the final compatibility date. The chosen Worker name must match the configured hosting project. Keep Astro's emitted URL structure consistent with the host's HTML handling.

Cloudflare documents static-site asset routing and a `404-page` mode that serves a custom error document with a 404 status. Validate both document navigation and direct asset requests. [Static-site routing](https://developers.cloudflare.com/workers/static-assets/routing/static-site-generation/).

### 17.6 Build and release automation

Cloudflare can connect a Worker to GitHub or GitLab and build/deploy on push. Configure the repository root and website build path so that `docs-src/` remains available to the importer. Do not use a checkout containing only `website/`. [Workers Builds](https://developers.cloudflare.com/workers/ci-cd/builds/).

Choose one production deployment owner. Recommended initial arrangement:

- GitHub checks validate proposed changes.
- A protected production branch receives changes only after required checks pass.
- Cloudflare Builds publishes that branch after running the website's build/check pipeline.
- Preview branches use preview deployments and noindex behavior.

If later moving to a GitHub Actions deployment job to assemble an odoc artifact, disable duplicate Cloudflare production deployment triggers. Deploy the already-validated artifact rather than a separately rebuilt, unverified variant when practical.

Build-watch paths must include `website/**`, `docs-src/**`, relevant root documentation, selected example/assets paths, and any API source inputs when odoc is included. Changes only to unrelated runtime code need not rebuild prose unless the release process requires it.

### 17.7 Preview policy

Every website change should be reviewable through a local production preview or a hosted preview. Hosted previews for untrusted pull requests must not receive production credentials. Use the hosting platform's supported isolation or keep such contributions on build-only validation until trusted publication.

A preview should identify its source revision in build metadata, use the same rendering pipeline as production, and make deployment-specific differences explicit. Do not present a preview URL as the permanent project domain.

### 17.8 Headers and redirects

Generate static `_headers` and `_redirects` files where the hosting platform supports them. Use redirects for verified old-path mappings and hostname consolidation, checking for loops and unexpected chains. Keep fragment-specific logic out of path-only server rules. [Headers](https://developers.cloudflare.com/workers/static-assets/headers/), [redirects](https://developers.cloudflare.com/workers/static-assets/redirects/).

Set content-type handling appropriately, prevent MIME sniffing, and use a reasonable referrer policy. Design a Content Security Policy against actual built assets if introduced; Starlight theme scripts and code styles may need hashes or other allowances. Do not deploy an untested strict policy that breaks search, highlighting, or theme initialization.

Use caching appropriate to immutable hashed assets and document updates. Avoid manually applying long immutable caching to mutable HTML paths. Verify response headers on the deployed host rather than assuming local files enforce them.

### 17.9 Custom domain attachment

After the site is ready:

1. Confirm domain ownership, registrar verification, and DNS account access.
2. Attach the domain through the Worker's Custom Domains configuration.
3. Confirm certificate issuance and HTTPS response behavior.
4. Configure the canonical hostname and any alternate-host redirects.
5. Update the production site origin and rebuild metadata/sitemaps.
6. Verify homepage, nested documentation, static assets, downloads, and 404s.
7. Update GitHub links only after the domain works.

Cloudflare documents Custom Domains for routing requests to a Worker and managing the associated domain configuration. Follow that flow rather than guessing a CNAME target from a preview URL. [Custom Domains](https://developers.cloudflare.com/workers/configuration/routing/custom-domains/).

### 17.10 Cost model

| Item | Expected initial treatment | Verification needed |
|---|---|---|
| Domain | Annual or registrar-required term payment | Actual availability, price, renewal, taxes/fees |
| Static hosting | Target free tier | Current static-asset and build limits |
| Search | Build-time/client-side Pagefind | No separate search subscription planned |
| CI | Existing GitHub/Cloudflare allowances | Actual account usage and plan |
| Fonts/icons | Prefer appropriately licensed self-hosted assets | License and any acquisition cost |
| Demo storage | Small static asset budget | Per-file and total-platform constraints |
| Model usage | None for normal site builds or visits | Separate charges only for deliberately recorded live demonstrations |

No exact annual domain total is promised before the candidate is checked. Do not use temporary promotional pricing as the long-term budget.

### 17.11 Rollback

Retain the previous working deployment/artifact and its source revision. If a release breaks critical navigation or rendering, restore the previous known-good site first, then correct the source. Document the exact host rollback procedure in the implementation runbook and exercise it in a preview environment.

If a release changes public routes, retain compatible redirects when rolling forward again. A rollback should not require purchasing a new domain or changing registrars.

### 17.12 Deployment limits and complete-artifact checks

Count final output files and maximum file size after search shards, media, example bundles, and optional odoc output have been assembled. Counting Markdown pages alone is not a hosting-capacity check.

At review time, Workers Static Assets documents a Free-plan limit of 20,000 files per version and 25 MiB per individual file; Paid raises file count but retains the individual-file limit. The same reference gives limits for header and redirect rules. Treat these as dated deployment constraints, recheck before launch, and fail artifact validation with an actionable report before an upload exceeds the selected plan. [Workers platform limits](https://developers.cloudflare.com/workers/platform/limits/).

Workers Builds separately documents 3,000 build minutes per month on Free, one concurrent build, and a 20-minute build timeout. A free static request allowance does not remove build limits. Account for repeated previews and optional OCaml/API generation when choosing the CI arrangement. [Build limits and pricing](https://developers.cloudflare.com/workers/ci-cd/builds/limits-and-pricing/).

Do not add larger storage services preemptively. Optimize or split an oversized recording using a reviewed delivery decision only if actual assets require it. Include deployment-limit results in the release evidence bundle.

### 17.13 Enforced release checks and environment-specific artifacts

Define which system enforces each required check; a check in a document is not a deployment gate. A minimal arrangement is:

| Input change | Required check | Enforcement owner |
|---|---|---|
| Website styles/components/config | Website build/content checks and relevant browser checks | Required repository check before production merge |
| Canonical docs/examples | Website checks plus relevant `@agent-docs-check` coverage | Required repository check with OCaml environment |
| Code-backed generated docs/interfaces | Generated-source freshness and semantic docs gate | Repository OCaml job before publication |
| Host configuration/redirects/origin | Deployed preview routing/header checks | Release job/rehearsal before promotion |
| Included odoc artifact | API generation, provenance, and mounted-link checks | API job and final artifact-assembly check |

Configure branch protection or a tested equivalent so production deployment cannot race ahead of required checks. If a hosting build deploys directly on push, production-branch changes must already have passed the required checks, or the hosting build must enforce the complete applicable gate itself.

Keep documentation drafts, deploy previews, and production environments distinct. A development-only Starlight draft is not a mechanism for a hosted production-style preview. A preview with noindex metadata is not the exact production artifact: production canonical/noindex changes require their own build and relevant validation before publishing. Reuse common validated source inputs and tests while recording each environment's artifact hash and origin.

Scope credentials and account access to the deployment job; ordinary docs checks and untrusted contribution builds should not receive them. A website build must not inherit model credentials as public variables or perform external agent/tool calls while evaluating imported content.

When watch-path filtering is used, changes to `lib/`, `bin/`, generator/test sources, and package metadata must still trigger the semantic/generated-doc checks that depend on them. A prose-only hosting trigger may skip an unchanged site build, but it must not hide stale source-backed contracts from repository CI.

## 18. Testing and release evidence

### 18.1 Test strategy

Test high-risk behavior: source resolution, canonical routes, legacy fragments, copied example bytes, navigation/focus, search, and deployment routing. Avoid tests that merely assert arbitrary component names or repeat CSS declarations.

Playwright supports browser automation and is appropriate for checking the rendered production experience. Use accessible roles and observable behavior in tests rather than brittle internal selectors. [Playwright documentation](https://playwright.dev/docs/intro).

### 18.2 Content-pipeline test cases

| ID | Case | Required result |
|---|---|---|
| C01 | Relative link to sibling Markdown | Resolves to canonical route |
| C02 | Link through multiple parent directories | Resolves using original source location |
| C03 | Link to repository `.mli` | Correct source-host link |
| C04 | Link to repository-only audit | Explicit GitHub destination, no missing site page |
| C05 | Two sources normalize to one route | Build fails with actionable collision report |
| C06 | Case-sensitive module filename | Works in Linux clean checkout |
| C07 | Markdown-looking text in code fence | Bytes remain unchanged |
| C08 | Source H1 and page title | One visible article title; aliases preserved |
| C09 | Duplicate/punctuated headings | Rendered IDs match link checker |
| C10 | Old TUI fragment maps to new section | Alias/bridge behavior reaches correct content |
| C11 | Source file deleted | Generated output removed; unresolved references fail |
| C12 | Unknown language fence | Explicit fallback and report; content retained |
| C13 | Relative image or example | Only manifest-approved output copied |
| C14 | Encoded filename/query/fragment | Correct resolution without double encoding |
| C15 | Reference-style Markdown link | Definition destination transformed correctly |
| C16 | Raw HTML link | Parsed and handled consistently |
| C17 | Destination escapes source root | Rejected; no arbitrary filesystem publication |
| C18 | Manifest lacks a source disposition | Coverage validation fails |
| C19 | Draft/deferred entry linked from primary nav | Validation fails before deployment |
| C20 | Same input built twice | Stable routes and content hashes, excluding declared provenance fields |
| C21 | Generated Markdown contract or exact tested snippet | Source ownership/parity checks retained; display transformation does not rewrite canonical bytes |
| C22 | Existing explicit `a` anchor and `kbd`/inline `code` | Required anchors and semantic markup survive rendering |
| C23 | XML-like prose and nested code fences | Literal content remains visible; valid nesting and exact intended copy bytes |
| C24 | Example with specialist/import/script dependencies | Complete declared dependency layout or an explicit incomplete-template label |
| C25 | Symlink inside an asset/example source directory | Escaping target rejected; no unapproved publication |
| C26 | Hidden/search-excluded/noindex/repository-only page | Actual HTML/index/sitemap/header outcomes match each independent policy |
| C27 | Shallow Git history or generated-file mtime | No fabricated last-updated date; immutable source links use correct revision |
| C28 | Failed or interrupted content regeneration | Build fails clearly; no success with mixed old/new generated output |
| C29 | Website dependencies/generated examples present during Dune build | OCaml build scope remains isolated and docs gate works |
| C30 | Preview artifact promoted toward production | Origin/noindex changes validated; environment-specific provenance recorded |
| C31 | Final media/search/API artifact grows | File count, maximum size, and deployment-rule limits checked before upload |

Use small focused fixtures plus real representative source pages. Tests should validate user-facing consequences of transformations.

### 18.3 Browser scenarios

| ID | Scenario | Pass condition |
|---|---|---|
| B01 | Homepage first-agent action | Reaches correct tutorial with usable heading |
| B02 | Documentation sidebar | Active state and group expansion match route |
| B03 | Search identifier | Relevant result opens at correct page/section |
| B04 | Search keyboard flow | Open, type, select, close, and focus restoration work |
| B05 | ChatMD copy | Clipboard content matches intended source |
| B06 | Multiline shell copy | Continuations and variables preserved |
| B07 | Theme selection | Both themes readable; preference persists where available |
| B08 | Mobile navigation | Drawer usable; no page overflow or hidden focus |
| B09 | Direct fragment load | Correct heading visible below sticky header |
| B10 | JavaScript disabled | Main content, navigation links, and code remain accessible |
| B11 | Missing page | Branded error with real 404 on host preview |
| B12 | Source-edit link | Opens original Markdown path |
| B13 | Demo unavailable | Poster/explanation remains useful |
| B14 | Search asset fails | Clear error and topic-navigation fallback |
| B15 | Long table/code | Local scrolling; body width stays within viewport |
| B16 | Old explicit heading alias after full source migration | Direct navigation reaches the intended section without duplicate IDs |
| B17 | Source/API page on a deployed revision | View-source uses matching immutable revision; edit-source uses maintained branch |
| B18 | Example bundle download | Files, relative imports, names, and required notices match the catalog claim |

Run core smoke scenarios in Chromium, Firefox, and WebKit at supported versions. Use a smaller focused screenshot suite for visual regression; do not snapshot all 297 pages by default.

### 18.4 Visual regression matrix

Capture homepage, first-agent tutorial, ChatMD reference, ChatML reference, library page, search open, mobile menu open, and 404. Cover light/dark desktop and representative narrow/mobile layouts. Use stable fixtures for recordings and avoid time-dependent counters.

Review screenshot changes as evidence of intended design differences; do not automatically accept large diffs merely because the build passes.

### 18.5 Repository semantic checks

When changing runnable examples or technical instructions, run `dune build @agent-docs-check` in the appropriate OCaml environment and any directly relevant existing checks. Do not run live-provider tests as part of every website build.

A CSS-only change does not require rebuilding the entire Ochat runtime. A tutorial command change requires more than a browser screenshot. Match verification to the change.

### 18.6 Link-check policy

Fail CI for broken generated internal routes, missing local assets, unresolved published fragments, and malformed source links. External links should be checked periodically with retries and a reviewed exception list; transient remote rate limits should not be confused with a deterministic importer failure.

Check redirects for loops and excessive chains. Check both trailing-slash forms on the deployment preview when the host canonicalizes URLs.

### 18.7 Release evidence bundle

Each release should retain:

- Source revision and lockfile/toolchain identity.
- Content disposition and route report.
- Build result and relevant content/browser check results.
- Representative screenshots for a design release.
- Search evaluation results for search or corpus changes.
- Tutorial verification records for changed instructions.
- API artifact provenance if published.
- Deployment URL and verified response checks.

Evidence should be compact enough to review. Do not require rebuilding unchanged evidence without a new risk or change.

## 19. Implementation phases and work packages

### 19.1 Sequencing

Build one complete reader journey first, then expand the corpus. The first reviewable artifact should be a real homepage plus first-agent page using actual Ochat content. This establishes the design and content integration together.

Dependencies:

```text
WP01 foundation/spike
        |
        +--> WP02 identity + WP03 homepage
        |
        +--> WP04 migration --> WP05 docs shell --> WP06 tutorials
                                    |
                                    +--> WP07 search and examples
        |
        +--> WP08 optional API artifact

WP03 + WP04 + WP05 + WP06 + WP07
        |
        v
WP09 validation and polish --> WP10 production launch --> WP11 maintenance
```

These dependencies describe task sequencing, not a requirement to use parallel agents or separate teams.

### 19.2 Work-package detail

| Package | Work | Deliverables | Exit criteria |
|---|---|---|---|
| WP01 Foundation | Scaffold compatible Astro/Starlight; resolve ignore rules; prove representative content and static preview | Website package, lockfile, six-page spike, version record | Clean checkout builds and routes correctly |
| WP02 Identity | Develop wordmark, tokens, typography, surfaces, code treatment | Shared styles and brand assets | Homepage and reference prototype coherent in both themes |
| WP03 Homepage | Build hero, example, demonstration, explanations, use cases, learning links | Fully responsive homepage | Claims traced to docs; all actions work |
| WP04 Migration | Inventory, manifest, route map, AST transformations, heading aliases, reports | Import pipeline and disposition coverage | Every source accounted for; no unresolved internal links |
| WP05 Docs shell | Sidebar, article metadata, TOC, mobile navigation, source links | Reusable reading interface | Keyboard/mobile/long-page checks pass |
| WP06 Tutorials | Polish T01–T04; publish scoped advanced tutorials; extract shared source | Canonical tutorials and verification records | Beginner journey works; no duplicated maintained prose |
| WP07 Search/examples | Search corpus/relevance, example catalog, selected downloads | Search evaluation and example manifest | Query target met; copied/downloaded bytes correct |
| WP08 API | Build and mount fresh odoc artifact | Provenance and API landing/search | Links/assets resolve under `/api/`; otherwise deferred explicitly |
| WP09 Validation | Content/browser/accessibility/performance checks; fix defects | Evidence bundle | Launch gates satisfied |
| WP10 Launch | Select domain, configure hosting, verify metadata and redirects, update GitHub | Working canonical domain and runbook | Production responses and links verified |
| WP11 Maintenance | Document contributor flow, updates, checks, rollback | Website development/operations guide | Another contributor can build and edit a page |

### 19.3 Suggested milestone deliverables

**Milestone A — design proof:** homepage, first-agent page, one long reference, mobile navigation, and both themes. Uses real source content and actual code examples.

**Milestone B — content beta:** full disposition inventory, approved current content, working search, examples, source links, and compatibility handling. Available as a reviewable preview.

**Milestone C — release candidate:** editorial review, accessibility and browser checks, measured performance, final brand/media assets, and deployment rehearsal.

**Milestone D — public launch:** owned domain, verified deployment, updated GitHub entry points, and a documented maintenance path.

### 19.4 Effort estimate

For one experienced implementer, a planning range is approximately 10–20 focused working days for a polished first release, assuming the existing docs are mostly accurate and a domain can be registered normally. This is a rough estimate, not a commitment or research-derived industry benchmark.

The largest uncertainty is editorial/compatibility work across the corpus, followed by visual iteration and tutorial verification. A minimal preview can arrive much earlier; a fresh odoc build or extensive technical corrections can extend the full launch.

Re-estimate after WP01 and the first migration report. Record the actual number of deferred pages and broken/ambiguous links before promising a date.

### 19.5 Priority order when scope must be reduced

Protect first-agent correctness, readable docs, stable links, accessibility, and search. Reduce decorative media, advanced search filters, extra use-case pages, or initial API-site integration before compromising those essentials.

Do not call a release complete by silently omitting promised routes. Remove unfinished navigation and record deferred work explicitly.

### 19.6 How to execute the implementation phases

The work packages above group responsibilities. The phases below provide the execution order: establish the build, prove the content pipeline, build a complete reader journey, expand the documentation, and then prepare and launch the site. Use this phase sequence when deciding what to implement next; the earlier work-package diagram expresses dependencies rather than a strict construction order.

Each phase has prerequisites, ordered tasks, concrete deliverables, and a completion gate. All tasks start unchecked because this specification does not establish that implementation has begun. Check off a task only when its deliverable exists and its relevant verification passes.

Use task IDs such as `P02.04` in implementation notes, issues, or PR descriptions. Maintain the required persistent working-memory file at `scratch/ochat-website-implementation-notes.md` as specified in Section 19.22. Keep its phase/task state consistent with checkboxes in this specification; a linked issue or PR may provide additional evidence but does not replace the memory file. Record source revision, evidence, unresolved defects, and any explicitly deferred work when completing a phase.

Apply these execution rules:

- Deliver a working increment at the end of every phase. Keep the current preview usable as the next phase is added.
- Validate the behavior introduced in the current phase immediately. The release-quality phase consolidates evidence and addresses cross-cutting defects; it is not the first time testing begins.
- Preserve the authoritative source and route contracts established in earlier phases. Revisit them explicitly if evidence requires a change.
- Use focused changes that can be reviewed independently. Avoid combining an unrelated visual redesign, content rewrite, and deployment change into one unreviewable increment.
- Distinguish incomplete work from optional scope. A required task cannot be declared finished because another phase would be more interesting to start.
- Continue local development and validation while external inputs such as a domain decision are pending. Only work that actually depends on that input must wait.
- Phase gates are implementation checks, not mandatory requests for user permission. Seek a missing decision only where existing direction does not settle it; do not ask the user to reauthorize ordinary implementation work.
- This phase plan describes future implementation. Writing or updating the plan does not itself purchase a domain, publish a site, or change GitHub settings.
- Read the persistent implementation notes before resuming work after a new session or context compaction, and checkpoint them after meaningful progress and before a handoff or pause. Do not rely on conversation history alone for implementation state.

### 19.7 Phase overview and dependency map

| Phase | Main result | Depends on | Work packages | Milestone |
|---|---|---|---|---|
| P00 | Baseline and implementation decisions recorded | Existing repository and this spec | WP01, WP04 | Preparation |
| P01 | Reproducible website foundation | P00 | WP01 | Technical foundation |
| P02 | Representative source docs publish through a tested importer | P01 | WP01, WP04 | Technical foundation |
| P03 | Shared visual system and reading interface | P02 | WP02, WP05 | Design proof in progress |
| P04 | Complete homepage-to-first-agent journey | P03 | WP03, WP06 | Milestone A |
| P05 | Full corpus accounted for and approved docs published | P04 | WP04, WP05 | Content beta in progress |
| P06 | Tutorial curriculum and example catalog | P05 | WP06, WP07 | Content beta in progress |
| P07 | Searchable, connected documentation experience | P05, P06 | WP07 | Milestone B |
| P08 | Finished media, branding, and discoverability | P04, P07 | WP02, WP03, WP09 | Release candidate in progress |
| P09 | Verified API reference or explicit deferral | P05; complete before final release review if included | WP08 | Optional release addition |
| P10 | Validated release candidate and deployment rehearsal | P07, P08; P09 resolved | WP09, WP10 | Milestone C |
| P11 | Public custom-domain launch and GitHub links | P10 and required account/domain inputs | WP10 | Milestone D |
| P12 | Maintenance handoff and post-launch follow-through | P11 | WP11 | Operational completion |

The default path is sequential. Work with independent inputs may overlap when useful: brand assets can develop during content migration, API generation can be investigated after P05, and domain availability can be checked early. Such overlap does not remove the phase's completion criteria or require multiple agents.

```text
P00 -> P01 -> P02 -> P03 -> P04 -> P05 -> P06 -> P07 -> P08
                                      |                       |
                                      +-> P09 API decision ---+
                                                              |
                                                              v
                                                    P10 -> P11 -> P12
```

### 19.8 Phase P00 — Establish the baseline and make implementation decisions

**Objective:** Turn the research specification into a current, bounded implementation starting point.

**Prerequisites:** Access to the intended checkout and this specification. No domain or hosting account is required for this phase.

Ordered tasks:

- [x] **P00.01 — Confirm the working baseline.** Record the current branch/revision, inspect applicable repository instructions, and identify existing changes that implementation must preserve.
- [x] **P00.02 — Refresh the content inventory.** Recount tracked documentation and selected assets. Compare against the 297-page research baseline and record additions, deletions, or moves.
- [x] **P00.03 — Verify repository identity.** Resolve the intended public repository URL and maintained branch for source links without publishing credentials or assuming the repository name is `ochat`.
- [x] **P00.04 — Confirm launch scope.** Adopt the first-release requirements in Section 1, identify mandatory tutorial/reference pages, and record optional API-site integration separately.
- [x] **P00.05 — Create an implementation decision record.** Record framework choice, package manager, generated-content ownership, provisional route policy, hosting target, and unresolved questions from Section 21.3.
- [x] **P00.06 — Identify representative source fixtures.** Select the six documents required by the technical spike, including a forwarding page and embedded-markup/diagram coverage.
- [x] **P00.07 — Record external inputs.** List the domain choice, registrar account, hosting account, and launch access needed later. Use a local/preview origin for development until a real origin is known.
- [x] **P00.08 — Initialize persistent implementation memory.** Create `scratch/ochat-website-implementation-notes.md` using Section 19.22. Seed it with the baseline, current phase/task, decisions, risks, verification needs, and the next concrete action. Read and maintain this file throughout implementation, including across sessions and context compaction.
- [x] **P00.09 — Record supplemental sources and planning ownership.** Account for tracked root docs, prompt collections, historical transcripts, and code/example companions outside the 297-file corpus. Choose a durable tracked home for implementation requirements instead of relying on an ignored scratch file indefinitely.

**Deliverables:** Updated inventory baseline, implementation decision record, representative-fixture list, and phase progress log.

**Completion gate:** The implementer can identify the content source, route owner, first-release scope, and next build task without guessing. Pending domain/account decisions are recorded and do not prevent P01.

### 19.9 Phase P01 — Scaffold a reproducible website foundation

**Objective:** Create the smallest website project that reliably installs, builds, and previews from tracked files.

**Prerequisites:** P00 complete.

Ordered tasks:

- [x] **P01.01 — Resolve compatible versions.** Select supported Astro, Starlight, Node, and deployment-tool versions. Record the exact combination and use a committed lockfile. **Audit status:** Exact versions and the website lockfile are publicly committed in `a40dbd208e85d90873f4b960727a79448c9c5a78` on PR #20. Actual clean Linux qualification is recorded under P10.11; earlier temporary-clone checks remain historical evidence.
- [x] **P01.02 — Create `website/`.** Scaffold static output, minimal configuration, a custom root homepage, and one documentation route. Keep the homepage and documentation route owners distinct.
- [x] **P01.03 — Configure tracking.** Address the existing broad `src/` ignore rule with narrow exceptions. Ignore dependency, build, cache, and generated-content directories without hiding authored source.
- [x] **P01.04 — Define package commands.** Provide initial `dev`, `check`, `build`, and `preview` commands. Later phases can extend their work, but normal usage must be documented from the start.
- [x] **P01.05 — Centralize site configuration.** Establish repository URL, branch, source revision, and preview/production origin inputs. Do not hard-code an unowned domain into canonical metadata.
- [x] **P01.06 — Establish basic build validation.** Add a lightweight CI configuration or equivalent reproducible check that installs from the lockfile and builds the website. Keep deployment credentials out of these checks.
- [x] **P01.07 — Verify a clean checkout.** Run installation and build using only tracked inputs, including a Linux environment. Check `git check-ignore -v` on representative authored files.
- [x] **P01.08 — Document local development.** Write the initial `website/README.md` with prerequisites, commands, generated-output boundaries, and the current implementation status.
- [x] **P01.09 — Isolate the website from Dune.** Implement the reviewed directory boundary from Section 13.9; verify the OCaml build and docs gate with installed npm dependencies and generated website/example files present.

**Deliverables:** Tracked website scaffold, lockfile, version record, minimal build checks, and a working local production preview.

**Completion gate:** A clean checkout can build a custom homepage and documentation page without local OCaml tools, ignored source files, a model API key, or undocumented setup.

**Verification focus:** Route ownership, static HTML output, source tracking, and reproducible installation. Visual polish is not a gate at this stage.

### 19.10 Phase P02 — Build and prove the documentation import pipeline

**Objective:** Render representative existing Markdown through the architecture that will later publish the full corpus.

**Prerequisites:** P01 complete and representative source fixtures selected in P00.

Ordered tasks:

- [x] **P02.01 — Define and validate the manifest.** Implement the application-owned schema from Section 10.3, including source paths, publication dispositions, canonical routes, metadata, and related-page references.
- [x] **P02.02 — Build the route map first.** Validate source existence, reserved paths, case normalization, and duplicate ownership before any page generation occurs.
- [x] **P02.03 — Generate compatible content.** Parse source Markdown, inject page metadata, and handle the source H1 without creating duplicate titles. Write only to the owned generated directory.
- [x] **P02.04 — Resolve and rewrite links structurally.** Implement the Section 10.7 contract for published Markdown, repository-only files, code links, relative assets, queries, fragments, and reference-style links.
- [x] **P02.05 — Handle rendered headings and old links.** Inventory actual heading IDs, add needed aliases, and prove one real TUI forwarding page through a bridge or verified alias mapping.
- [x] **P02.06 — Preserve code and markup.** Render representative XML/ChatMD, ChatML, shell, OCaml, raw HTML, and Mermaid content with explicit fallbacks where needed. Keep code bytes unchanged.
- [x] **P02.07 — Make development regeneration reliable.** Connect generation to build/dev commands, support source edits and deletions, and prevent watch loops or writes outside generated output.
- [x] **P02.08 — Add focused transformation tests.** Implement the high-risk fixtures from Section 18.2, especially collisions, path escape, code preservation, missing destinations, and fragment handling.
- [x] **P02.09 — Produce a spike report.** Build all six representative pages, verify source-edit links, and record unresolved renderer differences before expanding the corpus.
- [x] **P02.10 — Preserve generated-source ownership.** Mark protocol/operator/coverage Markdown as generated-from-code, retain source-parity checks, and keep normal website builds from refreshing canonical repository outputs.
- [x] **P02.11 — Exercise real Markdown hazards.** Add fixtures for existing explicit anchors, keyboard markup, XML-like prose, and nested fences from Section 10.16; make any necessary canonical source corrections with repository checks.
- [x] **P02.12 — Make generation failure-safe.** Stage and validate output before replacing the generated tree; check interrupted generation, symlink containment, deletions, and paths containing spaces or Unicode.
- [x] **P02.13 — Implement publication adapters.** Map manifest disposition/search/navigation/sitemap/noindex fields to real framework behavior and assert their independent output effects.

**Deliverables:** Tested importer, validated manifest shape, representative generated pages, route/heading report, and reliable regeneration commands.

**Completion gate:** The six-page spike builds and renders correctly. No representative example is silently modified; unknown links and collisions produce actionable errors; old fragment behavior has a demonstrated solution.

**Scope boundary:** The full corpus does not need final dispositions yet. Keep unselected pages explicitly outside the spike's publication set, and use legitimate repository links for references to them.

### 19.11 Phase P03 — Implement the visual system and documentation shell

**Objective:** Establish the shared presentation and reading behavior before multiplying pages.

**Prerequisites:** P02 complete.

Ordered tasks:

- [x] **P03.01 — Implement shared design tokens.** Add the proposed semantic colors, typography, spacing, radii, and focus styles. Measure real foreground/background contrast and adjust the initial palette where necessary.
- [x] **P03.02 — Build the provisional brand assets.** Create a usable wordmark and small vector mark; confirm legibility at header and favicon sizes. Final media polish can follow in P08.
- [x] **P03.03 — Style the documentation shell.** Establish article measure, sidebar, global header, contents rail, and footer using Starlight's supported configuration and minimal overrides.
- [x] **P03.04 — Add article context.** Render section context, title, descriptions, relevant status labels, source links, and factual update/verification metadata.
- [x] **P03.05 — Implement navigation states.** Provide clear current-page treatment, collapsible groups, anchor offsets, and intentional previous/next navigation.
- [x] **P03.06 — Implement narrow-screen layouts.** Add mobile documentation navigation, an in-flow contents disclosure, and local scrolling for wide code/tables.
- [x] **P03.07 — Complete both theme states.** Follow system preference by default, support an explicit override, and keep the page usable when browser storage is unavailable.
- [x] **P03.08 — Verify interaction basics.** Check keyboard traversal, focus restoration for overlays, skip links, zoom/reflow, reduced motion, and code-copy feedback on representative pages.
- [x] **P03.09 — Capture a design baseline.** Save representative desktop/mobile screenshots of a tutorial and a long reference in both themes; note deliberate design choices and remaining polish work.

**Deliverables:** Shared styles, initial brand assets, reusable documentation shell, responsive navigation, and a baseline visual review.

**Completion gate:** A short tutorial and a long reference are readable and navigable on desktop and mobile in both themes. Main content remains useful without JavaScript. No known focus trap, hidden active control, or body-level horizontal overflow remains in the tested examples.

**Implementation evidence (2026-09-06):** `website/planning/milestone-a-review.md` records the completed local baseline, 55 passing browser scenarios (two clipboard skips), 12 visual combinations, measured contrast and full Tab traversals. P03 uses 200% root text and 320 CSS-pixel reflow; native browser-chrome zoom, screen readers and physical mobile keyboards remain P10 release checks.

### 19.12 Phase P04 — Complete the homepage-to-first-agent journey

**Objective:** Deliver the first coherent product experience using real Ochat content.

**Prerequisites:** P03 complete. Content expansion beyond this journey remains limited until the experience is coherent.

Ordered tasks:

- [x] **P04.01 — Establish canonical onboarding sources.** Choose where installation and first-agent prose live in `docs-src/`; extract or improve existing material without creating two separately maintained versions.
- [x] **P04.02 — Finish the first-agent tutorial.** Include prerequisites, working directories, provider setup, a complete prompt, the actual launch command, submission keys, expected result, exit behavior, and persistence qualifications.
- [x] **P04.03 — Build the homepage hero.** Implement the product message, complete code example, separate launch command, primary first-agent action, and verified repository link.
- [x] **P04.04 — Build the explanation sections.** Add define/compose/control explanations, concrete use cases, execution choices, learning links, and footer content from Section 7.
- [x] **P04.05 — Add an honest demonstration treatment.** Use a verified recording if ready; otherwise use a clearly labeled static walkthrough with a useful explanation and reserved media dimensions.
- [x] **P04.06 — Connect the entire journey.** Ensure every visible call to action has a working destination. Link unfinished advanced website content to appropriate maintained repository pages until their public routes are ready.
- [x] **P04.07 — Verify technical instructions.** Run relevant existing offline checks and record any live-run verification separately. Confirm copied ChatMD and shell content matches the maintained source.
- [x] **P04.08 — Review as a new reader.** Walk from homepage through installation and first response, including failure links and quit behavior. Correct unexplained terms, missing prerequisites, and assumptions about prior OCaml knowledge.
- [x] **P04.09 — Produce Milestone A.** Capture the working homepage, first-agent page, long reference, mobile navigation, and both themes in a local or hosted preview.

**Deliverables:** Complete homepage, canonical installation/first-agent pages, connected navigation, tutorial verification record, and Milestone A preview.

**Completion gate:** A reader can move from understanding the product to following the documented local-agent workflow without encountering a placeholder, fabricated command, or unexplained server requirement. Record any remaining live-verification limitation honestly; do not market the tutorial as live-verified if only offline checks ran.

### 19.13 Phase P05 — Migrate and organize the full documentation corpus

**Objective:** Account for every source document and publish the approved current material into a coherent structure.

**Prerequisites:** P04 complete and the importer/shell contracts proven.

Ordered tasks:

- [x] **P05.01 — Assign every source a disposition.** Review the refreshed inventory and mark publish, compatibility, bridge, repository-only, or deferred with an explanation where needed.
- [x] **P05.02 — Finalize canonical route mappings.** Apply the proposed structure in Section 5, resolve collisions, and preserve stable source identifiers independently of public URLs.
- [x] **P05.03 — Publish introductory and task-oriented content.** Import topic indexes, host concepts, operational guides, and current user references first.
- [x] **P05.04 — Publish reviewed library prose.** Inspect sidecars for historical/API-status ambiguity, attach accurate labels, and group approved pages by subsystem.
- [x] **P05.05 — Complete compatibility handling.** Map all relevant older TUI headings, verify bridge pages, and place legacy command material in the intended compatibility area.
- [x] **P05.06 — Build curated navigation.** Implement documentation-home paths, sidebar groups, topic hubs, related links, and deliberate reference ordering without exposing filesystem naming as the primary interface.
- [x] **P05.07 — Validate the complete built graph.** Check every generated route, asset, source link, and required fragment. Verify that repository-only targets resolve to appropriate repository URLs.
- [x] **P05.08 — Stress-test dense content.** Inspect long references, deep heading structures, code-heavy pages, tables, and unusual syntax. Add focused fixtures for newly discovered transformation cases.
- [x] **P05.09 — Generate the migration report.** Reconcile all disposition counts to the refreshed inventory and list every deferred page, compatibility rule, and source-content correction.
- [x] **P05.10 — Close capability and supplemental-source coverage.** Reconcile Section 5.6 against published hubs/search metadata and Section 10.17 against explicit supplemental dispositions.
- [x] **P05.11 — Preserve validation after source moves.** Update Dune dependency declarations and semantic-check selection where canonical tutorials or examples move; do not lose protocol JSON or exact snippet validation through a directory rename.

**P05 completion checkpoint (2026-09-06):** All eleven tasks are complete. Every canonical source has a reviewed disposition: 99 publish, 4 compatibility, 9 bridges, 175 repository-only, 10 explicitly deferred (297 total; 112 rendered documentation routes). All 119 legacy bridge headings and their forwarding destinations are checked. Curated navigation and the framework map cover fourteen capabilities; 1,250 supplemental sources have explicit policy coverage. The approved corpus passes the complete built graph, all source-heading checks, 43 unit tests, zero Astro diagnostics, and the full three-engine browser suite (109 passed, two expected clipboard skips). The forced offline docs gate passes for 297 pages and 38 methods, including three corrected shell hooks compiled through the real runtime. No canonical paths or semantic fixtures moved. This closes the approved-corpus migration gate; the ten named deferrals and intentional repository references are documented, not published as finished pages. P06 is next. See `website/planning/p05-completion-review.md`, `p05-route-map.md`, and `p05-capability-coverage.md` for evidence and exact boundaries. No live provider, remote CI, or deployment verification is implied. Earlier checkpoints below are historical.

**P05 first-group checkpoint (2026-09-06):** 15 additional canonical sources imported; 37 rendered documentation pages (36 publish, 1 bridge), 260 deferred, 297 accounted for. Curated topic navigation and docs-home paths are implemented. P05.09 now generates complete JSON/Markdown inventories with deferral reasons, compatibility policy, and current source-change evidence. Other P05 tasks remain open for full editorial, compatibility, and supplemental coverage. See `website/planning/p05-migration-review.md` for details.

**P05 hosting checkpoint (2026-09-06):** Another 15 sources published, bringing the total to 52 rendered docs (51 publish, 1 bridge), 245 deferred, 297 accounted for. Hosting tutorials, shared example setup, all agent-server Markdown, and daemon/stdio command references now resolve locally. The generated operator flag inventory was corrected in its owning generator; daemon reference retention/batch wording was corrected canonically. The forced offline documentation gate passes. Other P05 tasks remain open for broader editorial/compatibility/supplemental coverage. See `website/planning/p05-hosting-review.md`.

**P05 batch and shell checkpoint (2026-09-06):** Nine more reviewed sources published: 61 rendered docs (60 publish, 1 bridge), 236 deferred, 297 accounted for. The batch guide now uses a tracked tool-free prompt and private output directory, explains continuation/output-root semantics, and preserves its old first-section anchor. Runtime, tools, security, persistence, extensions, management, resource-helper, and internals pages resolve locally. Canonical source-directory/finalized-output claims were corrected; the offline gate checks prepared batch ChatMD without a provider call. Browser coverage passes across three engines after correcting a test selector. Partial shell examples remain deferred for stale ChatML snippet corrections; broader P05 tasks remain open. See `website/planning/p05-shell-review.md`.

**Deliverables:** Full publication manifest, canonical route map, curated documentation structure, compatibility mappings, and corpus-wide migration report.

**Completion gate:** Every source has a recorded disposition; every published route has one owner; the approved corpus has no unresolved internal links or required fragments. Deferred documents are explicit and do not masquerade as finished website pages.

### 19.14 Phase P06 — Finish tutorials and the example catalog

**Objective:** Turn the available content into an intentional learning progression with trustworthy examples.

**Prerequisites:** P05 complete. T01 already exists from P04.

Ordered tasks:

- [x] **P06.01 — Finish T02, adding tools.** Show a complete file-tool declaration, workspace assumptions, expected behavior, and the difference between instructions and capabilities.
- [x] **P06.02 — Finish T03, specialist agents.** Provide the parent and specialist files, exact relative-path assumptions, an example request, and a verifiable expected interaction.
- [x] **P06.03 — Finish T04, headless execution.** Show input and output files, execution directory, transcript inspection, and the documented behavior of existing output files.
- [x] **P06.04 — Review T05–T10.** Publish maintained advanced tutorials with explicit prerequisites, host modes, status, and verification scope. Correct technical ambiguities before promoting them in learning paths.
- [x] **P06.05 — Normalize tutorial anatomy.** Apply prerequisites, steps, expected observations, troubleshooting, cleanup/persistence, and next-step sections consistently without forcing every tutorial into identical prose.
- [x] **P06.06 — Create the example manifest and catalog.** Distinguish complete runnable examples, configurable templates, and illustrative output. Link each entry to a relevant tutorial or reference.
- [x] **P06.07 — Publish selected source downloads.** Copy only approved tracked example files, preserve their bytes, and provide descriptive filenames and source provenance.
- [x] **P06.08 — Verify commands and examples.** Run relevant offline checks, perform feasible explicitly scoped live verification, and record commit/platform/host assumptions for each first-class tutorial.
- [x] **P06.09 — Connect the learning progression.** Ensure completion panels and previous/next links guide readers from T01 through T04 and into appropriate advanced topics.
- [x] **P06.10 — Validate example dependency closures.** Include required imports, scripts, specialist prompts, assets, build files, and notices in each complete download or label the example as a template with missing prerequisites.
- [x] **P06.11 — Preserve observed runtime qualifications.** Check the current stdio data-root workaround, provider TLS limitation, host-specific shell flags, scripting capabilities, and platform setup against Section 9.7 before publishing new tutorial claims.

**P06 completion checkpoint (2026-09-06):** All eleven tasks are complete. T01–T10 have connected source-based instructions, explicit host/prerequisite/cleanup context, previous/next links, and source-hash-scoped verification records. New file-tool, specialist, and external ChatML workflow tutorials bring the canonical inventory to 300 and rendered documentation routes to 115 (102 publish, 4 compatibility, 9 bridges, 175 repository-only, 10 deferred). The catalog contains four complete source examples, five configurable templates, and one illustrative reading sample; nine deterministic plain `.tar` bundles and 23 individual destinations preserve all selected source/data/build companions and the original license. Exact catalog selection and supplemental approval control every copied file. The final gate passes 51 unit tests, zero Astro diagnostics, a 117-HTML-page build, and 124 browser tests across three engines with two expected clipboard skips. The forced offline docs gate passes 300 pages/38 methods; five actual provider-free CLI checks validate standalone stdio, Unix/HTTP hosting and canonical shell inspection. Every final saved archive matches its declared bytes; gzip-related browser transformations were resolved by using plain tar. No live provider response, physical TUI interaction, shell sandbox execution, remote CI, or public deployment is claimed. See `website/planning/p06-completion-review.md` for all eleven gate decisions and exact limitations. P07 is next.

**Deliverables:** Polished T01–T04, scoped advanced tutorials, example catalog, approved downloads, and verification records.

**Completion gate:** Every promoted tutorial has complete instructions and an honest verification record. Example downloads and copy actions preserve intended source. No advertised “Run” interaction pretends the static website executes an agent.

### 19.15 Phase P07 — Integrate and evaluate documentation search

**Objective:** Make exact technical information and appropriate learning pages easy to find.

**Prerequisites:** P05 and P06 complete so search is evaluated against a meaningful corpus. Search smoke checks may have run earlier during the spike.

Ordered tasks:

- [x] **P07.01 — Confirm indexing order.** Integrate Pagefind with the final Starlight build and ensure the corpus is indexed once at the correct stage.
- [x] **P07.02 — Apply publication/search boundaries.** Include approved current pages; exclude bridges, deferred material, repetitive navigation, and unapproved history.
- [x] **P07.03 — Integrate the search control.** Make it available from homepage and docs with consistent styling, labeling, keyboard access, focus management, and mobile behavior.
- [x] **P07.04 — Complete search states.** Check initial, loading, results, no-results, and failed-load behavior with useful navigation fallback.
- [x] **P07.05 — Run the query evaluation set.** Measure the expected destinations for Section 11.4 queries, especially punctuation-heavy identifiers, host flags, and maintained versus legacy MCP material.
- [x] **P07.06 — Tune only demonstrated relevance problems.** Improve page titles, corpus selection, supported weighting, or optional filters based on recorded results; avoid speculative custom search infrastructure.
- [x] **P07.07 — Check loading and output integrity.** Verify deferred index loading, correct canonical result paths, safe excerpts, and links to real headings.
- [x] **P07.08 — Produce Milestone B.** Combine the approved corpus, tutorials, examples, search, source links, and compatibility behavior into a content-beta preview.

**Deliverables:** Working search experience, explicit index scope, relevance evaluation, search browser tests, and Milestone B preview.

**Completion gate:** The proposed search benchmark is met or a specific measured deviation is resolved through an explicit scope decision. Search correctly distinguishes current entry points from history, and its keyboard/mobile/error behavior works against a production build.

**P07 completion checkpoint (2026-09-07):** All eight tasks and Milestone B are complete. The optimized local content-beta preview combines the approved corpus, ten tutorials, inline examples/downloads, source provenance, and shared homepage/docs search. Exactly 106 approved pages are indexed in one final-build pass; all 18 benchmark queries have an expected family in the top five (100%), with maintained entry points ahead of misleading compatibility results. The evaluator verifies 1,752 fragment destinations. Final validation passes 56 unit tests, zero Astro diagnostics, the 117-HTML-page build, and one full three-engine browser run with 163 passed and two existing clipboard skips (165 cases). The forced offline docs gate passes 300 pages and 38 methods. Six final screenshots cover desktop/mobile, both themes, empty/error states, and 200% text. See [the P07 completion review](p07-completion-review.md) for the query/excerpt review, implementation decisions, and evidence. P08 is next; custom-domain hosting, physical-device/screen-reader release checks, and the actual P01.01 commit remain later/open work.

### 19.16 Phase P08 — Finish presentation, media, and discoverability

**Objective:** Turn the functional content beta into a polished public-facing website.

**Prerequisites:** P04 and P07 complete. Earlier brand exploration can be reused and refined.

Ordered tasks:

- [x] **P08.01 — Finalize the visual identity.** Refine the wordmark, symbol, typography, colors, surfaces, and consistent homepage/docs treatments using the actual corpus.
- [x] **P08.02 — Finalize the demonstration.** Record and verify the chosen current workflow, or retain a deliberately labeled static treatment. Include usable controls, an equivalent explanation, asset provenance, and failure fallback.
- [x] **P08.03 — Finish explanatory diagrams.** Implement only useful, source-grounded diagrams; verify text alternatives, small-screen readability, theme behavior, and payload.
- [x] **P08.04 — Produce publishing assets.** Create favicons, social-preview art, appropriate image sizes, and the completed asset/license manifest.
- [x] **P08.05 — Implement page metadata.** Add unique titles, meaningful descriptions, canonical generation, sitemap filtering, and explicit preview/production indexing behavior.
- [x] **P08.06 — Finish error and auxiliary surfaces.** Polish 404, empty/error states, footer, contribution links, compatibility labels, and any optional API-entry treatment.
- [x] **P08.07 — Perform whole-site editorial review.** Check terminology, capitalization, claim accuracy, navigation labels, and stale homepage references after content migration.
- [x] **P08.08 — Measure and optimize payloads.** Compare page families to Section 15 budgets; resize assets, defer nonessential scripts/media, and fix layout shifts without removing necessary content.
- [x] **P08.09 — Verify provenance and notices.** Derive dates from original source history, distinguish immutable view-source from branch edit links, preserve required license notices, and retain useful README heading entry points.

**Deliverables:** Final visual assets, demonstration treatment, metadata/sitemap behavior, polished supporting states, and an initial performance report.

**Completion gate:** The site has a consistent visual identity, all public media has provenance, metadata derives from configuration, and no unfinished presentation or misleading product claim remains on the principal reader journeys.

**P08 completion checkpoint (2026-09-07):** All nine tasks are complete. The site retains its reviewed visual identity, explicitly labels the static README demonstration, offers opt-in diagrams with source/failure fallback and usable loading controls, and generates 116 deterministic social cards plus four PNG icon sizes. Metadata, original-source dates, media/font notices, unique titles, canonical/social URLs, and separate preview/production indexing policies are checked against built output. The production fixture emits exactly 103 sitemap URLs; preview emits none. Five measured page families meet all asset/CLS budgets. Final validation passes 59 unit tests, zero Astro diagnostics, and the 117-HTML-page/529-file build. One full 177-case browser run passes 175 with two existing clipboard skips; a subsequent localized diagram-control refinement passes the final 54-case presentation/site run (52 passed, two existing skips). Both artifacts and exact verification scopes are recorded. The offline gate passes 300 pages/38 methods; design review covers 12 combinations and 60 contrast pairs with zero failures. See [the P08 completion review](p08-completion-review.md) for the nine task decisions, final artifact, editorial review, performance table, and evidence. P09 is next. P01.01's actual checkout commit and P09–P12 remain open; the temporary production fixture is not a public deployment.

### 19.17 Phase P09 — Publish the OCaml API reference or record its deferral

**Objective:** Resolve API-reference scope explicitly before the release candidate is finalized.

**Prerequisites:** P05 complete. This is an optional implementation branch, but the decision to include or defer it is required.

Ordered tasks:

- [x] **P09.01 — Decide release inclusion.** Assess toolchain readiness and reader value. Record whether `/api/` will ship in this release; do not assume the local generated `docs/` directory is publishable.
- [ ] **P09.02 — Establish fresh generation if included.** Document the odoc toolchain and build a project-scoped artifact from the intended source revision. **Not applicable — deferred by the confirmed P09 decision.** No release API artifact is selected; the local diagnostic build is not release-generation evidence.
- [ ] **P09.03 — Inspect artifact scope.** Identify dependency material, generated scripts/fonts/styles, module indexes, and any files that should not be published. **Not applicable — deferred by the confirmed P09 decision.** No generated API files are approved for publication in this release.
- [ ] **P09.04 — Mount and validate the artifact.** Assemble it under `/api/` and check nested module links, resource URLs, specialized search, and the return path to the main docs. **Not applicable — deferred by the confirmed P09 decision.** No artifact is mounted; local assessment found broken targets and fragments.
- [ ] **P09.05 — Define search boundaries.** Keep specialized API search distinct initially and ensure the prose index does not accidentally ingest duplicated or unreviewed generated content. **Not applicable — deferred by the confirmed P09 decision.** No hosted specialized API search is included; accidental API input is rejected from prose indexing.
- [ ] **P09.06 — Record provenance and build ownership.** Tie the artifact to its revision and CI job; integrate it into the deployment output without introducing competing production deployment triggers. **Not applicable — deferred by the confirmed P09 decision.** No API generation, assembly, or deployment job is enabled for this release.
- [x] **P09.07 — Apply the chosen navigation outcome.** If verified, expose the API link. If deferred, omit `/api/` navigation and keep the OCaml integration landing page useful with current source/library links.

**Deliverables:** Either a freshly generated, verified API artifact with provenance, or a recorded deferral and a complete alternative OCaml integration entry point.

**Completion gate:** The chosen outcome is explicit and reflected in navigation, search, sitemap, and deployment assembly. If deferred, tasks P09.02–P09.06 are marked not applicable with a reason rather than falsely checked as implemented.

**P09 completion checkpoint (2026-09-07):** The user confirmed API-reference deferral for this release. P09.01 and P09.07 are complete; P09.02–P09.06 are explicitly not applicable, not falsely checked as implemented. Local generation succeeds but yields 271 warnings, three missing local targets, and 43 missing fragments; the historical checked-in snapshot also omits current agent libraries. The OCaml integration guide now pairs nine architecture guides with current public interfaces and documents local generation. Navigation, Pagefind input, sitemap, and final output enforce the API exclusion, and build evidence records the decision. See [the P09 completion review](p09-completion-review.md) for diagnostic scope, release boundaries, verification, and the conditions for reopening inclusion. P10 is next; no API artifact or public deployment is claimed.

**Pre-P10 design completion (2026-09-07):** The user selected Graphite + blue and approved the shared UX improvements. The selected light/dark theme now applies throughout the site, including favicon and social artwork. The shorter homepage, five navigation families, clearer typography, instructions-first tutorials, and inline file-picker/copy/wrap controls are permanent. Temporary palette controls and Stone styling are removed; obsolete palette URLs/settings cannot change the selected design. See [the UI design completion review](ui-design-completion-review.md) for implementation and validation evidence. This work does not start or complete P10. Preserve the user-confirmed P09 API deferral.

**Application-led UI follow-up (2026-09-07):** The user approved and requested implementation of all six recommendations: outcome-led homepage copy, a recorded inspectable workflow, application gallery, purposeful workflow visuals, intuitive curriculum/docs/search navigation, and benefit-first capability explanations. Added six canonical application guides, a tutorial curriculum, four complete source bundles, and a live-model documentation-review capture with explicit provenance and reproduction. Inline readers have desktop file navigation and stable file permalinks. Preserve the selected Graphite theme, Start here sequence, canonical content ownership, and P09 deferral. See [the application UI review](application-ui-review.md) for detailed implementation and validation. This does not advance P10.

### 19.18 Phase P10 — Validate the release candidate and rehearse deployment

**Objective:** Establish that the complete site is ready for public hosting and can be deployed and restored reproducibly.

**Prerequisites:** P07 and P08 complete; P09 inclusion or deferral resolved.

Ordered tasks:

- [x] **P10.01 — Run final content validation.** Reconcile the current source inventory, dispositions, routes, fragments, downloads, source links, and asset manifest against the release candidate.
- [x] **P10.02 — Run the browser matrix.** Execute relevant Section 18 scenarios in Chromium, Firefox, and WebKit against the production build. Investigate failures before accepting screenshots.
- [ ] **P10.03 — Perform manual accessibility review.** Complete keyboard, screen-reader sampling, zoom/reflow, reduced-motion, theme, and mobile-software-keyboard checks from Section 14. **Deferred by the user on 2026-09-07; not required for this launch.** Retain as follow-up work, not as a completed review. Automated accessibility checks remain in CI.
- [x] **P10.04 — Review visual regressions and performance.** Inspect the representative page/state matrix and confirm measured budgets or documented exceptions.
- [x] **P10.05 — Complete relevant Ochat semantic checks.** Run existing checks for changed commands/examples and reconcile tutorial verification records with their visible claims.
- [x] **P10.06 — Configure the deployment artifact.** Validate Wrangler settings, build root, static routing, headers, redirects, noindex behavior, watch paths, and the single production-deployment owner.
- [x] **P10.07 — Rehearse hosted behavior.** Use an authorized preview environment to check HTTPS, nested routes, missing pages, source downloads, cache behavior, search assets, and preview indexing headers. If account access is pending, complete local/static checks and record hosted rehearsal as outstanding.
- [x] **P10.08 — Rehearse rollback.** Restore a prior known-good preview deployment/artifact using the documented procedure, then return to the candidate and verify it.
- [x] **P10.09 — Assemble Milestone C evidence.** Record revision, toolchain, migration/search/test reports, screenshots, performance measurements, API provenance if applicable, and all unresolved release blockers.
- [x] **P10.10 — Validate complete artifact capacity.** Count final files, maximum asset size, and header/redirect rules, and check the selected build-plan limits before upload.
- [x] **P10.11 — Verify actual release enforcement.** Confirm required checks and input watch paths cannot be bypassed by the production trigger; distinguish preview and production artifact hashes, origins, and indexing behavior.

**Deliverables:** Verified release candidate, evidence bundle, deployment configuration, and a tested launch/rollback runbook.

**Completion gate:** Required launch checks pass; any accepted limitations are specific and recorded; hosted rehearsal is complete before promotion to production. Local success alone must not be recorded as verified custom-domain or hosting behavior.

**P10 qualification checkpoint (2026-09-07):** Local implementation and release-artifact checks are complete. P10.01/.02/.04/.05/.06/.08/.09/.10 are complete with evidence; P10.08 uses the explicitly permitted artifact rollback option. The production fixture passes 208 browser checks (two existing clipboard skips), 30 visual/axe combinations, seven-route performance and 21-query search gates; final local checks pass 68 unit tests and the 308-page/38-method offline semantic gate. Local Wrangler verifies routing, headers, downloads, ETags and candidate → previous artifact → candidate recovery. P10.03 manual review is deferred from launch by the user. P10.07 hosted rehearsal is now complete: the authorized workers.dev preview passes 3,579 HTTP assertions per stage, 18 browser checks on both candidate and restored candidate, and actual baseline → candidate → baseline → candidate version transitions. Detailed evidence is recorded in `website/planning/p10-hosted-rehearsal.md` and `scratch/ochat-website-evidence/p10-hosted/`. P10.11 is now complete: [actual clean GitHub run 34173359224](https://github.com/dakotamurphyucf/ochat/actions/runs/34173359224) passes the semantic gate, both environment matrices, and `release-gate`. Strict main protection applies to administrators and requires the GitHub Actions gate; actual missing/failed-check pushes were rejected on an identically protected temporary branch. Sources and lockfile are publicly committed on PR #20, closing P01.01. Legacy Pages automatic builds are disabled; no production publisher is enabled. Preview/production artifacts retain separate origins, hashes and indexing evidence. See [the GitHub enforcement record](p10-github-enforcement.md), [P10 release-candidate review](p10-completion-review.md), and runbook. P10 and Milestone C are **complete within the approved launch scope**, with P10.03 explicitly deferred. P11 must qualify the owned production origin and add the single protected publisher; no custom-domain launch is claimed.

### 19.19 Phase P11 — Launch on the custom domain and connect GitHub

**Objective:** Make the verified site publicly discoverable at its permanent address.

**Prerequisites:** P10 complete and the necessary domain/account access and authorization are available. Reuse existing authorization rather than introducing redundant approval steps.

Ordered tasks:

- [x] **P11.01 — Confirm the domain purchase details.** Check actual availability, full purchase/renewal price, required term, and owning account for the selected candidate.
- [ ] **P11.02 — Register or connect the domain.** Complete the intended registrar operation and required ownership/email verification; record renewal ownership and DNS configuration.
- [x] **P11.03 — Attach the production custom domain.** Follow the hosting platform's supported flow, verify certificates, and establish canonical-host redirects where applicable.
- [x] **P11.04 — Finalize production configuration.** Set the owned site origin, regenerate canonical metadata and sitemaps, remove preview noindex behavior, and run the checks affected by these changes.
- [x] **P11.05 — Publish the verified production artifact.** Use the designated deployment owner and retain the prior known-good artifact and revision for rollback.
- [x] **P11.06 — Verify the live site.** Test homepage, first-agent path, representative nested docs, search, downloads, optional API content, alternate-host redirects, and real 404 responses over HTTPS.
- [x] **P11.07 — Update GitHub entry points.** Set the repository Website field, add the prominent README docs link, and verify reciprocal repository links and source-edit destinations.
- [x] **P11.08 — Record Milestone D.** Capture the canonical URL, deployment revision, live verification results, ownership details, and any intentionally deferred features.

**Deliverables:** Working custom-domain website, updated GitHub discovery links, live verification record, and recoverable production deployment.

**Completion gate:** A visitor can reach the live site from GitHub and complete the first-agent reading path. The domain, canonical metadata, search, redirects, and published artifacts agree. A build on a temporary hostname alone does not complete this phase.

**P11 launch checkpoint (2026-09-08 UTC):** Milestone D's public launch is verified at https://ochatlabs.com. PR #21 merged at `b859aef70312a0f2553998a024f3c98561906106`; main run 34182707554 passed the release gate and deployment attempt 2 passed 3,601 hosted assertions. The failed first domain attachment was resolved by removing conflicting DNS records and retrying only deployment with the same qualified artifact. Final live checks passed 21 flows across three browsers, including inline ChatMD, search and first-agent onboarding. GitHub's Website field and README entry points are updated. P11.02 remains administratively open only for unconfirmed registrar contact-email verification; domain registration, DNS connection and HTTPS are verified. The $45 renewal quote's period remains unspecified. P09 API hosting and P10.03 manual accessibility remain deferred. An intermittent initial WebKit prefetch diagnostic is retained for P12 monitoring; subsequent diagnostic and uninstrumented browser checks passed unchanged bytes. See [the launch record](p11-launch.md) for release/version IDs, recovery baseline and evidence. Tasks 17–19 remain future gate enhancements.

### 19.20 Phase P12 — Complete maintenance handoff and post-launch follow-through

**Objective:** Leave the site maintainable and confirm that the first public deployment behaves as intended.

**Prerequisites:** P11 complete.

Ordered tasks:

- [x] **P12.01 — Finish contributor documentation.** Explain where article prose, homepage content, metadata, navigation, examples, assets, and redirect rules are edited.
- [x] **P12.02 — Document routine changes.** Cover adding/removing a page, changing a route, updating an example, upgrading dependencies, and regenerating optional API output.
- [x] **P12.03 — Validate a contributor workflow.** From a clean checkout, edit one source page, preview it, run relevant checks, and confirm the source-edit link points to the right file.
- [x] **P12.04 — Finalize operational ownership.** Record domain renewal, account recovery ownership, deployment responsibility, and rollback instructions without putting credentials in repository documentation.
- [ ] **P12.05 — Check initial production behavior.** Inspect available deployment/error evidence and revisit principal routes, search, downloads, and indexing headers after launch; fix concrete defects found.
- [x] **P12.06 — Create a prioritized follow-up backlog.** Carry forward specific deferred pages, optional API work, search limitations, accessibility findings, and media improvements with their rationale.
- [x] **P12.07 — Schedule appropriate later reviews.** Assign ownership for content freshness, external-link checks, framework upgrades, and a later usability/performance review when useful traffic or feedback exists.
- [ ] **P12.08 — Close the implementation record.** Mark completed phases with evidence, retain explicit deferred scope, and document the final operating configuration. Update the persistent memory file with the deployed revision, remaining follow-ups, and links to the permanent maintenance documentation.

**Deliverables:** Complete website development/operations guide, demonstrated author workflow, initial production review, and an owned follow-up backlog.

**Completion gate:** Another contributor can update a page without editing generated output or discovering undocumented setup. Required launch work is finished, operational ownership is clear, and remaining enhancements are explicitly tracked.

**Timing note:** Checks requiring future traffic or a later observation period remain scheduled follow-up work. Do not mark future observations as completed during the launch session.

**P12 implementation checkpoint:** Contributor documentation, routine workflows, clean-checkout exercise, operational ownership, prioritized backlog and dated future reviews are complete. The initial production review found readiness-loop and WebKit prefetch issues; fixes pass local checks and are being qualified through the protected publication workflow. P12.05/.08 remain open until publication/live verification and the outstanding P11.02 registrar confirmation are resolved. See [the maintenance handoff](p12-handoff.md); future observations and deferred API/manual-accessibility work are not claimed complete.

### 19.21 Phase completion record template

Use the following concise record when finishing a phase. Store detailed logs and screenshots separately and link them rather than embedding large outputs in this specification.

```text
Phase: Pxx — Name
Status: not started / in progress / complete / externally blocked
Source revision:
Completed task IDs:
Deliverables:
Verification performed:
Evidence links:
Open defects:
Deferred or not-applicable tasks and reasons:
External input needed, if any:
Next phase:
```

A phase is complete only when its gate is satisfied. Marking the optional API branch deferred is a valid scope decision; calling an unresolved mandatory launch check optional is not. If an external input blocks one task, name that input and continue any independent authorized work that remains.

### 19.22 Required persistent implementation memory

**File:** `scratch/ochat-website-implementation-notes.md`, relative to the repository root.

This file is the implementation's persistent working memory. Create it at P00.08 and keep it updated throughout the work. It must contain enough current context for an implementer to resume after a new session, context compaction, interruption, or handoff without reconstructing the project from the full conversation or repeating completed investigation.

This is an ordinary Markdown file on disk, not an assumption that conversational memory survives automatically. Its role is to summarize the current implementation state and link to evidence. The specification remains the source of requirements; source code and recorded checks establish what has actually been implemented.

#### Required contents

| Area | What to preserve |
|---|---|
| Last checkpoint | Date/time with timezone, source revision, branch/worktree, and implementation status |
| Current position | Current phase, task IDs in progress, last completed task, and next concrete action |
| Scope and user direction | Accepted preferences, constraints, corrections, scope changes, and unresolved user decisions |
| Completed work | Concise behavior-level description, relevant files, and verification evidence |
| Decisions | Chosen option, reason, alternatives rejected when useful, and conditions that would justify revisiting |
| Discoveries and traps | Repository quirks, framework/API differences, commands that failed, and known workarounds worth remembering |
| Verification | Commands/checks run, result, applicable revision or files, and checks still required |
| Open work | Defects, blockers, partial changes, dependencies, and any external input genuinely needed |
| Environment and previews | Toolchain versions, working directory, local preview command/URL, and any running process worth checking on resume |
| Evidence and artifacts | Paths to reports/screenshots, source manifests, preview/deployment records, PRs, and the main spec |

Record useful negative findings: for example, a tested importer approach that breaks anchor IDs, a version incompatibility, or a CLI invocation that selects the wrong host. Explain why it failed so the next session does not repeat the same experiment. Distinguish observations from hypotheses and planned work from completed behavior.

#### Update cadence

Update the file:

1. At implementation initialization, before substantive changes begin.
2. After completing a meaningful task or phase increment.
3. After a consequential design, dependency, content, or deployment decision.
4. After discovering a blocker, failed approach, source-code limitation, or changed user requirement.
5. After checks pass or fail when the result changes what should happen next.
6. Before pausing, handing off, ending an implementation session, or a known context-compaction boundary.
7. Immediately after resuming, if checking the actual checkout reveals the notes are stale.

Do not wait until the end of a long session to record important decisions. Context compaction may occur without an ideal stopping point; small checkpoints after meaningful work reduce the amount of state that can be lost. Updating memory should be lightweight and should not require a user prompt each time.

#### Resume procedure

1. Read this notes file first, then the current phase and referenced requirements in the specification.
2. Check applicable repository instructions, branch/revision, working-tree changes, and the existence of referenced files or artifacts.
3. Reconcile notes with the actual checkout. A recorded passing check may predate later changes; a saved process/session ID may no longer be live; an unchecked spec task may already have partially implemented code.
4. Confirm the next action and unresolved blockers. Reuse recorded decisions and valid evidence instead of restarting research unnecessarily.
5. Update the checkpoint if reconciliation changes the plan, then continue the next authorized task.

If the file is missing, reconstruct a concise initial checkpoint from the specification, checkout, and available artifacts. Do not mark historical tasks complete without evidence. Ask for missing information only when it materially blocks progress and cannot be recovered from available state.

#### Suggested file template

```markdown
# Ochat website implementation notes

Last updated: YYYY-MM-DD HH:MM TZ
Spec: scratch/ochat-website-research-spec.md
Branch/worktree:
Source revision:
Status: not started / in progress / paused / complete

## Resume here
- Current phase/task:
- Last completed task:
- Next concrete action:
- Important uncommitted or partial work:

## User direction and scope
- Accepted requirements/preferences:
- Recent corrections:
- Pending decisions:

## Completed work and evidence
| Task | Result | Files/revision | Verification |
|---|---|---|---|

## Decisions to retain
| Decision | Reason | Evidence | Revisit only if |
|---|---|---|---|

## Findings and failed approaches
- Observation or failure:
- Cause/evidence:
- Workaround or next investigation:

## Open work and blockers
- Required remaining work:
- Known defects:
- External input needed:
- Explicitly deferred scope:

## Verification state
- Last successful checks and revision:
- Failed checks and cause:
- Checks not run or now stale:

## Environment and useful references
- Toolchain and commands:
- Preview URL/process status to verify:
- Reports/screenshots/artifacts:
- Relevant spec/source paths:

## Recent checkpoints
- Date/time — meaningful change and next action.
```

#### Keep memory concise, accurate, and local

Put the current resumable state at the top. Update existing entries instead of appending contradictory summaries. Keep a short recent-checkpoint history; move old detailed investigation into an explicitly linked file only when necessary. Prefer concise conclusions and artifact links to complete tool output, transcripts, or repeated copies of the spec.

Do not store credentials, access tokens, private account recovery information, or sensitive raw output in this file. Refer to configuration locations and variable names where needed. The file must remain outside all website publication, example-download, and search manifests.

The checkout currently excludes `scratch/` through `.git/info/exclude`. These notes therefore persist on this workspace's disk but do not automatically travel with a Git commit, clean checkout, or another machine. For a cross-workspace handoff, explicitly transfer the notes through the agreed private handoff mechanism or move a reviewed non-sensitive summary to an intended tracked location. Do not assume a Git push transferred scratch memory.

**Completion requirement:** Each phase completion and each session handoff must leave the notes current enough to answer what was done, what was verified, what remains, and what to do next. The notes support continuity; they do not replace the implementation or its acceptance checks.

## 20. Operations and maintenance

### 20.1 Normal author workflow

1. Edit canonical Markdown or its metadata manifest entry.
2. Preview locally through the website development command.
3. Run content/build checks.
4. Run semantic documentation checks when technical instructions changed.
5. Review the generated route/content report.
6. Submit the change through the repository's normal review process.
7. Confirm automated checks and the appropriate preview.

Generated content is never edited directly. Website contributor documentation should explain where homepage copy, article prose, navigation, examples, and redirects each live.

### 20.2 Adding or removing a page

Adding a page requires a disposition, stable ID, route if published, title, section, and appropriate metadata. If it is a tutorial, add prerequisites and verification notes.

Removing or moving a published page requires checking inbound links and choosing a redirect, bridge, or explicit removal outcome. A deleted source cannot leave stale generated HTML behind.

### 20.3 Dependency updates

Group routine website dependency updates into reviewable changes. Run the representative render, search, copy, focus, and deployment checks after framework upgrades. Pay special attention to Starlight overrides, heading generation, and Markdown plugin ordering.

Do not update dependencies automatically during a production build. The committed lockfile defines the build input.

### 20.4 Content freshness

Tie user-facing docs to code changes where feasible. Review installation, first-agent instructions, provider environment, host flags, and shell authorization when related code changes. Check demonstration accuracy after significant TUI or CLI changes.

Use status labels based on actual source behavior. Avoid claiming every page is “latest” merely because it was deployed recently.

### 20.5 Versioning policy

Start with one current documentation set. Show the relevant release or source revision in appropriate metadata. Do not create a version switcher with only one option.

Add versioned docs when Ochat has maintained releases with meaningful incompatible instructions. Before enabling versions, specify release snapshots, canonical URLs, cross-version search, default version selection, and support for older routes.

### 20.6 Observability and feedback

Initially use build results, deployment checks, broken-link reports, and direct usability feedback. Add analytics only if there is a concrete question they will answer, such as whether readers reach the first-agent page.

If analytics are introduced, avoid collecting raw search queries by default; readers may paste private code identifiers or paths. Define event names, retention, and disclosure according to the actual chosen service and behavior. No analytics dependency is required for launch.

### 20.7 Ownership

Record who controls the repository, hosting project, domain registration, billing, and recovery methods. Set a domain-renewal process and retain deployment/rollback instructions where the owner can find them.

This is practical operational ownership for a public domain, not an additional product feature or a requirement for a larger team.

## 21. Risks and open decisions

### 21.1 Risk register

| Risk | Impact | Mitigation | Evidence |
|---|---|---|---|
| Old relative links break on website | Readers cannot follow guides | Explicit route map and built-fragment checks | Link report |
| Historical prose appears current | Incorrect expectations or commands | Per-file status/disposition and source review | Manifest review notes |
| All pages become MDX | ChatMD/brace syntax is misinterpreted | Keep Markdown default; use limited authored components | Representative render fixtures |
| H1 extraction loses anchors | Existing fragment links fail | Renderer-aware heading inventory and aliases | Fragment regression tests |
| Case normalization collides | Wrong page published or Linux-only failure | Collision checks and clean Linux build | Route report |
| Broad ignore rule hides website source | Local success cannot reproduce in CI | Narrow exceptions and tracking audit | Clean-checkout build |
| Importer copies private local files | Unintended public artifacts | Tracked allowlists and output audit | Asset manifest/report |
| Attractive demo uses obsolete commands | New users cannot reproduce it | Record current run and source revision | Demo verification record |
| Custom theme breaks focus/contrast | Navigation inaccessible | Supported extension points plus manual tests | Accessibility review |
| Search favors internal history | New users get wrong entry point | Curated corpus and query evaluation | Search benchmark |
| Generated API site is stale | API guidance disagrees with source | Fresh artifact with provenance | API build record |
| Hosted build ignores sibling docs | Deployment lacks content | Full checkout and correct build root | Hosted preview build |
| CI and hosting both deploy differently | Unverified or racing releases | One production deploy owner | Deployment runbook |
| Candidate domain is unavailable/premium | Brand or cost changes | Verify shortlist before purchase | Registrar checkout evidence |
| Preview is indexed publicly | Duplicate search results | Explicit preview noindex response checks | Deployed header test |
| Excessive frontend scripts | Slow reading experience | Static output and measured budgets | Performance report |

### 21.2 Decisions with reasonable defaults

| Decision | Default in this specification | When to revisit |
|---|---|---|
| Framework | Astro + Starlight | Only if the technical spike exposes a concrete blocker |
| Homepage style | User-selected Graphite + blue, calm typography, real source/terminal visuals | Subsequent usability feedback |
| Content source | `docs-src/` plus metadata manifest | If a proven Starlight loader offers a simpler equivalent |
| Domain | Prefer `ochat.dev` if available at acceptable renewal cost | Registrar check |
| Hosting | Workers Static Assets | Account or deployment requirements differ |
| Theme default | Follow system preference with user override | Usability feedback |
| API release timing | Separate gate; omit link until verified | WP08 completion |
| Search | Pagefind baseline | Measured relevance or scale problems |
| Analytics | None initially | A specific measurement need arises |
| Locale/version | English, one current docs set | Maintained translations/releases exist |

### 21.3 Research questions for the initial spike

1. Which exact supported Astro/Starlight/Node combination should be pinned?
2. Does the selected Starlight loader accept the generated tree cleanly with `/docs/` slugs and a custom root homepage?
3. Which existing fragments differ under the selected Markdown renderer?
4. How many raw HTML constructs need explicit treatment?
5. Which ChatML highlighting fallback is readable and technically honest?
6. Does Pagefind find punctuation-heavy identifiers adequately with the default index?
7. What proportion of library sidecars require a visible historical/compatibility label?
8. Can fresh odoc output be mounted under `/api/` without rewriting its internal references?
9. What are the measured payload and build-time baselines before customization?
10. Which domain is available at an acceptable full renewal price?

Questions 1–9 can be researched and prototyped without waiting for a domain purchase. Branding refinements can proceed on a preview hostname.

## 22. Launch acceptance checklist

### Product and editorial

- [ ] Homepage communicates agents defined in text files.
- [ ] First-agent path is obvious and technically correct.
- [ ] Installation requirements are visible and current.
- [ ] T01–T04 have complete instructions and recorded verification scope.
- [ ] Advanced tutorials retain correct host and permission distinctions.
- [ ] Experimental and compatibility status is labeled accurately.
- [ ] Demonstration is current or explicitly labeled illustrative/historical.
- [ ] No invented provider compatibility, install methods, metrics, or social proof.

### Content and navigation

- [ ] Every inventoried Markdown source has a disposition.
- [ ] Every published route has one owner.
- [ ] No unresolved internal routes, required fragments, or local assets.
- [ ] Old TUI heading links have verified aliases or bridge pages.
- [ ] Source-edit links point to original Markdown.
- [ ] Current guidance is prioritized in navigation and search.
- [ ] API navigation is present only if a fresh verified artifact is published.
- [ ] New source documentation remains readable on GitHub.
- [ ] Generated Markdown ownership and exact-source validation are preserved.
- [ ] Supplemental sources and advanced capability coverage have explicit dispositions.
- [ ] Existing explicit anchors and real nested-fence/XML-literal cases render correctly.
- [ ] Complete example downloads include required local dependencies and notices.

### Interface and quality

- [ ] Desktop and mobile layouts reviewed in both themes.
- [ ] Keyboard navigation and focus behavior verified.
- [ ] Search, menu, copy, and error states work.
- [ ] Code copied/downloaded matches intended source.
- [ ] Long tables/code do not overflow the page body.
- [ ] Essential content remains available without JavaScript.
- [ ] Accessibility review completed with limitations documented.
- [ ] Performance budgets measured and exceptions recorded.

### Build and release

- [ ] Clean checkout builds using pinned versions.
- [ ] Website source is tracked despite existing ignore rules.
- [ ] Generated content is reproducible and never hand-maintained.
- [ ] Relevant semantic documentation checks pass.
- [ ] Moving tutorials has not removed their semantic-check coverage or Dune dependencies.
- [ ] Website dependencies and output do not enter Dune's normal build scan.
- [ ] Publication/search/sidebar/sitemap/noindex policy is verified in actual output.
- [ ] Last-updated metadata and source links reflect original source provenance.
- [ ] Full artifact size/count and deployment-rule limits pass the selected hosting plan.
- [ ] Production cannot publish before required checks; preview/production differences are validated.
- [ ] Representative browser and content-pipeline tests pass.
- [ ] Deployment output contains only intended public artifacts.
- [ ] One production deployment owner is configured.
- [ ] Rollback process is documented and rehearsed appropriately.

### Public launch

- [ ] Domain ownership and registrar verification complete.
- [ ] HTTPS and canonical-host behavior verified.
- [ ] Nested routes, downloads, assets, redirects, and real 404s verified on host.
- [ ] Production canonicals and sitemap use the owned domain.
- [ ] Production is indexable; previews have verified noindex behavior.
- [ ] GitHub About/README and website repository links updated.
- [ ] Domain renewal and website ownership recorded.
- [ ] Deferred work is explicit and not advertised as complete.
- [ ] Persistent implementation notes are current and point to final evidence, operating documentation, and remaining follow-ups.

## 23. Research sources

External sources below were consulted on 2026-09-06. Official documentation supports the platform facts used in this specification. Project-specific design, budgets, information architecture, and implementation sequencing remain recommendations.

### 23.1 Primary external references

| Source | Used for |
|---|---|
| [Starlight overview](https://starlight.astro.build/) | Documentation feature baseline |
| [Starlight pages](https://starlight.astro.build/guides/pages/) | Custom homepage alongside content pages |
| [Starlight authoring](https://starlight.astro.build/guides/authoring-content/) | Markdown, frontmatter, code presentation |
| [Starlight frontmatter](https://starlight.astro.build/reference/frontmatter/) | Metadata integration research |
| [Starlight configuration](https://starlight.astro.build/reference/configuration/) | Supported configuration boundaries |
| [Starlight component overrides](https://starlight.astro.build/guides/overriding-components/) | Customization strategy |
| [Starlight search](https://starlight.astro.build/guides/site-search/) | Default Pagefind integration and exclusions |
| [Astro content collections](https://docs.astro.build/en/guides/content-collections/) | Content loading/schema options |
| [Astro Markdown](https://docs.astro.build/en/guides/markdown-content/) | Renderer/heading integration research |
| [Astro sitemap integration](https://docs.astro.build/en/guides/integrations-guide/sitemap/) | Implementing per-route sitemap exclusions |
| [Astro deployment to Cloudflare](https://docs.astro.build/en/guides/deploy/cloudflare/) | Current static deployment path |
| [Fumadocs introduction](https://www.fumadocs.dev/docs) | Alternative framework assessment |
| [Pagefind weighting](https://pagefind.app/docs/weighting/) | Search relevance options |
| [Pagefind filters](https://pagefind.app/docs/filtering/) | Optional result categorization |
| [Workers Static Assets](https://developers.cloudflare.com/workers/static-assets/) | Static hosting foundation |
| [Static assets billing](https://developers.cloudflare.com/workers/static-assets/billing-and-limitations/) | Cost boundaries |
| [Static-site routing](https://developers.cloudflare.com/workers/static-assets/routing/static-site-generation/) | HTML routing and 404 behavior |
| [Workers Builds](https://developers.cloudflare.com/workers/ci-cd/builds/) | Git-connected builds and deployment |
| [Workers platform limits](https://developers.cloudflare.com/workers/platform/limits/) | Complete-artifact file, size, and rule limits |
| [Workers build limits](https://developers.cloudflare.com/workers/ci-cd/builds/limits-and-pricing/) | Separate build-minute, timeout, and concurrency limits |
| [Workers Custom Domains](https://developers.cloudflare.com/workers/configuration/routing/custom-domains/) | Domain attachment |
| [Static asset headers](https://developers.cloudflare.com/workers/static-assets/headers/) | Response-header configuration |
| [Static asset redirects](https://developers.cloudflare.com/workers/static-assets/redirects/) | Redirect configuration |
| [Cloudflare Registrar](https://www.cloudflare.com/products/registrar/) | Registration and renewal model |
| [Register a domain](https://developers.cloudflare.com/registrar/get-started/register-domain/) | Nameserver and verification requirements |
| [WCAG 2.2](https://www.w3.org/TR/WCAG22/) | Accessibility target |
| [WAI-ARIA modal dialog](https://www.w3.org/WAI/ARIA/apg/patterns/dialog-modal/) | Focus and dialog behavior |
| [Web Vitals](https://web.dev/articles/vitals) | Performance metric definitions and thresholds |
| [Playwright](https://playwright.dev/docs/intro) | Browser validation tooling |
| [Odoc](https://ocaml.github.io/odoc/odoc/index.html) | OCaml API documentation integration |
| [Dune directories](https://dune.readthedocs.io/en/stable/reference/dune/dirs.html) | Keeping website trees outside OCaml build scanning |
| [Dune data-only directories](https://dune.readthedocs.io/en/stable/reference/dune/data_only_dirs.html) | Alternative treatment for imported data trees |

### 23.2 Repository evidence

| Source | Used for |
|---|---|
| [Readme.md](../../Readme.md) | Positioning, onboarding, examples, host distinctions |
| [Documentation index](../../docs-src/README.md) | Existing topic structure |
| [Project overview](../../docs-src/overview/project.md) | Architecture, maturity, roadmap, provider boundaries |
| [ChatMD introduction](../../docs-src/chatmd/README.md) | Minimal prompt and progressive learning |
| [Examples index](../../docs-src/examples/README.md) | Existing learning paths and example caveats |
| [Quickstart](../../docs-src/agent-server/quickstart.md) | Build/provider/host setup |
| [Local tutorial](../../docs-src/agent-server/tutorials/local-tui.md) | Local mode, persistence, and compatibility |
| [Library index](../../docs-src/lib/README.md) | Prose/API distinction and historical material |
| [Forwarding-page example](../../docs-src/chat_tui/app.doc.md) | Legacy fragment compatibility |
| [Coverage ledger](../../docs-src/development/documentation-coverage.md) | Existing audit structure and verification scope |
| [Development instructions](../../DEVELOPMENT.md) | Odoc/search workflows and documentation checks |
| [Package metadata](../../dune-project) | OCaml/Dune requirements |
| [Documentation-check declaration](../../test/agent_docs/dune) | Existing offline validation integration |
| [Documentation checker](../../test/agent_docs/docs_check.ml) | Protocol/source equality and current check selection |
| [Documentation inventory generator](../../test/agent_docs/docs_inventory.ml) | Ownership of three generated Markdown files |
| [ChatML documentation check](../../test/agent_docs/docs_chatml.ml) | Exact heading/fence/fixture contract |
| [Compiled documentation examples](../../test/agent_docs/docs_examples.ml) | Source parity and offline example coverage |
| [Shell documentation smoke checks](../../test/agent_docs/docs_smoke.ml) | XML fence and shell action checks |
| [TUI CLI](../../bin/chat_tui.ml) | Actual native/legacy/daemon flag normalization |
| [Stdio CLI](../../bin/ochat_agent_stdio.ml) | Standalone startup and local/gateway context |
| [Embedded host](../../lib/agent_server/embedded.ml) | Default policy, transient roots, and process lifetime |
| [ChatMD source loader](../../lib/chatmd/source_loader.mli) | Import and specialist dependency resolution |
| [Provider I/O](../../lib/io.ml) | Existing outbound TLS limitation |
| [Binary declarations](../../bin/dune) | Installed versus source-only command inventory |
| [Installed scripts](../../scripts/dune) | Additional executable names |
| [TUI screenshot](../../assets/tui-snapshot.png) | Existing visual asset assessment |
| [.gitignore](../../.gitignore) | Source tracking and generated-output constraints |
| [.rgignore](../../.rgignore) | Inventory visibility constraints |

## 24. Complete source inventory

The following inventory lists every tracked Markdown file under `docs-src/` at the recorded baseline. The suggested review class is a planning aid, not a completed editorial review or a final publication manifest. Final routes and dispositions must be established through WP04.

Review classes:

- **User docs candidate:** likely public user guidance; verify current behavior and navigation placement.
- **Library review:** inspect prose for current API descriptions versus historical design material.
- **Compatibility bridge:** preserve existing forwarding headings and map to canonical pages.
- **Maintainer review:** normally repository-only unless a clear reader need justifies publication.
- **Historical review:** inspect and label scope before any public-site inclusion.
- **Compatibility review:** retain for existing users with explicit compatibility status.

The inventory is intentionally broader than the first-release sidebar. Publication requires a deliberate disposition, not merely discovery by a filesystem glob.

| # | Source | Suggested review class |
|---:|---|---|
| 1 | [docs-src/README.md](../../docs-src/README.md) | User docs candidate |
| 2 | [docs-src/agent-server/README.md](../../docs-src/agent-server/README.md) | User docs candidate |
| 3 | [docs-src/agent-server/chatml-orchestration.md](../../docs-src/agent-server/chatml-orchestration.md) | User docs candidate |
| 4 | [docs-src/agent-server/concepts.md](../../docs-src/agent-server/concepts.md) | User docs candidate |
| 5 | [docs-src/agent-server/configuration.md](../../docs-src/agent-server/configuration.md) | User docs candidate |
| 6 | [docs-src/agent-server/embedding.md](../../docs-src/agent-server/embedding.md) | User docs candidate |
| 7 | [docs-src/agent-server/environment.md](../../docs-src/agent-server/environment.md) | User docs candidate |
| 8 | [docs-src/agent-server/operations.md](../../docs-src/agent-server/operations.md) | User docs candidate |
| 9 | [docs-src/agent-server/operator-contracts.md](../../docs-src/agent-server/operator-contracts.md) | User docs candidate |
| 10 | [docs-src/agent-server/permissions-and-security.md](../../docs-src/agent-server/permissions-and-security.md) | User docs candidate |
| 11 | [docs-src/agent-server/protocol-types.md](../../docs-src/agent-server/protocol-types.md) | User docs candidate |
| 12 | [docs-src/agent-server/protocol.md](../../docs-src/agent-server/protocol.md) | User docs candidate |
| 13 | [docs-src/agent-server/quickstart.md](../../docs-src/agent-server/quickstart.md) | User docs candidate |
| 14 | [docs-src/agent-server/sessions-and-workspaces.md](../../docs-src/agent-server/sessions-and-workspaces.md) | User docs candidate |
| 15 | [docs-src/agent-server/testing.md](../../docs-src/agent-server/testing.md) | User docs candidate |
| 16 | [docs-src/agent-server/transports/http.md](../../docs-src/agent-server/transports/http.md) | User docs candidate |
| 17 | [docs-src/agent-server/transports/stdio.md](../../docs-src/agent-server/transports/stdio.md) | User docs candidate |
| 18 | [docs-src/agent-server/transports/unix.md](../../docs-src/agent-server/transports/unix.md) | User docs candidate |
| 19 | [docs-src/agent-server/troubleshooting.md](../../docs-src/agent-server/troubleshooting.md) | User docs candidate |
| 20 | [docs-src/agent-server/tutorials/background-agent.md](../../docs-src/agent-server/tutorials/background-agent.md) | User docs candidate |
| 21 | [docs-src/agent-server/tutorials/http-client.md](../../docs-src/agent-server/tutorials/http-client.md) | User docs candidate |
| 22 | [docs-src/agent-server/tutorials/local-tui.md](../../docs-src/agent-server/tutorials/local-tui.md) | User docs candidate |
| 23 | [docs-src/agent-server/tutorials/shell-agent.md](../../docs-src/agent-server/tutorials/shell-agent.md) | User docs candidate |
| 24 | [docs-src/agent-server/tutorials/stdio-client.md](../../docs-src/agent-server/tutorials/stdio-client.md) | User docs candidate |
| 25 | [docs-src/agent-server/tutorials/unix-daemon.md](../../docs-src/agent-server/tutorials/unix-daemon.md) | User docs candidate |
| 26 | [docs-src/bin/README.md](../../docs-src/bin/README.md) | User docs candidate |
| 27 | [docs-src/bin/chat_tui.doc.md](../../docs-src/bin/chat_tui.doc.md) | User docs candidate |
| 28 | [docs-src/bin/developer-utilities.md](../../docs-src/bin/developer-utilities.md) | User docs candidate |
| 29 | [docs-src/bin/dsl_script.doc.md](../../docs-src/bin/dsl_script.doc.md) | User docs candidate |
| 30 | [docs-src/bin/eio_get.doc.md](../../docs-src/bin/eio_get.doc.md) | Compatibility review |
| 31 | [docs-src/bin/gpt.doc.md](../../docs-src/bin/gpt.doc.md) | Compatibility review |
| 32 | [docs-src/bin/key_dump.doc.md](../../docs-src/bin/key_dump.doc.md) | User docs candidate |
| 33 | [docs-src/bin/main.doc.md](../../docs-src/bin/main.doc.md) | User docs candidate |
| 34 | [docs-src/bin/mcp_server.doc.md](../../docs-src/bin/mcp_server.doc.md) | Compatibility review |
| 35 | [docs-src/bin/md_index.doc.md](../../docs-src/bin/md_index.doc.md) | User docs candidate |
| 36 | [docs-src/bin/md_search.doc.md](../../docs-src/bin/md_search.doc.md) | User docs candidate |
| 37 | [docs-src/bin/mp_prompt.doc.md](../../docs-src/bin/mp_prompt.doc.md) | Compatibility review |
| 38 | [docs-src/bin/mp_refine_run.doc.md](../../docs-src/bin/mp_refine_run.doc.md) | User docs candidate |
| 39 | [docs-src/bin/ochat_agent_server.doc.md](../../docs-src/bin/ochat_agent_server.doc.md) | User docs candidate |
| 40 | [docs-src/bin/ochat_agent_stdio.doc.md](../../docs-src/bin/ochat_agent_stdio.doc.md) | User docs candidate |
| 41 | [docs-src/bin/ochat_shell_resource_runner.doc.md](../../docs-src/bin/ochat_shell_resource_runner.doc.md) | User docs candidate |
| 42 | [docs-src/bin/odoc_index.doc.md](../../docs-src/bin/odoc_index.doc.md) | User docs candidate |
| 43 | [docs-src/bin/odoc_search.doc.md](../../docs-src/bin/odoc_search.doc.md) | User docs candidate |
| 44 | [docs-src/chat_tui/app.doc.md](../../docs-src/chat_tui/app.doc.md) | Compatibility bridge |
| 45 | [docs-src/chat_tui/controller.doc.md](../../docs-src/chat_tui/controller.doc.md) | Compatibility bridge |
| 46 | [docs-src/chat_tui/highlight_grammars.doc.md](../../docs-src/chat_tui/highlight_grammars.doc.md) | Compatibility bridge |
| 47 | [docs-src/chat_tui/highlight_registry.doc.md](../../docs-src/chat_tui/highlight_registry.doc.md) | Compatibility bridge |
| 48 | [docs-src/chat_tui/highlight_theme.doc.md](../../docs-src/chat_tui/highlight_theme.doc.md) | Compatibility bridge |
| 49 | [docs-src/chat_tui/highlight_tm_engine.doc.md](../../docs-src/chat_tui/highlight_tm_engine.doc.md) | Compatibility bridge |
| 50 | [docs-src/chat_tui/model.doc.md](../../docs-src/chat_tui/model.doc.md) | Compatibility bridge |
| 51 | [docs-src/chat_tui/renderer.doc.md](../../docs-src/chat_tui/renderer.doc.md) | Compatibility bridge |
| 52 | [docs-src/chat_tui/types.doc.md](../../docs-src/chat_tui/types.doc.md) | Compatibility bridge |
| 53 | [docs-src/chat_tui_renderer2_whitepaper.md](../../docs-src/chat_tui_renderer2_whitepaper.md) | Historical review |
| 54 | [docs-src/chatmd/README.md](../../docs-src/chatmd/README.md) | User docs candidate |
| 55 | [docs-src/chatml-async-completion-lifecycle.md](../../docs-src/chatml-async-completion-lifecycle.md) | Library review |
| 56 | [docs-src/chatml-budget-policy.md](../../docs-src/chatml-budget-policy.md) | Library review |
| 57 | [docs-src/chatml-host-session-controller-contract.md](../../docs-src/chatml-host-session-controller-contract.md) | Library review |
| 58 | [docs-src/chatml-safe-point-and-effective-history.md](../../docs-src/chatml-safe-point-and-effective-history.md) | Library review |
| 59 | [docs-src/chatml-ui-host-capabilities.md](../../docs-src/chatml-ui-host-capabilities.md) | Library review |
| 60 | [docs-src/chatml/README.md](../../docs-src/chatml/README.md) | User docs candidate |
| 61 | [docs-src/cli/chat-completion.md](../../docs-src/cli/chat-completion.md) | User docs candidate |
| 62 | [docs-src/cli/shell-runtime-management.md](../../docs-src/cli/shell-runtime-management.md) | User docs candidate |
| 63 | [docs-src/context_compaction/compactor.doc.md](../../docs-src/context_compaction/compactor.doc.md) | Library review |
| 64 | [docs-src/context_compaction/config.doc.md](../../docs-src/context_compaction/config.doc.md) | Library review |
| 65 | [docs-src/context_compaction/relevance_judge.doc.md](../../docs-src/context_compaction/relevance_judge.doc.md) | Library review |
| 66 | [docs-src/design/ochat-agent-server-implementation-spec.md](../../docs-src/design/ochat-agent-server-implementation-spec.md) | Maintainer review |
| 67 | [docs-src/design/ochat-agent-server-spec.md](../../docs-src/design/ochat-agent-server-spec.md) | Maintainer review |
| 68 | [docs-src/development/admin-remediation-notes.md](../../docs-src/development/admin-remediation-notes.md) | Maintainer review |
| 69 | [docs-src/development/code-documentation-audit.md](../../docs-src/development/code-documentation-audit.md) | Maintainer review |
| 70 | [docs-src/development/documentation-coverage.md](../../docs-src/development/documentation-coverage.md) | Maintainer review |
| 71 | [docs-src/development/documentation-worklog.md](../../docs-src/development/documentation-worklog.md) | Maintainer review |
| 72 | [docs-src/development/final-audit-remediation.md](../../docs-src/development/final-audit-remediation.md) | Maintainer review |
| 73 | [docs-src/development/library-reference-remediation.md](../../docs-src/development/library-reference-remediation.md) | Maintainer review |
| 74 | [docs-src/development/readme-content-audit.md](../../docs-src/development/readme-content-audit.md) | Maintainer review |
| 75 | [docs-src/development/typeahead-verification.md](../../docs-src/development/typeahead-verification.md) | Maintainer review |
| 76 | [docs-src/examples/README.md](../../docs-src/examples/README.md) | User docs candidate |
| 77 | [docs-src/examples/agent-server/README.md](../../docs-src/examples/agent-server/README.md) | User docs candidate |
| 78 | [docs-src/examples/agent-server/config/README.md](../../docs-src/examples/agent-server/config/README.md) | User docs candidate |
| 79 | [docs-src/examples/prompt-patterns.md](../../docs-src/examples/prompt-patterns.md) | User docs candidate |
| 80 | [docs-src/guide/build-troubleshooting.md](../../docs-src/guide/build-troubleshooting.md) | User docs candidate |
| 81 | [docs-src/guide/chat_tui.md](../../docs-src/guide/chat_tui.md) | User docs candidate |
| 82 | [docs-src/guide/chatmd-shell-examples.md](../../docs-src/guide/chatmd-shell-examples.md) | User docs candidate |
| 83 | [docs-src/guide/chatmd-shell-extensions.md](../../docs-src/guide/chatmd-shell-extensions.md) | User docs candidate |
| 84 | [docs-src/guide/chatmd-shell-host-integration.md](../../docs-src/guide/chatmd-shell-host-integration.md) | User docs candidate |
| 85 | [docs-src/guide/chatmd-shell-persistence-and-audit.md](../../docs-src/guide/chatmd-shell-persistence-and-audit.md) | User docs candidate |
| 86 | [docs-src/guide/chatmd-shell-runtime-internals.md](../../docs-src/guide/chatmd-shell-runtime-internals.md) | User docs candidate |
| 87 | [docs-src/guide/chatmd-shell-security.md](../../docs-src/guide/chatmd-shell-security.md) | User docs candidate |
| 88 | [docs-src/guide/chatml-implementation-architecture.md](../../docs-src/guide/chatml-implementation-architecture.md) | User docs candidate |
| 89 | [docs-src/guide/chatml-language-spec.md](../../docs-src/guide/chatml-language-spec.md) | User docs candidate |
| 90 | [docs-src/guide/chatml-match-semantics.md](../../docs-src/guide/chatml-match-semantics.md) | User docs candidate |
| 91 | [docs-src/guide/chatml-moderator-runtime.md](../../docs-src/guide/chatml-moderator-runtime.md) | User docs candidate |
| 92 | [docs-src/guide/chatml-parsing-and-diagnostics.md](../../docs-src/guide/chatml-parsing-and-diagnostics.md) | User docs candidate |
| 93 | [docs-src/guide/general-agent-workflow.md](../../docs-src/guide/general-agent-workflow.md) | User docs candidate |
| 94 | [docs-src/guide/search-and-indexing.md](../../docs-src/guide/search-and-indexing.md) | User docs candidate |
| 95 | [docs-src/guide/search-examples/README.md](../../docs-src/guide/search-examples/README.md) | User docs candidate |
| 96 | [docs-src/guide/search-examples/md-search.md](../../docs-src/guide/search-examples/md-search.md) | User docs candidate |
| 97 | [docs-src/guide/search-examples/ochat-query.md](../../docs-src/guide/search-examples/ochat-query.md) | User docs candidate |
| 98 | [docs-src/guide/search-examples/odoc-search.md](../../docs-src/guide/search-examples/odoc-search.md) | User docs candidate |
| 99 | [docs-src/lib/Io.doc.md](../../docs-src/lib/Io.doc.md) | Library review |
| 100 | [docs-src/lib/README.md](../../docs-src/lib/README.md) | Library review |
| 101 | [docs-src/lib/agent_client/architecture.doc.md](../../docs-src/lib/agent_client/architecture.doc.md) | Library review |
| 102 | [docs-src/lib/agent_protocol/architecture.doc.md](../../docs-src/lib/agent_protocol/architecture.doc.md) | Library review |
| 103 | [docs-src/lib/agent_server/architecture.doc.md](../../docs-src/lib/agent_server/architecture.doc.md) | Library review |
| 104 | [docs-src/lib/agent_session/architecture.doc.md](../../docs-src/lib/agent_session/architecture.doc.md) | Library review |
| 105 | [docs-src/lib/agent_session/compaction_archive.doc.md](../../docs-src/lib/agent_session/compaction_archive.doc.md) | Library review |
| 106 | [docs-src/lib/agent_store/architecture.doc.md](../../docs-src/lib/agent_store/architecture.doc.md) | Library review |
| 107 | [docs-src/lib/agent_transport_client/architecture.doc.md](../../docs-src/lib/agent_transport_client/architecture.doc.md) | Library review |
| 108 | [docs-src/lib/agent_transport_http/architecture.doc.md](../../docs-src/lib/agent_transport_http/architecture.doc.md) | Library review |
| 109 | [docs-src/lib/agent_transport_socket/architecture.doc.md](../../docs-src/lib/agent_transport_socket/architecture.doc.md) | Library review |
| 110 | [docs-src/lib/agent_transport_stdio/architecture.doc.md](../../docs-src/lib/agent_transport_stdio/architecture.doc.md) | Library review |
| 111 | [docs-src/lib/apply_patch.doc.md](../../docs-src/lib/apply_patch.doc.md) | Library review |
| 112 | [docs-src/lib/apply_patch_error.doc.md](../../docs-src/lib/apply_patch_error.doc.md) | Library review |
| 113 | [docs-src/lib/bin_prot_utils_eio.doc.md](../../docs-src/lib/bin_prot_utils_eio.doc.md) | Library review |
| 114 | [docs-src/lib/bm25.doc.md](../../docs-src/lib/bm25.doc.md) | Library review |
| 115 | [docs-src/lib/chat_response/agent_runtime.doc.md](../../docs-src/lib/chat_response/agent_runtime.doc.md) | Library review |
| 116 | [docs-src/lib/chat_response/cache.doc.md](../../docs-src/lib/chat_response/cache.doc.md) | Library review |
| 117 | [docs-src/lib/chat_response/chatml_moderation.doc.md](../../docs-src/lib/chat_response/chatml_moderation.doc.md) | Library review |
| 118 | [docs-src/lib/chat_response/config.doc.md](../../docs-src/lib/chat_response/config.doc.md) | Library review |
| 119 | [docs-src/lib/chat_response/converter.doc.md](../../docs-src/lib/chat_response/converter.doc.md) | Library review |
| 120 | [docs-src/lib/chat_response/ctx.doc.md](../../docs-src/lib/chat_response/ctx.doc.md) | Library review |
| 121 | [docs-src/lib/chat_response/driver.doc.md](../../docs-src/lib/chat_response/driver.doc.md) | Library review |
| 122 | [docs-src/lib/chat_response/fetch.doc.md](../../docs-src/lib/chat_response/fetch.doc.md) | Library review |
| 123 | [docs-src/lib/chat_response/fork.doc.md](../../docs-src/lib/chat_response/fork.doc.md) | Library review |
| 124 | [docs-src/lib/chat_response/history_stream_event.doc.md](../../docs-src/lib/chat_response/history_stream_event.doc.md) | Library review |
| 125 | [docs-src/lib/chat_response/in_memory_stream.doc.md](../../docs-src/lib/chat_response/in_memory_stream.doc.md) | Library review |
| 126 | [docs-src/lib/chat_response/mcp_discovery_cache.doc.md](../../docs-src/lib/chat_response/mcp_discovery_cache.doc.md) | Library review |
| 127 | [docs-src/lib/chat_response/moderation.doc.md](../../docs-src/lib/chat_response/moderation.doc.md) | Library review |
| 128 | [docs-src/lib/chat_response/response_loop.doc.md](../../docs-src/lib/chat_response/response_loop.doc.md) | Library review |
| 129 | [docs-src/lib/chat_response/tool.doc.md](../../docs-src/lib/chat_response/tool.doc.md) | Library review |
| 130 | [docs-src/lib/chat_tui/agent_event_apply.doc.md](../../docs-src/lib/chat_tui/agent_event_apply.doc.md) | Library review |
| 131 | [docs-src/lib/chat_tui/agent_history_layout.doc.md](../../docs-src/lib/chat_tui/agent_history_layout.doc.md) | Library review |
| 132 | [docs-src/lib/chat_tui/agent_permission_view.doc.md](../../docs-src/lib/chat_tui/agent_permission_view.doc.md) | Library review |
| 133 | [docs-src/lib/chat_tui/agent_projection.doc.md](../../docs-src/lib/chat_tui/agent_projection.doc.md) | Library review |
| 134 | [docs-src/lib/chat_tui/agent_security_projection.doc.md](../../docs-src/lib/chat_tui/agent_security_projection.doc.md) | Library review |
| 135 | [docs-src/lib/chat_tui/agent_session_client.doc.md](../../docs-src/lib/chat_tui/agent_session_client.doc.md) | Library review |
| 136 | [docs-src/lib/chat_tui/app.doc.md](../../docs-src/lib/chat_tui/app.doc.md) | Library review |
| 137 | [docs-src/lib/chat_tui/app_compaction.doc.md](../../docs-src/lib/chat_tui/app_compaction.doc.md) | Library review |
| 138 | [docs-src/lib/chat_tui/app_events.doc.md](../../docs-src/lib/chat_tui/app_events.doc.md) | Library review |
| 139 | [docs-src/lib/chat_tui/app_reducer.doc.md](../../docs-src/lib/chat_tui/app_reducer.doc.md) | Library review |
| 140 | [docs-src/lib/chat_tui/app_runtime.doc.md](../../docs-src/lib/chat_tui/app_runtime.doc.md) | Library review |
| 141 | [docs-src/lib/chat_tui/app_stream_apply.doc.md](../../docs-src/lib/chat_tui/app_stream_apply.doc.md) | Library review |
| 142 | [docs-src/lib/chat_tui/app_streaming.doc.md](../../docs-src/lib/chat_tui/app_streaming.doc.md) | Library review |
| 143 | [docs-src/lib/chat_tui/app_submit.doc.md](../../docs-src/lib/chat_tui/app_submit.doc.md) | Library review |
| 144 | [docs-src/lib/chat_tui/cmd.doc.md](../../docs-src/lib/chat_tui/cmd.doc.md) | Library review |
| 145 | [docs-src/lib/chat_tui/connection_status.doc.md](../../docs-src/lib/chat_tui/connection_status.doc.md) | Library review |
| 146 | [docs-src/lib/chat_tui/controller.doc.md](../../docs-src/lib/chat_tui/controller.doc.md) | Library review |
| 147 | [docs-src/lib/chat_tui/controller_cmdline.doc.md](../../docs-src/lib/chat_tui/controller_cmdline.doc.md) | Library review |
| 148 | [docs-src/lib/chat_tui/controller_normal.doc.md](../../docs-src/lib/chat_tui/controller_normal.doc.md) | Library review |
| 149 | [docs-src/lib/chat_tui/controller_shared.doc.md](../../docs-src/lib/chat_tui/controller_shared.doc.md) | Library review |
| 150 | [docs-src/lib/chat_tui/controller_shell_security.doc.md](../../docs-src/lib/chat_tui/controller_shell_security.doc.md) | Library review |
| 151 | [docs-src/lib/chat_tui/controller_types.doc.md](../../docs-src/lib/chat_tui/controller_types.doc.md) | Library review |
| 152 | [docs-src/lib/chat_tui/conversation.doc.md](../../docs-src/lib/chat_tui/conversation.doc.md) | Library review |
| 153 | [docs-src/lib/chat_tui/highlight_grammars.doc.md](../../docs-src/lib/chat_tui/highlight_grammars.doc.md) | Library review |
| 154 | [docs-src/lib/chat_tui/highlight_registry.doc.md](../../docs-src/lib/chat_tui/highlight_registry.doc.md) | Library review |
| 155 | [docs-src/lib/chat_tui/highlight_styles.doc.md](../../docs-src/lib/chat_tui/highlight_styles.doc.md) | Library review |
| 156 | [docs-src/lib/chat_tui/highlight_theme.doc.md](../../docs-src/lib/chat_tui/highlight_theme.doc.md) | Library review |
| 157 | [docs-src/lib/chat_tui/highlight_tm_engine.doc.md](../../docs-src/lib/chat_tui/highlight_tm_engine.doc.md) | Library review |
| 158 | [docs-src/lib/chat_tui/highlight_tm_loader.doc.md](../../docs-src/lib/chat_tui/highlight_tm_loader.doc.md) | Library review |
| 159 | [docs-src/lib/chat_tui/markdown_fences.doc.md](../../docs-src/lib/chat_tui/markdown_fences.doc.md) | Library review |
| 160 | [docs-src/lib/chat_tui/model.doc.md](../../docs-src/lib/chat_tui/model.doc.md) | Library review |
| 161 | [docs-src/lib/chat_tui/path_completion.doc.md](../../docs-src/lib/chat_tui/path_completion.doc.md) | Library review |
| 162 | [docs-src/lib/chat_tui/persistence.doc.md](../../docs-src/lib/chat_tui/persistence.doc.md) | Library review |
| 163 | [docs-src/lib/chat_tui/renderer.doc.md](../../docs-src/lib/chat_tui/renderer.doc.md) | Library review |
| 164 | [docs-src/lib/chat_tui/renderer2.doc.md](../../docs-src/lib/chat_tui/renderer2.doc.md) | Library review |
| 165 | [docs-src/lib/chat_tui/renderer_component_history.doc.md](../../docs-src/lib/chat_tui/renderer_component_history.doc.md) | Library review |
| 166 | [docs-src/lib/chat_tui/renderer_component_input_box.doc.md](../../docs-src/lib/chat_tui/renderer_component_input_box.doc.md) | Library review |
| 167 | [docs-src/lib/chat_tui/renderer_component_message.doc.md](../../docs-src/lib/chat_tui/renderer_component_message.doc.md) | Library review |
| 168 | [docs-src/lib/chat_tui/renderer_component_status_bar.doc.md](../../docs-src/lib/chat_tui/renderer_component_status_bar.doc.md) | Library review |
| 169 | [docs-src/lib/chat_tui/renderer_highlight_engine.doc.md](../../docs-src/lib/chat_tui/renderer_highlight_engine.doc.md) | Library review |
| 170 | [docs-src/lib/chat_tui/renderer_lang.doc.md](../../docs-src/lib/chat_tui/renderer_lang.doc.md) | Library review |
| 171 | [docs-src/lib/chat_tui/renderer_page_chat.doc.md](../../docs-src/lib/chat_tui/renderer_page_chat.doc.md) | Library review |
| 172 | [docs-src/lib/chat_tui/renderer_page_shell_security.doc.md](../../docs-src/lib/chat_tui/renderer_page_shell_security.doc.md) | Library review |
| 173 | [docs-src/lib/chat_tui/renderer_pages.doc.md](../../docs-src/lib/chat_tui/renderer_pages.doc.md) | Library review |
| 174 | [docs-src/lib/chat_tui/renderer_shell_approval.doc.md](../../docs-src/lib/chat_tui/renderer_shell_approval.doc.md) | Library review |
| 175 | [docs-src/lib/chat_tui/renderer_shell_security_palette.doc.md](../../docs-src/lib/chat_tui/renderer_shell_security_palette.doc.md) | Library review |
| 176 | [docs-src/lib/chat_tui/shell_management_service.doc.md](../../docs-src/lib/chat_tui/shell_management_service.doc.md) | Library review |
| 177 | [docs-src/lib/chat_tui/shell_security_page_state.doc.md](../../docs-src/lib/chat_tui/shell_security_page_state.doc.md) | Library review |
| 178 | [docs-src/lib/chat_tui/shell_security_snapshot.doc.md](../../docs-src/lib/chat_tui/shell_security_snapshot.doc.md) | Library review |
| 179 | [docs-src/lib/chat_tui/snippet.doc.md](../../docs-src/lib/chat_tui/snippet.doc.md) | Library review |
| 180 | [docs-src/lib/chat_tui/stream.doc.md](../../docs-src/lib/chat_tui/stream.doc.md) | Library review |
| 181 | [docs-src/lib/chat_tui/type_ahead_provider.doc.md](../../docs-src/lib/chat_tui/type_ahead_provider.doc.md) | Library review |
| 182 | [docs-src/lib/chat_tui/types.doc.md](../../docs-src/lib/chat_tui/types.doc.md) | Library review |
| 183 | [docs-src/lib/chat_tui/ui_helpers.doc.md](../../docs-src/lib/chat_tui/ui_helpers.doc.md) | Library review |
| 184 | [docs-src/lib/chat_tui/utf8_edit.doc.md](../../docs-src/lib/chat_tui/utf8_edit.doc.md) | Library review |
| 185 | [docs-src/lib/chat_tui/util.doc.md](../../docs-src/lib/chat_tui/util.doc.md) | Library review |
| 186 | [docs-src/lib/chatmd/chatmd_ast.doc.md](../../docs-src/lib/chatmd/chatmd_ast.doc.md) | Library review |
| 187 | [docs-src/lib/chatmd/chatmd_import_expansion.doc.md](../../docs-src/lib/chatmd/chatmd_import_expansion.doc.md) | Library review |
| 188 | [docs-src/lib/chatmd/chatmd_lexer.doc.md](../../docs-src/lib/chatmd/chatmd_lexer.doc.md) | Library review |
| 189 | [docs-src/lib/chatmd/chatmd_parser.doc.md](../../docs-src/lib/chatmd/chatmd_parser.doc.md) | Library review |
| 190 | [docs-src/lib/chatmd/chatmd_script_declaration.doc.md](../../docs-src/lib/chatmd/chatmd_script_declaration.doc.md) | Library review |
| 191 | [docs-src/lib/chatmd/prompt.doc.md](../../docs-src/lib/chatmd/prompt.doc.md) | Library review |
| 192 | [docs-src/lib/chatmd/source_loader.doc.md](../../docs-src/lib/chatmd/source_loader.doc.md) | Library review |
| 193 | [docs-src/lib/chatmd_shell_spec/architecture.doc.md](../../docs-src/lib/chatmd_shell_spec/architecture.doc.md) | Library review |
| 194 | [docs-src/lib/chatml/chatml_builtin_modules.doc.md](../../docs-src/lib/chatml/chatml_builtin_modules.doc.md) | Library review |
| 195 | [docs-src/lib/chatml/chatml_lang.doc.md](../../docs-src/lib/chatml/chatml_lang.doc.md) | Library review |
| 196 | [docs-src/lib/chatml/chatml_lexer.doc.md](../../docs-src/lib/chatml/chatml_lexer.doc.md) | Library review |
| 197 | [docs-src/lib/chatml/chatml_parser.doc.md](../../docs-src/lib/chatml/chatml_parser.doc.md) | Library review |
| 198 | [docs-src/lib/chatml/chatml_resolver.doc.md](../../docs-src/lib/chatml/chatml_resolver.doc.md) | Library review |
| 199 | [docs-src/lib/chatml/chatml_typechecker.doc.md](../../docs-src/lib/chatml/chatml_typechecker.doc.md) | Library review |
| 200 | [docs-src/lib/chatml/frame_env.doc.md](../../docs-src/lib/chatml/frame_env.doc.md) | Library review |
| 201 | [docs-src/lib/context_compaction/summarizer.doc.md](../../docs-src/lib/context_compaction/summarizer.doc.md) | Library review |
| 202 | [docs-src/lib/definitions.doc.md](../../docs-src/lib/definitions.doc.md) | Library review |
| 203 | [docs-src/lib/dune_describe.doc.md](../../docs-src/lib/dune_describe.doc.md) | Library review |
| 204 | [docs-src/lib/embed_service.doc.md](../../docs-src/lib/embed_service.doc.md) | Library review |
| 205 | [docs-src/lib/embedding.md](../../docs-src/lib/embedding.md) | Library review |
| 206 | [docs-src/lib/environment.doc.md](../../docs-src/lib/environment.doc.md) | Library review |
| 207 | [docs-src/lib/functions.doc.md](../../docs-src/lib/functions.doc.md) | Library review |
| 208 | [docs-src/lib/github.doc.md](../../docs-src/lib/github.doc.md) | Library review |
| 209 | [docs-src/lib/gpt_function.doc.md](../../docs-src/lib/gpt_function.doc.md) | Library review |
| 210 | [docs-src/lib/indexer.doc.md](../../docs-src/lib/indexer.doc.md) | Library review |
| 211 | [docs-src/lib/jsonaf_ext.doc.md](../../docs-src/lib/jsonaf_ext.doc.md) | Library review |
| 212 | [docs-src/lib/log.doc.md](../../docs-src/lib/log.doc.md) | Library review |
| 213 | [docs-src/lib/lru_cache.doc.md](../../docs-src/lib/lru_cache.doc.md) | Library review |
| 214 | [docs-src/lib/markdown_crawler.doc.md](../../docs-src/lib/markdown_crawler.doc.md) | Library review |
| 215 | [docs-src/lib/markdown_indexer.doc.md](../../docs-src/lib/markdown_indexer.doc.md) | Library review |
| 216 | [docs-src/lib/markdown_snippet.doc.md](../../docs-src/lib/markdown_snippet.doc.md) | Library review |
| 217 | [docs-src/lib/mcp/mcp_client.doc.md](../../docs-src/lib/mcp/mcp_client.doc.md) | Library review |
| 218 | [docs-src/lib/mcp/mcp_prompt_agent.doc.md](../../docs-src/lib/mcp/mcp_prompt_agent.doc.md) | Library review |
| 219 | [docs-src/lib/mcp/mcp_server_core.doc.md](../../docs-src/lib/mcp/mcp_server_core.doc.md) | Library review |
| 220 | [docs-src/lib/mcp/mcp_server_http.doc.md](../../docs-src/lib/mcp/mcp_server_http.doc.md) | Library review |
| 221 | [docs-src/lib/mcp/mcp_server_router.doc.md](../../docs-src/lib/mcp/mcp_server_router.doc.md) | Library review |
| 222 | [docs-src/lib/mcp/mcp_tool.doc.md](../../docs-src/lib/mcp/mcp_tool.doc.md) | Library review |
| 223 | [docs-src/lib/mcp/mcp_transport.doc.md](../../docs-src/lib/mcp/mcp_transport.doc.md) | Library review |
| 224 | [docs-src/lib/mcp/mcp_transport_http.doc.md](../../docs-src/lib/mcp/mcp_transport_http.doc.md) | Library review |
| 225 | [docs-src/lib/mcp/mcp_transport_interface.doc.md](../../docs-src/lib/mcp/mcp_transport_interface.doc.md) | Library review |
| 226 | [docs-src/lib/mcp/mcp_transport_stdio.doc.md](../../docs-src/lib/mcp/mcp_transport_stdio.doc.md) | Library review |
| 227 | [docs-src/lib/mcp/mcp_types.doc.md](../../docs-src/lib/mcp/mcp_types.doc.md) | Library review |
| 228 | [docs-src/lib/md_index_catalog.doc.md](../../docs-src/lib/md_index_catalog.doc.md) | Library review |
| 229 | [docs-src/lib/merlin.doc.md](../../docs-src/lib/merlin.doc.md) | Library review |
| 230 | [docs-src/lib/meta_prompting.doc.md](../../docs-src/lib/meta_prompting.doc.md) | Library review |
| 231 | [docs-src/lib/meta_prompting/aggregator.doc.md](../../docs-src/lib/meta_prompting/aggregator.doc.md) | Library review |
| 232 | [docs-src/lib/meta_prompting/context.doc.md](../../docs-src/lib/meta_prompting/context.doc.md) | Library review |
| 233 | [docs-src/lib/meta_prompting/evaluator.doc.md](../../docs-src/lib/meta_prompting/evaluator.doc.md) | Library review |
| 234 | [docs-src/lib/meta_prompting/meta_prompting.doc.md](../../docs-src/lib/meta_prompting/meta_prompting.doc.md) | Library review |
| 235 | [docs-src/lib/meta_prompting/mp_flow.doc.md](../../docs-src/lib/meta_prompting/mp_flow.doc.md) | Library review |
| 236 | [docs-src/lib/meta_prompting/preprocessor.doc.md](../../docs-src/lib/meta_prompting/preprocessor.doc.md) | Library review |
| 237 | [docs-src/lib/meta_prompting/prompt_factory.doc.md](../../docs-src/lib/meta_prompting/prompt_factory.doc.md) | Library review |
| 238 | [docs-src/lib/meta_prompting/prompt_factory_online.doc.md](../../docs-src/lib/meta_prompting/prompt_factory_online.doc.md) | Library review |
| 239 | [docs-src/lib/meta_prompting/prompt_intf.doc.md](../../docs-src/lib/meta_prompting/prompt_intf.doc.md) | Library review |
| 240 | [docs-src/lib/meta_prompting/prompting_guides.doc.md](../../docs-src/lib/meta_prompting/prompting_guides.doc.md) | Library review |
| 241 | [docs-src/lib/meta_prompting/prompts.doc.md](../../docs-src/lib/meta_prompting/prompts.doc.md) | Library review |
| 242 | [docs-src/lib/meta_prompting/recursive_mp.doc.md](../../docs-src/lib/meta_prompting/recursive_mp.doc.md) | Library review |
| 243 | [docs-src/lib/meta_prompting/task_intf.doc.md](../../docs-src/lib/meta_prompting/task_intf.doc.md) | Library review |
| 244 | [docs-src/lib/mime.doc.md](../../docs-src/lib/mime.doc.md) | Library review |
| 245 | [docs-src/lib/notty-eio/notty_eio.doc.md](../../docs-src/lib/notty-eio/notty_eio.doc.md) | Library review |
| 246 | [docs-src/lib/notty_scroll_box.doc.md](../../docs-src/lib/notty_scroll_box.doc.md) | Library review |
| 247 | [docs-src/lib/oauth/oauth2_client_credentials.doc.md](../../docs-src/lib/oauth/oauth2_client_credentials.doc.md) | Library review |
| 248 | [docs-src/lib/oauth/oauth2_client_store.doc.md](../../docs-src/lib/oauth/oauth2_client_store.doc.md) | Library review |
| 249 | [docs-src/lib/oauth/oauth2_http.doc.md](../../docs-src/lib/oauth/oauth2_http.doc.md) | Library review |
| 250 | [docs-src/lib/oauth/oauth2_manager.doc.md](../../docs-src/lib/oauth/oauth2_manager.doc.md) | Library review |
| 251 | [docs-src/lib/oauth/oauth2_pkce.doc.md](../../docs-src/lib/oauth/oauth2_pkce.doc.md) | Library review |
| 252 | [docs-src/lib/oauth/oauth2_pkce_flow.doc.md](../../docs-src/lib/oauth/oauth2_pkce_flow.doc.md) | Library review |
| 253 | [docs-src/lib/oauth/oauth2_server_client_storage.doc.md](../../docs-src/lib/oauth/oauth2_server_client_storage.doc.md) | Library review |
| 254 | [docs-src/lib/oauth/oauth2_server_routes.doc.md](../../docs-src/lib/oauth/oauth2_server_routes.doc.md) | Library review |
| 255 | [docs-src/lib/oauth/oauth2_server_storage.doc.md](../../docs-src/lib/oauth/oauth2_server_storage.doc.md) | Library review |
| 256 | [docs-src/lib/oauth/oauth2_server_types.doc.md](../../docs-src/lib/oauth/oauth2_server_types.doc.md) | Library review |
| 257 | [docs-src/lib/oauth/oauth2_types.doc.md](../../docs-src/lib/oauth/oauth2_types.doc.md) | Library review |
| 258 | [docs-src/lib/ocaml_parser.doc.md](../../docs-src/lib/ocaml_parser.doc.md) | Library review |
| 259 | [docs-src/lib/odoc_crawler.doc.md](../../docs-src/lib/odoc_crawler.doc.md) | Library review |
| 260 | [docs-src/lib/odoc_indexer.doc.md](../../docs-src/lib/odoc_indexer.doc.md) | Library review |
| 261 | [docs-src/lib/odoc_snippet.doc.md](../../docs-src/lib/odoc_snippet.doc.md) | Library review |
| 262 | [docs-src/lib/openai/completions.doc.md](../../docs-src/lib/openai/completions.doc.md) | Library review |
| 263 | [docs-src/lib/openai/embeddings.doc.md](../../docs-src/lib/openai/embeddings.doc.md) | Library review |
| 264 | [docs-src/lib/openai/responses.doc.md](../../docs-src/lib/openai/responses.doc.md) | Library review |
| 265 | [docs-src/lib/package_index.doc.md](../../docs-src/lib/package_index.doc.md) | Library review |
| 266 | [docs-src/lib/parallel_tool_calls.doc.md](../../docs-src/lib/parallel_tool_calls.doc.md) | Library review |
| 267 | [docs-src/lib/prompt_session.doc.md](../../docs-src/lib/prompt_session.doc.md) | Library review |
| 268 | [docs-src/lib/session.doc.md](../../docs-src/lib/session.doc.md) | Library review |
| 269 | [docs-src/lib/session_store.doc.md](../../docs-src/lib/session_store.doc.md) | Library review |
| 270 | [docs-src/lib/shell_access/architecture.doc.md](../../docs-src/lib/shell_access/architecture.doc.md) | Library review |
| 271 | [docs-src/lib/shell_runtime/architecture.doc.md](../../docs-src/lib/shell_runtime/architecture.doc.md) | Library review |
| 272 | [docs-src/lib/source.doc.md](../../docs-src/lib/source.doc.md) | Library review |
| 273 | [docs-src/lib/template.doc.md](../../docs-src/lib/template.doc.md) | Library review |
| 274 | [docs-src/lib/tikitoken.doc.md](../../docs-src/lib/tikitoken.doc.md) | Library review |
| 275 | [docs-src/lib/ttl_lru_cache.doc.md](../../docs-src/lib/ttl_lru_cache.doc.md) | Library review |
| 276 | [docs-src/lib/vector_db.doc.md](../../docs-src/lib/vector_db.doc.md) | Library review |
| 277 | [docs-src/lib/webpage_markdown/driver.doc.md](../../docs-src/lib/webpage_markdown/driver.doc.md) | Library review |
| 278 | [docs-src/lib/webpage_markdown/fetch.doc.md](../../docs-src/lib/webpage_markdown/fetch.doc.md) | Library review |
| 279 | [docs-src/lib/webpage_markdown/html_to_md.doc.md](../../docs-src/lib/webpage_markdown/html_to_md.doc.md) | Library review |
| 280 | [docs-src/lib/webpage_markdown/md_render.doc.md](../../docs-src/lib/webpage_markdown/md_render.doc.md) | Library review |
| 281 | [docs-src/lib/webpage_markdown/tool.doc.md](../../docs-src/lib/webpage_markdown/tool.doc.md) | Library review |
| 282 | [docs-src/meta_prompting/evaluator.doc.md](../../docs-src/meta_prompting/evaluator.doc.md) | Library review |
| 283 | [docs-src/meta_prompting/recursive_mp.doc.md](../../docs-src/meta_prompting/recursive_mp.doc.md) | Library review |
| 284 | [docs-src/meta_prompting/templates.doc.md](../../docs-src/meta_prompting/templates.doc.md) | Library review |
| 285 | [docs-src/notty_examples_research.md](../../docs-src/notty_examples_research.md) | Historical review |
| 286 | [docs-src/notty_examples_research.md.report.md](../../docs-src/notty_examples_research.md.report.md) | Historical review |
| 287 | [docs-src/openai_responses_tool_output.md](../../docs-src/openai_responses_tool_output.md) | Historical review |
| 288 | [docs-src/overview/chatmd-language.md](../../docs-src/overview/chatmd-language.md) | User docs candidate |
| 289 | [docs-src/overview/chatmd-shell-runtime.md](../../docs-src/overview/chatmd-shell-runtime.md) | User docs candidate |
| 290 | [docs-src/overview/chatmd-shell-tools.md](../../docs-src/overview/chatmd-shell-tools.md) | User docs candidate |
| 291 | [docs-src/overview/project.md](../../docs-src/overview/project.md) | User docs candidate |
| 292 | [docs-src/overview/tools.md](../../docs-src/overview/tools.md) | User docs candidate |
| 293 | [docs-src/response_api_tool_output_image_support.md](../../docs-src/response_api_tool_output_image_support.md) | Historical review |
| 294 | [docs-src/shell/README.md](../../docs-src/shell/README.md) | User docs candidate |
| 295 | [docs-src/test/chat_tui_type_ahead_debounce_test.doc.md](../../docs-src/test/chat_tui_type_ahead_debounce_test.doc.md) | Maintainer review |
| 296 | [docs-src/test/chat_tui_type_ahead_test.doc.md](../../docs-src/test/chat_tui_type_ahead_test.doc.md) | Maintainer review |
| 297 | [docs-src/tools/README.md](../../docs-src/tools/README.md) | User docs candidate |

## 25. Codebase review and requirements audit

### 25.1 Review scope and evidence

**Reviewed:** 2026-09-06 at revision `bc76b6c72a280b4bad48a793a63373e6db26d2b4`.

The follow-up compared the website specification and its implementation phases with the actual documentation generators/checkers, executable declarations, TUI mode normalization, standalone stdio startup, embedded-host defaults, ChatMD source/import contracts, provider transport implementation, and the documented host/security/runtime boundaries. It also re-inventoried all 297 tracked Markdown documents, inspected cross-root link destinations and markup shapes, and checked current official framework/hosting/Dune documentation for specific integration questions.

The review focused on requirements that affect a correct public website and migration. It did not audit every runtime module, establish support for every platform, or certify every historical snippet. A filesystem/text inventory is not an Astro rendering test. The website still needs the concrete implementation spike, browser checks, and hosted verification described in the phases.

### 25.2 Findings incorporated into the specification

| ID | Finding or missing requirement | Evidence | Resolution in this spec | Implementation tasks |
|---|---|---|---|---|
| A01 | Some canonical Markdown is generated from code, so ordinary editorial edits can be overwritten or break parity | `docs_inventory.ml`, `docs_check.ml` | Section 10.15 defines generator ownership and read-only website consumption | P02.10, P05.11 |
| A02 | Existing tests depend on exact headings, fence labels, source bytes, and directory-based selection | `docs_chatml.ml`, `docs_examples.ml`, `docs_smoke.ml`, `test/agent_docs/dune` | Preserve or deliberately migrate validation with the source | P02.10, P05.11 |
| A03 | Website/npm directories need a Dune boundary, independently of Git tracking | Root build layout and Dune directory documentation | Section 13.9 specifies exclusion and coexistence checks | P01.09 |
| A04 | Raw-markup handling must preserve existing anchors and keyboard/code semantics | 61 explicit anchors across 12 docs; `kbd`, `code`, `br` usage | Sections 10.8 and 10.16 plus C22/B16 fixtures | P02.11 |
| A05 | Real source contains XML-like prose and an invalid same-length nested-fence example | ChatMD parser and Markdown renderer sidecars | Reviewed source repairs and exact intended-output fixtures, without globally rewriting code | P02.11 |
| A06 | The docs-src inventory omits useful root/prompt/example material and historical transcripts | Tracked root, `prompt-examples/`, and `real-world-example-session/` inventory | Supplemental manifest and explicit publication scope | P00.09, P05.10 |
| A07 | Single-file downloads can omit imported prompts/scripts or OCaml build companions | ChatMD source-loader contracts and tracked example layout | Section 9.8 requires a declared dependency closure or honest template labeling | P06.10 |
| A08 | Local stdio has a documented transient-root RNG startup limitation | Stdio binary, embedded host, current tutorial/troubleshooting | Preserve explicit private data-root workaround and verification status | P06.11 |
| A09 | Website HTTPS must not be confused with runtime provider TLS verification | `lib/io.ml` and current permissions guide | Contextual preservation of the existing provider transport qualification | P06.11 |
| A10 | Broad feature coverage and host qualifications were too implicit in the original route plan | Tools, ChatML, retrieval, compaction, refinement, CLI and embedding surfaces | Section 5.6 coverage matrix and Section 9.7 migration invariants | P05.10, P06.11 |
| A11 | Manifest publication fields do not automatically become framework exclusions | Starlight frontmatter and Astro sitemap APIs | Section 10.18 requires explicit adapters and independent output assertions | P02.13 |
| A12 | Generated mtimes, mutable code links, and shallow history can misrepresent provenance | Proposed generated-content architecture and exact-source contract checks | Section 10.19 separates source revision, edit branch, and factual dates | P08.09 |
| A13 | Source generation needs a defined failure state and real-path containment | Importer/write/watch design and source-loader boundary review | Section 10.19 stages output; Section 10.17 checks symlinks | P02.12 |
| A14 | Static request pricing does not cover file-size/count or build capacity constraints | Workers platform/build limit references | Section 17.12 adds final-artifact and plan checks | P10.10 |
| A15 | A deployment trigger can race required checks, and preview noindex artifacts differ from production | Proposed Git/hosting workflow | Section 17.13 assigns actual enforcement and environment-specific validation | P10.11 |
| A16 | Distributing examples/grammar assets needs notice preservation; README fragments need continuity | Project/grammar notices and existing README navigation | Sections 12.5 and 10.14 require notice retention and meaningful old README headings | P06.10, P08.09 |

These are closed **specification gaps**. The implementation tasks remain unchecked. Adding a requirement to this document does not mean the corresponding website behavior or runtime correction has been implemented.

### 25.3 Verification performed during this review

The existing documentation gate was explicitly rerun, rather than relying only on a cached successful build:

```sh
dune build --force @agent-docs-check
```

Observed result: successful exit with the final report “Documentation checks passed: 297 pages, 38 methods; no live provider calls.” Intermediate output reported successful ChatML moderator source parity/compilation/state/tool rejection, selected compiled documentation examples, generated observer workflow, library behavior checks, request codec validation, and the custom-tool example.

The local Dune version was 3.21.1 in the `default` opam switch. This evidence applies to that local environment and the recorded source revision. No live-provider, soak, broad end-to-end, or public deployment tests were run as part of this review.

The specification itself is checked for valid local link targets and contents anchors, balanced code fences, sequential unique phase task IDs, intact 297-row source inventory, and consistent review/task cross-references. Full Markdown rendering under Astro remains a P02 implementation check.

### 25.4 Remaining implementation-time decisions

The following remain explicit verification work rather than omitted requirements:

1. Pin the exact compatible frontend versions and prove the real Starlight integration.
2. Assign final per-page publication dispositions through editorial review; the inventory's review classes are not final status assertions.
3. Correct identified Markdown source hazards in a separately reviewed implementation change.
4. Verify tutorials on their claimed host/platform and distinguish offline checks from any permitted live demonstration.
5. Measure actual search relevance, accessibility, browser behavior, and asset budgets after building the site.
6. Decide whether fresh odoc output is ready for the initial release.
7. Confirm domain availability, price, account ownership, and hosted behavior at launch.

No website or runtime source was changed by this audit; the deliverable is the strengthened specification. The review supports proceeding with P00–P02, with concrete gates for the matters that cannot be verified before implementation.
