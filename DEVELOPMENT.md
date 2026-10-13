# Local OCaml documentation & search workflows

## Complexity estimates and cycle tracking

Cycles are analytics windows, not deadlines. They do not change milestone order, dependency gates, engineering standards, review requirements or the agreed merge/CI gates.

Use Fibonacci estimates **1, 2, 3, 5, 8**. Estimate technical complexity for one cohesive, reviewable outcome: interacting contracts, breadth of semantic change, uncertainty and the validation needed to establish correctness. Points are not hours, agent count, lines changed, test count or a performance target. More parallel agents do not reduce a ticket's complexity.

| Points | Working anchor |
| --- | --- |
| 1 | A localized, understood change or narrow investigation with a straightforward contract and validation. |
| 2 | A bounded component change with a few interactions or edge cases and established implementation patterns. |
| 3 | A moderate cohesive feature or substantial bounded investigation involving several invariants or callers. |
| 5 | A substantial coherent component spanning layers or meaningful uncertainty, with a clear ownership and validation boundary. |
| 8 | The largest sensible single ticket: multiple interacting contracts and substantial validation, but one outcome that can be implemented and reviewed coherently. |

Choose the closest anchor holistically; do not add a point per file, layer or risk. Include required documentation, integration and correctness evidence. Research is estimated for its own questions and deliverables, not the implementation it may generate.

## Ticket workflow

1. Before starting a ticket, read its acceptance criteria and dependencies, set its estimate and record a short rationale. Preserve an existing estimate unless scope or new evidence warrants an explained revision.
2. If the work exceeds 8, split it into independently verifiable, cohesive outcomes before further implementation. Preserve all acceptance requirements and dependencies. A research ticket can first resolve uncertainty and produce an estimated implementation breakdown.
3. Estimate actual work once. Leave pure tracking parents unestimated; estimate their implementation children. If a parent retains real work, explicitly distinguish that work from its children.
4. Before marking a ticket Done, ensure it has an estimate and the required review, validation and merge evidence. Tickets already In Review receive estimates before closure.
5. Keep the original estimate and scope-change rationale in comments when changing points. Do not rewrite estimates merely to match elapsed time or make analytics look better.
6. Retrospective estimates must be labelled as such. Keep historical delivered tickets intact; document any oversized aggregate rather than manufacturing a past breakdown.
7. Review the anchors after a few cycles using concrete completed-ticket comparisons, surprising complexity and decomposition quality. Version substantive rubric changes. Retrospective points are an initial baseline, not evidence of forecasting accuracy.

The practical size limit is a coherent outcome with bounded contracts and a reviewable validation argument, not a fixed number of features or files. Safety and correctness remain absolute; splitting work never drops acceptance requirements.

## Cycle estimation calibration

Review at cycle close using the completed cohort. If tickets remain unfinished, publish a provisional review and add a final review when that cohort is complete; do not delay learning or call unfinished work delivered. Record tickets added, removed, split or cancelled, and preserve their original cycle membership in the review. Completion includes required review, tests and merge evidence.

1. **Preserve the baseline.** At ticket start, record the estimate, rubric version, acceptance scope, estimate date and a short rationale naming the main contracts, uncertainty and validation. Mark estimates assigned after implementation as retrospective. Keep original estimates when scope or estimates change; missing historical evidence stays unknown rather than being reconstructed as fact.
2. **Build one evidence table.** For each ticket, include its original estimate and timing, original versus delivered scope, relevant PRs, final acceptance evidence, discovered dependencies, required validation and material review/rework findings. Record active work and waiting time separately when reliably available. Neither elapsed time, token use nor lines changed defines complexity.
3. **Assess realized complexity independently.** Review the delivered work against the same rubric, preferably with the original score hidden during the first pass. Propose a retrospective comparison score with evidence and confidence, then reconcile reviewer disagreements. This is a judgment about complexity, not a measured quantity called actual points. Separate the difficulty of the original scope from added or removed scope.
4. **Explain differences.** Classify each outcome as well calibrated, overestimated, underestimated, changed scope, poorly decomposed or insufficient evidence. Identify causes: unknown design constraints, missed dependencies/integration, underestimated correctness work, reusable infrastructure that simplified the work, or avoidable implementation/review churn. A long external wait is a workflow finding, not automatically an estimation miss. Record mixed causes rather than forcing one label.
5. **Compare across tickets.** Sort by assessed complexity and compare adjacent scores and tickets with the same score. Report sample counts, prospective versus retrospective coverage, agreement and movement between Fibonacci categories. Treat points as ordered relative categories, not a ratio scale; do not report a 5-point ticket as a measured percentage more difficult than a 3-point ticket. Analyze research and implementation examples with their different deliverables in view. Exclude tracking parents from point totals and retain parent/child lineage when splits occur.
6. **Choose concrete anchors.** Select completed, reviewed tickets that exemplify each supported score, plus boundary examples explaining why a ticket is 3 rather than 5 or 5 rather than 8. Do not invent anchors for scores without evidence. Describe interacting contracts, semantic breadth, uncertainty and the correctness argument; avoid mechanical file-count or subsystem-count formulas. Work that lacks one cohesive reviewable outcome or exceeds the 8-point anchor should be decomposed before implementation.
7. **Publish the next rubric and test it.** Keep the cycle report, evidence table, anchor tickets, rubric changes and a small set of actionable improvements in the project. Version substantive changes as the next rubric; use them for future estimates without rewriting historical scores. Calibrate a few upcoming tickets against the new anchors and check agreement again next cycle. Do not optimize point throughput at the expense of correctness or interpret more completed points as inherently better work.

The first cycle contains retrospectively estimated completed work. Use it to establish comparative anchors, but assess forecast accuracy only for tickets estimated before work began. Later-discovered defects or integration work can update the review as new evidence; retain the earlier assessment and explain the revision. A cycle report should distinguish estimation misses, scope changes and execution/process issues, with a concrete adjustment for each recurring cause.

## Current agent-server and documentation workflow

Start at [the documentation index](docs-src/README.md) and
[agent-core embedding](docs-src/agent-server/embedding.md). Build current hosts with
`dune build bin/chat_tui.exe bin/ochat_agent_server.exe bin/ochat_agent_stdio.exe`.
Installed names are `chat-tui`, `ochat-agent-server`, and `ochat-agent-stdio`.
The old `mcp_server` prompt-serving host is deprecated; maintained MCP tools are
not. Prefer the new daemon tutorials for remote agent sessions.

Use `dune build @agent-docs-check` for offline documentation validation and
`dune runtest` for normal tests. E2E/load/soak/live-provider/manual tiers remain
explicit; see [testing](docs-src/agent-server/testing.md). The architecture and
implementation specs now live in `docs-src/design/`.

Markdown guides are not automatically rendered by odoc. To index all repository
prose, use `docs-src` rather than only `docs-src/lib`; indexing may call an embedding
provider and is not part of offline docs validation. The API/odoc indexing workflow
below is separate. Core is the standard library; new application I/O uses Eio.

## Maintaining the installed authoring reference

Start with the [extensibility maintenance handoff](docs-src/development/chatml-extensibility-handoff.md)
for execution forms, ownership, host boundaries, migration and qualification.

The authoring corpus embeds selected `docs-src` guides and compiler-owned
signatures, so a running agent can retrieve the installed version without this
checkout or network access. `lib/authoring_sources/dune` also embeds hashes of
reviewed implementation files; it does not embed their source bodies. Runtime
reference assembly lives in `lib/authoring_sources/authoring_corpus.ml`.

There are six separate coverage inventories: compiler bindings, grammar
productions, language semantics, ChatMD declarations, native operations and
runtime transactions/recovery. Runtime mappings include compilation/execution
policy and distinguish shared job APIs from moderator state/control, subscriptions,
timers, notifications and ingress. A topic
being readable on a surface does not grant its described capabilities.

When changing an inventoried implementation or its documentation:

1. Review the behavior and all affected reference topics, including prerequisites.
   Add a feature entry when behavior is new; source hashes cannot discover new
   features. Add the implementation path to the source bundle if necessary.
2. Update the shared guide and its checked examples. Run the relevant behavior
   suites named by the mappings; an evidence path is not evidence of a passing run.
3. Build the maintainer exporter and inspect candidate pins. For runtime contracts:

   ```bash
   dune build test/authoring_sources/review_coverage.exe
   _build/default/test/authoring_sources/review_coverage.exe --runtime
   ```

   `--semantics`, `--declarations`, `--native` and `--grammar` select the other
   maintained taxonomies. `--compiler` reports all compiler binding candidates
   alongside their previous reviewed pins and topics. Selected candidates use `MODULE TOPIC_ID`,
   `--globals TOPIC_ID` or `--alias NAME TOPIC_ID`. An optional list of exact
   surface IDs limits the report. Output contains candidate hashes for review;
   the exporter never rewrites approved mappings.
4. Update only the reviewed source/topic pins in the corresponding coverage data
   module (compiler bindings are maintained in `authoring_corpus.ml`). A topic pin
   includes its entire prerequisite closure. After source pins match, use
   `review_coverage.exe --changed-docs` to identify changed topic closures across
   all six inventories. Do not regenerate pins automatically in CI.
5. Run `dune build @test/authoring_sources/runtest @agent-docs-check` and the affected
   runtime suites. The authoring query service also checks the installed inventories
   before serving guidance. A stale mapping therefore needs an explicit review.

Inventory tests prove completeness against the maintained feature lists, not
against every possible runtime behavior. The installed corpus supplies qualified
task packages; completeness is checked against the selected surface, prerequisites
and maintained inventories. Offline example checks and the authoring evaluation harness do not
establish model-quality results; optional provider evaluation is separate.


This document explains how to set up a development environment in which
**all installed libraries in the current opam switch** and this project’s
own documentation are searchable via:

- a **vector + BM25 index** of HTML docs generated by `odoc`
  (`odoc-index` / `odoc-search`);
- a **semantic index** of the project’s Markdown docs
  (`md-index` / `md-search`);
- a **type-signature search database** built with `sherlodoc`.

The goal is to give both humans and LLM agents a rich, *local* knowledge
base of the OCaml ecosystem and this repository.

All commands below assume:

- you are in the **project root** of this repository; and
- your shell is already configured to use the **correct opam switch**
  (e.g. you started it with `opam switch <name>` or `opam exec -- bash`
  once, but we omit `opam exec --` from each command).

---

## 0. Prerequisites

You should have, in the *active* switch:

- OCaml ≥ 5.1 (see `dune-project`),
- `dune` ≥ 3.18,
- `odoc`, `odig`, and `sherlodoc` installed:

  ```bash
  opam install dune odoc odig sherlodoc
  ```

Environment:

- `OPENAI_API_KEY` set – required by:
  - `odoc-index` and `odoc-search`,
  - `md-index` and `md-search`,
  - `ochat index` / `ochat query`.
- `OPAM_SWITCH_PREFIX` set – this is done automatically by `opam` for the
  active switch and is used to locate the `odig` caches.

Throughout the rest of this document we assume these are already set.

---

## 1. Install the current dune project into the switch

Before indexing, install the project so that:

- its libraries and docs appear as *installed packages* in the switch;
- `odig` and `odoc` can see the project’s documentation;
- the CLI tools in `bin/` are built and runnable via `dune exec` or from
  your `$PATH` once installed.

From the project root:

```bash
dune build @install
dune install
```

After this:

- The umbrella CLI `ochat` and helper binaries (`odoc-index`, `md-index`,
  `odoc-search`, `md-search`, `mcp_server`, etc.) are installed in the
  switch.
- `odig` will treat the project’s libraries (e.g. `ochat`) like any other
  opam package.

---

## 2. Generate ODoc HTML for all installed packages (via `odig`)

`odoc-index` and `sherlodoc` both work on top of the HTML/odoc artefacts
cached by `odig`.

### 2.1 Optional: clear the odig cache

If you want a *hard refresh* (e.g. after pinning many packages or changing
switches):

```bash
odig cache clear
```

This wipes `odig`’s cache under:

```text
$OPAM_SWITCH_PREFIX/var/cache/odig/
```

### 2.2 Regenerate docs for all installed packages

```bash
odig odoc
```

This:

- compiles `.odoc` / `.odocl` documentation files; and
- produces HTML docs under:

  - `"$OPAM_SWITCH_PREFIX/var/cache/odig/html/"` – HTML hierarchy
  - `"$OPAM_SWITCH_PREFIX/var/cache/odig/odoc/"` – `.odocl` files

These locations are used in later steps.

---

## 3. Index type signatures with `sherlodoc`

[`sherlodoc`](https://github.com/art-w/sherlodoc) provides **fuzzy type
search** over `.odocl` files. We’ll point it at the `odig` cache.

### 3.1 Choose a database location

Decide where the Sherlodoc database should live, e.g. inside the project:

```bash
export SHERLODOC_DB="$PWD/.cache/sherlodoc.marshal"
mkdir -p "$(dirname "$SHERLODOC_DB")"
```

(If you prefer the `ancient` format on older OCaml, follow the package docs.)

### 3.2 Index all odoc type information from the current switch

```bash
sherlodoc index \
  $(find "$OPAM_SWITCH_PREFIX/var/cache/odig/odoc" -name '*.odocl' | grep -v __)
```

This:

- reads all `.odocl` files generated by `odig odoc`;
- ignores internal artefacts containing `__` in the path;
- writes a Sherlodoc database to `$SHERLODOC_DB`.

### 3.3 Query by type

You can now run type-based queries such as:

```bash
sherlodoc search --print-docstring-html "('a list -> 'a list) -> 'a list -> 'a list"
```

Sherlodoc will:

- fuzzy-match the query against the indexed type signatures;
- print matching symbols and docstrings (HTML).

This database is useful to LLM agents that need to answer “what function
has this type?” questions over your *entire* environment.

---

## 4. Build a vector + BM25 index over HTML docs (`odoc-index`)

`odoc-index` (implemented in `bin/odoc_index.ml` and documented in
`docs-src/bin/odoc_index.doc.md`) converts odoc-generated HTML into:

- dense vectors in a [`Vector_db`](docs-src/lib/vector_db.doc.md) corpus;
- BM25 lexical indices;
- Markdown snippets.

By default, it writes one subdirectory per opam package, under
`.odoc_index/`.

### 4.1 Run the indexer

From the project root:

```bash
dune exec bin/odoc_index.exe -- \
  --root "$OPAM_SWITCH_PREFIX/var/cache/odig/html/"
```

Once the binary is installed in the switch, you can also invoke it directly:

```bash
odoc-index --root "$OPAM_SWITCH_PREFIX/var/cache/odig/html/"
```

Optional: change the output directory (defaults to `.odoc_index`):

```bash
odoc-index \
  --root "$OPAM_SWITCH_PREFIX/var/cache/odig/html/" \
  --out  .odoc_index
```

Resulting layout (simplified):

```text
.odoc_index/
  ├─ core/
  │   ├─ vectors.binio
  │   ├─ bm25.binio
  │   ├─ <id>.md
  │   └─ ...
  ├─ eio/
  ├─ ochat/
  └─ package_index.binio
```

### 4.2 Package filter in `bin/odoc_index.ml`

`bin/odoc_index.ml` hard-codes a `package_filter` to avoid reindexing
noisy or duplicated docs while always updating key packages:

```ocaml
Odoc_indexer.index_packages
  ~filter:
    (Odoc_indexer.Update
       ( Exclude
           [ "ocaml"
           ; "ocaml_intrinsics_kernel"
           ; "ocaml-compiler-libs"
           ; "ocamlgraph"
           ; "tls"
           ]
       , [ "ochat"; "textmate-language"; "irmin"; "irmin-git"; "irmin-watcher" ] ))
  ~env
  ~root:root_path
  ~output:out_path
  ~net:env#net
  ()
```

Interpretation:

- `Exclude [...]` — skip the listed packages entirely (mostly stdlib /
  compiler internals / noisy libs).
- `Update (prev, [ pkgs ])` — treat the given `pkgs` as *must-update*
  even if incremental logic would otherwise skip them.

To adjust behaviour:

- **Add newly important packages**  
  Append them to the second list:

  ```ocaml
  , [ "ochat"
    ; "textmate-language"
    ; "irmin"
    ; "irmin-git"
    ; "irmin-watcher"
    ; "your-new-package"
    ]
  ```

- **Full refresh of all packages (except exclusions)**  
  Temporarily simplify the filter to a plain `Exclude`:

  ```ocaml
  ~filter:(Odoc_indexer.Exclude
             [ "ocaml"
             ; "ocaml_intrinsics_kernel"
             ; "ocaml-compiler-libs"
             ; "ocamlgraph"
             ; "tls"
             ])
  ```

  This asks `Odoc_indexer` to rebuild *every* non-excluded package.

See `docs-src/lib/odoc_indexer.doc.md` for full semantics of
`package_filter` (`All`, `Include [pkgs]`, `Exclude [pkgs]`,
`Update (prev, pkgs)`).

---

## 5. Build semantic indexes for Markdown docs (`md-index`)

This project ships two Markdown indexes (see `bin/md_index.ml` and
`docs-src/bin/md_index.doc.md`), both rooted at `docs-src/lib` but with
different **logical names**:

- `docs-src/bin`
- `docs-src/lib`

Each call to `md-index`:

- crawls the `--root` tree for `*.md` / `*.markdown` / `*.mdx` files;
- slices them into token-bounded overlapping windows;
- embeds all unseen snippets with the OpenAI embeddings API;
- writes a vector DB to `.md_index/<name>/`.

### 5.1 Index the project docs

```bash
dune exec bin/md_index.exe -- \
  --root docs-src/lib \
  --name "docs-src/bin"
```

```bash
dune exec bin/md_index.exe -- \
  --root docs-src/lib \
  --name "docs-src/lib"
```

Notes:

- `--root` is the directory tree to crawl. In the current setup both
  logical indexes happen to be built from `docs-src/lib`; you can point
  them at separate subtrees if you prefer (e.g. `docs-src/bin` vs
  `docs-src/lib`).
- `--name` is just a *label*; it determines:
  - the subdirectory under `.md_index/`,
  - how the index is referred to by `md-search --index <name>`.
- `--out` defaults to `.md_index`. You can override it if you want the
  vector DB elsewhere.

The on-disk layout for an index called `docs-src/lib` looks like:

```text
.md_index/
  └─ docs-src/lib/
       ├─ vectors.binio
       ├─ snippets/
       │    ├─ <id>.md
       │    └─ ...
       ├─ meta.json
       └─ ...
md_index_catalog.binio
```

The global `md_index_catalog.binio` (in `.md_index/`) stores centroids
and metadata so tools can quickly pick relevant indexes.

---

## 6. Optional: smoke-test the indexes

Once all indexes are built you can run quick checks.

### 6.1 Search odoc snippets

```bash
dune exec bin/odoc_search.exe -- \
  --query "query newly indexed opam doc snippets" \
  -k 20
```

Or, using the installed binary:

```bash
odoc-search --query "query newly indexed opam doc snippets" -k 20
```

Expected behaviour:

- The tool embeds the query text using OpenAI.
- It selects a subset of packages (via `package_index.binio`) if you did
  not pass `--package`.
- It prints ranked Markdown snippets with package and id:

  ```text
  [1] [eio] 0000abcd-...:
  <snippet markdown>

  ---
  ```

See `docs-src/bin/odoc_search.doc.md` for full CLI details.

### 6.2 Search Markdown docs

```bash
dune exec bin/md_search.exe -- \
  --query "textmate newly indexed markedow docs" \
  -k 10 \
  --index docs-src/lib
```

Or, after installation:

```bash
md-search --query "textmate newly indexed markedow docs" \
          --index docs-src/lib \
          -k 10
```

This:

- embeds the query;
- either searches a specific index (`--index docs-src/lib`), or if you
  pass `--index all`, picks the five closest ones by centroid;
- prints the best‑matching snippet bodies.

See `docs-src/bin/md_search.doc.md` for details and limitations.

### 6.3 Re-check Sherlodoc type search

```bash
sherlodoc search --print-docstring-html "sherlodoc type search query"
```

If everything is wired correctly you should see matching symbols with
their docstrings rendered as HTML.

---

## 7. How this helps LLM agents

Once the above workflows have been run **inside the same workspace** as
your agent, the following local resources become available:

- **Type-level search** (`sherlodoc` on `$SHERLODOC_DB`) for any
  installed package.
- **API‑level semantic search** over docs (`odoc-search` over
  `.odoc_index/`).
- **Project‑doc semantic search** over Markdown (`md-search` over
  `.md_index/`).
- **Source‑level indexing & search** via the umbrella `ochat` CLI:

  ```bash
  # index OCaml source files under ./lib into ./vector
  ochat index -folder-to-index ./lib -vector-db-folder ./vector

  # search those snippets
  ochat query -vector-db-folder ./vector \
              -query-text "tail-recursive map over a list" \
              -num-results 5
  ```

For agent integrations:

- If your LLM runtime can call **shell commands**, you can expose:

  - `odoc-search`, `md-search`, and `sherlodoc search` as tools;
  - optionally `ochat index` / `ochat query` for source‑level search.

- This repository also ships an **MCP stdio server** (`bin/mcp_server.ml`)
  that exposes generic tools over JSON‑RPC:

  - `echo`
  - `apply_patch` (V4A diff)
  - `read_dir`
  - `get_contents`
  - `meta_refine`
  - `webpage_to_markdown`
  - plus dynamically loaded `.chatmd` prompts under `./prompts` or
    `$MCP_PROMPTS_DIR`.

  You can run it as:

  ```bash
  dune exec bin/mcp_server.exe
  # or over HTTP:
  dune exec bin/mcp_server.exe -- --http 8080
  ```

  While the MCP server does not (yet) expose doc-search as first‑class
  tools, the combination of:

  - local vector indexes (`.odoc_index`, `.md_index`, `$SHERLODOC_DB`),
  - filesystem access (`read_dir`, `get_contents`),
  - and `apply_patch`

  provides a solid foundation for LLM agents that reason about and edit
  this OCaml codebase.

---

## 8. Summary

To prepare a fresh environment where LLM agents (and humans) can query
local OCaml libraries and this repository:

1. **Install the project** into the active opam switch:

   ```bash
   dune build @install
   dune install
   ```

2. **Regenerate docs** with `odig odoc`.

3. **Build type-signature search** with
   `sherlodoc index $(find "$OPAM_SWITCH_PREFIX/var/cache/odig/odoc" -name '*.odocl' | grep -v __)`.

4. **Build HTML-doc search** with
   `odoc-index --root "$OPAM_SWITCH_PREFIX/var/cache/odig/html/"`.

5. **Build Markdown-doc search** with `md-index --root docs-src --name …`.

6. **Smoke-test** with `odoc-search`, `md-search`, and `sherlodoc search`.

Once these steps are part of your regular workflow (e.g. after `opam
upgrade` or significant doc changes), contributors and tools will always
have up‑to‑date local documentation they can search semantically.


## Required GitHub validation

The Website workflow's required `release-gate` includes Ochat framework tests
when the changed inputs require them. Normal tests use `opam exec -- dune runtest
--force`; PR-safe daemon, transport, workspace and multi-client scenarios use
`opam exec -- dune build --force @agent-e2e-pr`. Live-provider, load, soak and
manual-terminal tiers remain separate.

Complete helper/native watcher, managed-session and authoring workflow matrices
also run explicitly, outside ordinary CI: `opam exec -- dune build --force -j 2
@agent-qualification`. Select `@agent-helper-qualification`,
`@agent-managed-qualification` or `@agent-authoring-qualification` for affected
workflows. Normal tests retain focused runtime/transport/security regressions;
the full matrices remain available for local change and release qualification.
See [test tiers](docs-src/agent-server/testing.md#complete-offline-workflow-qualification)
for the coverage split. A green PR does not certify the explicit matrices.

Linux CI installs the committed `ochat.opam.locked` dependency set and verifies
restored dependencies before testing. Update the lock and `.github/ci-toolchain.json`
when changing `ochat.opam`/`dune-project` or source pins. The CI lock targets Linux;
ordinary local setup on other platforms uses the base package definition.
See [CI coverage and maintenance](https://github.com/dakotamurphyucf/ochat/blob/main/website/planning/ci-enforcement.md) for changed-file
selection, periodic cold checks, cache recovery and the deployment rules.


## Documentation during the development cycle

Keep public interface contracts, essential architecture/behavior notes and affected
`docs-src/` references and examples accurate alongside implementation changes.
Continue required documentation and offline semantic checks. Record deferred
editorial gaps with links to their owning feature tickets.

The comprehensive public website refresh belongs to M10, after all preceding
product milestones, including desktop and Linux delivery, are reviewed, merged
and qualified. It covers content coverage, onboarding, tutorials, final UI captures,
navigation/search, and publication verification. The initial audit
[OCH-194](https://linear.app/ochat/issue/OCH-194) must create the concrete follow-up
work; research completion alone does not finish the milestone.

Technical prose remains canonical in `docs-src/`; `website/` owns publication
metadata, navigation and rendering. Edit original sources, never generated output.
Deferring the broad editorial pass does not defer correctness fixes or relax
existing documentation checks. See `website/README.md` for source ownership and
validation commands.
