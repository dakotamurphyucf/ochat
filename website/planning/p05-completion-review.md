# P05 completion review

Date: 2026-09-06. Source revision: `bc76b6c72a280b4bad48a793a63373e6db26d2b4`.
Status: P05 complete. All eleven phase tasks and local static, offline, and browser gates pass.

## Publication result

All **297 canonical Markdown documents** now have reviewed publication dispositions:

| Disposition | Count | Reader behavior |
| --- | ---: | --- |
| Publish | 99 | Current or explicitly experimental website documentation |
| Compatibility | 4 | Labeled older APIs/command material; searchable, omitted from sitemap, noindex |
| Bridge | 9 | Preserved legacy headings and forwarding links; no sidebar/search/sitemap, noindex |
| Repository-only | 175 | Intentional source/reference links; no website page or automatic download |
| Deferred | 10 | Concrete editorial/API/template issues listed below; no website page |
| Total | 297 | Every tracked docs-src Markdown source accounted for |

The site renders **112 documentation routes** plus its homepage and 404 page.
This closes the spec's **approved-corpus** migration gate. It does not assert that
all 297 sources should become website pages or that every retained API example is
current or executable. Detailed module internals, design specifications, audit
ledgers, research, and historical source-only demos remain repository references.
The library index continues to provide access to them. Publication review is
separate from runtime verification; page metadata stays conservative.

This final group adds 51 routes to the batch/shell checkpoint. It brings maintained
command references, moderator/runtime guides, pattern matching and diagnostics,
compaction/configuration, custom OCaml tools, MCP client/HTTP/OAuth integration,
agent subsystem maps, and selected TUI/source-loading APIs into the site. The
MCP prompt server and three legacy session references have separate compatibility
routes. No existing ID or previously published route was renamed.

The definitive route ownership lives in `config/docs-manifest.json`, with a
reviewable [route ledger](p05-route-map.md). The complete source-level reasons,
code-generated ownership, source corrections, language fallbacks, supplemental
inventory, capability coverage, and bridge mappings are also emitted atomically
in `.generated/migration-report.{json,md}`. They are excluded from `dist/` and
included in the configured CI evidence upload; remote CI has not run here.

## Navigation and capability coverage

The documentation home retains its four starting paths and adds a collapsible,
server-rendered framework map. All fourteen Section 5.6 capabilities have
searchable reader destinations with host, cost, history, or compatibility
qualifications. The map works without JavaScript and links to the curated library
index. Current MCP consumption is separated from deprecated prompt serving;
current actor-owned embedding is separated from legacy snapshot APIs.

`config/capabilities.json` supplies this UI and the checked coverage report.
Missing/unsearchable destinations, duplicate capability IDs, and invalid deferred
references fail generation. The [capability ledger](p05-capability-coverage.md)
records every requirement and the older detail intentionally left in the repository.
The library index begins with task-oriented integration paths, followed by its
complete subsystem/module references. Sidebar groups distinguish authoring,
operations, MCP integration, language runtime, agent libraries, TUI internals,
and compatibility. Deep pages remain reachable without placing every module in
the sidebar.

## Compatibility and dense Markdown

All nine older `docs-src/chat_tui/*.doc.md` pages are bridges. Their **119 original
heading fragments** are retained. Each has a recorded forwarding destination:
current approved pages resolve locally; deferred model/highlighting sidecars
resolve to their immutable repository sources. Those targets do not masquerade
as newly approved website pages.

The expanded built-graph check caught inline `<a>` elements inside the TUI app
headings producing trailing-hyphen renderer slugs. The canonical empty anchors
now precede the headings, retaining aliases such as `architecture`, `shutdown`,
and `run_chat` while restoring expected heading fragments. The importer's heading
inventory now excludes raw HTML and image-alt text, matching the renderer for
these fixtures. A focused regression covers embedded markup and retained aliases.

Every published source heading is now checked against actual HTML IDs, not just
fragments referenced by an incoming link. Every bridge forwarding target must
appear in its HTML, and every output HTML file must have a publication owner.
Existing duplicate-ID, full internal graph, source URL, search/sitemap/noindex,
code-fence parity, media containment, and artifact-limit checks remain enabled.

## Supplemental source boundary

`config/supplemental-sources.json` accounts for **1,250 tracked supplemental files**
at this revision. Exact-file entries cover the selected README hero source,
contributor instructions, license/build files, older root Markdown, and approved
TUI screenshot. Directory rules cover interface/implementation/test/script links,
non-Markdown example companions, prompt packs, historical sessions, and unselected
assets. Every expanded entry has a disposition and reason in the migration report.

Directory rules permit repository links only. An asset copy needs an exact-file
media disposition **and** the existing destination allowlist; containment and
symlink checks still apply at copy time. Root prose, prompt packs, historical
sessions, assets, and example companions cannot silently become unclassified
supplemental material. References outside the canonical manifest must match the
supplemental inventory. Missing/untracked sources, escaping/overlapping rules,
and unauthorized promotion to media fail validation.

Prompt packs are marked for example review. Source-only companions currently
remain GitHub links; complete downloadable closures and their catalog belong to
P06. No arbitrary root Markdown, historical transcript, ignored scratch file,
or original generated docs tree is copied into public output. The durable spec
lives in `website/planning/implementation-spec.md`; the requested scratch spec
and persistent implementation notes remain local working memory.

## Canonical corrections in this increment

- Shell pattern examples 5, 8, and 9 now use the actual `before`, `review`, and
  `filter` entrypoints, purpose-specific task actions, and state-returning tasks.
  The Python interceptor copies argv, changes only the executable to an explicit
  deployment path, and returns `Intercept.rewrite`; review defers and audit keeps
  through their real APIs. Deployment dependencies remain explicit.
- The `ochat` command reference now names Responses, uses the tracked `Readme.md`
  spelling and case-correct `Io` link, and replaces stale test/model claims with
  current verification/configuration boundaries.
- The odoc-index illustrative layout now uses packages included by the wrapper's
  fixed selection. md-search installation points to this repository's setup.
- The compatibility MCP server no longer claims simultaneous stdio/HTTP registry
  sharing. The MCP client example uses Jsonaf's `Object` constructor.
- The moderator guide clarifies host-owned budgets versus spending caps. The
  library index gains curated integration paths. TUI app anchors are corrected
  as described above.

Earlier source corrections and validation are recorded in
[the first migration review](p05-migration-review.md),
[the hosting review](p05-hosting-review.md), and
[the batch/shell review](p05-shell-review.md). The Git diff is authoritative for
all working-tree edits; no runtime implementation was changed in this increment.

## Validation and its limits

- `npm run check`: **43 unit tests pass; zero Astro errors, warnings, or hints**.
  This includes exact fenced-source parity across the complete approved corpus,
  supplemental/capability failures, inline heading markup, and all bridge mappings.
- `npm run build`: **114 HTML pages, 373 files, 15,039,639 bytes**. Complete route,
  heading, fragment, ownership, publication-policy, and configured artifact-limit
  checks pass. Largest HTML: protocol types, 924,401 bytes; the deferred Mermaid
  chunk remains 662,096 bytes. Hosted performance remains P08/P10 work.
- Forced `dune build --force @agent-docs-check`: **297 pages, 38 protocol methods**,
  no live provider calls. Existing protocol JSON, generated-source equality,
  exact callable excerpts, setup/observer checks, and example validation remain.
- `Docs_smoke.shell_examples` extracts all three exact inline hook bodies,
  compiles them through the real `Shell_runtime.Chatml_extension`, instantiates
  sessions, and checks their actions using synthetic inputs. It verifies argv
  preservation through rewrite, reviewer defer, and audit keep. It does not
  execute external commands, backend policies, providers, or deployment manifests,
  and its minimal synthetic event is not wire-schema certification.
- The docs checker explicitly depends on `ochat.shell_runtime` for this check.
  Canonical paths and semantic fixtures were not moved. Existing `source_tree`
  dependencies still select all docs/lib/bin inputs; no protocol/example check
  was dropped through a rename. Changed OCaml helpers pass formatting.
- Full browser suite: **109 passed, 2 expected non-Chromium clipboard skips**
  across Chromium, Firefox, and WebKit in one complete run (4.4 minutes).
  Four new scenarios per engine cover
  all bridge fragments without JavaScript, capability navigation, OAuth and
  Prompt_session search, and narrow library/map accessibility in both themes.
- Visual evidence: framework map desktop/light and mobile/dark, library desktop,
  OAuth mobile/dark, and compatibility sessions desktop/dark. All five have no
  document overflow. Accessibility checks supplement these visual inspections;
  assistive technologies, physical-device input, and hosted checks remain P10.

Logs: `scratch/ochat-website-evidence/p05-complete-{check,build,docs-check,browser}.log`.
Screenshots: `scratch/ochat-website-evidence/p05-complete/`.
No domain purchase, account change, deployment, commit, push, or provider call
was performed. Existing local preview remains at `http://localhost:4321`.

## Explicit deferred corpus

These pages stay out of website routes/search/sitemaps. Their manifest reasons
are deliberate follow-up work, not placeholder pages or hidden publication:

- `docs-src/chatml-host-session-controller-contract.md`: Historical phase contract contradicts itself about deferred user notes becoming canonical versus leaving canonical history unchanged. Current moderator/host guides publish; reconcile the detailed old contract against implementation before promotion.
- `docs-src/chatml-safe-point-and-effective-history.md`: Phase-era host contract requires a coordinated deferred-steering review and correction of its finalized-output UTF-8 claim; use current moderator runtime and shell security guides.
- `docs-src/examples/prompt-patterns.md`: Mixed current, deprecated MCP, and moderator examples require P06 example-level classification and dependency/semantic checks; the current minimal agent and custom-tool paths already publish separately.
- `docs-src/guide/general-agent-workflow.md`: Deployment-specific Brave/Gmail/git template imports an unbundled git-tools.md and mixes prompt-injection-prone instruction prose with external account/tool prerequisites. Requires a complete reviewed template and capability policy before P06 promotion.
- `docs-src/lib/chat_tui/highlight_grammars.doc.md`: Contradictory embedded-versus-runtime grammar-loading prose and examples using unavailable default_dark require a focused current-interface audit.
- `docs-src/lib/chat_tui/highlight_registry.doc.md`: Retained singleton API/loading claims and default_dark example need reconciliation with current grammar lifecycle and interfaces before publication.
- `docs-src/lib/chat_tui/highlight_theme.doc.md`: Retained duplicate historical sections, a malformed nested OCaml/Markdown fence, and palette claims need canonical cleanup and compiled examples.
- `docs-src/lib/chat_tui/highlight_tm_engine.doc.md`: Quick-start examples reference Highlight_theme.default_dark, absent from the current public theme interface; validate current rendering examples before publication.
- `docs-src/lib/chat_tui/model.doc.md`: Stale raw OpenAI history_items, index-keyed metadata, Types.msg_buffer, and old constructor examples conflict with the current identity-bearing model. Preserve repository access and bridge headings; rewrite/compile examples before website publication.
- `docs-src/lib/meta_prompting.doc.md`: The broad overview mixes older Meta_prompt.Make/Chatmd.Prompt examples with current strategies and an obsolete mp-refine-run invocation missing explicit strategy context. Publish the reviewed mp-refine-run command; reconcile API examples against interfaces before promoting the overview.

## Phase gate mapping

| Task | Evidence / final disposition |
| --- | --- |
| P05.01 | All 297 sources have individual explicit dispositions; no initial-spike placeholder reasons remain. |
| P05.02 | Stable IDs and unique routes; durable route ledger and actual HTML-owner check. |
| P05.03 | Approved current task/command/runtime guides published and linked by topic. |
| P05.04 | Approved integration and subsystem prose published; detailed internals repository-only, identified stale sidecars deferred. |
| P05.05 | Nine bridges, 119 preserved mappings, four labeled compatibility pages, output and no-JS browser checks. |
| P05.06 | Curated home paths, framework map, library entry paths, ordered sidebar groups, related links. |
| P05.07 | Complete built graph and source headings pass; repository links require tracked classified targets and canonical offline link checks. |
| P05.08 | Dense protocol, library tables, language references, inline anchors, source fences, narrow themes, and accessibility exercised. |
| P05.09 | Atomic JSON/Markdown reports include all counts, reasons, corrections, compatibility, and publication rules. |
| P05.10 | Fourteen checked capabilities; 1,250 supplemental source dispositions; no implicit source downloads. |
| P05.11 | No canonical/fixture moves; Dune source-tree dependencies retained, new hook-check library dependency explicit; forced docs gate passes. |

P06 is next: finish the intentional tutorial progression and dependency-complete
example catalog/downloads. P07 search evaluation and P08/P10 media, performance,
and release verification remain separate gates. Do not infer their completion
from P05's working search, browser coverage, or static build.
