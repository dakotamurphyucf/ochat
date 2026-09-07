# P09 API-reference release decision

Status: **P09 complete through API deferral, explicitly confirmed by the user on 2026-09-07.** P09 closes through its documented deferral branch; it does not certify an API artifact as publishable.
Base revision: `bc76b6c72a280b4bad48a793a63373e6db26d2b4`, with the existing uncommitted website work and this phase's guide/policy changes.

## Decision and task disposition

| Task | Disposition | Outcome |
|---|---|---|
| P09.01 Inclusion decision | Complete | Defer hosted `/api/`. Current interface links and architecture guides provide a useful entry point while generated-reference quality is addressed separately. The user confirmed deferral after the local assessment. |
| P09.02 Release generation | Not applicable | No release API artifact is selected. The diagnostic local build below is not fresh, clean release-generation evidence. |
| P09.03 Publication scope | Not applicable | No API files are approved for publication. Diagnostic inventory is recorded below, not an asset/license/scope approval. |
| P09.04 Mounted artifact | Not applicable | No API artifact is mounted. The hypothetical mount audit found broken links; native search and browser behavior are not certified. |
| P09.05 Specialized search | Not applicable | There is no hosted API index to integrate or label. The existing prose index is checked separately and accidental API input is rejected before indexing. |
| P09.06 API provenance/CI | Not applicable | No API generation/assembly/deployment job is enabled. Local assessment provenance does not claim a release artifact. |
| P09.07 Navigation outcome | Complete | Keep `/api/` absent. The existing OCaml guide links nine library architecture pages and nine current public interfaces, the wider library overview, and local generation instructions. |

P09.02–P09.06 remain unchecked and explicitly annotated **Not applicable** in both specifications, as the phase gate requires. They are not unfinished release requirements under this decision. P10 can proceed; P01.01's actual checkout commit and P10–P12 remain open.

## Local assessment

The installed tools are OCaml 5.3.0, Dune 3.21.1, odoc 3.1.0, odig 0.1.0, and sherlodoc 3.1.0. `dune-project` requires Dune language 3.21 and OCaml >=5.1. The generated opam manifest already includes odoc for documentation builds. Tool availability is not the blocker.

Ran `dune build @doc` from the repository root, with exit status 0. This is a dependency-aware local build in the existing `_build/`, not an isolated clean build from a chosen committed release. It produced 271 warnings, including 173 unresolved-reference warnings. Other diagnostics include malformed markup and unsupported references in interface/module comments. Generation success alone is insufficient publication evidence.

| Assessment | Tracked historical `docs/` | Local `_build/default/_doc/_html/` |
|---|---:|---:|
| HTML pages | 403 | 916 |
| All files | 441 | 954 |
| Bytes | 5,694,570 | 11,868,886 |
| Current agent library entry points | 0 | 9 |
| Missing local target occurrences | 1 | 3 |
| Missing local fragment occurrences | 14 | 43 |

The last commit touching the tracked snapshot is `21554ab6feca56d73ecaf2b4eccae0993a7c6605` (2026-03-22). The snapshot omits all nine `Agent_*` library entries found in the current build. Neither directory is copied into the website.

The diagnostic audit resolves HTML `href`/`src` values as if mounted under `https://docs.ochat.test/api/`, checks local files and HTML IDs, and records every failure. It is not a browser, CSS-resource, dynamic-search, license, or accessibility audit. Unresolved odoc references may render as plain text and are additional to the counted broken links. The existing build directory may retain stale files; a later inclusion effort must use isolated clean generation.

Examples from the current output:

- `Chat_response/Chatml_turn_driver/Safe_point_input/` links to a nonexistent `In_memory_stream/Safe_point_input/` module page.
- `Chat_tui/Renderer_page_agent/For_testing/` links to a nonexistent `Model/Agent_page_state/` page.
- `Meta_prompting/Templates/` interprets a repository-relative Markdown path as a nested API URL.
- Agent-session pages link to a missing `#type-post_stream` fragment in `Chat_response/In_memory_stream/`.

The observed top level is `index.html`, `ochat/`, `odoc.support/`, and `sherlodoc.js`. Both inventories contain 38 non-HTML support/search files, including `ochat/db.js`, native search scripts, highlighting, KaTeX resources, CSS, and 31 font files. No separate dependency-package HTML directories appear at the top level, but unresolved external dependencies and public implementation/testing modules still need scope decisions. This observation is not permission to publish those files.

Full diagnostics and hashes are in local `scratch/ochat-website-evidence/p09/api-assessment.json` and `odoc-build.log`; the reproducing helper is `scratch/audit-p09-api.py`. The raw generated API stays in Dune's output directory and is excluded from the deployment artifact.

## Enforced release boundaries

`config/api-reference.mjs` owns the deferral decision, fallback route, and review pointer. The build evidence records that decision alongside the artifact hash.

- **Navigation:** published manifest routes cannot claim `/api/`; actual built same-origin API links are rejected, including absolute URLs, encoded paths, and relative paths resolved from the page. `/docs/integrations/ocaml/` remains in the existing Learn more group.
- **Search:** the single Pagefind build hook refuses an `api` directory before indexing, including a copy containing only scripts. The prose index still must exactly match the manifest. Generated OCaml types are not promised as website search results.
- **Sitemap:** preview has no sitemap. Production permits the existing eligible prose routes; an API URL in the generated sitemap fails output validation.
- **Assembly:** output validation rejects API files even if copied after indexing. Only `website/dist/` is the deployment artifact. No API copy stage, OCaml dependency, API CI job, or second production trigger is added. The existing workflow uploads build evidence containing the deferral status.
- **Reader fallback:** the canonical embedding guide describes the deferral, pairs nine architecture guides with source contracts, and explains `dune build @doc`. Local generation requires project dependencies and odoc but no provider call. The separate development/indexing guide retains its own provider requirements.

## Verification

- `npm run check`: zero Astro errors, warnings, or hints; all 60 tests then present passed. After adding the pre-index rejection regression, the final complete unit suite passes 61 tests.
- `npm run build`: 117 HTML pages, 529 files, 20,672,702 bytes; checked links/fragments, metadata, media, search ownership, and output capacity. Preview has no sitemap. Artifact SHA-256: `98711dc35c417abe6cb887ab978154c7422b716f3a662958b692b0ab2e5a7c7b`. Build evidence explicitly records `apiReference.status: deferred`.
- `npm run evaluate:search`: 18/18 benchmarks pass over exactly 106 approved pages and 1,754 checked fragment destinations, tied to the same artifact.
- The focused OCaml-entry browser test passes in Chromium, Firefox, and WebKit (3/3). It checks all nine revision-qualified source links, the library overview, the visible deferral/local-generation instructions, no API navigation, zero axe violations, 320px reading with 200% text, and a noindex 404 at `/api/`.
- Negative boundary checks reject an accidental API script directory before Pagefind creates an index, reject published API routes and normalized same-origin URLs, and reject a deliberate API script copied into the final artifact after indexing. Removing only that injected fixture restores a passing output check and the same artifact hash.
- `dune build --force @agent-docs-check`: 300 pages and 38 methods pass, including offline library/tutorial checks; no live provider calls.
- A separate temporary committed production fixture passes with 117 HTML pages, 531 files, exactly 103 sitemap URLs, the deferral decision in build evidence, and no API files or links. It uses `https://docs.ochat.test` and copied installed dependencies. This tests production configuration and assembly, not domain ownership, live source URLs for its synthetic commit, public HTTPS, a clean Linux install, or a remote deployment.
- Changed code formatting, Git whitespace, both specs' 122 task states/reasons, unchanged legacy `docs/`, and the main checkout's unchanged HEAD/empty staged diff are checked.

P09 command logs and final generated reports are in `scratch/ochat-website-evidence/p09/`; production-fixture evidence is in the sibling `p09-production/`. The full earlier P08 browser/performance matrix was not repeated for this guide/policy change; its scope remains documented in the P08 review. P10 owns comprehensive final release validation.

## Conditions for reconsidering inclusion

A later API-reference task must explicitly reopen the decision and complete P09.02–P09.06 before removing the exclusion checks:

1. Choose and record the source revision; generate in a clean isolated OCaml environment with reproducible dependency/toolchain inputs. Keep the normal prose build independent of opam and live providers.
2. Repair malformed doc comments and broken project references, distinguish missing dependencies from local API errors, and decide which public/testing/compatibility modules belong in the release. Do not repair generated HTML with broad string substitutions.
3. Inventory intended project/dependency files and licenses for every script, font, style, index, and other asset. Exclude local caches and unreviewed material.
4. Mount beneath `/api/`, add a supported return path, and verify nested links, fragments, CSS/font/script loading, native search, keyboard access, mobile reading, and both theme contexts. Keep specialized API search separate from prose search initially.
5. Define API navigation, canonical/noindex/sitemap behavior, artifact size limits, and source/CI provenance. Assemble and validate one final deployable artifact under a single production owner before exposing an API link.

Deferral does not change the exact-source ChatMD readers, the OCaml highlighting used for ChatML, or any runtime behavior. No domain was purchased and nothing was committed, pushed, or deployed from the main checkout.
