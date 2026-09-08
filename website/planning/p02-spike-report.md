# P02 importer spike and onboarding increment

Checkpoint: 2026-09-06. Baseline `bc76b6c72a280b4bad48a793a63373e6db26d2b4`
with uncommitted website and canonical-doc changes. This is local implementation
evidence, not a published release or live-model verification.

## Result

All 297 canonical Markdown files retain explicit dispositions. The representative
site renders 22 docs: 21 published preview pages and one heading-preserving bridge;
275 remain deferred. The first-agent journey now includes installation, provider
setup, the exact tracked ChatMD prompt, launch, submission keys, expected response
behavior, quitting, and the native-local persistence boundary.

## Importer contract and evidence

- `.generated/` is the single complete snapshot consumed by Astro: Markdown,
  public assets, homepage example, content report and provenance. The importer
  locks, stages, validates and swaps the whole tree with rollback/recovery.
- Failure tests exercise late preparation errors, failed promotion, concurrent
  importers, stale files, generated-path symlinks, and recovery after a child
  is killed between backup and promotion. A failed retry first restores the
  prior complete snapshot. Real filesystem watcher tests cover edits, deletions,
  ignored output changes and fail-stop behavior. A live browser-server probe also confirmed source edits and restoration refresh correctly. The dev reader restarts after every complete snapshot swap, since filesystem watchers can otherwise remain attached to the replaced directory.
- Zod checks the manifest's types, dispositions, metadata and unknown fields.
  Route ownership, exact-case tracked sources, related IDs, generated-source
  ownership and publication policies receive semantic validation. Legacy aliases
  use the tested bridge policy; unsupported alias declarations fail explicitly.
- Markdown AST/HTML parsing rewrites actual links only. Original code values
  match across all 22 rendered pages. The hello-agent tutorial is independently
  compared to its tracked runtime fixture. Source frontmatter must agree with
  manifest-owned metadata; it cannot introduce route or publication overrides.
- Source hashes and immutable Git revision are recorded. Modified source gets
  a local-edit label and an explicitly committed-source link. Modified/new or
  shallow-history sources receive no invented update date. Production generation
  refuses uncommitted canonical source/hero bytes.
- Search, navigation, sitemap and robots are separate policies. The Io reference
  demonstrates a searchable page omitted from the sidebar. The legacy bridge is
  excluded from sidebar, Pagefind and sitemap and has noindex metadata.
- Highlighting aliases/fallbacks are recorded in the report. Mermaid receives
  the actual parsed source, renders only near the viewport, and keeps readable
  source as the no-JavaScript/error fallback. Its diagram was checked in Chromium,
  Firefox and WebKit.

## Representative real sources

| Source | Coverage |
|---|---|
| `agent-server/tutorials/local-tui.md` | Complete beginner journey, XML, shell continuations, keyboard markup |
| `overview/chatmd-language.md` | Long ChatMD/XML reference and internal fragments |
| `guide/chatml-language-spec.md` | Long reference and preserved exact moderator contract |
| `lib/Io.doc.md` | Case-sensitive source, explicit anchors, OCaml, hidden navigation |
| `chat_tui/renderer.doc.md` | Historical headings pointing to a canonical module |
| `guide/chat_tui.md` | Raw HTML, allowlisted image and keyboard semantics |
| `lib/webpage_markdown/driver.doc.md` | Real Mermaid diagram with exact-source fallback |
| `lib/webpage_markdown/md_render.doc.md` | Valid nested outer fence preserving inner example |
| `lib/chatmd/chatmd_parser.doc.md` | Literal XML-like exception text, tested before publication |

## Validation performed

- 32 content/metadata/provenance/transaction/watcher tests passed.
- 34 browser scenarios passed across Chromium, Firefox and WebKit; two
  non-Chromium clipboard cases remain explicit skips. Added diagram rendering,
  the onboarding reading path, dark-theme accessibility and 320px reflow.
- Static output checks pass for routes, fragments, duplicate IDs, keyboard access
  to code, index/sitemap/noindex policies, local assets and deployment capacity.
- A clean Linux source snapshot passed `npm ci`, diagnostics, all 32 tests,
  static build and output validation. Logs are in local scratch evidence; the
  final renderer-fingerprint addition was separately validated in the local build.
- `dune build --force @agent-docs-check` passed after canonical doc edits:
  297 pages, 38 methods, no live provider calls. No generator-owned protocol or
  moderator snippet was moved or refreshed by the website build.
- Screenshot evidence is in `scratch/ochat-website-evidence/p02-*.png`, including
  mobile onboarding/navigation, desktop onboarding, dark reference and Mermaid.

## Renderer discoveries retained for maintenance

Astro 7 needs an explicit `unified()` processor for the remark/rehype pipeline.
Starlight's `markdown.processedDirs` must include `.generated/docs` for a custom
glob loader. Expressive Code's `postprocessRenderedBlock` hook supplies static
keyboard access to its own rendered HTML; a normal rehype pre visitor alone
cannot modify that output. A renderer fingerprint in generated comments changes
content-cache keys when configuration, render hooks or locked dependencies change.
These comments are not included in visible/searchable prose.

Do not reconstruct Mermaid from highlighted HTML `textContent`: Expressive Code
uses line elements without newline characters. The importer passes the original
parsed fence values directly. The viewport observer loads Mermaid only on pages
with diagrams, and code remains visible if rendering is unavailable.

## Remaining work

The later [Milestone A review](milestone-a-review.md) completes the scoped
P03/P04 shell and onboarding gates, with release accessibility and live-execution
limits explicitly retained. P05–P12 full-corpus editorial publication,
tutorial/download catalog, retrieval evaluation, media/performance work, hosting,
domain and operational handoff remain open. Mermaid's deferred bundle currently
triggers a Vite chunk-size warning and needs a measured release optimization;
the warning has not been hidden. Optional odoc remains deferred.
