# Pre-P10 UI design completion

This review records the earlier Graphite design artifact. The subsequent [application UI iteration](application-ui-review.md) owns current implementation and validation evidence.

On 2026-09-07 the user selected **Graphite + blue** and approved the shared UX improvements from the comparison. The selected design is implemented across the homepage, documentation, search, inline source readers, and 404. Existing light/dark/system appearance controls remain available. P09 API hosting remains deferred; this design iteration does not start or complete P10.

## Implemented experience

| Agreed direction | Result | Implementation owner |
|---|---|---|
| Graphite + blue identity | White and cool gray surfaces in light mode; charcoal surfaces in dark mode; blue links, actions, marks, and focus indicators. Temporary palette controls and Stone styling are removed. Old palette URLs/storage cannot restore the comparison theme. | `src/styles/tokens.css`, shared page components |
| A more focused homepage | Project introduction with real ChatMD, three concrete tutorial entry points, an explained agent-composition illustration, and clear next steps. Redundant principles and execution cards are removed. | `src/pages/index.astro`, `src/styles/home.css` |
| Clearer typography and spacing | Strong heading hierarchy, calmer labels, readable supporting text, restrained borders, and consistent spacing. Header and cards adapt to narrow screens and enlarged text. | Shared tokens, homepage/docs styles |
| Simpler documentation navigation | Seventeen existing sections organized into Getting started, Tutorials, Guides, Reference, and Internals. Each previously navigable route remains present exactly once. Documentation, Tutorials, and Examples are shared header destinations. | `config/navigation.mjs`, `src/components/DocsHeader.astro` |
| Instructions-first tutorials | Canonical instructions precede verification and source panels; a compact resource jump link makes files directly reachable. Ordinary docs no longer receive empty resource landmarks. Associated reference pages label their links as example resources. | `src/components/MarkdownContent.astro`, `TutorialRecord.astro` |
| Read examples directly | Inline file selection, copy, and line wrapping; native file disclosures and full source remain readable without JavaScript. The homepage source wraps initially. Copy uses original bytes and failures leave readable source with selection guidance. ChatML uses OCaml highlighting; ChatMD uses XML. | `src/components/ExampleSource.astro`, homepage controls |
| Consistent publishing artwork | Blue-on-charcoal favicon, four PNG icons, and 116 generated social cards. Homepage social card matches the new headline. Browser theme-color metadata uses the same brand configuration. | `public/favicon.svg`, `config/presentation.mjs`, `scripts/publishing-assets.mjs`, `SocialHead.astro` |

Manrope and IBM Plex Mono remain self-hosted. Light mode uses `#ffffff` background, `#20242c` text, and `#245bd6` accent. Dark mode uses `#12151b` background, `#edf0f7` text, and `#91b3ff` accent. Code controls have a dedicated blue focus token. The search backdrop and diagram caption also use neutral graphite colors. The existing mark geometry remains intact; the documentation mark now correctly receives its accent through the child SVG.

Homepage CSS was consolidated after comparison: removed unused section selectors and redundant overridden declarations while preserving responsive cascade order, the automatically growing header, and shrinking/wrapping cards. Comparison components and their imports are deleted rather than retained as dormant production features.

## Content and implementation boundaries

All 115 rendered canonical source hashes and the complete example/verification/download report match the comparison evidence. No tutorial commands, source examples, archive membership, verification claims, route dispositions, or runtime behavior changed. Plain `.tar` downloads and the `dune.txt` download URL adaptation remain intact. The homepage example is still identified as source with no recorded model output.

The selected live preview is `http://127.0.0.1:4321/`. The earlier comparison review and `scratch/ochat-ui-comparison/` screenshots are explicitly archived; the archived gallery now links to the selected live design. Persistent working notes remain in `scratch/ochat-website-implementation-notes.md`, with prior authored UI backups in `scratch/ochat-ui-before-comparison/` and `scratch/ochat-ui-selected-before/`.

## Validation

- `npm run check`: 61 unit tests passed; Astro reports zero errors, warnings, and hints. A later direct Astro check also passed. Formatting and Git whitespace checks pass.
- Optimized preview build: 117 HTML pages, 529 files, 20,916,134 bytes. SHA-256: `a34e158e1602891c2d7f172e1c2cd721e4cc920552d60ac419776641f6d9ef62`. The rebuild after formatting produced the same hash. Internal links, fragments, metadata, source ownership, API exclusion, and capacity checks pass.
- Final full browser matrix: **190 passed, two existing non-Chromium clipboard skips**, across Chromium, Firefox, and WebKit in 3.3 minutes with two workers. Includes no-JavaScript reading/download bytes, source picker/copy/wrap, theme synchronization, blocked storage, ignored legacy palette settings, search, keyboard navigation, accessibility, and 320px enlarged-text reflow.
- The initial 192-case run passed 185, skipped two, and failed five: three assertions incorrectly distinguished equivalent minified `#fff` and `#ffffff`; two Firefox checks timed out during concurrent heavy validation. The assertion now checks the rendered background RGB. Both timeout cases pass in the final full run with less concurrency and unchanged timeouts. No initial failure is represented as a pass; its artifact is separately saved as `full-matrix-build.json`. An interrupted early rerun is excluded from final evidence.
- Design audit: 12 homepage/tutorial/ChatML-reference × light/dark × desktop/mobile combinations, with no document overflow or axe violations; 42 text pairs meet 4.5:1 and 16 focus pairs meet 3:1. Inspected homepage, tutorial, mobile reference, favicon-derived icon, and social-card images. This screenshot audit preceded only the final neutral diagram-caption/search-backdrop color refinements; the complete browser matrix covers the final colors. Actual browser-chrome zoom, screen-reader, and mobile software-keyboard review remain P10 checks.
- Final search evaluation: all 18 benchmark queries pass in the top five; exactly 106 indexed pages and 1,766 anchor destinations are checked. Removal of empty resource wrappers explains the reduction from 1,860 comparison anchors; canonical heading destinations remain intact.
- Final five-family performance gate passes eager JavaScript, CSS, font, layout-shift, and reflow budgets, plus the homepage initial-transfer budget. Conditions: cold Chromium, 390×844, 4× CPU slowdown, 150ms latency, and 1.6 Mbps download. This is one local diagnostic sample per page, not hosted wire-byte or field Web Vitals evidence. The report is tied to the final preview hash.
- Source/record parity: all 115 canonical rendered-source hashes and the complete example report remain unchanged; all 122 task states match between the specifications.
- Final local production fixture: 117 HTML pages, 531 files, and 103 sitemap routes under the reserved test origin `https://docs.ochat.test`. Source cleanliness, production indexing/header policy, asset capacity, internal links, and API exclusion pass. All 116 social images and the SVG/four PNG icons match preview bytes. This uses a synthetic commit in an isolated temporary repository with copied installed dependencies; it does not verify actual public source links, domain ownership, HTTPS serving, hosted builds, or deployment. Main checkout history is unchanged.

Evidence is collected under `scratch/ochat-graphite-evidence/`; final production reports are in `scratch/ochat-website-evidence/graphite-production/`. Command logs use the `scratch/graphite-` prefix. The generated reports record separate preview/production revisions, origins, and artifact hashes.

## Next work

P10 remains the next implementation phase: final release-candidate validation, manual accessibility sampling, deployment configuration, hosted rehearsal, rollback, and release enforcement. This local UI review does not establish domain ownership, public HTTPS behavior, a GitHub required check, or deployment. The main checkout remains uncommitted at `bc76b6c72a280b4bad48a793a63373e6db26d2b4`. All 122 task states remain synchronized between the specifications; P01.01's actual commit and P10–P12 remain open.

## Follow-up: Start here reading order

The user requested this order after design completion: Explore Ochat → Build and configure Ochat → Build troubleshooting → Run your first local agent. Manifest ordering now matches the sidebar. The installation page’s Next link goes to build troubleshooting, which continues to the first-agent lesson; that lesson still continues to Give an agent a file tool. Canonical prose and tutorial curriculum order are unchanged.

The existing reading-path browser check now verifies the Start here sidebar and clicks every requested transition. All three engines pass (4.2 seconds). The optimized build and output checks pass: 117 HTML, 529 files, 20,916,139 bytes; SHA-256 `d09e8cead463251e28343b6f5bee7278d23d3d70e45ebd478fbab9837e22b9c1`. Formatting and Git whitespace checks pass. An initial test selector also matched Host troubleshooting in a different section; scoping it to the Start here disclosure resolved that test-only failure. The earlier full matrix and performance evidence remain associated with their recorded design artifact. Follow-up evidence: `scratch/ochat-start-order-evidence/`.
