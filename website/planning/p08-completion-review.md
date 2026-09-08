# P08 presentation, media, and discoverability review

Status: **P08 complete, 2026-09-07. All nine tasks are closed.**
Base source revision: `bc76b6c72a280b4bad48a793a63373e6db26d2b4`, plus local uncommitted implementation.

## Decisions and task evidence

| Task | Result and evidence |
|---|---|
| P08.01 Visual identity | Retained the reviewed forest/cream palette, self-hosted Manrope and IBM Plex Mono, geometric composition mark, and blue keyboard focus. Wordmark weight/tracking now come from shared tokens. The homepage, docs, search, 404, icons, and social cards share the identity. Visual and contrast review uses the actual migrated corpus. |
| P08.02 Demonstration | Chose the specification's static-treatment option. The homepage explicitly labels its complete README-derived ChatMD as static source with no recorded model output. Copy preserves canonical bytes, unavailable clipboard access has a reading fallback, installation and launch context remain visible, and separate immutable-source and moving README-walkthrough links explain where it comes from. No player, autoplay, model call, or fictional response is presented. |
| P08.03 Diagrams | The homepage composition remains a labeled illustration with an equivalent prose explanation and a link to the maintained specialist tutorial outside its image role. Canonical Mermaid source stays visible. Rendering is explicitly requested, deferred, keyboard-scrollable, collapsible, and accompanied by a source fallback on failure. A reserved viewport prevents late rendering from moving the surrounding article. Neutral, high-contrast diagram paper is intentionally retained in both page themes. |
| P08.04 Publishing assets | Deterministic 1200×630 PNG cards cover the homepage and all 115 rendered documentation routes. Cards use actual titles/sections and visible Experimental, Compatibility, or Previous location status when applicable. The vector favicon produces 32, 180, 192, and 512px PNG icons. Each generated image has dimensions, source/license attribution, exact bytes, and a hash. No remote font or image generator runs during builds. |
| P08.05 Metadata | Unique titles, descriptions, canonical URLs, Open Graph/Twitter cards, image alternatives/dimensions, and favicon declarations are checked against actual HTML. Documentation uses topic-first titles with consistent Ochat capitalization. Preview emits no sitemap and has noindex metadata/headers; production includes only eligible canonical routes. The 404 remains noindex with no canonical. An isolated production fixture tests the positive production path. |
| P08.06 Supporting surfaces | 404 uses the site theme, skip link, shared search, docs/tutorial/example destinations, and a link to report broken URLs. Documentation footers preserve Starlight edit/date/pagination behavior and add project/contribution/license navigation. Existing search failure/empty states and compatibility labels remain tested. OCaml integration stays useful without an unverified `/api/` link; P09 still owns that decision. |
| P08.07 Editorial review | Reviewed all 115 route titles/descriptions, entry-point navigation, homepage claims, tutorial/catalog terminology, compatibility/experimental qualifications, and current host distinctions. Updated the catalog description to mention inline source reading, standardized HTTP capitalization in a library title, and labeled the TUI screenshot as historical. No runtime capability was broadened. |
| P08.08 Payloads | Measured homepage, first tutorial, ChatML reference, protocol-types reference, and webpage-ingestion reference against Section 15 budgets. Social images are metadata assets, never eagerly fetched by readers. No hero poster or recording player is required. Diagram rendering moved from viewport-triggered to explicit interaction. Full long references retain source content and anchors. Final figures are recorded below. |
| P08.09 Provenance | Dates are checked against original-source Git history; modified/new/shallow sources do not receive invented dates. Immutable view-source and branch edit links remain distinct. Public media, generated cards/icons, repository screenshot, font assets, inline diagrams, and original notices have explicit owners. README remains byte-identical to HEAD with all original heading entry points intact. |

## Publishing and metadata ownership

`config/presentation.mjs` owns homepage identity, social-card paths, and sitemap policy. `scripts/publishing-assets.mjs` generates cards with Satori and Sharp during the existing atomic content snapshot. It uses a pinned local static Manrope WOFF; Satori embeds glyph outlines before rasterization, avoiding a dependency on machine-installed fonts. Social cards contain no star counts, runtime measurements, unverified domain name, or changing marketing claims. The generator uses actual title length to select type size; the longest title and representative current/experimental/compatibility cards were visually checked.

`config/media.json` classifies authored public files, repository media, inline graphics, and generators. An undeclared file in `public/` fails generation. `.generated/publishing-assets.json` records generated bytes/hashes/dimensions, and `.generated/media-report.json` joins the public, repository, font, and inline inventories. Exact approved source-download ownership and notices remain in the P06 example report. Build checks reject missing notices, changed card bytes, absent cards, or undeclared repository/font media.

The historical TUI PNG is deliberately retained at its approved URL to preserve existing references. Its source image is 2420×2076; the canonical HTML now reserves that real aspect ratio, requests lazy/async loading, and has a meaningful alternative plus a visible historical caption. It is on the maintained TUI guide, not a bridge page. This corrects the initial P08 audit note's mistaken classification. The image is not a hero poster and is not used as evidence of today's workflow.

Starlight automatically installs a sitemap integration when one is absent. The website therefore retains the explicit supported integration in both environments and applies the environment-aware filter. With no eligible preview URLs, the integration emits its expected “No pages found; sitemap-index.xml not created” notice. The shallow Head override also omits Starlight's preview sitemap link. Production uses the approved indexable corpus; bridges and compatibility pages remain excluded regardless of their separate internal-search policy.

A focused fault check found that failed dynamic imports can stay cached even after a dialog is reopened. The diagram error message therefore directs readers to the complete source and does not promise an in-page retry. Fresh-page successful rendering and keyboard scrolling are checked separately.

New build dependencies are pinned: Satori 0.33.4, Sharp 0.35.4, and static Manrope 5.3.0. Satori pins an older fflate patch with a malformed-ZIP advisory; a scoped `satori → fflate 0.7.5` override uses the fixed patch. The installed lockfile audit reports zero vulnerabilities. This generator processes local reviewed font data and is not shipped as a browser runtime service.

## Performance evidence

All measured asset and layout-shift budgets pass. Values use decimal KB, with inline scripts and style tags included in their respective budgets:

| Page family | Eager JS gzip | CSS gzip | Initial fonts | Initial transfer estimate | Observed CLS |
|---|---:|---:|---:|---:|---:|
| Homepage | 4.8 KB | 8.4 KB | 39.5 KB | 57.1 KB | 0.0068 |
| First-agent tutorial | 10.0 KB | 20.3 KB | 39.5 KB | 81.7 KB | 0.0009 |
| ChatML reference | 10.0 KB | 20.3 KB | 39.5 KB | 127.3 KB | 0.0009 |
| Protocol types | 10.0 KB | 20.3 KB | 39.5 KB | 116.4 KB | 0.0053 |
| Webpage ingestion | 10.0 KB | 20.3 KB | 39.5 KB | 80.5 KB | 0.0031 |

Homepage JS is below 100 KB, documentation JS below 150 KB, CSS below 100 KB, fonts below 150 KB, and homepage initial transfer below 800 KB. Observed layout shift stays below 0.1 on every family. The local LCP samples range from 564 to 776 milliseconds under the recorded conditions; these are diagnostic samples, not a public performance guarantee. The final Astro build takes under ten seconds locally; the five-minute CI target still needs remote-run confirmation. The baseline report is `scratch/ochat-website-evidence/p08-baseline-performance.json`. No asset-budget exception was needed.

`npm run measure:performance` owns an ephemeral local preview, opens a cold Chromium context per route at 390×844, applies 4× CPU slowdown and 150ms/1.6Mbps network settings, and records resources, DOM nodes, local LCP, and observed layout shifts. It computes gzip body sizes for HTML/JS/CSS and counts already-compressed font/image bytes directly. These are reproducible asset-budget comparisons, not measured public-host wire transfers or field Web Vitals. One timing sample per family is diagnostic; no field INP or 75th-percentile performance claim is made.

The homepage initial budget excludes user-requested search and diagram downloads. Social-card images are not page content and make no eager reader request. The long ChatML/protocol pages are reported separately instead of splitting or truncating source to satisfy a page-size target. The optional Mermaid engine remains an on-demand payload and is intentionally excluded from the eager budget; its exact requests and reserved rendering viewport are tested separately.

## Verification and limits

The complete 177-case browser matrix passed: **175 tests passed and two existing non-Chromium clipboard skips**, in 5.9 minutes. After that run, a localized two-line control refinement allows a diagram to collapse while its renderer is still loading. The final optimized build and 54-case presentation/site browser run pass: **52 passed and two existing clipboard skips**, including the slow-load control regression in all three engines (38.2 seconds). The full-matrix artifact is retained in `p08/full-matrix-build.json`; it is not represented as a full rerun of the final refinement.

Other checks:

- 59 unit tests and zero Astro errors, warnings, or hints.
- Optimized preview: 117 HTML pages, 529 files, 20,665,034 bytes. Artifact SHA-256: `60d3549934aca1aecbba54140d242f70a7bec1f3ff99d08e8b8e85f0de727b51`.
- 18/18 search queries pass over exactly 106 indexed pages, with 1,752 valid fragment destinations. Search and performance reports identify this same artifact.
- Offline Dune gate: 300 canonical pages and 38 protocol methods pass after the final screenshot-caption edits; no provider calls.
- The focused 33-case reading/presentation suite passes in all three engines after the footer reflow fix. A separate 320px/200% text check covers homepage, 404, and first tutorial with no overflow.
- Design review: 12 page/theme/width combinations, 42 text-contrast pairs, and 18 applicable focus-contrast pairs, with zero failures. Ten additional final P08 screenshots cover homepage, 404, expanded diagrams, and project footers in both themes; no document overflow. Fresh-page diagram scrolling moves 40px with the keyboard in all three engines.
- Four representative social cards, including the longest compatibility title and an experimental reference, were visually inspected. All 125 authored/generated publishing-asset hashes match the independent production fixture. Original complete MIT/OFL notices match their source bytes.
- The production fixture builds 117 HTML pages and 531 files with exactly 103 sitemap URLs. Preview emits none. Formatting and Git whitespace checks pass; both specs preserve 122 matching task states.

Evidence is under `scratch/ochat-website-evidence/p08/`, with final command logs prefixed `p08-` in the parent directory. Production evidence is in `p08-production/`. The early nested-interactive issue and later enlarged-text footer overflow were fixed; failed/interrupted logs are retained as diagnostics and are not completion evidence.

An isolated temporary Git fixture is used for production checks because this checkout's original-source commit gate correctly rejects uncommitted production source. That fixture copies the candidate source and installed dependencies, commits only inside its temporary repository, and uses `https://docs.ochat.test`. This verifies configuration-derived canonicals, sitemap membership, metadata, and noindex separation. It does not establish domain ownership, public GitHub targets for that synthetic revision, HTTPS serving, CDN behavior, or deployment readiness. No commit is created in the user's checkout, and P01.01 remains open.

P09's API inclusion/deferral decision is next. Physical-device keyboard and assistive-technology sampling, hosted behavior, final release performance and security checks, and actual domain/GitHub launch remain P10/P11. This is a polished local preview, not a published site.

## Maintenance

Edit the canonical docs and manifest rather than generated pages. Keep source-reader ChatML and Markdown ChatML mapped to OCaml. Preserve the strict image-attribute allowlist: only reviewed image `loading`/`decoding` values are accepted; event handlers remain rejected. Run the offline docs gate for canonical changes.

Run `npm run check`, `npm run build`, then `npm run measure:performance`, `npm run evaluate:search`, and the browser suite. Do not run content/check/build concurrently. Keep the production source-commit gate and both independent indexing policies intact. CI uploads publishing/media/performance reports alongside the existing corpus, example, build, and search reports.

Implementation references: [Satori font and SVG behavior](https://github.com/vercel/satori), [Sharp image construction](https://sharp.pixelplumbing.com/api-constructor/), and the pinned Starlight source's Head/Footer/sitemap integrations inspected locally. Site identity and media are code-authored assets; no AI bitmap-generation dependency was needed.
