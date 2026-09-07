# Pre-P10 UI comparison

> Archived comparison: the user subsequently selected Graphite + blue. The live site no longer switches palettes; screenshot evidence below is historical. See [the selected design completion review](ui-design-completion-review.md) for current implementation and validation.

Status: comparative previews implemented; final palette choice pending. Requested by the user after P09 and before P10 on 2026-09-07. This is a design iteration, not a release rehearsal or an API-scope change.

## Compare the directions

The website preview at `http://127.0.0.1:4321` accepts `?palette=graphite` or `?palette=stone` on any documentation page or the homepage. The palette is remembered locally when browser storage is available. An explicit URL choice takes precedence; blocked storage does not prevent choosing a palette on the current page. Theme selection continues to use the existing light/dark/system controls.

- Graphite + blue: white/cool-gray surfaces, charcoal dark mode, and blue actions/links.
- Stone + orange: warm-white/stone surfaces, warm charcoal dark mode, and burnt-orange actions/links.

Both use the same content, spacing, layout, fonts, source files, and navigation. The initial default is Graphite for comparison purposes; it is not an approved final brand decision.

The local comparison gallery at `http://127.0.0.1:4322` presents matching screenshots side by side. Its page, appearance, and screen controls select from 16 screenshots: two palettes × two themes × homepage/tutorial × desktop/mobile. Screenshots hide only the temporary comparison toolbar so the layout can be judged clearly. Each screenshot opens at full size and each column links to the live palette.

The gallery is entirely in ignored `scratch/ochat-ui-comparison/`. Start it from the repository root with:

```sh
python3 -m http.server 4322 --bind 127.0.0.1 --directory scratch/ochat-ui-comparison
```

The screenshot helper is `scratch/review-ui.mjs`, run with the website's pinned Node binary while the website preview is serving on port 4321. Neither the gallery nor screenshots are website routes or deployable content. The on-page comparison controls are excluded from production rendering and Pagefind; the head initializer ignores palette query/storage settings in production.

## Shared design and UX changes

- Replaced the green-tinted foundations with neutral surfaces and semantic accent tokens. Homepage, documentation, search, source readers, and 404 share those tokens. Light and dark palettes each specify readable text, borders, controls, and focus colors.
- Shortened the homepage to the project introduction and real ChatMD editor, three concrete tutorial entry points, an explained composition illustration, and clear next steps. Removed the redundant principles strip and separate execution-mode card section. Existing canonical tutorial destinations remain available through navigation.
- Strengthened heading hierarchy, increased small supporting text, reduced uppercase labeling, and simplified the editor's surrounding borders. Exact README source remains intact; the demonstration still explicitly has no recorded model output.
- Added a keyboard-operable wrap control to the homepage example, with wrapping enabled initially. All source bytes remain available and copy uses the original source string.
- Organized the 17 existing sidebar sections into five families: Getting started, Tutorials, Guides, Reference, and Internals. Every previously navigable route still appears exactly once; no routes, canonical sources, or search ownership were changed. Nested sections open through Starlight's current-page behavior.
- Added the same Documentation, Tutorials, and Examples destinations to the desktop docs header. Existing responsive menu/search behavior is retained. Theme controls now synchronize all rendered desktop/mobile instances.
- Moved tutorial verification and source readers after the canonical instructions. A compact link above each tutorial jumps to those resources. The canonical introduction provides the outcome and prerequisites without duplicating technical instructions in another metadata file.
- Added progressive file selection, copy, and wrap controls to inline source readers. Native file disclosures and full code remain usable without JavaScript. The picker opens the selected file and closes the other file panels; copy failures retain a readable selection fallback. ChatMD is XML-highlighted, and ChatML remains OCaml-highlighted.

The prior authored UI is saved in ignored `scratch/ochat-ui-before-comparison/` for reference. No source example, canonical Markdown, download selection, tutorial verification claim, or runtime behavior was changed by this iteration.

## Verification and remaining decision

- `npm run check` passes 61 unit tests with zero Astro errors, warnings, or hints. The existing navigation invariant now traverses nested groups and still requires each approved route exactly once.
- Optimized preview output passes all existing link, fragment, metadata, source, asset, search-ownership, API-exclusion, and capacity checks: 117 HTML pages, 529 files, 21,367,749 bytes. Final SHA-256: `7197dbb0a070176a0c42dacc3396b96e768968b1274c5372490bab3da30ff605`. A final formatting-only rebuild produced the identical artifact hash.
- The initial full 195-case browser run passed 187, skipped the two existing non-Chromium clipboard cases, and exposed six failures across the three engines: enlarged-text card overflow and an enlarged-text header overlap that intercepted the search button. Both defects were fixed by allowing cards and links to shrink/wrap and retaining an automatically growing header height.
- The final affected 105-case suite passes **103 with the same two existing clipboard skips** in 53.5 seconds. It covers reading, search, the homepage, palette comparison, and source-picker/copy/wrap behavior across Chromium, Firefox, and WebKit. All six previously failing cases pass. The final comparison tests include 320px enlarged-text controls, both palettes/themes, blocked storage, preserved route fragments, and source-copy failure fallback. This is not a claim that the full matrix was repeated after the two CSS fixes.
- Visual review captures 16 palette/theme/page/width combinations with no document overflow. Axe checks on the eight desktop combinations report no violations; the broader browser suite also exercises mobile accessibility and enlarged text. Inspected full-size homepage and tutorial treatments and the mobile homepage. The gallery successfully loads all 16 screenshot variants.
- Search passes all 18 benchmark queries over exactly 106 indexed pages and 1,860 checked anchor destinations. No API output is introduced.
- All 115 rendered canonical source hashes and the complete example/verification/download report are unchanged from the preceding phase evidence. No runtime semantic changes require new provider execution.
- Formatting, Git whitespace, and parity of all 122 task states in both specifications are checked. The main checkout remains uncommitted at its original revision.

Command logs are `scratch/ui-comparison-*.log`. Final report copies and screenshots are in `scratch/ochat-ui-comparison/`; the earlier full-matrix artifact is separately recorded as `full-matrix-build.json`. The full-matrix failures remain diagnostic history, not passing release evidence. The five-family local performance gate also passes all eager-JavaScript, CSS, font, layout-shift, and reflow budgets against the final artifact. Measurements use the existing cold Chromium / 390×844 / 4× CPU / 150ms / 1.6 Mbps diagnostic setup; these are local samples, not public hosting or field Web Vitals. The report is `performance-report.json` alongside the screenshots.

The user still needs to choose a palette or request another visual iteration. After that decision, consolidate the selected styling, retire the temporary comparison controls, and align the existing favicon/social-image artwork with the chosen palette. Existing generated publishing art remains the P08 treatment during this comparison; its ownership and byte checks still apply. Revalidate the selected design before beginning P10's release-candidate work. P09 API hosting remains explicitly deferred. P10 and all domain, deployment, GitHub, and launch gates remain open.
