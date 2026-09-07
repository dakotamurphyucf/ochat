# Milestone A: homepage and documentation reading experience

Candidate checkpoint: 2026-09-06, baseline
`bc76b6c72a280b4bad48a793a63373e6db26d2b4` plus uncommitted implementation.
Local preview: `http://127.0.0.1:4321/`. No public deployment or live provider
execution is represented by this checkpoint.

## Changes established by the review

The homepage and documentation now share semantic colors, typography, spacing,
and radii through `src/styles/tokens.css`. Cream backgrounds, forest green text,
Manrope headings and IBM Plex Mono examples carry the same identity across the
landing page and reading shell. Blue keyboard focus is independent of decorative
borders. Control boundaries use a stronger color than separators.

The homepage keeps a complete canonical ChatMD example at 14px. Enlarged text
wraps in the header, filename, launch command and workflow drawing. The compact
mobile header keeps Documentation, Tutorials and Examples available in a second
row. Main menu, theme and search controls have at least 44px target areas.

The mobile contents disclosure expands in the document flow, preserving its
native links, Escape handling and no-JavaScript usefulness. Opening navigation
moves focus into its current page; closing returns it to the menu control. Main
content is inert while navigation is open. Opening search first closes navigation,
and Escape restores search focus. Starlight still owns the underlying controls.

Code retains the original bytes and gets a visible inset keyboard ring when it
scrolls. Native tables keep row/column semantics inside a separately focusable
horizontal region. Unmodified horizontal arrows on the focused region scroll
predictably in WebKit; nested controls retain their native keys. Touch and native
scrollbars remain available without JavaScript. Copy feedback announces both
success and denied-clipboard fallback; the source stays available.

## Evidence and scope

The representative site still contains 22 docs, with all 297 original Markdown
sources accounted for and 275 explicitly deferred. Installation and the first
local-agent tutorial remain canonical `docs-src/` prose. The existing offline
semantic gate passed after those edits; this increment changes only the website.

Local evidence lives under `scratch/ochat-website-evidence/`:

- `p03-build.log`: static build and emitted HTML validation.
- `p03-browser.log`: cross-browser regression results.
- `p03/review.json`: theme/viewport checks and measured contrast pairs.
- `p03/keyboard-traversal.json`: focus stops and visibility for complete tab cycles.
- `p03/*.png`: homepage/tutorial/reference at 1440 and 390px in both themes,
  reference reading positions, mobile navigation and expanded contents.

`npm run review:design` reproduces the visual and traversal evidence against a
running preview. Screenshots are locally reviewed artifacts, not a pixel-diff
baseline automatically approved on future changes.

The responsive check uses 200% root text sizing at 1280, 640 and 320 CSS pixels.
A 320px viewport models the layout width of 400% browser zoom on a 1280px window.
This is reflow evidence, not a native browser-chrome zoom or text-only-zoom test.
Reduced-motion and forced-color settings are emulated. VoiceOver, NVDA, physical
mobile software keyboards and native browser zoom remain P10 release work.

The contrast audit records 42 text pairs and 18 applicable focus pairs. Normal
text meets 4.5:1 and measured focus boundaries meet 3:1. All 12 page/theme/width
combinations have no axe violations or body overflow. The 32 importer tests and
Astro diagnostics pass. The final browser suite passes 55 scenarios with two intentional clipboard
skips. Six complete keyboard cycles visit 938 stops without an obscured control.
Measured text contrast is at least 5.43:1 and focus contrast at least 6.51:1.
The 320px tools-reference review also finds three semantic tables and no axe
violations in either theme.

## Findings to retain

- WebKit on macOS uses Option+Tab for the all-links sequence. Testing plain Tab
  alone can skip links under the platform's keyboard preference.
- Expressive Code's resize/idle observer deliberately removes `tabindex` and
  `role` from blocks that fit. Measure the focus ring on scrollable blocks; do not
  infer a product defect from an unfocusable block that needs no scrolling.
- A focused table, and sometimes a code scroller during layout, did not respond
  to a single horizontal arrow in WebKit. The narrow key handler targets only
  focused reading regions with actual overflow and no modifier keys.
- Inset focus rings are needed within clipped code frames. Outer rings can be
  invisible even when computed outline colors meet contrast requirements.
- Compare a focus ring against the surface it actually occupies: the inner
  surface for negative offsets and the surrounding surface for outer offsets.

## Remaining work

P05 starts the editorial review and publication of the remaining corpus. P06–P08
cover tutorial/download expansion, retrieval evaluation and media/performance.
The deferred Mermaid bundle still exceeds the Vite chunk warning threshold.
Optional odoc remains deferred. P10–P12 cover release accessibility, hosting,
domain ownership, launch and operations. These tasks remain separate from the
local Milestone A preview.


Keyboard traversal measures each client rectangle of wrapped links; testing only
their combined bounding rectangle falsely flags the empty space between lines.
Header controls and the skip link are tested by actual hit visibility rather
than treating every rectangle above the header edge as obscured.


The subsequent [P01–P04 audit](p01-p04-audit.md) records corrected edge cases,
updated test totals and the still-pending P01.01 repository commit. Its evidence
supersedes earlier claims of complete P01 version-control delivery.
