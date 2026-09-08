# Implementation decisions and foundation scope

Checkpoint: 2026-09-06. Baseline `bc76b6c72a280b4bad48a793a63373e6db26d2b4`, branch `main`.

## Product and ownership

Build the static project/docs website specified in `implementation-spec.md`. P05 migration is complete; P06 is complete, with ten connected tutorial paths and the selected source catalog. P07 and Milestone B are complete, including shared search and the 18-query benchmark. See [the P07 completion review](p07-completion-review.md) for final verification. P08 presentation, publishing assets, metadata, and payload checks are complete; see [the P08 review](p08-completion-review.md). P09 is complete through user-confirmed API deferral; see [the P09 review](p09-completion-review.md). P10 local qualification and authorized hosted preview/rollback are complete; actual GitHub release enforcement remains open. See [the hosted rehearsal](p10-hosted-rehearsal.md). Canonical technical prose stays in `docs-src/`. All 308 canonical Markdown sources have explicit dispositions: 110 publish, 4 compatibility, 9 bridges, 175 repository-only, and 10 deferred. The 123 rendered documentation routes have unique owners and checked source headings. See [the P05 completion review](p05-completion-review.md) for all eleven gate decisions, fourteen capability mappings, supplemental policy, and verification limits. Publication does not imply live-runtime verification.

The full specification has a tracked snapshot here so a new checkout does not depend on ignored scratch files. Keep it synchronized with the requested `scratch/ochat-website-research-spec.md` when requirements change. Read and update `scratch/ochat-website-implementation-notes.md` while implementing; the notes are local memory and never site content.

Public repository/source links use `https://github.com/dakotamurphyucf/ochat`, maintained branch `main`, and the checked-out Git revision. Edit links use the branch. Uncommitted technical changes are not part of that immutable source revision; record them in implementation evidence. Do not imply a live or production verification from a successful static build.

## Technology

Astro 7.3.1, Starlight 0.42.0, npm lockfile, Node 22.22.1. Versions were resolved against the registry and Starlight's actual peer declarations, then exercised locally. Node 22.14 met Astro's top-level engine but not Wrangler's current transitive `undici >=22.19` requirement. Use the pinned Node version. Deployment CLI: Wrangler 4.129.0, static assets only, compatibility date 2026-09-06.

Custom Astro homepage owns `/`. Starlight owns manifest-declared `/docs/` pages. The importer uses a Markdown AST and parse5 for raw HTML. It performs offset edits so code fences are never serialized as prose. Generated documents live exclusively in ignored `.generated/docs/`. Staged replacement prevents partially transformed content from being treated as a successful build. On a watch failure the development server stops and reports the source problem.

Pagefind runs once through Starlight. Explicit `pagefind` frontmatter controls indexing, explicit sidebar entries control navigation, and the configured sitemap integration filters by the separate manifest field. Preview builds are noindex in HTML and host headers; `robots.txt` disallows crawling. A bridge preserves headings and is omitted from navigation, search, and sitemap. Aliases requiring different fragment translations are not silently accepted.

The default origin is `http://localhost:4321`. `SITE_ENV=production` requires an explicit HTTPS `SITE_URL`. The user purchased `ochatlabs.com` through GoDaddy; its zone is active in Cloudflare. Production CI explicitly uses `https://ochatlabs.com`. Local preview listens on loopback only. Development/preview use Astro's public programmatic API because Astro 7 automatically backgrounds CLI servers in an agent environment, which breaks a foreground process supervisor such as Playwright.

The root Dune file excludes `website/` from recursive OCaml builds. Narrow `.gitignore` exceptions expose authored website sources while excluding dependencies and generated files.

## Representative fixtures

- `agent-server/tutorials/local-tui.md`: simple tutorial and shell continuation.
- `overview/chatmd-language.md`: long XML/ChatMD language reference.
- `guide/chatml-language-spec.md`: long language reference and exact moderator contract.
- `lib/Io.doc.md`: case-sensitive sidecar, explicit anchors, OCaml.
- `chat_tui/renderer.doc.md`: legacy heading bridge to a canonical module.
- `guide/chat_tui.md`: raw HTML, allowlisted screenshot, keyboard markup.
- `lib/webpage_markdown/driver.doc.md`: lazy Mermaid SVG with readable source fallback.
- `lib/webpage_markdown/md_render.doc.md`: corrected nested outer fence.

Additional current concept, setup, permissions, and navigation pages make the preview navigable. Generated protocol/operator/coverage sources retain `generated-from-code` ownership and are now included in the approved corpus; website builds never run their refresh commands.

## Supplemental source policy

`Readme.md` owns the homepage's complete ChatMD example; the importer extracts the first XML fence and checks its expected role. `DEVELOPMENT.md`, `dune-project`, `ochat.opam`, `LICENSE.txt`, tracked `lib/`/`bin/` files, and approved example companions can be linked on GitHub. They are not automatically published as documents. Only the allowlisted TUI screenshot is imported as a media asset. Site-authored public branding/notices are explicitly under `website/public/`.

The supplemental manifest expands exact-file and repository-link-only directory rules to 1,260 tracked sources. Media imports require exact-file approval as well as the asset allowlist; prompt packs are classified for P06 example review.

Root `ochat-guide.md`, `control-flow-chatmd.md`, historical session transcripts, prompt collections, and development audits remain repository-only unless individually selected later. Do not glob Markdown outside the manifest into publication. Do not copy local logs, ignored prompts, credentials, scratch notes, or old generated `docs/` artifacts.

## Unresolved later work

- P02 is complete; see [spike evidence](p02-spike-report.md) for transaction recovery, watcher behavior, schema/provenance, renderer and publication-policy validation.
- P03/P04: scoped local Milestone A is complete; see [review evidence and limits](milestone-a-review.md). Manual native zoom, assistive technology and physical mobile keyboard review are deferred from launch by the user.
- P05 is complete with explicit repository-only/deferred boundaries; see the completion review. P06 completed the tutorial and complete-download catalog; P07 completed retrieval evaluation and the content beta; P08 completed finished media, metadata, and initial payload verification.
- P09: odoc explicitly deferred; no `/api/` navigation.
- P10/P11: account selected; authorized workers.dev preview, hosted checks and actual rollback/restore complete. Domain launch, public source candidate and actual GitHub gate enforcement remain outstanding. No domain purchase, production deployment or GitHub settings mutation has occurred.
- P12: permanent operational ownership and post-launch evidence.

## Reference material used for the spike

- [Starlight manual setup](https://starlight.astro.build/manual-setup/)
- [Starlight configuration](https://starlight.astro.build/reference/configuration/)
- [Starlight frontmatter](https://starlight.astro.build/reference/frontmatter/)
- [Astro programmatic API](https://docs.astro.build/en/reference/programmatic-reference/)

Exact installed source and package peer declarations were also inspected to resolve behavior at the pinned versions.

## Historical foundation checkpoint (superseded by P02 report)

- macOS diagnostics, 18 importer tests, static build, and output validation pass. A clean Linux source snapshot also passes `npm ci` and these checks. The Linux snapshot contained only committed baseline plus candidate source changes; no preexisting node_modules, generated content, OCaml environment, or model credentials.
- `dune build` and `dune build --force @agent-docs-check` pass with npm dependencies installed and Dune's website boundary present.
- Browser runner pinned to Playwright and playwright-core 1.61.1. Version 1.63.0's WebKit client could not create a page with the frozen macOS 14 WebKit binary (`PushAPIEnabled` protocol mismatch). Matching 1.61.1 packages resolve this. Keep browser-package versions aligned when upgrading; Linux CI is the path to current WebKit coverage on newer platforms.
- Browser suite passes 25 checks across Chromium, Firefox, and WebKit; two non-Chromium clipboard cases are explicitly skipped. Search Escape focus restoration needed a small supported Search component wrapper because Safari does not focus pointer-clicked buttons automatically.
- All 22 rendered docs retain the original source's fenced code values. Redundant legacy anchors are removed only where the renderer already owns the same ID. The final HTML checker verifies uniqueness and all local fragments.
- Full-corpus publication, finished tutorial curriculum, comprehensive release accessibility/performance evaluation, and hosted deployment remain incomplete. The later P02 increment is documented in the spike report; P05–P12 milestone gates remain open; P03/P04 completion is documented in the Milestone A review.


## P01–P04 audit follow-up

See [the correctness audit](p01-p04-audit.md) for five fixes and fresh evidence.
P01.01 is reopened for the actual repository commit of the website/lockfile;
version selection and isolated tracked-candidate reproducibility already pass.
The current worktree remains uncommitted. Development now regenerates and serves
through fresh child processes to avoid stale imported renderer modules.

## P06 curriculum and source ownership

The canonical example catalog lives in `docs-src/examples/catalog.json`. The
root Dune exclusion for `website/` remains intact; its existing docs-src source-tree
dependency now includes the catalog and all companions. `config/tutorials.json`
owns website curriculum order and explicit host/verification records. Ten promoted
lessons have scoped offline evidence; none claims a live provider response.

Exact supplemental download approval plus catalog selection authorizes each copied
source. Nine `.tar` archives and individual source files are generated atomically,
retain bytes/notices, and receive hash checks against actual built output. The
uncompressed archive format avoids preview/browser gzip transformations; extensionless individual
files receive `.txt` URLs while preserving actual names in downloads and archives.
Changed source/revision invalidates recorded verification. New local source paths
are labeled without inventing immutable GitHub targets. Full release builds still
require committed source bytes. See [the P06 review](p06-completion-review.md).

## P07 search ownership and failure handling

The website owns one final-build Pagefind index because the required `${}`
character option is not exposed by Starlight's indexing configuration. Starlight's
built-in pass stays disabled. The index is exactly the 106 manifest-approved search
routes, including four labeled compatibility APIs. Supported weights and two
canonical prose improvements resolve measured relevance problems; the original
Pagefind ranking remains in use.

The shared homepage/docs dialog replaces a widget with demonstrated homepage and
failed-loading gaps. It preserves keyboard focus, navigation fallback, query return
state in local history, and deferred loading. Pagefind 1.5.2 swallows some failed
index fetches; an isolated worker observes its own fetch failures around the
supported API and returns a recoverable error. Do not remove this adapter without
repeating real failed-index/fragment tests. No generated vendor code is modified.
The build-bound 18-query evaluator and three-engine browser cases protect these
decisions. See [the P07 review](p07-completion-review.md) for baseline evidence,
measured results, and release boundaries.


## P08 presentation and publication policy

Retain the established forest/cream visual identity, shared wordmark tokens, and
self-hosted Manrope/IBM Plex Mono. The homepage demonstration remains a visibly
labeled static README example. Illustrative composition is linked to the maintained
specialist tutorial; it is not a fabricated execution recording. Mermaid rendering
is requested explicitly, reserves a scrollable viewport, and retains exact source
on loading failure. Failed module imports can remain cached, so the error message
does not promise an in-page retry. Loading does not prevent collapsing the figure.

Local Satori/Sharp templates generate 116 social cards and four PNG icon sizes.
`config/media.json` owns public media/notices; generated and final media reports
record dimensions, hashes, repository assets, and self-hosted fonts. The scoped
fflate patch override fixes Satori's pinned vulnerable version without a forced
major update. All image hashes agree between preview and the isolated production
fixture; original notices remain byte-exact.

The explicit sitemap integration stays installed in previews to suppress Starlight's
automatic default. Its environment-aware filter emits no preview sitemap; the Head
override also removes the preview sitemap link. Production contains 103 approved
canonical URLs and retains the source-commit gate. A temporary local committed
fixture checks production output without committing this checkout or claiming a
public domain. Full reader journeys and the five-family payload report support the
P08 completion review. P09 has resolved the API decision as deferred; see the P09 review.

## P09 API deferral

The user confirmed deferral on 2026-09-07. Local `dune build @doc` succeeds,
but a hypothetical `/api/` mount has three missing target occurrences and 43
missing fragments, in addition to 271 generation warnings. The checked-in
`docs/` snapshot lacks current agent libraries. Neither output is approved for
publication. Details, limitations, and re-entry criteria are in
[the P09 review](p09-completion-review.md).

The OCaml landing page remains the public entry point, with nine direct source
contracts and matching architecture guides. Explicit policy prevents API routes,
links, output files, and search-index input; production sitemap membership stays
prose-only. Build evidence records deferral, and CI retains a single Node-only
website build with no API assembly job. P09.02–P09.06 are not applicable under the
chosen release scope. Keep those tasks unchecked with their explicit reasons.

## Pre-P10 selected design

On 2026-09-07 the user selected Graphite + blue and approved the shared UX
improvements. Neutral surfaces and blue links/actions apply to both themes,
the homepage, documentation, search, source readers, and 404. Favicon and
publishing artwork use the same identity. Temporary palette controls and Stone
styling are removed; old preview settings no longer affect the site.

The shorter homepage, five sidebar families, instructions-first tutorials,
clearer typography, and inline file-picker/copy/wrap controls are retained as
the final design. Preserve exact source bytes, native reading without
JavaScript, and ChatML highlighting as OCaml. Implementation and validation
are recorded in [the UI design completion review](ui-design-completion-review.md).
The [comparison review](ui-comparison-review.md) is historical evidence.
P09 API hosting remains deferred and P10 remains open.

## Application-led discovery and learning

The user approved all six recommendations after the Graphite iteration. The
site now presents useful outcomes through six application guides, a filtered
gallery, an inspectable recorded documentation-review workflow, and a dedicated
ten-lesson curriculum. Four new source bundles accompany the guides. Required
setup remains visible; detailed capability constraints sit beside benefit-first
explanations. Search labels document kinds, and source links open the specific
file in desktop/mobile readers.

The flagship capture includes real model responses and actual file-tool and
specialist execution, with explicit transport/provenance and reproduction
details. Other application previews remain labeled illustrations. Captures are
validated build inputs, never generated through model calls during website
builds. Keep Graphite, the approved Start here order, and the P09 API deferral.
See [the application UI review](application-ui-review.md). P10 remains open.

## P10.11: protected GitHub release checks

GitHub main protection requires the actual `release-gate` check from GitHub
Actions, strict up-to-date PRs, and resolved conversations, including for admins.
Zero outside approvals supports the sole-maintainer repository. Actual missing
and failed-check administrator pushes were rejected on an identical temporary
protected branch, then the probe was removed. All source inputs trigger the
OCaml prerequisite and both environment-specific website checks.

Clean CI exposed source pins not consumed by opam, the TextMate/Oniguruma 0.2
API incompatibility, and the missing Menhir generator dependency. Exact source
pins, Oniguruma 0.1.2, and the declared generator fix those failures; uploaded
package/pin evidence records the resolved Linux toolchain. Real checks remain
required. Legacy GitHub Pages branch publishing is disabled without removing its
existing served site. No production publisher or credentials are enabled.

P10 and Milestone C are complete within the approved launch scope; manual
accessibility remains explicitly deferred. P11 will qualify the owned production
origin and connect one protected publisher. See [the enforcement record](p10-github-enforcement.md).
