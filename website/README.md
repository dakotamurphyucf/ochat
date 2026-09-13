# Ochat website

Production domain: **https://ochatlabs.com**. The protected Website workflow builds for this origin, retains the tested artifact, and publishes only after the main release gate passes. See [the production launch record](planning/p11-launch.md) and [release runbook](planning/release-runbook.md).

A static Astro/Starlight website with an application-led homepage and repository-owned documentation. Six application guides, an inspectable recorded workflow, and a ten-lesson curriculum help readers discover and build useful agents. The catalog contains fourteen entries: eight complete examples, five configurable templates, and one illustrative reading sample.

The extensibility update adds authoring, script-tool, background-work and child-session guides. The inventory contains 340 canonical documents: 136 published, 4 compatibility, 9 bridges, 181 repository-only and 10 deferred. It produces 149 documentation routes; search includes 140 approved pages. Generated migration/content reports are authoritative for a particular build. The former shell-resource-runner URL now explains linked child-process setup, preserving existing links without requiring that removed executable.

The recorded demo retains its original live capture and authenticates runtime hashes against its recorded Git revision. Changed current runtime files are labeled as such; they do not turn old model output into current-runtime verification. Build with full Git history. Recording regeneration and optional live-provider calls are never implicit website-build steps.

See [the application UI review](planning/application-ui-review.md), [contributor workflows](CONTRIBUTING.md), and [maintenance ownership](planning/maintenance.md). The [P10 review](planning/p10-completion-review.md) and [production launch record](planning/p11-launch.md) describe the original launch qualification. Odoc/API publication and manual accessibility review retain their user-approved deferrals. A local branch build does not imply that its changes have been deployed.

## Run locally

Use Node **22.22.1** (see `.nvmrc`) and npm. The website build needs Git, but no OCaml installation, model credentials, or provider calls.

From the repository root:

```sh
cd website
nvm use # If using nvm; otherwise select Node 22.22.1 with your usual manager.
npm ci
npm run dev
```

Open <http://localhost:4321>. Edit original Markdown in `../docs-src/` or homepage/components under `src/`. The dev command watches Markdown, configuration, the README example, public assets, and importer/renderer helpers. Each regeneration runs a fresh generator and replaces the supervised Astro reader process, so directory swaps and module edits appear reliably. Changes to the dev supervisor itself require restarting `npm run dev`. An invalid content change stops the server with an actionable error; fix it and restart. Pagefind searches the production build, so use the following for a fully working search preview:

```sh
npm run check
npm run build
npm run preview
```

`npm run check` runs Astro diagnostics and focused importer tests. `npm run build` regenerates content, builds static HTML and Pagefind, and validates every generated local link/fragment, indexing policy, and artifact capacity. Do not run content/check/build simultaneously: they share generated output.

For browser checks:

```sh
npx playwright install chromium firefox webkit
npm run evaluate:search
npm run test:browser
```

The browser suite includes search, themes, mobile navigation/overflow, accessibility checks, source links, no-JavaScript reading, and exact clipboard bytes in Chromium. Hosted headers/routing are separate release checks; Astro preview does not establish Cloudflare behavior. The Linux CI workflow runs the locked build and browser suite; its actual passing runs and production deployment are recorded in [the launch record](planning/p11-launch.md). Run `dune build @agent-docs-check` from the repository root when technical docs/examples change; this independent offline semantic gate remains necessary.

## Where changes belong

| Change | Edit |
|---|---|
| Technical prose and examples | Original `docs-src/` source |
| Inclusion, canonical route, description, section | `config/docs-manifest.json` |
| Sidebar groups and documentation-home learning paths | `config/navigation.mjs` |
| Tutorial order, hosts, and verification records | `config/tutorials.json` |
| Example catalog, dependencies, and verification records | `../docs-src/examples/catalog.json` |
| Exact source download approval | `config/supplemental-sources.json` |
| Search benchmark and relevance policy | `config/search-queries.json`, `config/search.mjs` |
| Shared search and index lifecycle | `src/components/Search.astro`, `src/scripts/search*.ts`, `scripts/search-index.mjs` |
| Framework capability map | `config/capabilities.json` |
| Supplemental source dispositions | `config/supplemental-sources.json` |
| Approved repository media | `config/assets.json` plus exact supplemental media approval |
| Homepage layout | `src/pages/index.astro` |
| Shared styling and document shell | `src/styles/`, `src/components/` |
| Social card identity and sitemap policy | `config/presentation.mjs` |
| Public media provenance and notices | `config/media.json`, `scripts/publishing-assets.mjs` |
| Site origin/repository identity | `config/site.mjs` and deployment environment |
| Small site-owned assets and license notices | `public/` |
| Import/build checks | `scripts/`, `tests/` |
| Requirements and decisions | `planning/` |
| Local implementation memory | `../scratch/ochat-website-implementation-notes.md` |

Never hand-edit `.generated/docs/`, `.generated/`, `.astro/`, or `dist/`. They are generated and ignored. Runtime code-backed Markdown stays owned by its existing OCaml generator, not the website importer. New canonical docs must be added to Git's index (`git add docs-src/path.md`) and receive an explicit manifest entry, even when deferred. Assets need both tracked source and an allowlist mapping. Source downloads need exact catalog selection and supplemental download approval. New local files have no committed source link until they exist at the recorded Git revision; production rejects uncommitted source bytes. New routes must be lowercase under `/docs/`; collisions fail.

The homepage ChatMD comes from the first XML example in the root README. Its launch line assumes the file is saved as `assistant.chatmd` after Ochat/provider setup. That hero is a static source example. The separate documentation-review viewer presents a captured model run, with source files, execution steps, and provenance.

## Output and deployment boundaries

Default builds use `http://localhost:4321`, preview noindex metadata/headers, and a crawl-disallowing robots file. `SITE_URL` controls the origin; `SITE_ENV=production` requires an explicit HTTPS origin. A production build must be generated and checked again after domain ownership is established. Do not deploy a localhost-origin preview as production.

`wrangler.jsonc` configures Workers Static Assets with separate `preview` and `production` environments. Production attaches `ochatlabs.com` to `ochat-website`; `redirect/wrangler.jsonc` attaches `www.ochatlabs.com` to the redirect Worker. Configuration contains public identifiers, never credentials. `dist/` is the only static-asset directory; the qualified release separately retains the redirect Worker and deployment evidence. Local checks and the [hosted rehearsal/rollback](planning/p10-hosted-rehearsal.md) pass. The [release runbook](planning/release-runbook.md) records the enforced GitHub gate, current production publisher, and recovery procedure. Keep alternate deployment triggers disabled.

The complete source inventory and deferral reasons are regenerated in `.generated/migration-report.json` and `.generated/migration-report.md`. They remain outside `dist/` and are uploaded as CI evidence. Published content/provenance is reported in `.generated/content-report.json`; output sizes and artifact SHA-256 are in `.generated/build-evidence.json`. The tutorial/example verification and download inventory is `.generated/examples-report.json`. These reports describe the local build and are not proof of live-runtime correctness or domain ownership. Immutable view-source links identify the Git HEAD snapshot; uncommitted changes can differ and must be recorded during development.

Fonts are self-hosted Manrope and IBM Plex Mono through pinned Fontsource packages. Their OFL notices and Ochat's original MIT license are copied under `public/licenses/`. The vector mark is site-authored. No TUI grammar files were copied into the website.

See [implementation decisions](planning/decisions.md) and [the complete specification](planning/implementation-spec.md) for scope and remaining gates.


## Importer recovery and metadata

Every generated input is published as one `.generated/` snapshot. Generation
holds `.content-lock/`, stages `.generated-stage-*`, and retains
`.generated-previous/` only during the swap. An exception restores the old
snapshot. A killed importer leaves a recoverable backup; rerun after the
10-second stale-lock window. Do not delete an active importer's lock.

Schema errors include source/field information. Source YAML frontmatter may
repeat `title`, `description`, `status`, `audience`, `kind`, or verification
fields only when they agree with the manifest. Publication and route settings
remain manifest-owned. Complex legacy routes use explicit bridge entries;
unimplemented alias declarations fail instead of being silently ignored.

Git provenance is computed from the original sources. Local edits are labeled
in preview; production generation rejects uncommitted published source bytes.
Shallow history and modified/new source files do not receive a last-updated
date. A verification label is effective only for its recorded current revision.
The migration report lists highlighting fallbacks and per-source hashes.

Mermaid loads only when a reader chooses **Show rendered diagram**. The diagram
uses strict rendering, the original parsed fence bytes, and a reserved scrollable
viewport. Source stays visible without JavaScript or when rendering fails. The
renderer is an optional download, excluded from the eager page budget.


## Visual and keyboard review

Optional Astro page prefetching is disabled in `astro.config.mjs`: rapid navigation
reproduced cancelled-prefetch errors in WebKit. Links use ordinary browser
navigation; the separate on-demand search worker/index remains enabled.

With the built local preview running, run `npm run review:design` from
`website/`. It captures the homepage, first-agent tutorial and ChatML reference
at desktop/mobile widths in both themes, measures actual text/focus contrast,
runs axe, and records complete Tab traversals. Images and JSON reports go to
`../scratch/ochat-website-evidence/p03/` and never enter the public build.
Each underlying script also accepts an output-directory argument.

Shared design foundations live in `src/styles/tokens.css`; homepage and docs
styles map these into their respective layouts. Mobile contents stays in the
article flow. The supported menu/search wrappers coordinate focus and prevent
overlapping layers. Native table markup lives inside a horizontal scroll region;
a small handler supplements horizontal arrow scrolling in WebKit without taking
keys from links or modified shortcuts.

The browser suite covers 200% root text sizing, 320px reflow, skip links, mobile
menu focus/return, search, contents links, code/table scrolling, reduced motion,
forced-color controls, copy success/failure and no-JavaScript navigation. On
macOS WebKit, Option+Tab follows the browser's all-links keyboard mode. These
checks are not a screen-reader assessment or a claim that native browser zoom
and mobile software keyboards have been reviewed. Manual assistive-technology and physical-device reviews remain explicitly deferred from launch; see the maintenance backlog.

## Tutorial, source reader, and download maintenance

Keep source examples in `docs-src/examples/`; the catalog is canonical there so
the offline Dune gate can check the exact selected files without including npm
dependencies in the OCaml build. `website/config/tutorials.json` maps T01–T10 to
existing canonical page IDs, host scopes, examples, and verification. It drives
actual previous/next navigation and the context shown on each tutorial.

The shared `src/components/ExampleSource.astro` reader displays the approved source
files inside tutorials, associated reference pages, and catalog cards. Tutorials initially show the first
example’s entrypoint; native details controls expose companions and notices even
without JavaScript. ChatMD is highlighted as source (XML), ChatML as OCaml, and
OCaml/NDJSON with their own grammars; highlighting does not execute or validate
these languages. The importer decodes the exact download bytes as strict UTF-8
into `files[].content`; malformed text fails generation. Astro escapes source
tags. The reader uses both site themes and keyboard-scrollable code regions.
Markdown `chatml` fences also use the OCaml grammar. Astro’s snippet highlighter
removes a final newline; the reader restores the original full-file text through
Shiki’s preprocessing hook so selecting code preserves its contents.
Source bodies are excluded from Pagefind to avoid duplicating tutorial prose.

Each downloadable entry declares its entrypoint, local companions/data/build
files, dependency edges, prerequisites, and original license. Directory-level
supplemental rules never grant download permission. The importer checks real-path
containment, copies exact bytes into the same generated snapshot as the pages,
and produces deterministic `.tar` bundles with original relative filenames.
Individual extensionless files use a `.txt` URL for static hosting; their download
attribute and archive preserve names such as `dune`. Plain `.tar` avoids automatic HTTP decompression and browser recompression
behavior encountered with gzip archive extensions. Hosted response behavior is checked by the P10/P11 release verifier and must be rechecked when download packaging or hosting changes. No source bundle is an executable website action.

Run `dune build --force @agent-docs-check` after changing selected source files or
tutorials. It checks actual captured dependency loading, missing companions, file
root confinement, moderator effects, protocol/examples, and source parity. For
the provider-free standalone host checks, run from the repository root:

```sh
python3 test/agent_docs/check_tutorial_hosts.py
```

This needs Python 3 and the configured OCaml checkout. It starts only owned local
hosts and loopback listeners, uses private temporary credentials/state, sends no
model messages, cleans up after shutdown, and writes a credential-free summary
under `scratch/ochat-website-evidence/`. It does not exercise terminal keys, a
sandboxed shell command, live provider output, or public deployment.

Verification records include base revision, platform/toolchain, commands exercised,
observations, limits, and hashes of the tested tutorial/example/runtime inputs.
After affected checks pass, update the record and exact SHA-256 values; never
refresh hashes merely to suppress a stale label. Commands containing PRIVATE_ROOT
or AVAILABLE_PORT record temporary contexts; use the complete helper or tutorial
to reproduce them. A mismatched hash/revision downgrades the visible state to
`not-checked`. T09 retains `known-limitation` for its explicit data-root workaround.
No P06 record claims a live provider run. Verification details/hashes are excluded
from Pagefind; example descriptions and host/capability metadata remain searchable.

## Search maintenance

Homepage and documentation share one accessible search dialog. Search runs against
114 approved pages: 110 published pages and four labeled compatibility pages.
Bridges, unpublished content, repeated navigation, source-reader bodies, and
verification controls are excluded. Compatibility APIs remain discoverable for
explicit queries such as `Prompt_session`; current entry points receive priority.

The website owns one Pagefind pass at `astro:build:done`. Keep Starlight's
`pagefind: false` while `scripts/search-index.mjs` is installed; enabling both
would index twice. Pagefind is pinned directly and retains `${}` as index
characters. Supported content weights address measured relevance problems.

The worker and index load only after a nonempty query. A dedicated worker keeps
Pagefind's supported API and ranking, observes failed static-asset fetches that
Pagefind can otherwise swallow, and reports a recoverable error. Retry starts a
fresh worker. Keep that failure boundary when updating Pagefind and repeat actual
index/fragment fault tests. Results render as text and accept only local canonical
documentation URLs. Queries remain in the browser; following a result can retain
the query in that page's local history entry. Quotes request an exact phrase;
unquoted queries retain Pagefind's default prefix/fuzzy behavior.

After building, `npm run evaluate:search` starts its own temporary preview and
checks all 21 benchmark queries, exact index ownership, current-before-compatibility
ordering, context/excerpts, and real heading destinations. It requires Playwright
Chromium. Reports are `.generated/search-index-report.json` and
`.generated/search-report.{json,md}`; the evaluation records the built artifact's
SHA-256. CI runs this gate and uploads the reports. Browser tests additionally
exercise dialog controls, mobile layouts, safe rendering, and network failures
across Chromium, Firefox, and WebKit. See [the P07 review](planning/p07-completion-review.md)
for measured results and Milestone B evidence.

## Presentation, publishing assets, and performance

The homepage hero is a labeled static source example. Its complete ChatMD comes
from the README, and links lead to installation, the source, and the maintained
walkthrough. The separate workflow viewer presents a recorded documentation
review with inspectable calls, output, source files, and explicit provenance.
Other application previews identify their illustrative scope.

Each build generates 1200×630 PNG social cards for the homepage and every rendered
docs route, plus 32/180/192/512px PNG icons from the vector favicon. The template uses
local Manrope font outlines, actual page titles/sections, and explicit status labels.
These assets make no eager reader request. Keep the static font and its OFL notice
when changing the template. Satori, Sharp, and the font package are pinned; Satori's
fflate dependency has a scoped fixed-patch override recorded in `package.json`.

Every authored file in `public/` must appear in `config/media.json`; unexpected files
fail generation. Exact copied repository media still requires `config/assets.json`
and supplemental approval. Generated dimensions/hashes are in
`.generated/publishing-assets.json`; `.generated/media-report.json` joins generated,
public, repository, font, and inline provenance. Example downloads retain their
separate canonical catalog/byte checks. All reports remain outside `dist/`.

`npm run build` checks unique page titles, descriptions, canonical/social URLs,
image dimensions/alternatives, source-history dates, and indexing policy. Preview
builds emit no sitemap; the sitemap integration's expected “No pages found” notice
confirms that. Production includes only approved canonical indexable URLs and still
requires committed source. 404 pages remain noindex without a canonical. The isolated
production fixture described in the P08 review verifies policy, not domain ownership.

After building and installing Chromium, run `npm run measure:performance`. It owns
a temporary preview, measures seven routes (including the application gallery and
recorded workflow), includes inline scripts/styles in
JS/CSS budgets, and writes `.generated/performance-report.json` tied to the artifact.
Computed gzip sizes and one throttled local timing sample are diagnostic; they are
not hosted transfer measurements or field Web Vitals. CI runs this budget check and
uploads the reports. See [the P08 review](planning/p08-completion-review.md) for the
editorial, visual, metadata, provenance, and performance evidence.

## API reference: deferred for this release

The OCaml integration guide at `/docs/integrations/ocaml/` provides current
library architecture, public interface links, and local `dune build @doc`
instructions. Hosted generated API pages and native API search are deferred.
Website search continues to cover the approved prose corpus.

`config/api-reference.mjs` records the decision. Navigation and final-output
checks reject API routes/links/files, and the Pagefind hook rejects an `api/`
directory before indexing. Build evidence includes the deferral status; the
production sitemap contains only eligible prose routes. Deploy only `dist/`.
Never copy the historical root `docs/` snapshot or local `_build/` output into
it. The CI workflow includes an OCaml semantic prerequisite and separate website
validation jobs; it has no API job or additional deployment trigger. Reopening inclusion requires the clean artifact, scope/license,
mounted-link/search, provenance, and assembly checks described in
[the P09 completion review](planning/p09-completion-review.md).

## Selected design

Graphite + blue is the user-selected design in light and dark modes. Preview it
at `http://127.0.0.1:4321/`; theme controls support light, dark, and system
appearance. Temporary palette controls have been removed.

The design includes a shorter homepage, five sidebar families, clearer
reading hierarchy, instructions before tutorial resources, and inline file
selection, copy, and wrap controls. ChatML uses OCaml syntax highlighting.
Favicon and generated social artwork match the selected colors. Native source
reading remains available without JavaScript.

See [the application UI review](planning/application-ui-review.md) for the current
implementation and verification. The [Graphite design review](planning/ui-design-completion-review.md)
and earlier comparison preserve historical evidence. P09 API hosting remains deferred; see the P10 review for current release qualification.

## Applications and learning experience

The homepage demonstrates a recorded documentation-review workflow and links to
[applications](http://127.0.0.1:4321/docs/applications/) and the
[tutorial curriculum](http://127.0.0.1:4321/docs/tutorials/). Canonical guides live
in `docs-src/applications/`; discovery metadata is in `config/applications.json`.
`config/lesson-overviews.json` provides concise outcome, setup, and next-step
labels for the maintained ten-lesson curriculum. Search metadata identifies each
page’s documentation kind.

`npm run content` validates the flagship recording against its source and runtime
hashes and generates `applications-report.json`. Website builds never make model
calls. `scripts/record-showcase.py` is an explicit maintainer tool; its default
uses scripted responses, and `--live` records bounded model calls against OpenAI.
Either invocation replaces the capture, so review the output and verification
records before accepting it. The source readers retain literal file bytes and
use `config/source-links.mjs` for stable, directly openable file links.

See [the application UI review](planning/application-ui-review.md) for content
scope, capture provenance, validation, and remaining release boundaries.

## Release qualification and recovery

Use `npm run check:semantics` for the actual offline Ochat documentation gate.
The CI workflow runs this prerequisite before preview and production website
jobs and has no input path filters. Its final gate rejects failed, skipped and
cancelled prerequisites. Actual clean GitHub execution and strict main protection are verified in
[the enforcement record](planning/p10-github-enforcement.md). Main requires
`release-gate` from GitHub Actions, including for administrators. Production publishing is enabled only after the passing main release gate, using that run’s retained artifact.

`npm run artifact -- retain DIRECTORY` saves a verified output manifest;
`npm run artifact -- verify DIRECTORY` checks retained bytes.
`npm run rehearse:static -- REPORT.json ARTIFACT [ARTIFACT ...]` exercises the
pinned local Workers runtime and can restore a previous retained artifact.
`npm run rehearse:hosted -- ARTIFACT REPORT.json` checks a retained HTTPS preview or owned production artifact
against its public origin without deploying or changing remote state.
`npm run release:ready -- ARTIFACT APPROVAL.json` rejects fixture origins,
stale approvals and incomplete hosted release records.

See [the release runbook](planning/release-runbook.md),
[manual accessibility worksheet](planning/manual-accessibility-review.md), and
[P10 qualification record](planning/p10-completion-review.md). Local candidate and public deployment evidence remain distinct. The [P11 launch record](planning/p11-launch.md) identifies the first verified production release; [maintenance](planning/maintenance.md) owns ongoing reviews.


## Required CI and selective publication

The required `release-gate` covers selected framework normal/E2E tests,
documentation semantics, and both website environments. Change detection and
the final gate always run. Maintainer-documentation-only changes can skip heavy
jobs and publication; unknown inputs or missing history trigger full checks.
Unshipped website changes are included before deciding whether to publish.
See [CI coverage, recovery and cache maintenance](planning/ci-enforcement.md).
