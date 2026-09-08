# Contributing to the Ochat website

Start here for a routine edit. The [website README](README.md) explains the
importer, source readers, search and rendering internals. The
[release runbook](planning/release-runbook.md) owns publication and recovery;
[maintenance](planning/maintenance.md) records owners, review dates and backlog.

## First edit from a clean checkout

```sh
git clone https://github.com/dakotamurphyucf/ochat.git
cd ochat
git switch -c docs/my-change
cd website
nvm use
npm ci
npm run dev
```

Use Node 22.22.1 from `.nvmrc`; another Node version manager is fine. Read the
printed preview URL: the server starts at port 4321 and selects another port
when it is occupied. Keep this terminal running while editing. No model keys
or OCaml installation are needed for the website preview.

For an initial exercise, edit a prose sentence in
`docs-src/agent-server/tutorials/local-tui.md` and visit
`/docs/start/first-agent/`. The watcher regenerates source input and restarts
the reader; reload after it reports ready. The page's edit link points to that
original Markdown file on GitHub. Its committed-source link identifies the
checkout's HEAD, so it does not include an uncommitted local edit. New files
cannot have a working public source link before they are committed and pushed.

Stop the dev process before running generation or build checks in that checkout:

```sh
npm run check
npm run build
npm run preview
```

Search needs a built Pagefind index; the dev server alone is not a complete
search test. Do not run `content`, `check` and `build` concurrently: they write
the same generated snapshot. An invalid source stops the dev supervisor with
an error; fix the input and restart `npm run dev`.

Review `git diff`, stage only intended source files, commit and open a PR.
Generated output stays ignored. Production requires committed source bytes
and public revision links; use the protected workflow for publication.

## Find the source of a change

| Change | Source of truth |
| --- | --- |
| Article prose | `docs-src/`; find its `source` in `config/docs-manifest.json` |
| Code-generated reference prose | The existing OCaml generator identified by the manifest's `generatedBy`; regenerate through its documented Dune target |
| Title, description, route, publication/indexing status | `config/docs-manifest.json` |
| Sidebar and Start here groups | `config/navigation.mjs` and manifest section/order fields |
| Tutorial sequence and example associations | `config/tutorials.json` |
| Homepage copy and layout | `src/pages/index.astro` and its imported components; the hero ChatMD comes from root `Readme.md` |
| Application guides and recorded workflow | `docs-src/applications/`, `config/applications.json`, and the selected canonical example/recording files |
| ChatMD/ChatML source and downloads | `docs-src/examples/catalog.json`, its exact files, and `config/supplemental-sources.json` |
| Themes, typography, shared UI | `src/styles/tokens.css`, other `src/styles/`, and `src/components/` |
| Images, fonts and license notices | `public/`, `config/media.json`, and approved repository mappings in `config/assets.json` |
| Search ranking and regression queries | `config/search.mjs`, `config/search-queries.json`, and `scripts/search-index.mjs` |
| Sitemap/social metadata | `config/presentation.mjs`, manifest policy and `config/site.mjs` |
| HTTP headers and asset routing | `scripts/deployment-policy.mjs` and `wrangler.jsonc` |
| www-to-apex redirect | `redirect/worker.mjs` and `redirect/wrangler.jsonc` |
| Requirements, operations and evidence summaries | `planning/`; implementation memory stays in ignored `scratch/` |

Never hand-edit `.generated/`, `.astro/`, `dist/`, or Dune's `_build/` output.
Planning documents are repository documentation and are not served by the site.

## Add a page

1. Write original Markdown under `docs-src/` and stage it with Git; the importer
   inventories tracked/staged sources. Copy the structure of a nearby manifest
   entry, giving the new page a unique ID and the exact repository-relative source.
2. Assign a disposition. A published page needs a unique lowercase `/docs/…/`
   route, useful title/description, appropriate section/order and explicit
   navigation/search/sitemap/noindex settings. Use existing audience, kind,
   status and provenance values from `scripts/manifest-schema.mjs`.
3. Add navigation or tutorial/application associations when appropriate. Related
   targets use manifest IDs. Keep source-relative Markdown links; the importer
   resolves approved destinations. Add useful image descriptions and language
   labels on code fences (`chatml` uses OCaml highlighting).
4. Run the checks below. Review `.generated/migration-report.md` for inclusion
   and deferral reasons and `.generated/content-report.json` for provenance.
   Do not invent a verification date or call an example live-checked without
   execution evidence for its actual inputs.

## Remove or move a page

Decide whether the old URL should remain a compatibility/bridge page or needs
an explicit redirect. Update incoming links, related IDs, tutorial/application
associations, navigation and search benchmarks. Preserve established heading
anchors with a reviewed `fragmentAliases` mapping when headings change.
Do not delete an entire manifest entry while leaving its canonical source
unaccounted for; choose `repository-only` or `deferred` where appropriate.

There is currently no authored path-redirect registry. The `aliases` field is
not an implemented redirect mechanism. Prefer an explicit bridge page for a
routine move. If a real HTTP redirect is needed, add reviewed generation of
`_redirects` to the authored build pipeline, including source/provenance and
capacity handling; do not hand-edit `dist/_redirects`. Validate old/new paths,
fragments, queries, loops and chains in the deployed environment. Host redirects
belong to the separate www Worker, not a static path rule.

## Update an example or capture

Edit canonical files and reconcile catalog entrypoint, companions, dependency
edges, exact download approvals, prerequisites and licenses. Readers and `.tar`
downloads must retain the original full text and relative filenames. Run the
offline semantic documentation checks before refreshing verification hashes.
Changing a recorded input without rerunning its actual verification must leave
the example visibly not checked.

`scripts/record-showcase.py` overwrites the tracked showcase with scripted data
by default; `--live` makes paid provider calls. Do not run it as a routine build
or replace the real capture to clear stale verification. An intentional capture
update needs its own reviewed inputs, provenance and bounded execution scope.

## Choose validation appropriate to the edit

| Edit | Local validation before the full PR gate |
| --- | --- |
| Prose/metadata/navigation | `npm run check`, `npm run build`, inspect the edited route and source link |
| Tutorial, tool/runtime contract, selected example | Above plus `opam exec -- dune build --force @agent-docs-check` from the repository root |
| Search or changes to discoverability | Build, then `npm run evaluate:search` |
| UI, CSS, templates or browser behavior | Build, install browsers, then `npm run test:browser -- --workers=2`; also `npm run measure:performance` for affected presentation |
| Hosting, headers, redirects or downloads | Retain the qualified artifact and follow the runbook's local/hosted routing checks |
| Planning-only documentation | Check referenced paths/commands and `git diff --check`; current GitHub policy still runs the full gate |

Install browser binaries with
`npx playwright install chromium firefox webkit` (Linux runners also need the
supported system dependencies via `--with-deps`). Search and performance scripts
start their own temporary previews. For a subset of browser tests, choose an
existing relevant file/test; that does not replace full release qualification.
The semantic gate uses the project's qualified OCaml setup and pins from the
runbook, not a website-installed compiler. It makes no provider calls.

## Upgrade dependencies

Use a dedicated branch. Review official release notes and the current
Node/framework compatibility requirements; keep exact package versions and
commit the resulting `package-lock.json`. Keep Astro/Starlight integration,
Pagefind ownership, Shiki grammars, Satori/Sharp output, Wrangler schema and the
scoped `fflate` override in the review. Upgrade only the intended packages.
Use `npm ci` in a fresh checkout to demonstrate the lockfile, then run the full
affected build/browser/search/performance/hosting checks. Record cold and warm
results when changing cache/toolchain behavior; do not silently reuse old
artifact evidence. Tasks 17–19 cover future gate improvements separately.

## Optional generated API reference

API hosting is deferred. For local investigation use the Ochat toolchain and
`opam exec -- dune build @doc`; inspect `_build/default/_doc/_html/` locally.
Do not copy it into `dist/`, change the exclusion flag, or expose `/api/` alone.
Reopening hosting requires the link/scope/license/search/assembly work listed
in [the P09 review](planning/p09-completion-review.md). The regular prose site
continues to build without OCaml.
