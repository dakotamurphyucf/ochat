# Release candidate, launch checks, and recovery

P10 qualifies a candidate; P11 publishes it at the owned domain. The local P10
fixture uses `https://release.ochat.test`, which must never be deployed. A build
or an automated accessibility scan does not establish hosted behavior or manual
accessibility conformance. See [the P10 review](p10-completion-review.md) for the
current evidence and outstanding gates.

## One publication owner

The Website workflow is the sole production publisher for `https://ochatlabs.com`.
Every pull request and main push runs the OCaml documentation gate and both
preview/production website matrices. Production builds use the owned origin;
preview builds remain noindex. The final `release-gate` rejects failed, cancelled
or skipped prerequisite jobs. The broader Ochat normal/E2E test gate is a separate
follow-up in scratch/todo.md; do not describe it as already implemented.

Only a successful **main push** can enter `deploy-production`, whose GitHub
`production` environment permits only the `main` branch. Deployment concurrency
is serialized, and the publisher checks the current main SHA before uploading.
It downloads `website-production-release` from its own run; it does not rebuild.
Integrity verification matches the retained file inventory, redirect code,
Wrangler configuration, package lock, origin, source revision, semantic report,
search/performance hashes, capacity and browser result. It also checks anonymous
access to the exact committed first-agent tutorial. `approval.json` combines that
check with the reviewed historical P10 rehearsal/rollback/enforcement records.
Those records qualify the procedure; the current candidate is separately tested.

The only credential-bearing step receives the repository Actions secret
`CLOUDFLARE_API_TOKEN`. Pull-request builds and tests receive no hosting or model
credentials. The token's Workers permissions are account-scoped; its zone policy
is restricted to ochatlabs.com by the owner. Review workflow changes carefully:
repository administrators can change policies or workflow code, so this is not
an immutable trust boundary against the owner. Do not enable Cloudflare Git
builds or another automatic publisher. Legacy GitHub Pages remains workflow-only.

Main protection requires up-to-date PRs, `release-gate` from GitHub Actions app
15368, resolved conversations, and applies to administrators. Force pushes and
branch deletion are disabled. No outside approval is required for the sole
maintainer. Deployment failure is visible as a failed workflow/environment;
`release-gate` itself reports validation, not post-deployment health.

The main Worker `ochat-website` serves static assets at the apex domain without
invoking application code. A separate `ochat-website-redirect` Worker redirects
www to HTTPS apex with path/query preservation and sampled logs/traces. Static
asset requests do not produce Worker invocation traces. Both configurations and
the redirect code are retained in the qualified artifact. Enable the zone's
**Always Use HTTPS** setting for HTTP apex requests. Never point production at
the noindex `ochat-website-preview` Worker.

Production release artifacts and deployment evidence are retained by GitHub for
90 days. Download known-good archives to durable storage before retention expires.
The publisher records existing and resulting Cloudflare deployment versions.
The first production release has no previous production version; the P10 preview
rollback is a rehearsal, not a prior production release.

## Build and retain the candidate

From `website/`, after installing pinned dependencies and the project toolchain:

```sh
npm ci
npm run check:semantics
npm run check
npm run build
npm run test:browser -- --workers=2
npm run evaluate:search
npm run measure:performance
npm run artifact -- retain ../scratch/release-preview
```

The artifact command also retains the www redirect Worker. It refuses an existing destination, checks the final tree
against build evidence, and retains `dist/`, reports, the Wrangler configuration,
and a manifest of file hashes. `verify` rejects changed, additional, or symlinked
output. Reports include the environment, source revision, origin, lockfile hash,
and artifact hash. Retaining an artifact is an integrity check, not a signature
or permission to publish. Keep its manifest and evidence in trusted storage.

Before public production, commit the approved sources, select the owned HTTPS
origin, build with `SITE_ENV=production` and `SITE_URL` set to that origin, and
repeat the relevant checks. The importer requires committed canonical/example
bytes. View-source links must resolve at that public commit; edit links use
`main`. Never promote a noindex preview or relabel its evidence as production.

For the CI semantic toolchain, `opam install` does not consume the source pins
in `dune-project`. The Website workflow explicitly pins `textmate-language`
and `piaf` to the exact public Git revisions qualified locally before installing
dependencies, with Oniguruma 0.1.2: the TextMate fork's metadata does not exclude
Oniguruma 0.2, whose `Syntax.default` API changed incompatibly. The clean GitHub
run exposed this missing transitive constraint. It uses the published OCaml 5.3.0 compiler and does not copy the
developer's opam switch or its macOS-only `core_unix` pin. Keep these revisions
explicit and review changes with the actual semantic documentation gate. The
semantic evidence artifact includes the installed package versions and pins; this
records the resolved toolchain without claiming a complete OCaml dependency lock.

For local qualification while the checkout is uncommitted:

```sh
python3 website/scripts/create-release-fixture.py \
  --record scratch/ochat-website-evidence/p10/fixture.json
```

This copies authored inputs and installed dependencies into a temporary Git
repository and builds a production fixture there. It creates no main-checkout
commit and proves neither a clean dependency install nor public source links.
The fixture record identifies its directory and synthetic revision. Run its
browser suite with `PLAYWRIGHT_BASE_URL` set to the server serving that fixture.
An explicitly supplied base URL disables Playwright's default local server.

## Local routing and rollback rehearsal

```sh
npm run rehearse:static -- ../scratch/release-routing.json \
  ../scratch/release-candidate
npm run rehearse:static -- ../scratch/release-rollback.json \
  ../scratch/release-candidate ../scratch/previous-good ../scratch/release-candidate
```

Each argument is a retained artifact directory. The helper verifies its manifest,
starts the pinned local Wrangler/workerd asset server, checks representative HTML
against retained bytes, tests nested routes and slash normalization, real 404s,
indexing headers, downloads, search MIME, cache policy and conditional ETags,
then stops that server. The rollback sequence restores the retained previous
artifact and returns to the candidate. It checks each artifact's own cache policy;
a previous build can legitimately predate the candidate's caching changes.
This is an artifact recovery rehearsal, not a Cloudflare deployment rollback.

## Hosted rehearsal before launch

The configured `preview` Wrangler environment targets `ochat-website-preview`
in the owner's Cloudflare account, with `workers_dev` enabled and separate
version-preview URLs disabled. It inherits this static site's asset routing;
there are no application bindings, custom-domain routes, or Git build triggers.
The current rehearsal origin is
`https://ochat-website-preview.dakotamurphyucf-c3c.workers.dev`.

Build with `SITE_ENV=preview` and `SITE_URL` set to that origin, then retain the
artifact. Use the retained directory for both the dry run and upload:

```sh
npx wrangler deploy --env preview --assets ../scratch/release-preview/dist --dry-run
npx wrangler deploy --env preview --assets ../scratch/release-preview/dist
npm run rehearse:hosted -- ../scratch/release-preview ../scratch/hosted-http.json
```

The read-only hosted helper verifies all served asset hashes, routing, preview
headers, JavaScript/WASM MIME, cache policy, conditional JavaScript ETags, and
branded private/missing-route 404s. It uses four concurrent requests at most.
Cloudflare may omit HTML ETags: in that case the check records their absence and
verifies an explicit fresh request returns the complete current HTML. This does
not claim an HTML 304. Browser interactions and TLS certificate details are
recorded separately. Treat an upload success as the start of qualification:
first-deployment propagation can expose transient asset errors, and the public
checks must pass before the environment is declared ready.

Use an explicitly selected account/project and a preview artifact built for its
preview origin. Record the Worker name, version/deployment IDs, origin, source
revision, and exact uploaded artifact hash. Verify over public HTTPS:

- Certificate and canonical host; homepage and deep documentation routes.
- Both slash forms, query strings, direct fragments, and branded nested 404s.
- Preview `X-Robots-Tag: noindex, nofollow`, noindex HTML, and crawl-disallowing robots.
- Download bytes and browser-saved `.tar` files, filenames, and MIME types.
- Search input, worker/index/shards, and an actual result navigation.
- Revalidation on mutable HTML and Pagefind entry points, immutable caching only
  on hashed `_astro/` assets, ETag behavior, and observed CDN responses.
- `_headers`, scratch, implementation notes, and API output are not public assets.

For the separate production build, verify the owned canonical/social origin,
indexable canonical pages, noindex bridges/404s, and the approved sitemap. A local
HTTP rehearsal does not satisfy certificate, CDN, domain, or account checks.

## Manual accessibility follow-up (deferred from launch)

The user deferred this review on 2026-09-07; it is not required for launch. Automated accessibility tests remain enabled. For later review, use [the manual review worksheet](manual-accessibility-review.md). Record the
reviewer, date, device, browser/assistive technology versions, artifact hash,
route/state, observed behavior, defects and retests. ARIA snapshots and simulated
viewports supplement this review; they do not certify VoiceOver or a phone keyboard.

## Approval record and promotion boundary

`npm run release:ready -- ARTIFACT APPROVAL.json` verifies retained bytes and
requires the exact production origin/revision/hash plus completed records for
hosted rehearsal, hosted rollback, actual remote enforcement,
and public source-commit availability. It rejects reserved fixture origins.
The trusted publisher must invoke this gate as well as require CI success.
This tool checks recorded attestations; it cannot independently authenticate a
reviewer's statement or replace the underlying checks.

Approval JSON has `artifactSha256`, `origin`, `revision`, and a `reviews` object.
Each of `hostedRehearsal`, `hostedRollback`,
`remoteEnforcement`, and `publicSourceCommit` needs `status: "pass"`, `reviewer`,
an ISO `reviewedAt`, and an `evidence` reference. Leave unfinished reviews pending.
Never insert placeholder passes to satisfy the gate.

## Hosted recovery

Before each upload retain the current known-good version/deployment ID and its
artifact. In the selected preview Worker, use `wrangler rollback VERSION_ID`
(or the dashboard Deployments rollback action), verify the old routes, search,
downloads and headers, then restore the reviewed candidate version and verify
again. Capture IDs and observations for both transitions. Confirm exact command
options with the pinned CLI's `rollback --help` before execution. Actual hosted
rollback remains unverified until this exercise runs in the chosen account.

For the initial account rehearsal, a separately validated baseline can use the
same site bytes with a diagnostic `X-Ochat-Rehearsal: baseline` response header.
Record this as a constructed rehearsal baseline, not an older production release.
Retain and verify both artifacts; deploy and qualify the baseline first, deploy
and qualify the candidate, then roll back to the exact baseline version. Verify
the marker returns and the browser checks pass. Restore the exact candidate
version and verify the marker is absent and all checks pass again. The header
change makes the actual remote transition observable without altering the UI.

For production incidents, restore the known-good version first, then investigate.
Keep compatible route redirects when rolling forward. This static site has no
bound databases to roll back, but confirm the deployed configuration still has
no bindings. Cloudflare documents version/binding rollback limitations in its
[rollback guide](https://developers.cloudflare.com/workers/versions-and-deployments/rollbacks/).

## Deployment limits and policies

Wrangler uses `website/` as its working directory, `dist/` as its only asset root,
`404-page` handling, and automatic trailing slashes. There is no SPA fallback.
No legacy path redirects are approved yet; historical fragments remain in-page
bridges rather than invented server redirects. The capacity gate supports future
rules and rejects duplicate sources, simple loops, and chains above two hops.
Review dynamic-rule overlap and hosted redirects explicitly when adding them.

The checked Free-plan capacities are 20,000 files, 25 MiB per file, 100 header
rules and 2,000 characters per header line; redirect limits are 2,000 static,
100 dynamic, 2,100 total, and 1,000 characters per rule. Build output validates
all counts after generated search/media/download assembly. Recheck the selected
account before upload. Sources reviewed on 2026-09-07:
[Workers limits](https://developers.cloudflare.com/workers/platform/limits/#static-assets),
[headers](https://developers.cloudflare.com/workers/static-assets/headers/),
[redirects](https://developers.cloudflare.com/workers/static-assets/redirects/), and
[Workers Builds limits](https://developers.cloudflare.com/workers/ci-cd/builds/limits-and-pricing/).
GitHub owns builds in this design; Cloudflare Workers Builds is not configured.

## Production recovery

Prefer a normal revert PR: revert the faulty website change, let the full gate
qualify the reverted code at its new main revision, and publish through the same
workflow. This preserves the single automatic publisher and public source links.
The pre-deployment versions in `production-deployment-evidence/deployment.json`
and the prior run's retained archive identify the previous deployment. If an
emergency manual Cloudflare version rollback is needed, pause automatic releases,
follow the qualified Wrangler rollback procedure, restore both Workers when both
changed, verify HTTPS/search/downloads/redirects, and reconcile main before
resuming automatic publishing. A manual rollback is an explicit incident action,
not an alternate automatic trigger.

After publication, `rehearse-hosted.mjs` checks exact public bytes for all assets,
indexing headers, robots, cache validators, 404s, HTTP and www redirects. It records
bounded initial certificate/asset readiness probes rather than hiding failed
full checks. Browser onboarding/search/inline-reader checks and domain ownership
evidence are recorded separately under `scratch/ochat-website-evidence/p11/`.
