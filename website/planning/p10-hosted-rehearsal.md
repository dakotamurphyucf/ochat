# P10 hosted deployment rehearsal

Completed on 2026-09-07. P10.07 and the hosted rollback rehearsal pass. P10.11
(remote GitHub release enforcement and publication of the source candidate)
remains open; P10 and Milestone C are not signed off. P11 has not started.

Preview: https://ochat-website-preview.dakotamurphyucf-c3c.workers.dev

The user authorized this deployment. It is a public preview with noindex headers,
noindex HTML and crawl-disallowing robots. No custom domain, paid upgrade,
production deployment, repository push, or GitHub setting change was performed.
The main checkout remains uncommitted at base revision
`bc76b6c72a280b4bad48a793a63373e6db26d2b4`; source availability must still be
qualified at the real public release commit.

## Artifacts and transitions

| Item                      | Identity                                                           |
| ------------------------- | ------------------------------------------------------------------ |
| Worker                    | `ochat-website-preview`                                            |
| Account                   | `ec3ca23aab8456f6369df62e5c6a982c`                                 |
| Candidate artifact        | `8e906ad199ed3421f0e93f9075275b4ec73a72b342707376ea33df1d267ee218` |
| Baseline artifact         | `66ce5570d7b3c3de758b36a25c048b290b4cee475b9988b4da4d839e2864cbbb` |
| Baseline version          | `afde8f18-884b-4cdc-92fb-d080dd4ec622`                             |
| Candidate/current version | `2b9f01f1-f880-4873-a806-e33fcc3bf3df`                             |
| Final deployment          | `e4809020-8e6a-4bd3-b517-f8f856c11ca3`                             |

The candidate contains 125 HTML pages and 575 files (22,253,087 bytes).
The baseline uses identical page/source/asset bytes with one diagnostic response
header: `X-Ochat-Rehearsal: baseline`. It was separately output-validated, retained,
dependency/configuration-hashed, deployed and qualified before testing rollback.
This is a constructed rehearsal baseline, not a prior production release.
Both retained directories remain unchanged under `scratch/p10-artifacts/`.

Actual Cloudflare transitions, all at 100% traffic:

1. Deploy baseline (`c2755fec-e0ef-46a8-ad7d-c3699a3ba900`).
2. Deploy candidate (`8d211e0b-098c-4f6f-85a0-d8445f8be57a`).
3. Roll back to baseline (`b2d6c5de-a167-45b2-8182-4187694d18d4`).
4. Restore candidate (`e4809020-8e6a-4bd3-b517-f8f856c11ca3`).

The diagnostic header reappeared after rollback and disappeared after restore.
Cloudflare version inspection confirms static routing, expected raw headers,
compatibility date 2026-09-06 and no bindings. The account API confirms workers.dev
enabled and separate version-preview URLs disabled. No Git build integration was
created; the new Worker was created through Wrangler.

## Validation

- Local preview check: 68 unit tests, zero Astro diagnostics; output/link/fragment,
  indexing and capacity checks pass. Both retained artifacts passed dry runs and
  integrity verification. Four affected deployment tests pass after helper/config
  additions; formatting and whitespace checks pass.
- TLS 1.3 certificate chain and hostname validation pass.
- 3,579 HTTP assertions pass at each stage: qualified baseline, candidate,
  rolled-back baseline, restored candidate. All served asset hashes, nested
  documents, query-preserving slash redirects, branded missing/private-route 404s,
  noindex headers, robots, JavaScript/WASM MIME and cache policies are checked.
- Candidate and restored candidate each pass 18 browser checks across Chromium,
  Firefox and WebKit, covering search/result navigation, source and archive bytes,
  browser-saved archive filename/content, historical fragments, the Start here
  reading path, 404 navigation and social image metadata.
- Rolled-back baseline passes six browser checks for actual search/navigation and
  downloaded files/archives across all three engines.
- Hosted desktop homepage and mobile first-agent screenshots are retained.

## Observations and limits

Initial baseline Chromium search received a 404 for the hashed search-worker
script immediately after the first deployment. The other 17 baseline browser
checks passed. Three fresh Chromium retests and every later suite passed without
an application code change. The original trace is retained. Deployment propagation
is a plausible explanation, not an established cause; release readiness requires
successful public checks after upload.

HTML ETags are absent at this workers.dev edge. The initial diagnostic required
one and failed; the checker now records absence and verifies a fresh full 200
response with exact HTML bytes. JavaScript ETags remain present and conditional
requests return 304. Mutable responses require revalidation; only hashed `_astro/`
assets are immutable. No compression or account security settings were changed.

These observations come from this machine's network vantage point (recorded
Cloudflare Ray/cache headers), not a global edge survey or field performance
study. Public-source links, production indexing/custom-domain behavior and actual
GitHub enforcement still need their own release evidence. The old reserved-domain
production approval record remains pending; preview evidence does not attest that
production artifact. Manual accessibility remains deferred by the user.

Evidence: `scratch/ochat-website-evidence/p10-hosted/closeout.json`, HTTP reports,
browser logs, TLS record, deployment/version API records, retained initial failure
trace, dry-run/upload/rollback logs and screenshots. The read-only helper is
`scripts/rehearse-hosted.mjs`; operational commands are in the
[release runbook](release-runbook.md).
