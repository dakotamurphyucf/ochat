# P11 production launch

**Public launch verified:** https://ochatlabs.com, with www redirecting to the
HTTPS apex. PR #21 merged normally at revision
`b859aef70312a0f2553998a024f3c98561906106`.
[Main run 34182707554](https://github.com/dakotamurphyucf/ochat/actions/runs/34182707554)
passed all release checks; deployment attempt 2 published and verified the site
on 2026-09-08 UTC (2026-09-07 CDT). GitHub's repository Website field and README
now point to the owned domain. Milestone D's public visitor path is verified.

The first deployment uploaded the static Worker but Cloudflare rejected the
custom-domain attachment with error 100117. The user removed the conflicting
apex A records (`13.248.243.5`, `76.223.105.230`) and www CNAME, preserving other
DNS records. Only the failed deployment job was rerun; it reused the original
passing checks and retained artifact without rebuilding.

The immutable per-run evidence is available from the
[Website workflow](https://github.com/dakotamurphyucf/ochat/actions/workflows/website.yml)
and [production environment](https://github.com/dakotamurphyucf/ochat/deployments?environment=production).

## Domain and ownership

The user purchased ochatlabs.com through GoDaddy for **$31 for two years**, with
an **auto-renewal quote of $45**. The quote's renewal term was not specified; do
not reinterpret it as an annual price. Registration and renewal remain in the
user's GoDaddy account. Registrant contact-email verification remains owner
managed and has not been independently checked here.

Cloudflare's API confirms the zone is active in account
`ec3ca23aab8456f6369df62e5c6a982c`, zone `3b704db6095d687c7170377de4adf7f1`.
The .com parent and two public resolvers agree on
`justin.ns.cloudflare.com` and `savanna.ns.cloudflare.com`.
The account API token was created by the user and stored directly in GitHub as
`CLOUDFLARE_API_TOKEN`; its value was never requested in chat or read locally.
Local Wrangler OAuth is separate from the CI credential.

## Publication behavior

The product, commands, and repository remain named Ochat. The owned domain is
passed explicitly to the production CI build, which generates the canonical,
Open Graph, sitemap, robots and search URLs. The preview environment remains
noindex. Deferred odoc API hosting and manual accessibility remain deferred.

The protected main workflow qualifies the artifact and retains its static files,
redirect Worker, reports and SHA256 inventory. Only the subsequent deployment
job receives the Cloudflare secret. It rejects a superseded main revision and
publishes the retained artifact without rebuilding. GitHub's production
environment only allows the main branch; publisher concurrency is serialized.

`ochat-website` hosts the static apex site with real branded 404 responses.
`ochat-website-redirect` handles www with a 308 redirect preserving the encoded
path/query. No models, databases, API endpoints, or visitor authentication are
introduced. The redirect Worker has sampled logs and traces; the main site's
static requests do not invoke Worker code. The zone's Always Use HTTPS setting
handles HTTP requests to the apex. The two Worker updates are sequential, not
an atomic multi-Worker deployment; failure of either or hosted validation marks
the deployment workflow failed and retains diagnostic evidence.

## Verification and recovery

CI keeps the OCaml docs check, both website matrices, browser accessibility,
search and performance gates. New checks reject production evidence with a
wrong origin/revision/hash, failed tests or changed redirect code, and check
hostname redirect path/query behavior. Both Worker configurations undergo
Wrangler dry-run packaging before the required gate can pass.

The deployment step records previous and new Cloudflare deployment versions;
GitHub retains each production artifact/evidence set for 90 days. A first release
has no previous production version. The P10 baseline/candidate rollback is a
qualified preview rehearsal, not an invented previous production release.
Prefer a revert PR through the same pipeline for recovery; preserve downloaded
known-good archives beyond GitHub retention. See the [runbook](release-runbook.md)
for emergency version rollback and post-restore verification.

The live verifier compares every served asset to its retained hash, checks
production robots/headers, redirects, conditional caching and real 404s. Initial
certificate/asset-readiness probes are bounded and recorded. Live browser checks
cover onboarding, search, inline source reading and downloads. Final version IDs,
artifact hashes and results belong in the per-run reports and the persistent
local record at `scratch/ochat-website-evidence/p11/closeout.json`.

## Verified production release

- Artifact SHA256: `304b0057bdecf2153d2099ed216124c69301ec2ce3866a3c61203270450b5182`.
- Main Worker version: `2fe02ab9-1cc4-41ea-b0d9-d4bd94b23d19`; deployment `e28cb083-aca9-473a-b035-d07a4a1ee945`.
- www Worker version: `1adc035f-ce24-4290-90bf-c14d024a730d`; deployment `e6b52ccd-f9ea-48d0-8c12-9d46331734d5`.
- Both CI environments: 71 unit tests and zero Astro diagnostics; 256 browser passes and two existing clipboard skips each. The semantic gate passed 308 pages/38 methods; both search and performance gates passed.
- Live hosting: 3,601 assertions passed for retained bytes, indexing, caching, MIME types, real 404s and HTTP/www canonical redirects. Initial certificate readiness failures were recorded before successful verification. Both public hostnames have trusted HTTPS certificates.
- Live browsers: 21 checks passed across Chromium, Firefox and WebKit, covering onboarding order, search, mobile overflow, committed source links, inline ChatMD without JavaScript, explicit downloads and 404s.
- GitHub: 18 post-merge policy checks passed. Website field, README links, reciprocal homepage repository link and representative exact-revision source destinations verified.

The first live browser smoke run reported an intermittent WebKit prefetch
access-control error during rapid navigation. Inspection found Astro already
catches prefetch failures. An instrumented WebKit run and a full uninstrumented
three-browser rerun passed against the unchanged artifact. Keep the initial
report for P12 monitoring; no application or upstream fix is claimed.

The first partial upload is not a previously live, verified production release.
The qualified artifact above is the first public recovery baseline; retain its
archive and version IDs beyond GitHub's 90-day artifact retention if needed.

Registrar contact-email verification and the renewal quote's period remain
owner-managed administrative follow-ups; neither is claimed independently
verified. Domain connection, certificates and public website operation are
verified. P11.02's contact-email confirmation remains open in the task checklist.
The API-hosting and manual-accessibility deferrals are unchanged.

Follow-up tasks in `scratch/todo.md`: 17 adds framework normal/E2E coverage,
18 selects checks/deployments by changed inputs, and 19 improves workflow speed.
These changes were not implemented as part of launch. The public site currently
uses the full gate and automatic deployment on passing main runs.
