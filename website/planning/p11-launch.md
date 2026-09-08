# P11 production launch

Implementation target: **https://ochatlabs.com**, with www redirecting to the
HTTPS apex. Production deployment is pending qualification and the first main
publish. This record does not claim that an unperformed live check passed.
The deployment history and immutable per-run evidence are available from the
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

The GitHub repository Website field is updated after live verification; README
entry points are included in this change. The full Ochat normal/E2E gate remains
separate todo item 17, not a prerequisite added silently to P11.
