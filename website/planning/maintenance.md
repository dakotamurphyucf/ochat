# Website maintenance, ownership and follow-up

The production site is **https://ochatlabs.com**. Ochat remains the product and
repository name. Use [contributor workflows](../CONTRIBUTING.md) for source edits,
the [release runbook](release-runbook.md) for deployment/recovery, and
[the launch record](p11-launch.md) for the first verified production baseline.

## Operational ownership

The accountable maintainer is **Dakota Murphy**, owner of
`dakotamurphyucf/ochat`. A contributor may prepare a change; the maintainer owns
its review, release policy and incident response. No backup account owner or
second maintainer has been designated.

| Responsibility | Owner and location | Maintenance action |
| --- | --- | --- |
| Domain registration and renewal | Dakota's GoDaddy account, `ochatlabs.com` | Keep billing/contact details current; purchase was $31 for two years, renewal quote $45 with period unspecified. Confirm the actual renewal date/term in the account; do not infer an annual price. |
| Registrar contact verification | Dakota, GoDaddy contact email | Owner confirmed completed verification on 2026-09-08 UTC. Keep the contact address current; no independent email/account audit is claimed. |
| DNS, certificates and hosting | Dakota's Cloudflare account; zone and Worker IDs in the launch record | Preserve the Worker-managed apex/www records and Always Use HTTPS. Keep unrelated email/verification records intact. |
| Repository and branch policies | Dakota, GitHub repository settings | Require `release-gate` from GitHub Actions, strict up-to-date checks and administrator enforcement; use PRs. |
| Publication | The repository's Website workflow | `main` only, after full qualification, exact same-run retained artifact, serialized publication. Cloudflare Git builds and alternate automatic publishers stay disabled. |
| Deployment credential | Dakota, GitHub Actions repository secret `CLOUDFLARE_API_TOKEN` | Rotate via Cloudflare and replace directly in GitHub; review account Workers and ochatlabs.com zone permissions. Never put token values in notes, commits or logs. |
| Account recovery | Dakota, each provider's account/recovery settings | Maintain recovery email, MFA/recovery methods and recovery codes in private storage. This document assigns responsibility; it does not claim those settings have been inspected. |
| Incident recovery and retained releases | Dakota, successful run artifacts and private archive storage | Preserve artifact, approval/evidence, revision and both Worker version IDs. Download before GitHub's 90-day retention expires; follow the runbook for a revert PR or emergency rollback. |

The first public artifact is retained locally in
`scratch/ochat-website-evidence/p11/qualified-production-b859aef7.tar.gz` with a
SHA256 file. Scratch is ignored and machine-local; it is not a shared backup.
The maintainer should keep a copy in their existing private backup storage.
Do not commit release archives, credentials, account-recovery data or personal
dashboard screenshots to make them shareable.

## After a routine release

1. Open the actual main run and confirm both `release-gate` and
   `deploy-production` succeeded. A passing validation gate alone does not mean
   the live release succeeded.
2. Review `production-deployment-evidence`: the artifact SHA/revision must match
   the run, and the hosted report must pass. Check the before/after versions of
   both Workers; their updates are sequential, not atomic.
3. Visit the homepage, Start here sequence, a nested reference, search result,
   inline example, explicit download and missing route. Confirm `www` and HTTP
   reach the HTTPS apex without losing a nested path or query.
4. Record concrete failures and their scope. Keep failed-attempt evidence before
   retrying. If only publication failed and the qualified revision is still
   current main, retry the failed job after fixing the actual external cause;
   do not rebuild or relabel an old preview as production.

Cloudflare deployment events and the redirect Worker's sampled logs/traces are
available to the owner in Workers & Pages. The static apex does not execute
Worker code, so absence of invocation errors is not proof of healthy static
assets. Use hosted byte/route checks and browser observations for that coverage.
Sampling can miss errors; this launch-session review is not an uptime history
or a field Core Web Vitals report.

## Review calendar

These are maintainer-owned review dates, **not installed calendar reminders or
completed observations**. Record actual completion, findings and next due date
here or in a linked repository issue when performed. No notifications have
been sent to anyone.

| Review | First due / cadence | Owner | Evidence to record |
| --- | --- | --- | --- |
| Initial operational follow-up | 2026-09-15, then after a failed deployment or reported outage | Dakota | Deployment status, representative routes/downloads, redirect errors, certificate validity and any account notices |
| Content and example freshness | 2026-10-08, monthly and whenever related runtime contracts change | Dakota / author of the runtime change | Changed source contracts, stale verification labels, offline semantic results, corrected tutorial prerequisites |
| External links and first-agent path | 2026-10-08, monthly | Dakota | Public provider/repository/install links, redirects, access changes and dated fixes; current build checks cover local links, not every external destination |
| Framework and dependency review | 2026-10-08, monthly; earlier for relevant security advisories | Dakota | Official release notes, compatibility/lockfile changes, full qualification and cold/warm results for cache changes |
| Usability, search and performance | 2026-10-08 or when useful reader feedback/traffic exists | Dakota | Real navigation difficulties, failed queries collected through approved means, device observations, lab budgets and field measurements only if available |
| Release archive retention | By 2026-11-08 for the launch baseline; repeat before each archive's 90-day expiry | Dakota | Private backup location recorded privately, checksum verification and both version IDs |
| Domain/account recovery | 2026-10-08, quarterly and before the actual GoDaddy renewal date | Dakota | Account access/contact/billing review; no secrets in repository evidence |

Manual accessibility review remains explicitly deferred from launch. Its later
timing is part of the usability review, using the existing
[worksheet](manual-accessibility-review.md), rather than a claimed launch audit.

## Prioritized backlog

Priorities describe follow-up order, not new launch blockers. Work requiring
provider calls, new analytics, paid infrastructure or external account changes
must retain the relevant user-authorized scope.

| Priority / ID | Work and rationale | Owner / acceptance |
| --- | --- | --- |
| P1 / CI-17 | Add normal Ochat and `@agent-e2e-pr` tests to the gate; website/docs tests do not establish full framework correctness. | Dakota; detailed task 17 in local `scratch/todo.md`, clean Linux qualification and failure enforcement |
| P1 / CI-19 | Reduce workflow time: independent jobs in parallel, reproducible dependency caching, then measured browser sharding. | Dakota; task 19, cold/warm timing evidence, unchanged test coverage and exact-artifact qualification |
| P1 / CI-18 | Select checks and deployments by real changed inputs; avoid unnecessary work while preserving required-gate behavior. | Dakota; task 18, conservative unknown-path handling and failed/skipped-job regression cases |
| P1 / WEBKIT-01 | Monitor the P12 mitigation for intermittent prefetch access-control reports during rapid WebKit navigation. P12 reproduced the error and disables optional Starlight/Astro prefetching; native navigation and search remain enabled. | Dakota; preserve the P11/P12 reports and the three-engine regression; require measured benefit and clean browser behavior before re-enabling prefetch |
| P1 / DEPS-01 | Rename the example prose `requirements.txt` under `docs-src/examples/applications/change-review/reference/` and update all canonical/catalog/capture references. GitHub's separate dependency graph workflow interprets it as pip requirements. | Dakota; dependency analysis succeeds and actual example/source parity remains checked; do not disable dependency scanning or overwrite the showcase |
| P2 / API-01 | Reopen hosted odoc only after repairing references, defining API scope, and checking mounted assets/search/licenses. | Dakota; complete P09.02–P09.06 and the P09 review's inclusion conditions; preserve prose-only builds |
| P2 / A11Y-01 | Perform the user-deferred screen-reader, native zoom, mobile keyboard and physical-device review. | Dakota / selected reviewer; dated worksheet, concrete fixes and retests, no blanket conformance claim |
| P2 / SEARCH-01 | Expand the 21-query benchmark using actual reader needs; revisit compatibility ranking and long-tail/exact-identifier searches. Source-reader text and deferred API pages are intentionally excluded. | Dakota; record failing examples before tuning and preserve worker failure/retry and current-before-compatibility checks |
| P2 / MEDIA-01 | Review existing workflow visuals and recorded/illustrative distinctions after feedback; consider a captioned short demonstration only if it improves comprehension. | Dakota; source provenance, captions/transcript/alternatives, self-hosting/license review and performance budgets |
| P2 / DOCS-01 | Review the ten deferred canonical pages listed below against current interfaces before publishing. | Relevant code author, Dakota accountable; corrected prose, compiled/offline examples, explicit manifest promotion |

Deferred-page inventory remains machine-owned by `config/docs-manifest.json`;
the build regenerates its reasons in `.generated/migration-report.md`:

- `chatml-host-session-controller-contract.md` and
  `chatml-safe-point-and-effective-history.md`: reconcile steering/history
  semantics and the finalized-output UTF-8 claim.
- `examples/prompt-patterns.md`: classify mixed current/deprecated examples and
  verify dependency closures.
- `guide/general-agent-workflow.md`: supply missing companion files and review
  external-account prerequisites and capability scope.
- `lib/chat_tui/highlight_grammars.doc.md`, `highlight_registry.doc.md`,
  `highlight_theme.doc.md` and `highlight_tm_engine.doc.md`: reconcile grammar
  loading/theme interfaces, obsolete `default_dark` examples and malformed prose.
- `lib/chat_tui/model.doc.md`: rewrite stale model/history/constructor examples
  against the identity-bearing current model.
- `lib/meta_prompting.doc.md`: reconcile old API/CLI examples with current
  strategy context.

Paths in that list are relative to `docs-src/`; grouped TUI filenames share
`docs-src/lib/chat_tui/`. The 175 repository-only pages are not an implied
commitment to publish every file. Promote material when it helps a defined
reader task and passes review.
