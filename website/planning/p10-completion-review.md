# P10 release-candidate review

Status: P10 and Milestone C complete within the approved launch scope. Local
qualification, hosted rehearsal/rollback, and actual GitHub release enforcement
pass. Manual accessibility review is deferred by the user. P11 has not started.

## Task disposition

| Task | Result |
|---|---|
| P10.01 content validation | Complete for the local candidate: 308 canonical documents accounted for, 123 rendered documentation routes, internal links/fragments, exact example downloads, source-link structure, and final asset ownership pass. Sources are now publicly committed on PR #20; anonymous new-tutorial source bytes were verified. |
| P10.02 production browser matrix | Complete: current clean GitHub production matrix passes 214 cases with two existing clipboard skips across Chromium, Firefox and WebKit. The earlier local Wrangler/workerd production fixture passed 208 cases before the added font regression and separate theme tests. |
| P10.03 manual accessibility | Deferred by the user on 2026-09-07; not required for this launch. Actual manual checks were not performed. Automated accessibility tests remain in CI; the worksheet is retained for later review. |
| P10.04 visual/performance review | Complete: 30 local representative page/state/theme/viewport combinations passed automated WCAG and overflow checks; screenshots inspected. The CI-discovered gallery font shift was corrected and reviewed separately. Both current GitHub environments pass the seven-route performance gate. |
| P10.05 Ochat semantics | Complete: actual offline `dune build --force @agent-docs-check` passes 308 pages and 38 methods. No new live provider calls. Recorded tutorial/example source bytes remain unchanged; fixture revision mismatches correctly keep conservative verification labels. |
| P10.06 deployment configuration | Complete locally: static asset root, trailing slashes, real 404s, headers, cache behavior, noindex, download MIME/bytes and all-input CI ownership are implemented and tested. GitHub is the intended single publisher; no deploy job or alternate trigger has been enabled. Actual remote enforcement passes under P10.11. |
| P10.07 hosted rehearsal | Complete: authorized workers.dev preview; TLS, exact served bytes, routing, indexing and cache behavior pass 3,579 HTTP assertions per stage. Candidate and restored candidate each pass 18 hosted browser checks. Actual version rollback/restore also pass. See [hosted evidence](p10-hosted-rehearsal.md). |
| P10.08 artifact rollback | Complete for the task's artifact option: retained candidate → prior known-good artifact → candidate, each integrity-checked and served through a fresh local Workers runtime. Actual hosted version rollback and restore subsequently passed on the authorized preview; see the hosted rehearsal record. |
| P10.09 evidence bundle | Complete: local, hosted, and remote-enforcement evidence; deferred manual review and P11 production boundaries are explicit. |
| P10.10 artifact capacity | Complete: production 577 files, largest asset 930,044 bytes, two header rules, maximum header line 52 characters, zero redirect rules. All checked Free-plan limits pass. |
| P10.11 actual release enforcement | Complete: [actual clean GitHub run 34173359224](https://github.com/dakotamurphyucf/ochat/actions/runs/34173359224) passes semantics, preview, production, and release-gate. Strict main protection applies to administrators; actual missing/failed-check pushes were rejected. Public sources are committed; legacy Pages branch publishing is disabled. See [the enforcement record](p10-github-enforcement.md). Production promotion remains P11 work. |

## Delivered behavior

Browser tests accept `PLAYWRIGHT_BASE_URL` and read the built artifact's origin
instead of assuming localhost metadata. This permits the same tests to qualify
the production build on a separate server while keeping the user's preview open.

The build now checks header/redirect capacities alongside file count and size.
Mutable HTML and search entry points revalidate; only hashed `_astro/` assets
receive immutable caching. The local Workers checks verify no conflicting cache
headers, conditional ETags, slash normalization, branded nested 404s, indexing
headers, search JavaScript MIME and every downloadable file's hash. No path-only
redirects are invented for historical fragment bridges.

Artifact retention refuses overwrites and records environment, origin, revision,
lockfile/configuration hashes and the complete output manifest. Verification
rejects modified, added and symlinked output. The release readiness checker also
rejects reserved fixture origins, stale artifact approvals and unfinished hosted,
rollback, enforcement and public-source attestations. It checks recorded
attestations; it is not independent proof that a human or hosted review occurred.

CI has an OCaml prerequisite and separate preview/production website jobs, with
no path filters. The final release gate was tested against all sixteen combinations
of successful, failed, skipped and cancelled prerequisite results. Only two
successful prerequisites pass. There are no deploy credentials or publication
steps. Clean GitHub execution now passes after explicit source/API compatibility pins
and the missing Menhir build dependency were declared. Installed packages and
pins are retained; see the enforcement record for failures and successful evidence.

The [release runbook](release-runbook.md) gives build, artifact retention, local
rehearsal, hosted checks, promotion records and rollback instructions. The
[manual accessibility worksheet](manual-accessibility-review.md) retains the
manual checks without recording simulated checks as human observations.

## Candidate identity and evidence

The initial local qualification used the uncommitted checkout based on
`bc76b6c72a280b4bad48a793a63373e6db26d2b4`. Its artifact identities below remain
historical local evidence. The subsequent authorized preview deployment and
rollback are recorded in [the hosted rehearsal report](p10-hosted-rehearsal.md).
P10.11 then committed/published the authored sources on PR #20, configured/tested
GitHub protection, and qualified clean Ubuntu builds. Their actual source and
artifact identities are in [the enforcement record](p10-github-enforcement.md).
No domain purchase, production deployment, or new model recording occurred.

| Artifact | Identity |
|---|---|
| Candidate preview | 125 HTML pages, 575 files; SHA-256 `50b1e86b5f82c6b214bed06d8c0b7ed26a070d97119e29e6e6f408504019f826`; origin `http://localhost:4321` |
| Candidate production fixture | 125 HTML pages, 577 files, 22,251,903 bytes; SHA-256 `137a4647ced38aaa2d7951de97dc19d26ee330ea31822b7a4b6bf586c3a685b1`; origin `https://release.ochat.test`; synthetic revision `6885e243e73bdf0203dbc7e08a74734c37026595` |
| Previous known-good preview | SHA-256 `f2c86e4f79f83308777d3ba5960e260db4b5e4b816f428d7b7a076998f93db56` |

The isolated candidate copies authored files and installed dependencies into a
temporary Git repository. It is not a clean dependency installation or a public
source commit. Its revision differs from the original verification records, so
those remain conservatively `not-checked` where required by existing policy;
historical observations and actual source hashes are preserved. Do not rewrite
verification hashes merely to hide stale labels. Reconcile release verification
against the real public candidate before launch.

All 123 canonical page source hashes remain identical to the completed application
UI iteration. Graphite + blue, the approved Start here sequence, inline ChatML
OCaml highlighting, application UI, and P09 API deferral are preserved.

Evidence root: `scratch/ochat-website-evidence/p10/`. Retained artifacts:
`scratch/p10-artifacts/`. Logs: `scratch/p10-*.log`.

- Main-checkout final checks: 68 unit tests pass; zero Astro diagnostics; formatting and Git whitespace pass.
- Frozen production fixture: 66 then-existing unit tests and build pass; the final two added tests exercise release gate and approval rejection without changing the website artifact.
- Semantic evidence identifies OCaml 5.3.0, Dune 3.21.1, revision and semantic-input hash.
- Full production browser log and build reports retain the same fixture artifact identity.
- Visual evidence includes homepage, first agent, ChatMD/ChatML references, library page, search, mobile menu and 404, in both themes. Desktop search, dark mobile menu and narrow ChatML screenshots were inspected; existing application visual evidence remains applicable to unchanged UI source.
- Search passes 21/21 queries across 114 indexed pages and 1,860 valid anchors. Automated keyboard traversal completes six route/width combinations (1,014 focus stops), with no obscured stops. These tests use the production fixture; they are not physical-device or screen-reader observations.
- Production and preview routing reports, three-transition rollback report, capacity reports, prerequisite/approval negative tests, access findings and pending approval template are retained.

## Remaining release gates

P11 must select the owned production origin, rebuild and verify its exact
artifact, and connect one protected publisher. GitHub source publication, clean
qualification, and required-check enforcement are now complete.

The remaining production work is described in Section 19.19 of the implementation spec.
P10.07 and P10.11 are complete with hosted and actual GitHub evidence. P10.03 is explicitly deferred by the user, not completed or required for launch. The executable release approval gate now reflects this decision; automated accessibility checks remain enabled.
