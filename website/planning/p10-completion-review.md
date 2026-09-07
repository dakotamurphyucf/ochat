# P10 release-candidate review

Status: local implementation and artifact qualification complete; P10 remains
in progress pending remote-enforcement release checks; hosted rehearsal and rollback now pass; manual accessibility review is deferred by the user. Milestone C is not
signed off. P11 has not started.

## Task disposition

| Task | Result |
|---|---|
| P10.01 content validation | Complete for the local candidate: 308 canonical documents accounted for, 123 rendered documentation routes, internal links/fragments, exact example downloads, source-link structure, and final asset ownership pass. Public source-commit availability remains a release blocker. |
| P10.02 production browser matrix | Complete: 208 passes and two existing non-Chromium clipboard skips across Chromium, Firefox and WebKit, in 3.8 minutes with two workers. Tests use the actual production fixture served by pinned local Wrangler/workerd. |
| P10.03 manual accessibility | Deferred by the user on 2026-09-07; not required for this launch. Actual manual checks were not performed. Automated accessibility tests remain in CI; the worksheet is retained for later review. |
| P10.04 visual/performance review | Complete locally: 30 representative page/state/theme/viewport combinations pass automated WCAG checks and overflow checks; screenshots inspected. Seven-route throttled performance gate passes. |
| P10.05 Ochat semantics | Complete: actual offline `dune build --force @agent-docs-check` passes 308 pages and 38 methods. No new live provider calls. Recorded tutorial/example source bytes remain unchanged; fixture revision mismatches correctly keep conservative verification labels. |
| P10.06 deployment configuration | Complete locally: static asset root, trailing slashes, real 404s, headers, cache behavior, noindex, download MIME/bytes and all-input CI ownership are implemented and tested. GitHub is the intended single publisher; no deploy job or alternate trigger has been enabled. Actual remote enforcement remains P10.11. |
| P10.07 hosted rehearsal | Complete: authorized workers.dev preview; TLS, exact served bytes, routing, indexing and cache behavior pass 3,579 HTTP assertions per stage. Candidate and restored candidate each pass 18 hosted browser checks. Actual version rollback/restore also pass. See [hosted evidence](p10-hosted-rehearsal.md). |
| P10.08 artifact rollback | Complete for the task's artifact option: retained candidate → prior known-good artifact → candidate, each integrity-checked and served through a fresh local Workers runtime. Actual hosted version rollback and restore subsequently passed on the authorized preview; see the hosted rehearsal record. |
| P10.09 evidence bundle | Complete as a qualification record, including unresolved blockers. This does not signify Milestone C acceptance. |
| P10.10 artifact capacity | Complete: production 577 files, largest asset 930,032 bytes, two header rules, maximum header line 52 characters, zero redirect rules. All checked Free-plan limits pass. |
| P10.11 actual release enforcement | Open. Local CI now gates website jobs on semantics, tests preview and production separately, watches all input changes, and fails the final gate on failure/cancellation/skipping. The workflow is not committed/run remotely; GitHub reports no main-branch protection or rulesets. Public promotion remains disabled. |

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
steps. A clean GitHub runner still needs to execute successfully; the local opam
switch contains pins and does not establish clean Ubuntu dependency installation.

The [release runbook](release-runbook.md) gives build, artifact retention, local
rehearsal, hosted checks, promotion records and rollback instructions. The
[manual accessibility worksheet](manual-accessibility-review.md) retains the
manual checks without recording simulated checks as human observations.

## Candidate identity and evidence

Main checkout HEAD remains `bc76b6c72a280b4bad48a793a63373e6db26d2b4`; the staged
diff remains empty. The initial local qualification performed no remote mutation.
The subsequent authorized preview deployment and rollback are recorded in
[the hosted rehearsal report](p10-hosted-rehearsal.md). No main-checkout commit,
push, domain purchase, production deployment, GitHub settings mutation or new
model recording was performed.

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

1. Commit/publish the approved source candidate, get the new CI workflow green
   on GitHub, configure and verify the required check or tested equivalent, and
   confirm that no independent hosting trigger bypasses it. Rebuild and verify
   against the eventual owned production origin before P11 promotion.

These are mandatory launch checks from Section 19.18 of the implementation spec.
P10.07 is complete with hosted evidence; keep P10.11 open until actual remote enforcement passes. P10.03 is explicitly deferred by the user, not completed or required for launch. The executable release approval gate now reflects this decision; automated accessibility checks remain enabled.
