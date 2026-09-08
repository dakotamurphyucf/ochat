# Required CI coverage, selection and maintenance

The required `release-gate` coordinates framework tests, documentation semantics,
and both website environments. The Website workflow is the sole production
publisher. Strict branch protection, administrator enforcement, and the exact
`main` restriction on the production environment remain in place.

## Coverage

| Job | Required behavior when selected |
| --- | --- |
| `changes` | Run policy tests, inspect the complete Git diff, record selection and deployment baseline. This job always runs. |
| `framework (normal)` | Run `opam exec -- dune runtest --force`. |
| `framework (e2e)` | Run `opam exec -- dune build --force @agent-e2e-pr`: daemon smoke, Unix/stdio/HTTP transports, transport conformance, workspaces and multiple clients. |
| `semantics` | Run the offline documentation checks and retain evidence for this exact revision. |
| `website (preview)` and `website (production)` | Unit/Astro checks, build, performance, search, Chromium/Firefox/WebKit tests; production packaging and retained artifact checks. |
| `release-gate` | Always run; require successful detection and every selected job. Accept a skipped job only when detection explicitly marked it unnecessary. |
| `deploy-production` | Publish a same-run qualified artifact on main when selected, then verify the hosted site. |

Framework commands run through `.github/scripts/framework-tests.mjs`, which
records the command, revision, elapsed time, exit status, and full output. Commands
have a 25-minute bound within a 45-minute job. Failures, timeouts, cancellations,
missing selections, and unexpected skips cannot satisfy the gate. Both framework
tiers run independently; a failure does not cancel evidence from the other tier.

Load, soak, live-provider, and manual-terminal tiers remain separate. These jobs
receive no model credentials and make no paid model requests. Website jobs start
after detection, concurrently with the OCaml jobs.

## Change selection and publication

There are no workflow-level path filters. The required check always gets a
result, including for changes that do not need expensive tests.

| Input | Selection |
| --- | --- |
| Immediate `website/planning/*.md`, `website/README.md`, `website/CONTRIBUTING.md` | Maintainer documentation only; expensive jobs may skip. These files are not published website inputs. |
| Other `website/` files | Both website environments and same-revision semantic evidence. |
| Runtime, OCaml tests/dependencies, published docs/examples, assets, CI configuration, root files, or other unrecognized repository paths | All validation jobs. |
| Missing/unreadable Git history, unavailable deployment history, invalid baseline, or unknown event | Conservative full validation. |

PRs compare the current source revision with the merge base of the PR's base
revision. Main pushes inspect the entire push range. Git diffs preserve NUL
separators and disable rename collapsing, so both old and new paths of a move
participate in selection. Deletions are included.

Main also compares against the last successful production deployment recorded
by GitHub. A failed newer attempt is not a deployment baseline. An older success
can remain the baseline even when GitHub marks it inactive. Unshipped website
inputs force new same-revision qualification and publication, including when the
latest push changes only maintainer documentation. A missing baseline triggers
full qualification instead of permission to skip.

A passing unrelated main run does not publish. The site can therefore correctly
retain an earlier source revision: its source links refer to the revision of the
last shipped artifact. Do not compare that artifact against an unrelated newer
main commit and call it stale solely because the commit IDs differ.

Production remains serialized. A queued publisher checks that its revision is
still current main before uploading. It downloads and verifies its own run's
artifact and semantic evidence; it does not rebuild or borrow another run's
qualification. The Cloudflare token is available only to the publication step.

## Recovery and clean validation

The workflow has three manual modes:

- `validate`: run full qualification without publishing.
- `cold`: run full qualification without the project dependency or Dune caches;
  do not publish. The compiler-only bootstrap cache may be reused.
- `redeploy`: run full qualification and publish its own artifact, **only on
  main**. Use this for a deliberate recovery after a superseded or failed release.

For example, `gh workflow run website.yml --ref main -f mode=redeploy` requests a
new qualified release. Follow the resulting run through hosted verification.
The existing failed-job-only retry remains appropriate when the failed publisher
still targets current main and its qualified artifacts are available. Never use
an older artifact to bypass current-revision checks.

A weekly Monday 08:17 UTC schedule runs a cold audit with no publication.
Superseded PR validations can be cancelled; main publications are not cancelled
by that policy. These are actual workflow settings, not a claim that future
scheduled reviews have already happened.

## Reproducible dependencies and cache maintenance

`ochat.opam.locked` is the Linux CI dependency lock, not a promise of a portable
macOS lock. `.github/ci-toolchain.json` records exact installed versions, public
source pins, an immutable opam repository revision, the compiler/opam versions,
and the hash of the base package definition. The local setup action is pinned
to its reviewed upstream commit.

The dependency cache contains `~/.opam` and the workspace `_opam` switch. Its
exact key includes the compiler, opam, runner architecture/image version, GCC,
GCC's native CPU target and instruction flags,
lockfile, base dependency definition, Dune project, setup action and verification
script. It has no broad fallback restore key. A base dependency edit without a
corresponding lock refresh fails with a direct instruction to regenerate it.

Every restore installs required system packages, runs locked opam reconciliation,
and verifies all recorded package versions and source pins. A cache hit is not
test evidence. Missing caches rebuild normally. The compiler-only upstream cache
can help bootstrap a miss or a cold audit without reusing project dependencies.

Dune's content-addressed compilation cache is separate and uses copy storage.
It is scoped to the same toolchain and test tier. `_build` is never restored;
all selected test aliases use `--force`. A cold audit disables this cache.
GitHub's cache scope restrictions remain in effect; do not move cache saving
into a privileged `pull_request_target` workflow.

The disposable runner disables automatic Git maintenance before opam setup.
This avoids a repository-copy race with a disappearing `maintenance.lock` during
fetch/refresh; dependency installation and version verification still run.
CPU target changes also invalidate both compiled caches: hosted x64 runners do
not all expose the same instruction sets. Initial warm-cache qualification
exposed illegal-instruction failures before scenario execution, so architecture
alone is insufficient for these native dependencies.

For a dependency upgrade, update the base definition and source/repository pins
deliberately, generate a Linux lock from the intended installed dependency set,
and update the configuration's versions and base-file hash. Qualify cold and
warm runs before merging. Nottui is constrained below 0.5 because the project's
Notty integration conflicts with the Notty Community dependency introduced by
Nottui 0.5. Do not silently remove that constraint without qualifying the terminal
integration. Oniguruma 0.1.2 resolves through the pinned repository checksum;
Piaf and the TextMate fork use immutable source commits.

For corruption, inspect `gh cache list` and delete the affected exact cache IDs,
then rerun qualification. A reviewed change to the cache schema prefix also
invalidates that cache. Periodic cold checks establish reproducibility; dependency
upgrades and repository snapshot refreshes remain explicit maintenance work.

## Evidence and measurements

The initial Linux expansion run
[34188359989](https://github.com/dakotamurphyucf/ochat/actions/runs/34188359989)
passed website and semantic checks but failed both framework tiers. The required
gate failed and publication skipped. This exposed the Nottui dependency conflict,
platform-specific `wc` padding in a test, and Linux connection-reset behavior when
rejecting oversized socket input. The fixes retain the byte-count, connection
rejection and daemon-health assertions.

The next Linux run also exposed hard-coded executable paths in the workspace
fixture: merged-`/usr` systems resolve `/bin/pwd`, `/bin/cat`, and `/bin/cp` under
`/usr/bin`. Those allow rules now use each executable's canonical path, matching
the runtime policy evaluator while preserving default denial and the original
commands. The normal framework tier passed that run.

The previous successful main baseline
[34186360736](https://github.com/dakotamurphyucf/ochat/actions/runs/34186360736)
took 1,217 seconds to its release gate and approximately 1,856 summed validation
runner-seconds, without the new framework coverage. The first parallel run's
failure is retained as enforcement evidence, not as a successful speed result.
Cold/warm measurements and final main qualification are being collected for the
new workflow. Browser sharding is evaluated only after those results; all three
engines and both environments remain required.

CI retains selection reasons/baselines, framework logs and E2E artifacts, installed
versions, semantic reports, website reports, the tested production artifact, and
publication/hosted evidence. Local investigation and per-job/per-step timing
reports are under `scratch/ochat-ci-evidence/`; resumable notes are in
`scratch/ochat-ci-implementation-notes.md`. Scratch is local evidence storage,
not a shared backup.

Implementation references: [GitHub cache semantics](https://docs.github.com/en/actions/reference/workflows-and-actions/dependency-caching),
[setup-ocaml dependency cache guidance](https://github.com/ocaml/setup-ocaml#caching),
[opam lock behavior](https://opam.ocaml.org/doc/man/opam-lock.html), and
[Dune caches](https://dune.readthedocs.io/en/latest/reference/caches.html).
