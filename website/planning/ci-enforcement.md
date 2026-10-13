# Required CI coverage, selection and maintenance

The required `release-gate` coordinates framework tests, documentation semantics,
and both website environments. The Website workflow is the sole production
publisher. Strict branch protection, administrator enforcement, and the exact
`main` restriction on the production environment remain in place.

## Draft pull requests

Draft PR events skip all runner jobs, including change detection and the release
gate. Local validation continues during development. Marking a PR ready for
review starts normal qualification even without a new commit; converting it back
to draft cancels an older PR run through the existing concurrency group. A skipped
draft run is not qualification evidence. Review and passing CI for the final
revision are still required before merging.

The workflow listens for opened, synchronized, reopened, ready-for-review and
converted-to-draft PR events. Main pushes, manual validation and scheduled audits
retain their existing checks. An explicit manual run can validate a draft branch
when needed. Existing branches must contain this workflow change to adopt it.

## Coverage

| Job | Required behavior when selected |
| --- | --- |
| `changes` | Run policy tests, inspect the complete Git diff, record selection and deployment baseline. Runs except on draft PR events. |
| `framework-plan` | Discover every normal Dune alias owner and assign its nonrecursive alias to exactly one of eight groups. Bind the plan to tracked source and workflow run. |
| `framework (normal, shard, x64)` | Run the assigned `@@directory/runtest` aliases with `--force -j 2`; all eight groups are required. Together these select the normal suite. |
| `framework (e2e, x64)` | Run `opam exec -- dune build --force -j 2 @agent-e2e-pr`: daemon smoke, Unix/stdio/HTTP transports, transport conformance, workspaces and multiple clients. |
| `framework (git-native, arm64)` | Run the ChatML Git host, bounded-process and private-storage test aliases with `-j 2` on the native ARM64 runner. |
| `semantics` | Run the offline documentation checks and bundled authoring-source tests; retain evidence for this exact revision. |
| `website (preview)` and `website (production)` | Unit/Astro checks, one build per environment, performance, search, full browser-test discovery and retained build; production packaging checks. |
| `browser (environment, shard)` | Two shards per environment, two workers each, zero retries; Chromium/Firefox/WebKit tests against that environment's downloaded build. |
| `website-qualification (environment)` | Merge both shard reports, verify complete disjoint test coverage and exact artifact/revision/run identity; retain the qualified production artifact. |
| `release-gate` | Run except on draft PR events; require successful detection and every selected job. Accept a skipped job only when detection explicitly marked it unnecessary. |
| `deploy-production` | Publish a same-run qualified artifact on main when selected, then verify the hosted site. |

Framework commands run through `.github/scripts/framework-tests.mjs`, which
records the command, revision, elapsed time, exit status, and full output. Commands
have tier-specific execution bounds: 65 minutes for normal, 25 for E2E and 45
for Git-native. Setup and evidence collection have additional job headroom:
90 minutes for normal/Git-native and 60 for E2E. These are upper bounds, not
expected durations. Individual test deadlines remain in force. Failures,
timeouts, cancellations, missing selections and unexpected skips cannot satisfy
the gate. The framework jobs run independently; a failure does not cancel
evidence from the other tiers.

### Normal test groups

The normal suite runs on eight standard Linux x64 runners. Each uses two Dune
workers. E2E, native ARM64, semantic and website checks retain their existing
coverage. At peak, this workflow can use roughly fifteen concurrent runners;
other repositories and PRs share the account's concurrency allowance.

The planner audits tracked Dune definitions, including `bin/`, `lib/` and legacy
`test/dune`. It discovers directories with inline tests, test/cram stanzas or an
explicit `runtest` alias. Each direct alias is assigned once using measured test
CPU costs from `.github/framework-test-weights.json`; new owners are included
automatically with a default scheduling weight. Weights affect balance, never
coverage. A directory's tests remain together, so the largest directory still
limits the speedup. This is not a claim that underlying shared build actions run
exactly once across machines.

Unsupported source traversal and normal aliases that recursively include or
cross into another directory fail planning. This prevents silently omitted or
overlapping directory selections. Dune still controls execution, dependencies,
sandboxing, test partitions and expected-output comparisons. Ordinary local
`dune runtest` is unchanged; complete workflow qualification stays explicit.

Each normal job verifies the downloaded plan against its checkout and reports
the exact aliases it executed. After all jobs pass, `release-gate` checks eight
distinct successful reports against the same revision, source digest, plan and
workflow run. The gate downloads only small summary artifacts; full diagnostic
logs and traces remain available separately. Missing, duplicate, stale or
partial evidence fails the gate.
Successful reports from earlier attempts of the same run remain usable when
rerunning failed jobs. Only the shard owning `test/chatml_git_host` builds the
pinned Git fixture; the separate ARM64 native job still builds and tests it.

Load, soak, live-provider, and manual-terminal tiers remain separate. These jobs
receive no model credentials and make no paid model requests. Website jobs start
after detection, concurrently with the OCaml jobs.

Full helper/native watcher, managed-session and authoring workflow matrices are
also separate: `dune build --force -j 2 @agent-qualification` runs them explicitly
for local or release qualification. The component aliases are
`@agent-helper-qualification`, `@agent-managed-qualification`, and
`@agent-authoring-qualification`. They are not dependencies of `runtest` or
`@agent-e2e-pr`, and this workflow does not schedule them. Required checks retain
focused real-runtime and transport regressions. Record broad qualification
separately; a successful release gate proves only the selected CI checks.

## Browser sharding and artifacts

Each environment builds once and uploads `website-build-preview` or
`website-build-production`. Each contains the retained manifest, exact `dist/`
bytes, generated fixture reports and an unsharded Playwright discovery plan.
The four browser jobs download their environment's build, verify its revision,
lockfile and content hash, restore its bytes and fixtures without rebuilding,
and verify the served copy again after execution. Existing platform-specific
clipboard skips remain explicit; retries remain disabled.

Every shard uploads a machine-readable report, a Playwright blob report and
failure traces as `website-browser-ENVIRONMENT-INDEX`. The environment's
qualification job merges the blob reports into HTML/JSON and requires both
shard indices, all three engines, every planned test exactly once, successful
execution, and matching revision, artifact hash and workflow run. Failed or
cancelled browser jobs still block `release-gate`, even if another environment
qualifies. Missing evidence fails qualification. Merged diagnostic reports are
retained when available; setup failures may have no Playwright report.

Only successful qualification can upload `website-production-release`.
Production verification rechecks the full shard evidence, not just a summarized
pass flag. All downloads are from the current run. Failed-job reruns can reuse
successful shards from an earlier attempt of that same run when the artifact
hash and revision match; rerun uploads replace their own named evidence.
A previous run's browser summary cannot qualify a new release.

All named evidence uploads use `overwrite: true`, including change selection,
semantic reports, framework tiers and production-deployment evidence. A job rerun
replaces its own artifact under the existing name, so a successful retry does
not fail solely because that name was uploaded by the earlier attempt. Upload
errors still fail jobs; replacement does not waive any test or release check.

Replacement deletes the previous artifact and creates a new artifact ID. Download
an earlier attempt's evidence before rerunning if you need to preserve it; the
named artifact represents the latest upload, not a history of every attempt.
Other jobs' artifacts are not replaced by that job. Consumers continue to use
same-run names and verify the required revision and content hashes. A retried
publisher must still target current main and pass exact-release verification.
See the [artifact upload contract](https://github.com/actions/upload-artifact/blob/v4/README.md#inputs).

The additional runners and artifact transfer have overhead. Compare time to the
required gate, individual browser durations, total validation runner time and
failures in the [qualification record](ci-qualification.md) before expanding
beyond two shards. The two-worker limit avoids overloading each runner.

## Change selection and publication

There are no workflow-level path filters. For ready PRs and non-PR events, the
required gate always runs, including for changes that do not need expensive tests.

| Input | Selection |
| --- | --- |
| Immediate `website/planning/*.md`, `website/README.md`, `website/CONTRIBUTING.md` | Maintainer documentation only; expensive jobs may skip. These files are not published website inputs. |
| Other `website/` files | Both website environments and same-revision semantic evidence. |
| Existing `docs-src/` Markdown outside `examples/`, with a verified prose-only diff | Both website environments, browser checks, and same-revision semantic/authoring evidence. |
| The six reviewed authoring coverage owners, changing only documentation excerpt/closure hashes | Same documentation qualification; compiler/body/descriptor pins and source changes still require full validation. |
| The exact CI selection scripts and their tests | Mandatory policy tests plus documentation qualification. Workflow, setup, toolchain and framework runners are not exempt. |
| Runtime, OCaml tests/dependencies, executable docs/examples, assets, other CI configuration, root files, or other unrecognized repository paths | All validation jobs. |
| Missing/unreadable Git history, unavailable deployment history, invalid baseline, or unknown event | Conservative full validation. |

The documentation fast path checks both Git blobs, not just filename extensions.
Only existing regular non-executable text files qualify. Executable examples,
protected Markdown content, code changes, additions/deletions, mode changes and
unreadable or uncertain inputs retain full validation. Documentation hash updates
must leave all other source bytes unchanged and still pass the authoring corpus's
current-excerpt, closure and compiler-contract checks. Updating hashes is not a
substitute for reviewing the prose against the implementation.

Selection-policy changes are validated by the mandatory Node policy tests and
review. The exact allowlist is in `ci-policy.mjs`; it does not include workflow,
release-gate, dependency setup or framework execution changes. Required job status
checks, exact-revision artifacts, browser coverage and hosted verification remain
unchanged. Scheduled/manual audits still request full validation.

A documentation proof applies only to its own pair of revisions. The immediate
push and the last successful production deployment are checked independently;
an unshipped runtime change still selects all jobs, even if the latest push is
only prose. Selection evidence lists the paths proven documentation-only for each
baseline. This reduces redundant framework work, not the need to check that the
documentation is correct. It does not promise a fixed CI duration: dependency
setup, semantic examples and browser checks still take time.

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
Restoration prefers the same shard, then the same tier, then compatible objects
from another tier. Every fallback retains the complete dependency key, including
native CPU compatibility. Each shard saves under its own revision key to avoid
concurrent writers competing for one immutable cache entry. `_build` is never restored;
all selected test aliases use `--force`. A cold audit disables this cache.
GitHub's cache scope restrictions remain in effect; do not move cache saving
into a privileged `pull_request_target` workflow.

Toolchain evidence records separate dependency-contract and CPU-target digests
to help explain misses. PR-created caches cannot seed unrelated PRs or `main`;
successful default-branch runs establish the broadly reusable cache scope.
Missing or incompatible caches rebuild normally. This design does not transfer
a complete build directory or assume that one runner's native binaries can run
on every other runner. The first run after changing the setup contract is cold.

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
Cold/warm measurements, final main qualification, and the browser-sharding
decision are recorded in the [qualification record](ci-qualification.md).
All three engines and both environments remain required.

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

### Documentation fast-path qualification

The first documentation fast-path PR run
[38313856914](https://github.com/dakotamurphyucf/ochat/actions/runs/38313856914)
qualified revision `5b5c1194509d123757b93bd5e01843b83c4b6286` in 13 minutes
32 seconds from workflow creation to the required release gate. The framework
matrix was explicitly skipped; semantic/authoring checks, both website builds,
all four browser shards and both artifact-qualification jobs passed. Selection
evidence matched the locally reviewed diff: two prose files and six documentation
fingerprint owners qualified for the fast path. This is one observed PR run, not
a runtime-suite speedup or a guaranteed future duration. Main publication and
hosted verification are separate checks.
