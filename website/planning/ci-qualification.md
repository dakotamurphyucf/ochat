# CI qualification record

The framework, selection and caching changes were merged through protected
[PR #24](https://github.com/dakotamurphyucf/ochat/pull/24), after full Linux
qualification of source `0ec4b4997bdcaee7cd6aefa77e8b4a012e850b2d`.
The merge is `0595f08ced3467641d697d1b1d2a36ed3b201dcc`. It also includes the
previously qualified launch closeout from PR #23, which GitHub marked merged.

## Successful qualification and timing

| Run | Cache condition | Time to required gate | Summed validation runner time |
| --- | --- | --- | --- |
| [34186360736](https://github.com/dakotamurphyucf/ochat/actions/runs/34186360736) | Previous workflow baseline, without framework tiers | 1,217 s (20m 17s) | 1,856 s |
| [34191242088](https://github.com/dakotamurphyucf/ochat/actions/runs/34191242088) | Both framework dependency caches hit; semantics rebuilt for a different CPU | 748 s (12m 28s) | 2,269 s |

The passing PR gate was approximately 39% faster with the added framework
coverage. Summed validation runner time increased approximately 22%; it is a
measure of elapsed job time, not a billing estimate. The new run was a mixed-cache
run, not a claim of a fully warm workflow. GitHub queue and runner variation limit
what can be inferred from individual runs.

Both website environments passed 75 unit tests, Astro validation, build checks,
7 performance routes, 21 search benchmark queries, and 259 browser tests each.
Two existing non-Chromium clipboard cases remained intentionally skipped in each
environment. Chromium, Firefox and WebKit all participated. The production
artifact, browser/search/performance reports, and same-revision semantic evidence
were downloaded and reconciled; exact-artifact production verification passed.

| Framework command | Fresh dependency/object builds, run 34190368673 | Verified dependency reuse, run 34191242088 |
| --- | --- | --- |
| `dune runtest --force` | 191.459 s, pass | 119.283 s, pass |
| `dune build --force @agent-e2e-pr` | 68.642 s, pass | 15.353 s, pass |

These command timings exclude toolchain setup. The earlier run's framework tiers
passed, but its semantic link check failed, so it did not qualify a release.
Forced aliases still executed the selected tests on cache hits. Dune caches only
compiled objects; `_build` and prior test outcomes are not restored.

The runner fingerprints actually included `icelake-server`, `znver3`, and
`graniterapids` CPU targets. The final PR's semantic job correctly missed the
cache for the third target, rebuilt locked dependencies, and passed. Both
framework tiers restored the compatible `znver3` cache and passed. This validates
both reuse and CPU-based invalidation on real hosted runners.

## Failure enforcement and fixes

The initial Linux expansion run
[34188359989](https://github.com/dakotamurphyucf/ochat/actions/runs/34188359989)
failed framework tests, failed the required gate, and skipped publication while
website and semantic jobs passed. No protection bypass was used.

Qualification exposed and addressed:

- Nottui 0.5 introducing a second incompatible Notty implementation; constrain
  Nottui below 0.5 and qualify the matching Linux lock.
- BSD/GNU `wc` whitespace differences; assert the parsed byte count and status.
- Linux resetting an oversized Unix socket request; accept only EOF or the typed
  connection reset, retain response rejection and subsequent daemon-health checks.
- `/bin` executable symlinks on Linux; use canonical executable paths in fixture
  allow rules while preserving default denial and the intended commands.
- Git background maintenance removing a lock while opam copies repository data;
  disable automatic maintenance on the disposable CI runner.
- Illegal-instruction failures after native dependency reuse; include the native
  CPU target in both dependency and Dune cache keys.
- A maintainer guide link entering Dune's documentation build tree; link from
  `DEVELOPMENT.md` to the GitHub guide, preserving the intentional exclusion of
  the website's separate toolchain from Dune.

Superseded PR run 34190128251 was cancelled by the new concurrency policy. Its
already completed failure evidence remains useful; its timing is not treated as
a passing speed measurement.

## Main deployment and selection verification

Main run [34192166776](https://github.com/dakotamurphyucf/ochat/actions/runs/34192166776)
selected framework, semantics, website and deployment for the actual merge.
Its deployment lookup found successful production revision
`077b00f905cb4ac61608294b35acd89a7a42a679`, with no fallback, and included the
unshipped input diff. Full main qualification and publication are in progress.
The final maintainer-only PR/main selection probe follows successful publication.

Manual `validate`, `cold`, and main-only `redeploy` modes and the weekly cold audit
are configured. Policy tests cover their selection; the actual publisher entry
point rejects disallowed events, refs, repositories and modes before touching
credentials or artifacts. This is not a claim that a future scheduled audit or a
manual recovery has already executed.

## Browser sharding decision

Retain the two-worker suites in both environments for this change. Concurrency,
qualified dependency reuse, and Dune object reuse have already reduced the gate
by approximately eight minutes while adding framework coverage. Browser suites
remain the longest warm-workflow component (about 9.3 minutes each). Sharding is
a possible separate optimization, with additional runners and artifact/evidence
coordination; it has not shipped here. Any future implementation must test the
same artifact, retain all three engines and both environments, and require every
shard result before publication.

See [CI coverage and maintenance](ci-enforcement.md) for the selection policy,
lock refresh, cache recovery, manual modes and retained evidence. Detailed local
reports and resumable implementation notes remain under `scratch/` and are not
published website assets or a shared backup.
