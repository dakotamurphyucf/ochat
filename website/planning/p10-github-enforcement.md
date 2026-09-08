# P10.11 GitHub release enforcement

Status: P10.11 qualification complete. Actual clean GitHub checks pass, main
protection is configured, and missing/failed-check probes were rejected. PR #20
lands through the normal protected merge path after its final checks pass;
production publishing remains P11 work.

## Protected contribution path

Repository: `dakotamurphyucf/ochat`. All authored website work and its npm lockfile
are publicly committed on [PR #20](https://github.com/dakotamurphyucf/ochat/pull/20),
starting at `a40dbd208e85d90873f4b960727a79448c9c5a78`. An anonymous raw-source
request for a new tutorial returned bytes identical to the committed source.

Actual `main` branch protection, read back through GitHub's API:

- Require `release-gate`, issued by the GitHub Actions app (ID 15368).
- Require the pull request branch to be up to date before merging.
- Require a pull request and resolution of review conversations.
- Apply enforcement to administrators; disallow force pushes and branch deletion.
- Require zero outside approving reviews, matching this sole-maintainer repository.

The workflow runs for every pull request and every push to main, without path
filters. Each job checks out the exact public source SHA with full history and
without persisting credentials. The real OCaml documentation semantic gate
precedes separate preview and production fixture jobs. Each website job performs
content/type/unit checks, builds, performance and search checks, and the complete
Chromium/Firefox/WebKit matrix with two workers. A final `if: always()` job requires
both prerequisite results to be exactly `success`. Failure, skipping, and
cancellation cannot produce a successful gate. Four policy unit tests include
all sixteen combinations of prerequisite outcomes.

## Observed enforcement and clean-run fixes

An owned temporary branch, `website/enforcement-probe-p10`, received the same
protection as main. Two actual administrator pushes were rejected with GH006:
first because `release-gate` was expected but missing, then because it was
failing. Both responses also required changes through a pull request. The probe
branch and its protection were removed after testing; main was never a test target.

The superseded [initial run](https://github.com/dakotamurphyucf/ochat/actions/runs/34169050677)
was cancelled after dependency-source drift was identified. Its semantic and
website jobs were cancelled, and its actual final gate failed. The subsequent
[pinned-source run](https://github.com/dakotamurphyucf/ochat/actions/runs/34169349296)
failed during dependency compilation; website jobs were skipped and the final
gate failed. PR #20 remained blocked. No required check was bypassed.

Clean Ubuntu qualification exposed missing build prerequisites:

1. `opam install` does not consume `dune-project` source pins. CI now explicitly
   pins TextMate to `e9d1ea854b0734db5e616cacb2ce7e2e5fd744bf` and Piaf to
   `c7428ec14dc681e0ef13375cdf657b2727143678`, matching the qualified sources.
2. That TextMate fork assumes `Oniguruma.Syntax.default` is a value. Oniguruma
   0.2 changes it to a function; the fork's metadata does not exclude 0.2.
   CI pins the locally qualified 0.1.2 version and retains the real semantic gate.

3. The project declares `menhirLib` but also invokes the separate `menhir` parser
   generator. The next [semantic run](https://github.com/dakotamurphyucf/ochat/actions/runs/34169941010)
   installed dependencies successfully but failed because that executable was
   absent. `dune-project` now declares `menhir`, and Dune regenerated `ochat.opam`.
   This makes the generator a declared prerequisite for both CI and ordinary users.

4. A [clean semantic build](https://github.com/dakotamurphyucf/ochat/actions/runs/34170447493)
   then exposed undeclared documentation-link inputs. The docs gate referenced
   test source files that happened to exist in the developer build tree. Its Dune
   rule now includes the test source tree, so a clean build can inspect those
   links without relying on prior unrelated builds.

CI uses published OCaml 5.3.0, without the developer's macOS-only `core_unix` pin.
Installed versions and pin evidence are uploaded with the semantic report.
These explicit compatibility constraints are not a complete OCaml dependency lock.

## Gallery performance correction

[Run 34170942123](https://github.com/dakotamurphyucf/ochat/actions/runs/34170942123)
passed the real semantic gate and both website builds. Production passed all
performance/search checks and 208 browser cases (two existing clipboard skips).
Preview failed the unchanged 0.1 CLS budget on the application gallery (0.1972),
and the final release gate correctly failed despite production succeeding.

Layout-shift attribution showed that the fallback-to-Manrope font swap changes
filter widths and can move Research to another row. The filters now use stable,
responsive grid columns, including before JavaScript enhancement. A new delayed-font
browser regression fails against the original flex layout and passes with the
fix in Chromium, Firefox, and WebKit. All 21 scoped application cases and the
seven-route local performance gate pass; the corrected local gallery sample is
0.00125. Linux container measurements and GitHub retesting provide separate
platform evidence; local measurements alone do not close the original CI failure.

The performance report now retains bounded layout-shift attribution (affected
text/elements and before/after rectangles). The original threshold and cumulative
measurement remain unchanged. Diagnostic instrumentation follows Chrome's
[Layout Instability API guidance](https://web.dev/articles/debug-layout-shifts).
The live workers.dev preview still refers to its earlier hosted artifact; P11
must qualify the final production bytes at the owned origin.

## Full accessibility scan scheduling

[Run 34171939816](https://github.com/dakotamurphyucf/ochat/actions/runs/34171939816)
confirmed the gallery fix on GitHub: preview CLS 0.0222, all performance/search
checks passed, and the complete preview browser matrix passed. Production passed
210 cases and retained two existing skips, but Firefox exhausted one shared
90-second test budget while scanning the dense protocol reference in both themes.
The trace records about 45 seconds for the first full axe scan and 34 seconds
for the second partial phase before the remaining scan/overhead exhausted the test.

Light and dark now have separate test cases, each retaining the existing slow-test
budget, full-document axe scan, WCAG rule tags, narrow reflow, focus, and keyboard
scrolling assertions. No accessibility rule or source content is excluded. This
separates independent checks and reports the failing theme directly; page-load
performance budgets are unchanged. Both themes must pass in every browser. The six scoped Linux container checks
pass with two workers: Chromium 37–38 seconds, Firefox 55–56 seconds, and WebKit
39–40 seconds per theme (2.3 minutes total).
The actual final gate rejected this production failure despite preview succeeding.

## Successful qualification

[GitHub run 34173359224](https://github.com/dakotamurphyucf/ochat/actions/runs/34173359224) passed at
`2be7647d660225b0ce7ab6012c1cda6e24450d87`: semantic gate, both website jobs, and
the required final release gate. The semantic report records OCaml 5.3.0 and
Dune 3.24.2; installed package and pin lists are retained with it.

| Environment | Origin | Artifact SHA-256 |
|---|---|---|
| preview | `http://localhost:4321` | `5af649a4574ad6efefc76eb0b862984ea770a29048cf87233cd10fe5e69559a0` |
| production | `https://release.ochat.test` | `227c4cd478e6fde4b2e80b4e0d8d00e04539ee54bf427fdc742a1fffa2138316` |

Both builds contain 125 HTML pages; preview has 575 files and production has
577. Output checks require preview noindex metadata and no sitemap; production
uses its own canonical/social origin, indexable approved pages, 111 sitemap
URLs, and noindex bridge/404 pages. Both artifacts pass capacity checks, all
68 unit tests, zero Astro diagnostics, seven performance routes, and all 21
search queries across 114 indexed pages and 1,860 fragment destinations.
Each environment passes 214 browser cases with two existing clipboard skips
(216 cases across Chromium, Firefox and WebKit), including both complete theme
accessibility scans and the delayed-font regression. The run logs and downloaded
evidence retain exact case outcomes.
Search and performance report hashes match their respective build reports.
The two artifacts have different hashes and are not interchangeable.

These identifiers describe this qualification run. Later PR/main runs qualify
their own source revisions and artifact hashes; use their uploaded reports when
preparing a release. The final read-only verification and merge/main run records
are retained in the evidence directory below.

## Bounded native source-reader checks

The final-records [run 34174591638](https://github.com/dakotamurphyucf/ochat/actions/runs/34174591638)
passed preview but exposed a WebKit test timeout in production: one 30-second
case read all 39 catalog files, ten tutorial pages, and associated guide pages.
The trace reached the first tutorial at 24.76 seconds and the final application
guides near 30 seconds. Assertions were passing until the aggregate budget expired.

Catalog entries now have independent cases, followed by separate tutorial and
associated-guide cases. All original visible-source, literal-byte, highlighting,
accessible-name, escaping, keyboard focusability, and no-download assertions are retained
with JavaScript disabled. The default per-case timeout remains unchanged. The
new case count reflects smaller test units, not removed or duplicated source coverage.
All 45 scoped Linux checks pass across Chromium, Firefox and WebKit in 1.1 minutes;
individual WebKit cases finish in roughly 3–5 seconds. The complete suite now has
258 cases, including the same two existing clipboard skips.
The first successful qualification above records its historical 214-pass count;
subsequent runs report the expanded case count for the same reader coverage.

## Publishing boundary

GitHub is the single intended production publisher. The workflow has read-only
repository permission, no deployment job, and no hosting/model credentials.
Repository secret metadata reports zero secrets. Legacy GitHub Pages automatic
builds from `main:/docs` were changed to workflow-only publishing, and no Pages
publishing workflow is installed. The existing legacy Pages site still returned
HTTP 200 after this change. Cloudflare Git auto-deployment is not configured.
The previously qualified workers.dev preview remains at its recorded candidate
version; this task performs no production upload.

The CI production origin is the reserved `https://release.ochat.test`; it must
never be deployed. P11 must build at the owned production origin, qualify those
exact bytes, and add one trusted publisher that depends on this gate and invokes
`release:ready` with a real approval/evidence record. The publisher must use the
qualified source and artifact identity, never a separate unchecked branch build.

Branch protection enforces the configured contribution path, not an immutable
security boundary against a repository owner changing settings. Workflow changes
also require review: a same-named check alone does not prove that its definition
has remained correct. Re-run policy tests and inspect the actual workflow when
changing release logic. No production environment or publisher is being claimed
as tested before P11 implements it.

## Evidence

Remote API snapshots, rejected-push logs, dependency failure logs, CI reports and
read-only enforcement verification are retained under
`scratch/ochat-website-evidence/p10-github/`. These maintainer records are excluded
from the public website. The workflow and this report are committed sources.

GitHub's documented controls:
[branch protection API](https://docs.github.com/en/rest/branches/branch-protection#update-branch-protection),
[Pages publishing configuration](https://docs.github.com/en/rest/pages/pages#update-information-about-a-github-pages-site).
