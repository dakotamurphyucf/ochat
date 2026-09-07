# P10.11 GitHub release enforcement

Status: enforcement configured and negative probes passed; final successful CI
qualification and merge are pending. This record must be finalized against the
successful current PR head before P10.11 is marked complete.

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
   This fixes clean dependency installation for both CI and ordinary users.

CI uses published OCaml 5.3.0, without the developer's macOS-only `core_unix` pin.
Installed versions and pin evidence are uploaded with the semantic report.
These explicit compatibility constraints are not a complete OCaml dependency lock.

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
