# P12 maintenance handoff

The contributor and operations handoff is implemented. Formal phase closeout
still records the outstanding P11 registrar contact-email confirmation, rather
than claiming an account detail that was not checked. Future reviews are assigned
dates and owners, not marked as already performed.

## Delivered documentation

- [Contributor workflows](../CONTRIBUTING.md): clean setup, source ownership,
  add/remove/move a page, example/capture maintenance, relevant checks,
  dependency upgrades and the optional API regeneration boundary.
- [Maintenance](maintenance.md): Dakota's operational/account responsibilities,
  recovery/archive ownership, dated review calendar, and prioritized backlog
  including all ten deferred canonical documents and tasks 17–19.
- [Release runbook](release-runbook.md): current production publisher,
  failed-job-only retry, readiness semantics and recovery procedure.
- [Launch record](p11-launch.md): the first verified production artifact,
  domain-conflict resolution, public links, versions and limits of the evidence.
- Website README: removed stale statements that CI/publication/P11 were pending;
  links now lead to the permanent contributor and maintenance documents.

## Demonstrated contributor workflow

A fresh public Git clone at launch revision
`b859aef70312a0f2553998a024f3c98561906106` installed the locked dependencies
with Node 22.22.1. No existing `node_modules`, generated website snapshot or
OCaml switch was copied into that checkout.

The exercise served the original first-agent tutorial, edited a prose sentence
in `docs-src/agent-server/tutorials/local-tui.md`, observed watcher regeneration,
and checked the new text in the dev response. The source-edit link resolved to
that exact file on main; the immutable source link retained the public base
revision. Both `npm run check` and `npm run build` passed with the edit, and the
static output contained it. The temporary source change was restored; the clone
ended with a clean Git status and its owned server was stopped. Eight exercise
assertions passed. This demonstrates a local contributor workflow, not a new
production artifact or a live model run.

Evidence: local `scratch/ochat-website-evidence/p12/contributor-report.json`,
install/check/build/dev logs and edited-preview HTML. The dev server selected
port 4324 because earlier ports were occupied; the existing user's servers
were preserved. An initial install using the ambient Node version was repeated
with the pinned Node explicitly selected; both logs are retained.

## Initial production review and fixes

The deployed launch artifact passed a fresh 3,601-assertion hosted review,
including principal/nested routes, search assets, downloads, indexing headers,
real 404s, conditional caching and HTTP/www canonical redirects. The original
publication failure was a resolved DNS conflict. Initial and failed-attempt
evidence remains available; no long-term uptime or traffic claim is made.

Two concrete issues were addressed:

1. **Readiness retries:** the old probe assigned readiness after the apex fetch
   before awaiting www. If www then threw, the next loop condition could stop
   retries early. The new helper only succeeds after both checks finish in the
   same observation, records failed probes and exhausts a bounded retry count.
   Regression tests cover delayed www certificates, permanent failure, stale
   apex bytes and wrong redirects. The full hosted review passes with the new
   helper; later hosted checks had already protected the original release.
2. **WebKit speculative requests:** the post-launch browser review reproduced
   the earlier prefetch access-control report. Astro catches fetch rejection,
   but WebKit still reported an error during rapid navigation. Optional
   Starlight/Astro prefetching is disabled explicitly. A browser regression
   verifies hover/focus does not issue speculative document requests and that
   installation → troubleshooting → first-agent navigation remains error-free
   in Chromium, Firefox and WebKit. Search remains a separate on-demand worker.
   This is a site-level mitigation, not a claimed upstream browser fix.

Local validation passes 74 unit tests, zero Astro diagnostics, the fixed preview
build, and all three targeted browser regressions. The full protected CI matrices
and the post-merge hosted report qualify the final publication independently;
their immutable reports are available from the
[Website workflow](https://github.com/dakotamurphyucf/ochat/actions/workflows/website.yml).
Local reports live in `scratch/ochat-website-evidence/p12/`.

## Scope retained

P09 odoc hosting and P10.03 manual accessibility remain explicitly deferred.
Tasks 17–19 are backlog items, not implemented gate changes. GoDaddy contact-email
verification and the renewal quote's period remain owner-managed/unconfirmed.
The review calendar schedules future observations without installing reminders
or sending messages. The maintainer owns their follow-through.

The production website is the qualified Cloudflare artifact, not local generated
output. Both Workers and the exact artifact/commit remain linked through per-run
deployment reports; scratch memory records the current deployed revision and
open work. Do not make a new production claim from local checks alone.
