# P01–P04 correctness audit

Reviewed 2026-09-06 against the phase tasks, the actual source, existing evidence,
and fresh local/isolated-build checks. Baseline checkout revision:
`bc76b6c72a280b4bad48a793a63373e6db26d2b4`; website work remains uncommitted.

## Findings fixed

| Priority | Finding | Correction and verification |
|---|---|---|
| Medium | Reference-style Markdown images were resolved as ordinary links, allowing an unapproved local image to become a GitHub HTML image URL. | Collect image-reference identifiers and apply the image allowlist to their definitions. A regression covers approved and rejected cases. |
| Medium | An H1 containing a Markdown link produced overlapping edits when the title was replaced with its historical anchor. | Stop descending into the replaced title node. A regression verifies its anchor and subsequent links. |
| Medium | Development regeneration did not watch renderer/importer helpers, and restarting Astro within the same process retained cached renderer imports. | Watch those inputs and run both generation and the Astro reader in fresh processes. Live HTTP probes verify renderer and importer edits plus restoration. Invalid JSON stops the process and preserves the last complete snapshot. |
| Medium | Public assets rejected nested symlinks but followed a symlink used as the public directory itself. | Reject a symlinked public root before copying anything; regression verifies no output is created. |
| Low | Homepage theme followed the system only at initial load and did not expose the toggle state. | Reuse initial ThemeProvider behavior, listen for system changes until explicitly overridden, and expose dark mode through aria-pressed. A regression passes in all three browsers. |

Also limited WebKit's Option+Tab test path to macOS, consistent with the documented
platform behavior, and corrected the spec's stale manifest-directory reference.
No canonical runtime examples or OCaml implementation changed in this audit.

## Phase conclusions

- **P01:** Foundation, narrow ignore rules, Node/package pins, commands, route
  ownership, configuration, CI scaffold and Dune isolation are implemented.
  A clean tracked candidate builds on Linux without OCaml or provider credentials.
  **P01.01 remains open for the actual working-repository commit of the lockfile.**
  Earlier completion wording was too strong for an uncommitted checkout. The
  temporary audit commits affect only the isolated clone, not the user's branch.
- **P02:** Representative import contract passes after the fixes above. Source
  ownership, exact code values, provenance, independent publication policies,
  snapshot locking/recovery and output-link checks retain their coverage. This
  is the 22-page representative site, not a completed migration of 297 pages.
- **P03:** The scoped local reading/interaction gate passes. Existing contrast,
  screenshots and full keyboard traversal evidence remains applicable; the new
  theme behavior is covered by browser regressions. Native browser-chrome zoom,
  screen-reader sampling and physical mobile keyboards remain P10 release checks.
- **P04:** Canonical installation and first-agent instructions match the maintained
  fixtures and current CLI/build contracts. Dune's install build and forced
  documentation gate pass. No live model request or live verification is claimed.

## Fresh evidence

All local audit evidence is under `scratch/ochat-website-evidence/`:

- `p01-p04-audit-check.log`: 35 tests pass; Astro reports no errors/warnings/hints.
- `p01-p04-audit-browser.log`: 58 pass across Chromium, Firefox and WebKit;
  two intentional non-Chromium clipboard skips. The macOS-only Tab guard leaves
  the exercised macOS path unchanged; Linux browser execution remains pending.
- `p01-p04-audit-build.log`: 24 HTML pages; source/edit links, local routes,
  fragments, indexing/sitemap policy and output capacity pass.
- `p01-p04-audit-live-dev.json`: real renderer/importer changes and restoration
  appear in served HTTP output after fresh child-process restarts.
- `p01-p04-audit-fail-stop.json`: invalid manifest closes the server, preserves
  the previous snapshot and is restored after the probe.
- `p01-p04-audit-linux.log`: clean npm ci, diagnostics/tests, preview build and
  production-mode build in Node 22.22.1 on Linux, from a tracked temporary candidate.
- `p01-p04-audit-linux-final.log`: same check/build branches after updating the
  candidate with the final dev supervisor. Later README/audit text and the
  platform guard do not change the generated site or Linux package checks.
- `p01-p04-audit-dune.log`: `dune build @install --force @agent-docs-check`
  passes with npm dependencies and generated website output present; 297 pages,
  38 methods, no live provider calls.

Production-mode testing uses an explicitly reserved `.test` origin inside the
isolated clone. It checks generated behavior only; no domain was acquired and
no artifact was deployed. GitHub Actions has not run remotely. The existing
Mermaid bundle-size warning and optional empty i18n warning remain documented;
performance/release work is still outside the completed local milestone.
