# P05 hosting publication review

Date: 2026-09-06. Source revision: `bc76b6c72a280b4bad48a793a63373e6db26d2b4`.
Status: hosting group implemented; overall P05 remains in progress.

## Scope

This group adds 15 pages to the first migration group's 37: **52 rendered docs**
(**51 publish, 1 bridge**), **245 deferred**, **297 tracked sources accounted for**.
All Markdown under `docs-src/agent-server/` now has a published route. The shared
example setup and server/stdio command references are published too. Deeper
library, design, shell-policy, and example source links can still lead to GitHub.
This is not completion of the complete corpus or the downloadable example catalog.

| Canonical source relative to docs-src | Website route |
| --- | --- |
| `agent-server/tutorials/unix-daemon.md` | `/docs/tutorials/unix-daemon/` |
| `agent-server/tutorials/http-client.md` | `/docs/tutorials/http-client/` |
| `agent-server/tutorials/shell-agent.md` | `/docs/tutorials/shell-agent/` |
| `examples/agent-server/README.md` | `/docs/examples/agent-server/` |
| `agent-server/configuration.md` | `/docs/reference/agent-server/configuration/` |
| `agent-server/protocol.md` | `/docs/reference/agent-server/protocol/` |
| `agent-server/protocol-types.md` | `/docs/reference/agent-server/protocol-types/` |
| `agent-server/operator-contracts.md` | `/docs/reference/agent-server/operator-contracts/` |
| `agent-server/transports/unix.md` | `/docs/reference/agent-server/transports/unix/` |
| `agent-server/transports/stdio.md` | `/docs/reference/agent-server/transports/stdio/` |
| `agent-server/transports/http.md` | `/docs/reference/agent-server/transports/http/` |
| `agent-server/chatml-orchestration.md` | `/docs/guides/agent-orchestration/` |
| `agent-server/testing.md` | `/docs/guides/testing/` |
| `bin/ochat_agent_server.doc.md` | `/docs/reference/commands/agent-server/` |
| `bin/ochat_agent_stdio.doc.md` | `/docs/reference/commands/agent-stdio/` |

Every existing ID and route is retained. Navigation separates Agent hosting from
Agent protocol so exact contracts do not overwhelm operational guidance. The
operator appendix is intentionally hidden from the sidebar but searchable,
sitemapped, and linked from configuration. Tutorial order is stdio, narrow shell,
Unix daemon, HTTP client, then background agent. Hosting hub pagination leads to
shared setup, then Unix and HTTP tutorials.

All new pages retain conservative `not-checked` runtime verification metadata.
Editorial review and a passing offline gate do not certify each published command
or example as live-tested. ChatML orchestration carries experimental status.
Search examples from the preceding group remain explicitly illustrative.

## Editorial corrections

Two stale statements in `bin/ochat_agent_server.doc.md` were corrected before
publication to agree with the detailed configuration and HTTP guides:

- `completed_stream_ms` supplies a default for raw response-artifact retention;
  it does not implement completed live-stream replay. Protocol 1.0 recovery uses
  durable events and replacement snapshots.
- HTTP batch response ordering does not serialize command side effects. Commands
  after initialization may execute concurrently.

The generated operator appendix previously had an empty daemon flag inventory.
The extractor recognized quoted options beginning with a dash, but the daemon
uses Core `flag "config" ...` declarations without that dash in source.
`Docs_inventory.cli_flags` now recognizes both forms, normalizes named flags to
single-dash display, deduplicates, and sorts them. A focused regression in the
existing docs checker covers named flags, multiline declarations, literal options,
duplicates, and an unrelated string. The appendix now lists nine daemon options.

Its generated introduction explicitly limits the inventory: parser-added aliases
and generated help/version flags are not enumerated. Readers should use executable
help and command references for accepted combinations. This remains a source-text
inventory, not an OCaml AST parser or an exhaustive executable-help implementation.

The correction was made in `test/agent_docs/docs_inventory.ml`, then regenerated
with the existing `docs_check --refresh` workflow. Inspection of the Git diff
confirmed that only `operator-contracts.md` changed among generated documents.
No generated Markdown was hand-edited. Website builds still never invoke the
OCaml generator. No runtime implementation, dependency list, canonical path, or
semantic fixture was moved.

## Verification

- Forced `dune build --force @agent-docs-check` passes: 297 pages, 38 methods,
  no live provider calls. This includes the new flag regression, generated-source
  equality, protocol JSON, links/anchors, isolated setup/config validation,
  observer authorization, and existing example/contract checks. It is not a fresh
  HTTP wire E2E, interactive TUI, or live-provider run.
- `npm run check`: 38 unit tests pass and Astro has no errors/warnings/hints.
  Fence-byte parity now covers all 52 rendered sources, including the complete
  protocol interfaces and regenerated operator appendix.
- `npm run build`: output graph, real fragments, search/sitemap/noindex policy,
  and capacity checks pass. 54 HTML pages, 257 files, 9,883,427 bytes.
- The protocol-types HTML is 913,586 bytes, now the largest individual artifact.
  Full source excerpts and anchors are preserved. The deferred Mermaid chunk
  remains 662,096 bytes. Hosted performance budgets and any evidence-based split
  remain release work; static-host capacity passing is not a performance claim.
- New browser checks cover the setup → Unix → HTTP → configuration path,
  no-JavaScript protocol fragments and code, protocol method search, corrected
  operator flags, and dense reference reflow/keyboard scrolling/axe in both themes.
- Browser outcome: **82 passed, 2 intentional non-Chromium clipboard skips**,
  across the full run and targeted rechecks. The first run had 79 passes and
  three 30-second timeouts during full-page axe scanning of the dense reference.
  The test retains both full scans and uses Playwright's explicit slow-test
  allowance (90 seconds); no rule or content was excluded. Isolated rechecks
  passed in Chromium (38.9 seconds), Firefox (about one minute), and WebKit
  (34.1 seconds). Logs: `p05-hosting-dense-chromium.log` and
  `p05-hosting-dense-other.log`. This is not one clean full-suite run, and test
  execution time is distinct from reading/page-load performance.
- Inspected all four screenshots: Unix tutorial desktop, protocol desktop,
  protocol mobile dark at `#session`, and operator appendix at 320px. Local
  samples showed no document overflow. Both changed OCaml helpers also pass
  `ocamlformat --check`.
- Evidence logs: `scratch/ochat-website-evidence/p05-hosting-{refresh,docs-check,check,build,browser}.log`.
  Screenshots and local reading samples: `scratch/ochat-website-evidence/p05-hosting/`.
  Local timing samples are not hosted release benchmarks.

The automatic migration reports enumerate the new routes, current source hashes,
publication policies, and every remaining deferral. They remain outside `dist/`.
The two modified canonical pages join the earlier source corrections described in
[p05-migration-review.md](p05-migration-review.md). The docs generator and checker
changes are recorded here and in the Git diff, not treated as runtime edits.

## Remaining work

This historical checkpoint is followed by the [batch and shell publication
review](p05-shell-review.md), which resolves the batch smoke-test issue and
publishes nine shell/batch references. The counts above describe this earlier group.

The broader P05 phase remains open. The next group should correct the deferred
batch smoke test and review shell management, security, persistence, and selected
library families. Complete older TUI bridges, capability coverage, and the
machine-readable supplemental-source manifest. The hosting setup's OCaml helper
and companion examples currently remain revision-linked repository files;
dependency-complete downloads belong to P06.

The initial capability inventory is now superseded for protocol, transports,
background orchestration, and daemon/stdio commands by the routes above. ChatML
runtime/budget contracts, legacy embedding, MCP authentication, refinement,
retrieval command details, and deeper library coverage are still deferred.
No new P05 checkbox is closed solely because these 15 pages render successfully.
