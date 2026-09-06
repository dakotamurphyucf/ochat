# Documentation implementation worklog

Scope: implement the root documentation-update plan against the September 6 local
tree. Preserve pre-existing source changes. No paid provider calls, public
listeners, normal user stores, new soak, or growing-history experiment.

## Latest semantic audit remediation

The later [library-reference follow-up](library-reference-remediation.md)
addresses the ten remaining findings, adds OAuth credential isolation, corrects
legacy snapshot publication and expands opt-in library/excerpt checks. Its
verification record is separate from the earlier remediation below.

The [final remediation record](final-audit-remediation.md) details A01–A10 and
D01–D12, their code corrections, focused regressions, and remaining boundaries.
That preceding record covers administration preparation/replacement/archives, Visual Escape,
maintained MCP/OAuth failure handling, real sanitized shell progress, and stale
tool/TUI/language/library documentation. Its verification status applies to
those findings; the earlier phase totals below are historical.

The opt-in documentation gate now also compiles and runs custom-tool and ChatML
examples. These checks do not imply execution of every Markdown snippet or
formal proof that the documentation is complete.

## Repository layout after README promotion

The beginner-friendly draft is now the root `Readme.md`. The documentation gate
checks that file and the current `docs-src` tree, with no dependency on a separate
draft or root-level planning files. The canonical specifications remain under
`docs-src/design`; their former root compatibility pointers are no longer present.
Earlier standalone planning/test records are also absent. Links now lead to the
retained guides, test sources and audit summaries rather than missing files. This
cleanup does not reconstruct or claim to recover those historical records.

The phase table and validation history below describe the earlier deliveries.

## Completed phases

| Phase | Delivered |
|---|---|
| D01 | Source/spec/doc inventory and generated [coverage ledger](documentation-coverage.md), including acceptance-criterion ownership, public interfaces and retained-document dispositions. |
| D02 | Both specifications relocated into `docs-src/design/`, root compatibility pointers, documentation and agent-server indexes. |
| D03 | Concepts/quickstart/native local tutorial; executable names, prerequisites and native/legacy/daemon TUI distinctions. |
| D04 | Configuration, all seven path variables, workspace/catalog policy, environment reference and generated operator-contract appendix. |
| D05 | Session lifecycle, durability versus execution, queues/history, prompt pinning/upgrades, recovery and cleanup. |
| D06 | All 37 protocol methods, typed payload/codec appendix, authorization, replay, events, pagination, idempotency and blob contracts. |
| D07 | Unix and stdio references/tutorials; standalone versus gateway lifetime and compiled full-duplex client. |
| D08 | Eight HTTP routes, headers, auth, batching, two SSE surfaces, reconnect and raw curl walkthrough. |
| D09 | Permission scopes, interactive/unattended fallback, manifest authorization, stock versus injected hooks and security limits. |
| D10 | Shell host integration, existing shell guide reconciliation, resource-runner reference, complete library module indexes, narrow prompt and 17-example prerequisite matrix. |
| D11 | ChatMD/source handling, ChatML developer-role and orchestration semantics, maintained MCP tools versus deprecated prompt serving. |
| D12 | Operations/troubleshooting, schema-1 limitations, shutdown/backups/import/export/retention and destructive-action qualifications. |
| D13 | Nine agent library maps, affected runtime/TUI/source sidecars and compiled Core/Eio embedding example. Existing shell architecture prose retained with added API indexes. |
| D14 | Test aliases, opt-ins, costs, artifacts, PTY/manual limitations and links to historical audit/soak/live-provider evidence. |
| D15 | README/DEVELOPMENT and CLI/workflow entry points; TUI sidecar consolidation with compatibility pointers and retained legacy notes. |
| D16 | Opt-in checker, generated private examples, source-contract drift checks, link/anchor checks, bounded offline tutorials and scenario reruns. |

## Validation performed

- Built `docs_example.exe` and `docs_check.exe` with real repository dependencies;
  ran `dune build --force @agent-docs-check` successfully. The checker covers
  repository-relative links/images, current agent guide/README anchors, all 37
  methods, all protocol interface excerpts, operator config/scope/header contracts,
  CLI-name/route inventories, request JSON codec validation, generated config/token
  validation, shell XML parsing and narrow manifest compilation.
- The timer ChatML tutorial ran through the embedded host without a provider:
  it stopped after its timer and exposed exactly one delivered schedule with
  delivery count one. Temporary checker state is removed with Eio cleanup.
- Ran the compiled E2E runner directly with explicit local server/stdio/TUI
  executable paths. Fresh passes: `unix-transport`, `stdio-modes`, `http-transport`,
  `multi-client`, `permission-reviewers`, `tui-parity`, and
  `tui-manual --case headless-self-check`.
- Fresh additional passes: `session-liveness`, `administration-idempotency`,
  `shell-grants-redaction`, `background-orchestration`, and `workspace-context`.
- Ran the exact raw HTTP tutorial handshake/header extraction, subsequent
  `session.list`, and `DELETE /v1/connection` against a generated private loopback
  fixture: typed initialization, empty session list, and `closed: true` returned.
  Stopped the daemon and removed only its disposable directory and credentials.
- Checked current executable `-help` output and source argument normalizers.
  Verified the examples appear in `git ls-files --others --exclude-standard`:
  repository exceptions also override this checkout's private `examples/` ignore.
- `dune rules @runtest` contained no docs checker/example executable or agent E2E
  dependencies. New integration work remains opt-in; no `runtest` alias added.
- `git diff --check` passed after editorial cleanup.

The initial combined forced E2E Dune invocation stalled without test workers and
was interrupted; it is **not** counted as a successful run. Serial/direct runs
above supply fresh evidence. Build linker duplicate-library warnings were
non-fatal. No normal developer store, public listener, real provider credential,
paid request, new load/soak, or manual-terminal interaction was used.

## Maintenance and verification boundaries

Regenerate the protocol/operator appendices and coverage ledger after source
changes, then check them:

```sh
dune build test/agent_docs/docs_check.exe
_build/default/test/agent_docs/docs_check.exe --refresh "$PWD"
dune build @agent-docs-check
```

The checked-in config fixture is a generator rather than reusable static secrets:
see [example configuration](../examples/agent-server/config/README.md).
Reference-code excerpts are not standalone OCaml programs. Historical examples,
shell patterns with external dependencies, and live-provider tutorial turns are
labelled; they were not all executed. Request templates use real codecs; full
response/event behavior is additionally exercised by the offline transport tests.
The checker is not an external-link availability checker or a full Markdown
renderer, and historical anchor spelling is not exhaustively normalized.

The prior user-assisted Zed, live-provider and one-hour soak records remain
historical evidence, not freshly rerun/frozen-hash certifications. Restart-free
growing-history testing remains postponed. Existing outbound provider TLS and
line-oriented SSE limitations are documented rather than silently changed in a
documentation task. Pre-existing runtime edits and user screenshots are preserved.

## README restoration following review

The first documentation pass shortened the README too aggressively. The follow-up
restores the product rationale, design principles, audience/use cases, ChatMD
examples, prompt composition, search/refinement, ChatML helpers, TUI presentation
detail, OCaml integration, layout, roadmap qualifications and license. Current
daemon/transport guidance is integrated rather than replacing that material.
Outdated commands, all-state-is-text claims, system/developer message wording,
and MCP prompt-server recommendations were corrected instead of blindly restored.
The docs rule now explicitly includes the restored example-index and historical
session links as dependencies. No runtime implementation was changed.

## Important verification constraints

- Use `rg --no-ignore` when inventorying docs in this environment.
- Nested `prompts/` fixtures require an explicit ignore exception.
- Native local TUI is transient; legacy-only flags must not be mixed with `--local`.
- HTTP connection notifications are not the durable per-session replay log.
- Stock binary reviewer/OAuth IDs need injected implementations; configuration
  alone is not a bundled reviewer/provider.
- `ochat shell` store administration is legacy; daemon IDs use protocol methods.
- Schema inspection is not journal/artifact integrity certification.

## Final code-to-documentation review

Reviewed the source paths behind CLI commands, protocol methods, transport
admission, session ownership, tools, ChatML shell extensions, compaction, and
TUI input/rendering. Corrected the discrepancies found in their guides and
library references. This includes previously undocumented shell action helpers,
the actual compaction request/retry behavior, and host-specific TUI behavior.
Both root README files were preserved during this pass.

Implementation gaps are explicitly recorded in the
[final review findings](code-documentation-audit.md#final-review-implementation-gaps):
native/daemon deletion and destination preparation, Insert key ordering,
Unicode/undo coverage, legacy raw-input validation, dormant compaction settings,
and the missing dedicated pre-compaction archive reference. Documentation edits
do not fix those runtime gaps or supersede the previously recorded open issues.

Verification:

- The expanded `dune build @agent-docs-check --force` passed: 291 Markdown
  pages, 37 protocol methods, isolated tutorial/request-codec checks, heading
  parser regressions and shell action-name coverage; no live provider calls.
- A navigation traversal reached all 291 docs pages from each root README.
  User guide/reference pages in the agent-server, guide, overview, bin, tools,
  chatmd, chatml and shell trees are within three links of each README.
- Literal-source scans found documentation mentions for all 88 CLI flag names
  and 50 environment-variable names extracted from `bin`/`lib`. These counts
  measure name coverage, not validation of every possible runtime value.
- The focused input/stream-scheduling and identity-history-lifecycle Dune test
  targets built successfully. Cached target success is not a claim that every
  test executed afresh. A subsequent forced combined invocation stalled without
  test workers and was interrupted; it is not counted as a pass. Running the
  seven built inline-test runners directly with `-strict -show-counts` then
  passed all 55 tests afresh: typeahead (12), debounce (13), redraw throttle (4),
  loader (7), streaming (14), parallel-tool configuration (4), and identity
  lifecycle (1).
- `dune rules @runtest` contained no docs checker, docs example or agent E2E
  dependencies. The new heading checks remain part of the opt-in docs alias.
- `git diff --check` passed.

The changes in this pass are documentation and documentation-checker changes,
not application runtime fixes. External links were not fetched; historical
examples were not all compiled or executed. No live-provider, external MCP,
user-assisted terminal or soak test was rerun. This is an evidence-backed
coverage review, not a proof that every documentation sentence is error-free.

## Runtime follow-up: seven final audit gaps

Implemented the seven [final audit findings](code-documentation-audit.md#final-review-implementation-gaps):
authoritative history deletion, native background layout/navigation, Meta+Shift
line duplication, grapheme-safe editing and ordinary undo, legacy raw-input
recovery, operational compaction configuration and dedicated compaction archives.
Updated both design specifications, protocol inventory, user guides and module
references. Core remains the stdlib; new filesystem operations use Eio.

Behavioral choices:

- Deletion is a writer/revision/idle-checked protocol mutation; matching tool
  occurrences are removed together without touching unrelated reused call IDs.
- Native layout coalesces aggregate background work; it is not the legacy
  progressive corridor algorithm. Warm-width streams update dirty chunks.
- Ordinary edits checkpoint once per controller action. Typeahead acceptance
  semantics remain intact. Unmodified ß inserts text instead of toggling selection.
- Relevance grading is explicitly opt-in to avoid new default provider costs.
  The resulting-context cap uses a documented local estimate, not exact billing.
- Pre-compaction archives are private framed state, committed before replacement
  and exportable by revision with current principal redaction. They survive
  ordinary journal pruning but not removal of session data.

Fresh automated verification (offline fixtures; no paid provider calls):

| Suite | Passed checks |
|---|---:|
| agent_session_test | 35 |
| agent_protocol_test | 26 |
| agent_client_test | 5 |
| chat_tui_agent_client_test | 14 |
| agent_server_restart_test | 8 |
| agent_server_gap_test | 7 |
| agent_server_permission_test | 3 |
| chat_tui inline tests | 15 |
| initial render lifecycle | 3 |
| typeahead editor | 15 |
| context-compaction library | 17 |
| meta-prompting self-consistency | 3 |

The 151 inline tests were run directly with `-strict -show-counts` and rebuilt
runners, not inferred from cached Dune targets. Relevant actor/layout runners
were rebuilt and repeated after their final corrections.

Process verification:

- All 12 `administration-idempotency` cases passed, including new archive
  restart/corruption/export and history-delete replay/restart assertions.
- All 10 `tui-parity` cases passed: six state/stream traces and four real PTY
  local/Unix/HTTP/terminal-restoration checks.
- `tui-typeahead` passed configuration plus manual legacy/local/Unix/HTTP and
  automatic legacy/local/Unix hosts. The first run exposed a real new layout
  regression: `Warm` publication did not re-enable normal-input eligibility.
  Fixed publication, added an explicit regression assertion, and reran the
  complete typeahead scenario successfully.
- `@agent-docs-check` passed: 294 pages, 38 methods, link/heading checks, request
  codecs and isolated tutorial examples. Its source scan now excludes generated
  hidden directories such as Dune's `.formatted`. A concurrent run hit the
  existing 15-second timer-example deadline; an isolated rerun passed without
  changing the timer or weakening its delivery assertions.

No live provider, human-assisted Zed check, external MCP integration or new soak
was run. This follow-up does not close the separately recorded older MCP/OAuth,
standalone-stdio RNG, patch-rollback or artifact-privacy findings. Original audit
text remains historical; its seven final findings are now marked resolved.

Final formatting/build verification: `dune build @all` and `git diff --check`
passed; generated protocol/operator references were refreshed and the docs gate
was repeated successfully after formatting.
