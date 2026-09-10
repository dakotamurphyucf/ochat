# Testing and evidence

## Documentation-audit gap regressions

The latest seven-finding follow-up checks complete HTTP connection-authority
binding across RPC/SSE/DELETE, configured handshake retention, read-only artifact
files and rejection of altered/missing/extra/symlinked tree content. Catalog
rebuild/restore and cached-revision runtime construction have separate integrity
regressions. Config polling is tested without explicit reload. The docs gate now
authenticates the generated admin/observer tokens and exercises stopped-session
creation, restricted observer attachment/read and rejected submission through the
real dispatcher. It also checks coarse history versus exact developer payload
roles. No provider call occurs; HTTP wire checks remain in opt-in E2E.

Protocol 1.0 live-delta replay is explicitly not implemented or claimed: recovery
uses durable events/snapshots. `completed_stream_ms` is a legacy fallback for raw
response retention, not a live replay setting. See the
[latest remediation record](../development/code-documentation-audit.md#seven-final-findings-authority-artifacts-and-documentation-september-6-2026).

The [selected library-contract follow-up](../development/code-documentation-audit.md#selected-library-contract-follow-up-september-6-2026)
covers ordered/strict binary reads, atomic catalogue saves, source spans and
developer-role compatibility steering. The ordinary `library_contract_audit`
runner also exercises documented legacy logger/crawler behavior in an isolated
child process. Five corrected library examples are compiled and checked against
their Markdown excerpts by `@agent-docs-check`; network/provider callables are
not invoked. These checks do not revive deprecated BM25/Merlin work.

The seven runtime findings from the final documentation audit now have focused
regressions and an updated [implementation/evidence record](../development/documentation-worklog.md#runtime-follow-up-seven-final-audit-gaps).
The `administration-idempotency` scenario's `admin.compact` case additionally
checks archived export before/after restart, corrupt-file rejection, idempotent
history deletion, archive independence from deletion, and deletion persistence
across another restart. Run it with `dune build @agent-e2e-admin` or select the
case using the runner. These process tests remain opt-in.

The `cross-transport-conformance` scenario's `conformance.history-deletion` case
executes `session.delete_history` over Unix, HTTP, stdio-to-Unix and stdio-to-HTTP.
It fetches a canonical ID from a snapshot, checks reader denial and stale-revision
rejection without state changes, verifies successful deletion and idempotent
replay, and compares committed replacement events for both subscribers. This is
executable method coverage, not only an entry in the method inventory.

Editor and layout regressions exercise Unicode clusters, line duplication,
undo/redo, raw-input recovery, 500-row resize/navigation, stale completions and
typeahead eligibility after layout publication. The existing
`@agent-e2e-typeahead` and `@agent-e2e-tui-auto` gates exercise real headless
terminal/client adapters; they do not replace a human visual check in Zed.

## Choose the appropriate tier

```sh
dune runtest
dune build @agent-docs-check
dune build @agent-e2e-pr
```

Normal `runtest` does not require the opt-in E2E runner, live provider, manual TUI,
load, or soak scenarios. `@agent-docs-check` validates the documentation/examples
separately. `@agent-e2e-pr` selects smoke, transports, workspaces and multi-client
checks. Read [the alias definitions](../../test/agent_server_e2e/dune) before
selecting a larger tier.

The docs check covers local inline links and heading anchors throughout
`docs-src`, the promoted `Readme.md`, and `DEVELOPMENT.md`, plus
local HTML image targets. Heading extraction handles ATX/Setext headings,
duplicate suffixes, inline links and explicit anchors. It does not fetch external
URLs or execute every historical API snippet. The check also compares protocol
and operator excerpts with source, checks shell ChatML action names against
their guide, validates protocol JSON, and runs isolated offline tutorial checks.
Passing it is not proof of semantic completeness.

The library-reference follow-up adds full-text equality checks for every excerpt
labeled “current callable contract,” plus offline Io/cache/template/tokenizer/
vector/session behavior checks in [docs_library.ml](../../test/agent_docs/docs_library.ml).
These remain opt-in with the documentation alias. OAuth identity and legacy
snapshot regressions run in ordinary testing; see the
[remediation record](../development/library-reference-remediation.md).

Two additional offline examples guard library documentation: the
[custom-tool example](../examples/tools/custom_tool.ml) checks typed results,
invocation-aware dispatch, progress and nested traces; the
[ChatML moderator](../examples/chatml/moderator.chatml) is compared with the
language guide, compiled and exercised for state updates and tool rejection.
These run under `@agent-docs-check`, without provider requests.

| Explicit selection | Purpose |
|---|---|
| `@agent-e2e-transports` | Local/gateway/HTTP/Unix protocol workflows. |
| `@agent-e2e-permissions`, `@agent-e2e-security` | Approval, authorization, projection and boundary cases. |
| `@agent-e2e-persistence`, `@agent-e2e-crash`, `@agent-e2e-recovery` | Store and interruption/recovery behavior. |
| `@agent-e2e-tui-auto` | Deterministic automated TUI traces. |
| `@agent-e2e-pty-headless` | Headless PTY capability and terminal lifecycle checks. |
| `@agent-e2e-load` | Bounded session/command, SSE/backpressure, reconnect, actor-unload and capacity loads. |
| `@agent-e2e-safe` | Broad offline suite, including load; not a lightweight normal-test alias. |
| `@agent-e2e-soak` | Explicit gated long-duration workload. |

The crash matrix includes `invocation.admission-publication-no-replay`: an actual
compiled standalone ChatML tool is interrupted by SIGKILL immediately after the
invocation admission journal sync and after its outcome sync, before provider
publication. Two fresh daemons must preserve the original invocation context and
publish one stable response. Unstarted handling becomes interrupted; a saved
successful outcome is published without repeating its real file mutation. Live
progress is observed through public snapshots; raw checkpoints are inspected only
after the child has been killed and joined.

E2E fixtures use private temporary directories, generated tokens and local
listeners; they should not touch normal stores. Reports are written under
`_build/agent-e2e-reports`, with isolated fixture artifacts. Check cleanup and
process exit, not just one passing assertion. Some resource observations use
platform tools such as macOS `ps`/`lsof`; platform coverage is not universal.

The default soak is one hour and requires explicit `OCHAT_E2E_ALLOW_SOAK=1` and
`OCHAT_E2E_RUNNER_PROFILE=isolated`. Do not run it implicitly. It is not the
postponed restart-free growing-history memory experiment. Live-provider cases
require explicit credentials/model/spending decisions and are separate from fake-
provider validation. Documentation work does not authorize new paid calls.

## What the records mean

The [documentation worklog](../development/documentation-worklog.md),
[code/documentation audit](../development/code-documentation-audit.md),
[final audit remediation](../development/final-audit-remediation.md) and
[typeahead verification](../development/typeahead-verification.md) retain dated
summaries and qualifications. The earlier standalone E2E plan, coverage audit,
load/soak and manual TUI records, spec/code audit and release-confidence notes
are not included in this checkout. These summaries do not replace those missing
records or reconstruct their raw evidence. For runnable checks, use the
[current scenario sources and aliases](../../test/agent_server_e2e/dune).
Historical results are not proof of all possible behavior.
Some load/live/manual evidence predates the final narrow fixes; no frozen-candidate
hash certification is claimed. Local `_build`/`/tmp` links may not exist on another
checkout.

The prior one-hour soak and bounded loads exercised particular workloads.
Observed RSS ranges do not attribute heap ownership or prove a universal memory
bound. Restart-free growing-history testing remains postponed, and ordinary-use
observation remains a follow-up rather than an unperformed test marked passed.

## TUI and PTY limits

Automated traces verify reducers/projections, event ordering, reconnection,
permissions, drafts and terminal lifecycle paths. A headless PTY does not reproduce
every terminal emulator's rendering, key encoding, resize behavior or visual
selection. The recorded Zed check covers the actual editor terminal used in that
run; its version is evidence, not a universal requirement.

For a new emulator, briefly verify tab labels/spacing, audit selection/detail
updates, streaming completion without duplicate rows or stuck busy state,
reconnect/draft preservation, and quit followed by a normal shell command. Arrange
user help only for that visual/input check; normal/docs tests must not wait on it.

Add future regressions around a concrete observed failure: storage faults,
concurrent writers, lease races, network fragmentation, external integrations or
resource pressure. Keep them isolated and bounded before promoting them into a
normal test tier.
