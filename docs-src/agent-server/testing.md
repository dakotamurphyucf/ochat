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

## Independent and native release reports

Independent ancestor resources keep script outcomes separate from actual
resource cleanup. The existing graph/Switch bracket records graph sealing,
queued release callbacks and graph finishing even when the script fails or is
cancelled. A hook raising the same exception as the script remains a cleanup
failure. The primary exception and its original backtrace remain primary.

`Independent_cleanup` reports only this bracket. A joined graph/Switch does not
certify outer ancestor Owner finalizers or the separate delegated child runtime
Switch. Its explicit retry uses the same failed factual graph legs; successful
legs and release hooks are not replayed. Graph registration closes before sealing;
escaped callbacks, foreign domains and duplicate physical registrations are
rejected. Only one factual retry can run at a time; a concurrent call returns
Conflict while the prior report stays immutable. An immutable release-hook failure stays
negative after graph repair. Native child close likewise normalizes requested
cancellation only after its actual Switch release completed without hook
failures.

Normal inline tests in `lib/agent_server` exercise real blocked child joining,
healthy failed/cancelled script cleanup and immutable native hook cancellation.
`test/agent_session/delegation_resource_tests.ml` uses the actual Actor, graph and
rejected persistence boundary to verify primary preservation and same-graph
factual recovery without erasing a release failure. These tests use no provider
or network credentials. The separate public Eio release-report tests qualify the
dependency observer; these consumer tests qualify its runtime use.

The delegated child runtime also records its own Switch release outcome and the
original runtime's close outcome. Cancellation-valued failures from a queued
hook, the revocation callback or original close remain visible through returned
close. Repeated close observes the same terminal outcome without replaying
cleanup. Actual worker aggregates and `Exn.protect` finalizer precedence remain
intact. Tests in `test/agent_session/delegated_runtime_tests.ml` use real Owner,
activity and cancellation boundaries; they do not certify outer Owner ancestry
finalizers or Independent parent-resource graph cleanup.

## Choose the appropriate tier

```sh
dune runtest
dune build @agent-docs-check
dune build @agent-e2e-pr
```

Normal `runtest` does not require the opt-in E2E runner, live provider, manual TUI,
load, or soak scenarios. `@agent-docs-check` validates the documentation/examples
separately. `@agent-e2e-pr` selects smoke, transports, workspaces, multi-client
checks, `@agent-e2e-extensibility-pr`, the checked-in authored recovery example
and the bounded `@agent-e2e-artifact-redaction` regression. The focused extensibility
subset checks retained child provider settings across restart, invocation
admission/publication without replaying an external effect, and recovery of a
committed native child creation outcome. It also checks that native tool-start and nested trace events
reach clients with secret arguments redacted. Recursive fork coverage exercises
native, standalone ChatML and moderator-handled child tools, parent rejection
before execution, and separation of child history from the root conversation.
The checked-in context recovery example exercises successful authored rollover
hooks and exact note delivery on the first successor request. Separate hook
trials cover veto and an after-hook failure that stays held after a cold reopen.
These use offline providers, production hosts and retained E2E artifacts. The
required framework CI runs this alias in its E2E tier alongside the separate
forced normal-test tier.

The [full-profile Goal accounting retirement qualification](https://github.com/dakotamurphyucf/ochat/blob/main/test/goal_accounting_retirement/README.md)
is an explicit slow local executable, outside normal CI and `runtest`:

```sh
dune build @test/goal_accounting_retirement/goal-accounting-retirement-local
```

It retains the real 256-admission boundary, atomic fold and physical reopen
oracles, with phase/count progress and a 900-second fixture timeout. The shared
Factory fixture keeps its normal 30-second default. Small accounting/retirement
laws remain in ordinary testing; this local executable adds full-profile evidence
when it completes, rather than turning a partial timeout into a pass.

The Goal recovery cases in `@test/factory_catalog_publication/runtest` use
controlled offline providers and the actual Factory, Actor and session store.
`goal_resume_tests.ml` fails the Factory handoff after the Actor has
acknowledged Resume, before the continuation runs. Replay returns the same
command receipt and keeps its original Run. `goal_finish_tests.ml` fails the
writer acknowledgement after syncing the authored `Goal.finish` checkpoint.
The old Actor leaves the projected bytes intact and reports retained cleanup.
A hard process exit releases the old store, then reopening recovers the pending
request and records the actual Interrupted Run settlement. The earlier unrelated
provider work stays intact.

The ordinary `@test/agent_transport_stdio_client/runtest` suite runs a real
Eio-owned child with separate stderr, checks bounded stderr retention and typed
response correlation, and verifies concurrent close, partial-write cancellation,
callback cancellation and exact-child reap. Socket and Developer-client suites
retain the broader notification and installed-method wire matrix.

Normal GitHub CI now divides the same suite into eight groups of nonrecursive
Dune directory aliases, with two workers per runner. A source-derived inventory
includes tests in `bin`, `lib` and `test`; the release gate requires every planned
group to pass with complete, matching execution evidence. Local `dune runtest`
continues to run the complete normal suite. Test grouping changes scheduling,
not the opt-in status of the broader qualification tiers below.

The subset does not replace full runtime, permission, security, persistence and
crash qualification. Use the affected aliases or `@agent-e2e-safe` for that wider
matrix. Read [the alias definitions](../../test/agent_server_e2e/dune) before
selecting a larger tier.

For local or release qualification of recovery delivery and assets, run

```sh
dune build --force -j 2 @agent-e2e-recovery-qualification
```

This runs ten offline cases with retained fixture reports. It covers asset
publication and converter adoption, original bytes and installed handoff,
reminders and their provider-write and required-input boundaries, authored
history and local and shared note delivery. The broader `@agent-e2e-safe` alias
includes this qualification. The PR and recovery aliases share the same
checked-in authored example action, so one safe build runs the example once.
Each case remains selectable with the runner's `--scenario` and `--case` flags.

### Complete offline workflow qualification

The normal suite keeps focused helper, native watcher, stdio/HTTP session,
compaction and child-isolation regressions. Longer workflow matrices are explicit
qualification commands, outside both `runtest` and `@agent-e2e-pr`:

```sh
dune build --force -j 2 @agent-helper-qualification
dune build --force -j 2 @agent-managed-qualification
dune build --force -j 2 @agent-authoring-qualification
# Run all three:
dune build --force -j 2 @agent-qualification
```

| Qualification | Additional coverage |
|---|---|
| Helper | Complete helper and native watcher workflows, including multiple daemon lifetimes, operator grants, pagination, cancellation, retained-source recovery and revoked grants. |
| Managed sessions | Complete stdio managed-send and HTTP authored-pending workflows, including transport lifetimes, timeout and restart combinations. |
| Authoring | Full installed-reference compaction in root and child sessions, the complete documentation-lab reviewer workflow, and all 24 offline task/policy evaluation combinations. |

Run the relevant qualification locally when changing those workflows, and run
the aggregate for release qualification. These commands use offline providers;
they require no model credentials. They are not scheduled or invoked by ordinary
GitHub CI. Passing the required PR checks does not mean these broader matrices
ran. Keep their results with the reviewed revision when reporting qualification.

The focused tests retain real execution and permission boundaries. Broader tests
must not be deleted merely to shorten the normal suite, and adding a focused test
does not imply that it covers every combination in its qualification counterpart.

The `background-orchestration` scenario starts legacy Model Jobs with a timer
issued by the authored `Session_start` callback. Its receipt identifies the
exact timer delivery consumed by the later idle callback. A raw operator timer
can drive a state-only wake, including after daemon restart. A raw `Go` event
with the same payload reaches the queue without a Model Job or provider request.

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
| `@agent-e2e-developer-trace-client` | Authenticated two-root Developer trace pages over embedded, HTTP, Unix and first-party stdio; exact revocation and reconnect. Included in transports. |
| `@agent-e2e-permissions`, `@agent-e2e-security` | Approval, authorization, projection and boundary cases. |
| `@agent-e2e-persistence`, `@agent-e2e-crash`, `@agent-e2e-recovery` | Store and interruption/recovery behavior. |
| `@agent-e2e-recovery-qualification` | Offline recovery delivery, asset custody and near-limit behavior for local and release checks. Included in safe. |
| `@agent-e2e-tui-auto` | Deterministic automated TUI traces. |
| `@agent-e2e-pty-headless` | Headless PTY capability and terminal lifecycle checks. |
| `@agent-e2e-load` | Bounded session/command, SSE/backpressure, reconnect, actor-unload and capacity loads. |
| `@agent-e2e-safe` | Broad offline suite, including load; not a lightweight normal-test alias. |
| `@agent-e2e-soak` | Explicit gated long-duration workload. |

The crash matrix includes `invocation.admission-publication-no-replay`: an actual
compiled standalone ChatML tool is interrupted by SIGKILL immediately after the
invocation admission journal sync, after its outcome sync before provider
publication, and while a nested native tool awaits approval. Two fresh daemons must preserve the original invocation context and
publish one stable response. Unstarted handling becomes interrupted; a saved
successful outcome is published without repeating its real file mutation. Live
progress is observed through public snapshots; raw checkpoints are inspected only
after the child has been killed and joined. The approval case must cancel the old
wait and reject a late approval submitted through a new valid attachment.

`pending.interrupted-root-recovery` is also in the required PR subset. It kills
an owned daemon process with an active turn and an input held behind that turn,
then physically reopens the store. A nonactivating query must preserve the
blocked boundary; actual recovery records `Interrupted` while the session stays
stopped. Explicit resume adopts the input once, and a second reopening preserves
that result. The harness joins the killed child and observes the recovery child's
successful exit.

The required `artifact.redaction` case uses bounded repeated registrations to
check that original secrets and their single Base64 encodings are redacted,
without recursively registering encodings of existing variants. It guards
registration growth independently of the larger conformance scenario. These
checks use fixture secrets and do not require credentials.

`job.committed-intent-launch-once` kills the daemon after a standalone ChatML
handler saves its selected native job and Pending outcome, before worker launch.
Recovery must launch that same job once, publish the original acknowledgement,
deliver its result and retain identical job/receipt/history on a second reopening.

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
