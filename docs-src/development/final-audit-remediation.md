# Final code/documentation audit remediation

This record tracks the follow-up requested after the September 6 cross-check.
Status: **completed September 6, 2026**, within the boundaries below. A01–A10
have implementation corrections, D01–D12 have documentation corrections, and
V01 has executable cross-transport coverage. The table preserves the original
findings, not current defects. Initial findings came from source inspection;
the verification section records the subsequent runtime evidence.

## Findings and intended corrections

| ID | Finding and evidence | Required correction and verification |
|---|---|---|
| A01 | Rebuild/upgrade commit a revision and clear moderator/shell state before runtime validation (`agent_server/command_handler.ml`, `session_actor.ml`), contrary to implementation-spec §18.7–18.8. | Prepare and authorize first; one actor commit after successful preparation. Inject invalid runtime construction and verify prior revision/history/security state survives. |
| A02 | Reset emits summary/lifecycle events but clears canonical history, deferred inputs, permissions, grants, jobs and schedules that client projections retain. | Publish a complete generation replacement or require a new snapshot; verify existing writer/reader projections converge without reconnecting. |
| A03 | Rebuild reuses old canonical history instead of reconstructing the selected prompt's initial messages. | Reparse the selected revision and install fresh initial history with valid occurrence identities; test changed/deleted prompt messages, not merely revision metadata. |
| A04 | Reset/rebuild lack the archives promised by architecture §14.6–14.7; compaction archives alone do not cover them. | Persist an independently retained pre-mutation archive before committing its reference; cover export/restart and archive-write failure. |
| A05 | Normal-mode dispatcher captures Escape before the Visual-selection dismissal branch. | Give selection dismissal precedence; regress the shared public dispatcher so Escape neither cancels nor quits with a selection active. |
| A06 | Maintained outbound MCP receiver EOF/error and explicit close leave pending RPC promises unresolved. | Fail all pending calls once, clear pending state, and handle failed sends/cancellation; test multiple in-flight calls and disconnect. |
| A07 | Stored MCP client secrets hit an unconditional exception. | Restore credential selection and test stored-secret setup without external authentication. |
| A08 | OAuth discovery request failure raises before its documented conventional-endpoint fallback. | Restore typed fallback/error behavior and cancellation propagation; inject discovery failure. |
| A09 | OAuth wire-token decoding requires locally owned `obtained_at` before assigning it; malformed responses escape the Result boundary. | Separate wire decoding from persisted token decoding; cover acquisition, PKCE and refresh, absent timestamp, malformed JSON and cancellation. |
| A10 | Shell `stream="sanitized"` parses but is ignored by the execution adapter. | Provide an explicit safe execution contract without exposing raw bytes or bypassing cross-chunk secret filtering/interceptors; test output bounds, UTF-8 and cancellation. |
| D01 | `Chat_tui.Persistence` promises all tool exports are bounded/sanitized/redacted, but it serializes supplied canonical payloads. | Document upstream tool/host-specific protection and sensitive exports; do not imply a daemon principal-projection bypass. |
| D02 | `gpt_function`, `definitions`, and `functions` sidecars use obsolete string-only APIs, omit `type_`, and lack progress/trace integration. | Use current typed outputs and invocation-aware dispatch; add a compiled offline tool example to the opt-in docs check. |
| D03 | App/Persistence API descriptions are obsolete; Conversation says 2,000 bytes instead of 10,000 and claims filtered indices equal source indices. | Replace obsolete sections with current APIs, checkpoint semantics, rendering limits and stable identity rules. |
| D04 | Normal-mode sidecar still says byte movement and history-scrolling `gg/G`; main guide retains the old raw-input warning. | Describe grapheme-aligned editing, draft navigation, and rejected-draft recovery consistently. |
| D05 | ChatML language example matches nonexistent host events. | Use `Item_appended` / `Pre_tool_call` and real helper contracts; validate the example offline. |
| D06 | ChatMD docs exclude top-level imports and describe imported scripts as root-relative. | Document top-level declaration expansion and declaring-source-relative script paths. |
| D07 | Default fixed-shell schema includes `rationale` although the default forbids it. | Align the example with `rationale="none"`; show explicit opt-in separately. |
| D08 | Fork sidecar exposes removed APIs and says parent cancellation is not propagated; prose and generated prompt falsely promise PERSIST-only retention. | Document identity-bearing APIs and inherited local cancellation; correct instructions to match all-new-assistant-text return without silently adding lossy extraction. |
| D09 | Idempotency docs suggest configurable receipt expiry. | Document fixed one-day standard expiry and non-expiring protected receipts, without promising exactly-once external effects. |
| D10 | `ochat tokenize` says `cl100k_base`; deprecated MCP `read_dir` says JSON array. | Correct to `o200k_base` and newline-separated text carried as a JSON string. |
| D11 | Summarizer limitations imply the whole compaction path lacks relevance filtering or a resulting-context bound. | Scope limitations to the standalone helper and link the configured compactor. |
| D12 | Embedding guide advertises removed `run_completion_stream_in_memory_v1` and unqualified shared cache placement. | Remove the nonexistent adapter and distinguish session-owned caches from file-backed/legacy hosts. |
| V01 | The final rerun found `session.delete_history` missing from the closed-protocol cross-transport coverage inventory, causing the conformance gate to reject the suite. | Add actual deletion/replay assertions across Unix, HTTP and both stdio gateways, retain the completeness guard, and rerun transport/security gates. |

## Scope and safety

Core is the standard library; new filesystem operations use Eio. Preserve the
existing dirty worktree. Tests use offline fixtures/private local listeners and
generated credentials, not user stores or paid provider calls. Process E2E and
documentation checks remain opt-in, outside ordinary `dune runtest`.

The earlier external MCP notification fan-out/catalog-refresh findings, OAuth
browser callback hardening, standalone-stdio RNG issue and other explicitly
tracked older limitations are separate unless a correction here requires them.
This work does not imply unrestricted external-server interoperability.

## Verification

- D02: custom-tool guide/definitions/registrations corrected; the new
  `docs-src/examples/tools/custom_tool.exe` builds and passes offline. It covers
  typed text/content output, invocation-aware dispatch, silent/observed progress,
  nested traces, duplicate names and malformed arguments. It is invoked by
  `@agent-docs-check`, not by a new process E2E dependency of `runtest`.
- D11–D12: standalone summarizer versus configured compactor scope corrected;
  removed the nonexistent streaming compatibility API and qualified cache paths.
- A01–A04: detached preparation, actor compare-and-set, principal-filtered
  replacement snapshots and retained administration archives implemented.
  All seven administration tests passed, including the fail-closed synchronous
  initializer model-call regression. Existing session/client runners passed
  36 and six tests respectively.
  `dune build @agent-e2e-admin` passed all 12 real-daemon cases, including reset,
  rebuild and upgrade archive export/restart/corruption checks.
- A05: fresh Normal-mode dispatcher runner passed three tests, including Visual
  selection dismissal without cancellation or quit and preservation of history
  selection. Agent-page and identity-interaction runners passed ten and four
  tests. Their obsolete expectations were corrected to cover selection dismissal
  and authoritative ID-addressed deletion, rather than reverting current behavior.
- D05/D08: the ChatML example compiles and exercises real events; the fresh
  response-loop identity runner passed nine tests including fork instructions.
- A06–A09: the final MCP/OAuth regression run passed 38 cases. Independent review
  identified idle SSE event delivery and async teardown ownership regressions;
  both were fixed and covered, along with HTTP error-body disposal needed to
  avoid shutdown waiting on an unread response. Cases include real local stdio,
  loopback HTTP, stored credentials, token acquisition/refresh, discovery fallback,
  malformed responses and cancellation. No browser or external MCP server used.
- A10: real sanitized pipe progress implemented with independent source
  decoders, overlapping/cross-chunk literal matching, channel and combined-output
  filtering, disclosure bounds, cancellation cleanup and deadline-covered EOF
  flushing. Unsupported filters are rejected explicitly at registration and
  execution. The shell suite passed directly and through `dune runtest`, including
  the corrected relative-path child-process fixture.
  The suite contains four filter groups, eleven executor groups and three
  ChatMD/tool-wiring groups, including live-before-exit synchronization,
  pipeline stderr, cancellation, cross-channel secrets and observer timeout.
- `dune build @agent-docs-check` passed: 296 Markdown pages, 38 methods, isolated
  tutorial checks and both new examples. A separate read-only navigation walk
  found every page reachable within three links from either README.
- Final normal `dune runtest` passed after correcting stale TUI expectations and
  adding the new regression suites. Process E2E/docs aliases remain opt-in.
- V01: the final transport/security rerun detected a pre-existing conformance
  inventory omission for the newly added `session.delete_history` method. The
  completeness guard was retained. A new case passed over Unix, HTTP, stdio→Unix
  and stdio→HTTP: canonical-ID deletion, read-only and stale-revision rejection
  with unchanged state, idempotent replay, and matching writer/reader replacement
  events. All nine conformance cases and the security matrix then passed.

### Final gates

Each of these commands/gates passed following the applicable corrections:

```sh
dune build @all
dune runtest
dune build @agent-docs-check
dune build @agent-e2e-admin
dune build @agent-e2e-transports
dune build @agent-e2e-permissions
dune build @agent-e2e-security
dune build @agent-e2e-tui-auto
git diff --check
```

The first broad runs exposed the stale TUI expectations and missing conformance
case described above; those failures were corrected and the affected gates rerun.
The TUI gate includes the typeahead fixture/self-check and ten automated
projection/PTY traces. No new manual Zed session, external MCP/browser login,
paid provider request, soak or postponed growing-history experiment was run.

### Administration boundary

Preparation preserves authoritative session state on failure, not arbitrary
external effects from executable initializers. Synchronous durable `Model.call`
requires an actor and therefore fails closed during detached preparation;
initializers must handle that error or defer the call until a later event.
These restrictions and the possibility of precommit filesystem/tool effects are
explicit in the [operator guide](../agent-server/operations.md#reset-rebuild-and-upgrade-initializers).
They are not a promise of transactional rollback for tools. Implementation
details and test scope are in the [administration notes](admin-remediation-notes.md).

### Remaining boundaries

Sanitized shell progress deliberately supports a conservative filter subset,
rejects all after-interceptors, and combines transient stdout/stderr into one
redacted stream. Finalized canonical output behavior is unchanged. See the
[streaming contract](../guide/chatmd-shell-security.md#sanitized-live-progress).

Older separately tracked MCP notification/catalog refresh, forced refresh after
401 rejection, interactive OAuth callback, transient stdio RNG, patch rollback
and artifact-privacy limitations are not silently declared fixed by this work.
The [broader audit](code-documentation-audit.md) and relevant user guides retain
those boundaries. A passing link/example check or finite regression suite is
not formal proof of semantic completeness or universal runtime correctness.
