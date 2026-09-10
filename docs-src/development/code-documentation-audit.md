# Code-to-documentation audit

## Seven final findings: authority, artifacts and documentation (September 6, 2026)

This follow-up addresses the seven findings from the latest source/docs review;
the previously excluded legacy findings remain excluded.

| Finding | Correction |
|---|---|
| HTTP authority binding | Compare principal ID, authentication kind, scopes and attributes on connection reuse for RPC, connection SSE and deletion. A rejected reuse does not close or modify the original connection. |
| Pinned source integrity | Create new artifact files as 0400; verify materialized file inventory/digests and reject symbolic links before parsing or constructing runtimes, including cached revisions. Parse imports/scripts from captured bytes without filesystem fallback. Existing artifact permissions are not automatically migrated. |
| Replay/retention contract | Correct initialization to advertise configured durable replay capacity. Explicitly revise Protocol 1.0 specs/guides to durable-event replay plus snapshots; no retained live-delta replay cursor/API is introduced. `completed_stream_ms` only supplies an omitted `response_artifact_ms` default, not completed-delta retention. |
| Observer tutorial | Generate separate credentials for the same principal with restricted observer scopes; document creator-based visibility and separate logical connections. Add a generated-credential dispatcher workflow to the opt-in docs gate. |
| Runtime paths/imports | Explain artifact-relative prompt/source directories, uncaptured assets and root-contained imports/scripts, including validation versus catalog availability. Reject absolute captured imports/scripts instead of permitting an external read during materialized parsing. |
| Config reload | Document one-second mtime polling, explicit SIGHUP, unchanged/backdated timestamps, prompt-only edits, rejected candidates and per-prompt unavailability. Add a polling regression. |
| History roles | Document coarse outer `system` classification versus exact developer payload/ChatML role. Test the actual developer payload delivered to a restricted observer. |

### Boundaries

File modes and load-time hash checks are not continuous filesystem monitoring or
an OS sandbox against the daemon account. Keep its data root outside tool write
authority. Directories stay owner-managed so pruning can remove artifacts.
Absolute/dynamic nested agent dependencies remain explicitly external; this work
does not snapshot workspaces or make arbitrary external assets immutable.

The replay item is a deliberate contract correction, not a claim that the
previously promised live-delta replay feature was implemented. Complete durable
state and active-call snapshot reconstruction remain the recovery mechanism.

### Verification

Verification completed:

- `dune build -j 2 @all @runtest @agent-docs-check @agent-e2e-security
  @agent-e2e-prompts @agent-e2e-transports` passed (exit 0).
- Focused store, config/catalog and server-gap runners passed: 35, 20 and 8 tests,
  respectively. The cached-runtime regression checks the actual integrity error,
  not merely that some unrelated request error occurred.
- All 15 auth-security cases and the security matrix passed, including complete
  connection-authority checks. All nine prompt-lifecycle cases passed; Unix,
  HTTP, stdio and cross-transport conformance scenarios passed as well.
- The docs gate passed for 297 pages and 38 protocol methods, including the new
  generated-credential observer workflow and developer-payload assertions.
- Formatting checks for touched OCaml files and `git diff --check` passed.

The initial raw-SSE regression omitted authentication headers; those were added.
The broader prompt suite then exposed an old corruption fixture attempting to
truncate a newly read-only artifact. It now replaces its own fixture file before
testing rejection; production permissions were not weakened. Both corrected
tests passed on rerun. No failing run is counted as a pass. No paid provider,
manual TUI or additional soak testing was performed in this follow-up.

## Selected library-contract follow-up (September 6, 2026)

The user selected findings **3–9** from the latest review. Findings 1–2
(deprecated BM25/Merlin paths) and 10–12 (older provider/GitHub/remaining
inventory gaps) are explicitly out of scope. No retirement/removal or claim
that those paths have no callers is implied. Older audit records below retain
their own scope and verification history.

| Finding | Disposition |
|---|---|
| 3 — binary records | Reads/maps now retain write order. EOF is accepted only at a record boundary; callback exceptions propagate. Single-value reads reject trailing data. Existing list-writer append semantics are retained and documented, including `File.write_all`; the deprecated BM25 publisher is not changed. |
| 4 — instruction roles | Legacy compatibility steering now creates developer-role input. Moderator overlay prose now agrees with its developer-role implementation; the language guide distinguishes legacy steering from native/daemon canonical deferred input. Explicit authored ChatMD system messages are not rewritten. |
| 5 — crawlers | Documentation and source/interface comments now describe actual per-directory limits, post-read size filtering, symlink following, error propagation and Odoc callback suppression. This is a contract correction, not crawler hardening. |
| 6 — catalogue | Markdown catalogue saves now use exclusive private per-writer temporary files and rename. Load propagates cancellation. No fsync or atomic read/modify/write transaction is claimed. |
| 7 — logger | Corrected failure, blocking I/O, duplicate-key, permissions, heartbeat timing and cancellation descriptions and examples. A regression exposed permanently poisoned locking after write failure; the serialization lock now recovers. Individual I/O failures still propagate. |
| 8 — source spans | Reversed/out-of-range slices are safe; merge supports overlapping spans and either order. Legacy blocking file loading is explicitly qualified; new examples use in-memory values/Eio loading. |
| 9 — examples | Corrected Environment, Source, web tool dispatch, Odoc invocation and Chat Completions record examples. Five compiled source excerpts are checked against the Markdown verbatim; provider/network functions are not invoked. Transport behavior in findings 10–11 is untouched. |

### Compatibility and remaining implementation boundaries

- Consumers relying on reversed binary lists or ignored truncated tails must
  adapt. List writers still append, preserving compatibility; no general
  transaction, recovery or input-size policy was introduced.
- Catalogue rename provides atomic visibility, not fsync durability. Concurrent
  read/modify/write updates may lose changes; abrupt termination may leave a
  private temporary file. A corrupt catalogue still loads as `None`.
- The crawlers need a separate design for global memory/concurrency limits,
  bounded reads, cycle/confinement policy and consistent cancellation. Odoc
  callbacks currently swallow failures; a successful crawl is not proof that
  every callback succeeded. These are not silently marked fixed by corrected docs.
- `Log` still uses blocking channels and can fail application work, including
  masking callback errors in `with_span`. Replacing it with a capability-owned,
  explicitly configured Eio sink requires a caller/API migration and a deliberate
  policy for logging failures. It is not the agent server's durable audit store.
- `Source.from_file` remains the legacy blocking constructor. Use Eio loading
  plus `Source.make` when no originating-filename metadata is needed. A future
  capability-taking constructor would require an explicit API addition/migration.

### Verification

The new ordinary `test/library_contract_audit` runner covers binary order,
append compatibility, every partial prefix of a sample record, trailing bytes,
callback EOF, extreme span offsets, old-reader catalogue visibility, failed and
concurrent publication, private permissions and temporary cleanup. Isolated
child-process probes also verify the documented logger failure/heartbeat behavior
and the different crawler callback contracts without changing the parent's cwd.
The logger failure probe also exposed poisoned-lock persistence; recovery after
repairing the destination is now a regression assertion rather than an ignored
failure. This does not implement a new logging sink or suppress individual errors.

The existing offline streaming test asserts developer-role compatibility
steering. The opt-in docs gate compiles five corrected excerpts, checks source
parity, exercises pure examples/tool registration and does not call a provider.
Verification completed:

- All eight library-contract regression groups passed directly.
- `dune build -j 2 @all @runtest @agent-docs-check` passed (exit 0).
  The initial combined rebuild hit the existing shell-stream audit's 30-second
  timeout; that test subsequently passed standalone and in the reduced-parallelism
  combined rerun. No timeout was increased or test waived.
- The docs gate passed for 297 pages and 38 protocol methods, including the five
  compiled excerpts, without live provider calls. Navigation checks found all
  297 pages reachable within three links from either README.
- Formatting checks for touched OCaml files and `git diff --check` passed.

## Previous library-reference follow-up

The subsequent ten-finding library-reference follow-up is recorded in
[library-reference remediation](library-reference-remediation.md), including
OAuth credential isolation and atomic legacy snapshot publication. Web-fetch,
legacy-maintenance and utility hardening boundaries are explicitly listed there;
documentation corrections do not silently mark those runtime limitations fixed.

The preceding cross-check and authorized corrections are tracked in the
[final audit remediation record](final-audit-remediation.md). Its status and
verification supersede older completion claims only for the findings it names.

This review follows the exposed feature surfaces from source to user guides,
checks the proposed beginner README's navigation, and corrects discrepancies.
The beginner-friendly README has now replaced the detailed original. See the
[README content audit](readme-content-audit.md) for the historical comparison.

## Coverage map

| Surface inspected | Source anchors | User documentation |
|---|---|---|
| Installed executables and utility subcommands | `bin/dune`, `bin/main.ml`, executable option parsers | [Command index](../bin/README.md) |
| ChatMD messages, configuration, imports, tools and path context | `lib/chatmd/`, `lib/chat_response/converter.ml`, `tool.ml` | [Language](../overview/chatmd-language.md), [tools](../overview/tools.md) |
| All currently dispatched built-in tool names and aliases | `lib/chat_response/tool.ml` | [Built-in catalog](../overview/tools.md#built-in-catalog-code-correct) |
| Agent tools, forks and maintained MCP tool integration | `tool.ml`, `response_loop.ml`, `in_memory_stream.ml`, `mcp_discovery_cache.ml` | [Tool reference](../overview/tools.md) |
| ChatML language and runtime capabilities | `lib/chatml/`, especially `chatml_builtin_spec.ml` | [ChatML index](../chatml/README.md), language/runtime references |
| Shell runtime modes, declaration grammar, authorization and tooling | `lib/chatmd_shell_spec/`, `lib/shell_runtime/`, `lib/shell_access/` | [Shell index](../shell/README.md) and references |
| Native, legacy and connected TUI; editing, streaming and type-ahead | `bin/chat_tui.ml`, `lib/chat_tui/app.ml`, `app_reducer.ml`, `controller.ml`, `type_ahead_provider.ml` | [TUI guide](../guide/chat_tui.md), [TUI CLI](../bin/chat_tui.doc.md) |
| Agent config, lifecycle, permissions, protocol and transports | `lib/agent_*`, executable adapters | [Agent-server index](../agent-server/README.md), generated contracts, tutorials |
| Markdown and odoc retrieval; rebuilds, filters and embedding settings | `bin/md_*.ml`, `bin/odoc_*.ml`, indexer libraries, `lib/openai/embeddings.ml`, `lib/embed_service.ml` | [Search guide](../guide/search-and-indexing.md), command/library references |
| Prompt factory and recursive refinement | `bin/mp_refine_run.ml`, `lib/meta_prompting/` | [Refinement CLI](../bin/mp_refine_run.doc.md), [library](../lib/meta_prompting.doc.md) |
| Interactive OAuth helper settings and boundary | `lib/oauth/oauth2_pkce_flow.ml` | [PKCE helper](../lib/oauth/oauth2_pkce_flow.doc.md) |
| Contributor and lower-level library navigation | `lib/` interfaces and existing sidecars | [Library index](../lib/README.md), [project overview](../overview/project.md) |

This is a surface-oriented audit, not line-by-line formal verification of every
implementation or every historical API example. Literal option/environment
scans detect missing mentions, not incorrect semantics; source inspection and
targeted offline runs provide the additional checks below.

## Corrected documentation

- Missing refinement strategy flags, defaults and precedence; task-file
  optionality; output append behavior; removed unsupported fixed truncation and
  nonexistent installed `mp-prompt` claims.
- Historical `gpt`/`mp_prompt` source pages distinguished from installed commands;
  added `highlight-debug` and `terminal_render` usage and limitations.
- Stale `dsl_script` example/output and non-library `open Dsl_script` advice
  replaced with the current five hard-coded demonstrations.
- TUI workspace text scoped to local mode; mid-stream steering corrected to
  safe-boundary admission instead of modifying an already sent provider request.
- Type-ahead API, actual model, input bounds, return handling, logging, and host
  availability documented; corrected the misleading bound in its interface prose.
- Embedding model default corrected to `text-embedding-3-large`; absent-key and
  explicit-stub behavior, environment timing, model consistency, and costs added.
- Markdown search catalog fallback, actual files, byte preview limit, successful
  empty results, and skipped unreadable indexes documented.
- Markdown indexing corrected from incremental/memory-mapped append claims to
  re-embedding and serialized replacement; extensions, 10 MiB post-read check,
  empty-crawl behavior, stale snippets, and best-effort ignore rules clarified.
- Replaced invalid `ochat md-index`/`ochat md-search` library examples with the
  actual standalone commands.
- Odoc CLI's ignored `--beta`, dense-only search, fixed five-package shortlist,
  per-invocation loading, and stub behavior corrected. Indexer's hard-coded update
  subset and partial-batch behavior made explicit.
- Embedding service concurrency, per-instance dispatch throttle, retries (four
  attempts), return value, and lack of persistent cache corrected.
- OAuth helper browser-suppression variables, wildcard listener, launcher errors,
  callback EOF wait, and lack of a general hardened callback contract clarified.
- Added browsable library references so lower-level features are not discoverable
  only through a generated audit ledger.

## Implementation issues surfaced, not fixed by this documentation work

1. **Transient standalone local stdio startup — subsequently fixed:** this audit
   reproduced RNG initialization failure before protocol initialization. E07 host
   integration moved RNG initialization before transient-root allocation. The
   cold-executable `stdio.local-transient-bootstrap` regression now checks startup,
   process-bound transient metadata and root cleanup on EOF without a data root.
   See [details](../agent-server/troubleshooting.md#local-stdio-rng-initialization).
2. **Type-ahead parity — implemented:** all three TUI modes now share the
   coordinator and client-local provider, default off, with explicit model/history
   settings and bounded no-log transport. See [behavior](../guide/chat_tui.md#type-ahead-availability-and-privacy)
   and the [verification record](typeahead-verification.md). Human visual
   verification remains separately recorded; documentation checks alone do not prove it.
3. **Interactive OAuth helper:** source inspection found wildcard binding, no
   state/nonce validation, and an EOF-based callback read. This is separate from
   daemon static bearer authentication. No browser-based login was attempted.
4. **Utility limitations:** the odoc CLI's narrow package filter and ignored
   search beta, successful empty/partial retrieval outcomes, and the bitmap
   utility's missing-argument guard are implementation limitations, not repaired
   merely by explaining them.

Do not interpret a passing documentation check as resolving these issues or as
proving complete host parity. Keep them visible when deciding subsequent code work.

## Follow-up semantic review

The follow-up review corrected these additional claims without changing runtime
code:

- `apply_patch` is not an atomic multi-file transaction. Preparation precedes
  mutation, but callback writes/deletes are sequential and have no rollback.
  Its library guide now also describes aggregate preparation memory rather than
  claiming a largest-file-only streaming bound, and uses current OCaml examples.
- File-backed `chat-completion` appends a supplied `-prompt-file` on every
  invocation; it does not deduplicate or prepend it to existing history. Omit
  the flag when continuing an existing transcript.
- That runner parses the output transcript as the root source, so its
  `${prompt_dir}` and relative dependencies use the output directory, not the
  original template directory. This is different from agent-host source pinning.
- A temporary transcript or `/dev/stdout` does not disable `.chatmd` caches,
  tool payload files or provider response logging. Removing the transcript is
  not complete cleanup or a privacy mode.
- The tools overview now qualifies workspace selection and parallel-tool flags
  by host: connected creation accepts a configured workspace name, while the
  parallel-tool CLI controls are legacy-local only.
- The MCP wrapper example now uses `run` with serialized JSON and handles the
  typed output. Notification printing was disabled in implementation, not an
  available stdout progress feed. Provider `strict` metadata is not local
  JSON-Schema validation.

### MCP discovery and notifications

**Open implementation defect: competing notification consumers.**
[`Mcp_client.notifications`](../../lib/mcp/mcp_client.ml) returns the client's
same `Eio.Stream` queue. The host's
[`register_invalidation_listener`](../../lib/chat_response/tool.ml) reads that
queue, but each [`Mcp_tool` wrapper](../../lib/mcp/mcp_tool.ml) also starts a
reader that discards notifications. Thus `notifications/tools/list_changed`
can be consumed without invalidating discovery. Per-client cache identity
isolation remains intact; reliable notification delivery does not follow from it.
This concerns maintained outbound MCP tools, not the deprecated prompt-serving host.

Fix with one notification owner and explicit routing/fan-out, removing the
per-tool discard readers. Add a deterministic integration regression with several
wrapped tools and repeated list-change notifications; verify invalidation,
progress routing if provided, cancellation and runtime shutdown. Cache-only
unit tests cannot establish correct shared-client wiring. The defect was found
by source inspection, not reproduced in a dedicated runtime test in this review.

**Open functionality gap: active catalog refresh.**
`Tool.mcp_tool` loads descriptors and constructs wrappers during runtime creation;
`Agent_runtime` retains that function list. Expiry/invalidation reloads only on
another cache access and does not rebuild active names or schemas. Documentation
now explicitly requires runtime recreation after catalog changes. Implementing
hot refresh requires a host safe-point contract for new/removed tools, schema
changes and in-flight calls, plus tests that inspect the next provider request
and dispatch table. Do not represent a five-minute TTL as periodic hot reload.

### Other implementation limitations to retain

- **Patch failure recovery:** current sequential application is documented,
  not repaired. If transactional patching is required, design staging/rollback
  and crash behavior separately; test injected failures after an earlier write
  and between a move's destination write and source deletion. Sources:
  [`apply_commit`](../../lib/apply_patch.ml),
  [filesystem callbacks](../../lib/functions.ml), [I/O helpers](../../lib/io.ml).
- **Batch source provenance:** documenting output-relative parsing does not
  preserve a copied template's original source identity. Changing this would
  require a deliberate compatibility design for resumed transcripts, imports,
  file roots and shell manifest identities, not just changing a path argument.
- **Artifact privacy:** no zero-artifact switch was added. Any future mode must
  cover driver caches/tool payloads and underlying provider logs together.

Follow-up validation reran the opt-in documentation checker. No live provider,
external MCP server, user store or browser authentication was used. Correcting
these docs does not close the implementation gaps above.

## Verification performed

The following subsection records the earlier audit's runs; the final review
addendum below has its own verification scope.

- Built the affected stdio, Markdown index/search, refinement and ChatML-demo
  executables from the current tree.
- Scanned literal dashed CLI flags in `bin/*.ml` and literal environment reads
  in `bin/` and `lib/`: no names remained without a documentation mention after
  corrections. Names assembled dynamically require separate inspection.
- Ran standalone local stdio discovery with no provider key: no-data-root mode
  reproduced the startup failure; a private explicit data root completed all
  five discovery requests and exited successfully at EOF.
- Ran local template-factory generation with no API key and no task file:
  succeeded and wrote a prompt pack, confirming actual optionality/local mode.
- Ran `dsl_script` with no API key: all five embedded demonstrations completed.
- Ran stub-mode Markdown search against an existing empty index root: diagnostic
  and success; a missing index directory instead produced a runtime error.
- Built a private one-document Markdown index with stub embeddings and retrieved
  its snippet through `md-search`; indexing and retrieval both exited successfully.

All ad-hoc state was isolated under a freshly created private `/tmp/ochat-doc-audit.*`
directory. No live provider, soak, browser authentication, or user-assisted TUI
session was run. Offline stub vectors verify wiring, not semantic retrieval quality.

The opt-in [documentation checker](../agent-server/testing.md) verifies the
protocol/config contracts and offline example plumbing; the navigation check
also covers the draft and newly added topic pages. Historical sidecars remain
subject to future source/API-example review; no finite audit proves that every
claim throughout the repository is error-free.

## Final review implementation gaps

The seven findings in this section are **resolved by the subsequent runtime
implementation**, not merely documentation edits. The original findings below
are retained as historical diagnosis. They do not describe current behavior.

| Finding | Implemented correction | Regression coverage |
|---|---|---|
| Authoritative deletion | `session.delete_history`, writer/revision/idle guards, protected idempotency, matching tool-pair removal and durable broadcast | Actor two-subscriber/read-only/stale tests; reused call-ID pairing; real daemon deletion/retry/restart |
| Native layout/destinations | Coalesced background initial/resize rendering, stale-result checks, Home/End/search handling and warm-width dirty chunks | 500-row resize/navigation/stream-update/late-close regression; TUI integration suites |
| Key ordering | Meta+Shift arrows duplicate lines instead of being consumed as movement | Decoded-event editor regression and undo |
| Unicode/undo | Extended-grapheme boundaries, Unicode input, per-action draft checkpoints and redo invalidation | Combining marks, joined emoji, ordinary typing/deletion, Normal x, undo/redo and unchanged typeahead acceptance tests |
| Legacy raw validation | Recoverable error, rejected draft restoration/notice, no turn/history mutation | Malformed and no-user raw input regression |
| Compaction settings | Eio JSON loading, strict fields/path precedence, resulting-context estimate, opt-in group grading | Config, budget/no-ID-allocation, filtering/default-no-grading and cancellation regressions |
| Compaction archives | Durable framed archive before journal commit; SHA-256 reference; authorized historical export | Archive-write failure and retained-history tests; real daemon export/restart/corruption/deletion independence |

Current contracts: [TUI](../guide/chat_tui.md),
[protocol](../agent-server/protocol.md), [layout](../lib/chat_tui/agent_history_layout.doc.md),
[configuration](../context_compaction/config.doc.md), and
[archive storage](../lib/agent_session/compaction_archive.doc.md).
The [worklog](documentation-worklog.md) records verification and its limits.

### Original findings (historical; resolved above)

1. **Native/daemon history deletion is not authoritative.**
   [`Controller_cmdline.execute_command`](../../lib/chat_tui/controller_cmdline.ml)
   calls `Model.delete_selected_canonical_entry`, which edits only local history.
   [`App.Agent_mode.handle_reaction`](../../lib/chat_tui/app.ml) handles the resulting
   `Refresh_messages` as a redraw, without a session mutation. The next projection
   can restore server state. Define an actor-authorized deletion contract or
   explicitly reject the command in these hosts; test two clients, read-only
   behavior, persistence, and projection restoration. Do not treat this as a
   server authorization bypass: no remote delete request is sent.
2. **Native/daemon destination-preparation/rendering parity.** The same handler
   treats `Prepare_chat_destination` as redraw-only. The shared controller emits
   this for Home/End, but the legacy reducer prepares the requested destination.
   Agent mode also uses synchronous relayout for projection damage/resize rather
   than the legacy background corridor service. Implement host-appropriate
   destination handling and test large history, resize, Home/End, search reveal
   and streaming; do not claim the existing native renderer has the legacy
   asynchronous pipeline. Ordinary scrolling and the tested small-history visual
   workflows are separate coverage.
3. **Insert key ordering.** In
   [`Controller.handle_key_insert`](../../lib/chat_tui/controller.ml), plain
   Ctrl+Up/Down scroll history. The broad Meta+Up/Down cases shadow intended
   Meta+Shift duplication. Fix specificity/ordering only after deciding the
   desired key contract, and regress actual Notty-decoded events. The docs now
   describe actual behavior, not the stale comments.
4. **General editor Unicode and undo coverage.** Ordinary Insert handles ASCII
   plus the special ß selection event, not general `Uchar` insertion. Horizontal
   movement/backspace can split UTF-8 bytes. `Input_display` measures terminal
   cells but cannot repair invalid edits. Ordinary Insert edits also omit
   `push_undo`, unlike typeahead acceptance and several Normal edits, so undo and
   redo invalidation are not uniform. Add grapheme/boundary-aware input and
   deliberate undo grouping, including non-ASCII input, deletion, multiline
   accept/undo/redo and editing after undo. Do not change typeahead's verified
   acceptance semantics as an incidental docs fix.
5. **Legacy raw-input validation.**
   [`App_submit.apply_user_submit_effects`](../../lib/chat_tui/app_submit.ml)
   catches parse errors into an empty list and then uses `List.find_map_exn`
   for a user message. Malformed/no-user input can therefore raise instead of
   returning a recoverable editor error. Agent runtime admission already returns
   a typed failure. Add legacy validation that preserves the draft and leaves
   canonical history untouched; test malformed XML, tool-only input and missing
   user messages without a live provider.
6. **Dormant compaction settings.** `Config.load` has a placeholder file reader,
   and `Compactor` does not use either config or relevance scoring. Documented
   as scaffolding, not working JSON configuration. Implement separately if those
   controls are desired; the current compactor has no resulting-context token cap.
7. **Pre-compaction archive reference.** Implementation-spec section 23.2 calls
   for committing an archived pre-compaction reference. The current actor's
   `compaction_terminal_delta` commits replacement history, generation, operation
   and lifecycle state, but no dedicated archive reference. Existing journals and
   fallback snapshots are retention-bound recovery data, not that contract.
   Either implement explicit archive storage/reference and retrieval tests or
   deliberately revise the requirement. The current user docs advise exporting
   before compaction; this does not satisfy the unimplemented archive requirement.

Previously recorded standalone-stdio RNG, MCP notification/catalog, OAuth, patch
rollback and artifact-privacy limitations remain open. The seven-gap runtime
follow-up does not silently close those separate findings.

## Final review documentation corrections and checks

- Reconciled TUI modes/pages, Plain `:edit`, user-only raw admission, actual
  quit/scroll keys, host-specific approvals, typeahead preview/status/undo, and
  rendering/destination host boundaries. Removed stale partial controller/type
  sketches that referenced nonexistent APIs or contradicted canonical identity.
- Replaced the outdated compactor/summarizer explanations: retained occurrence
  IDs and reminder counts, user-role summary, explicit error/cancellation,
  `gpt-5.6-sol`, independent output budget, parsing retries/two-part fallback,
  ordinary logging and non-archiving legacy behavior.
- Removed the nonexistent ChatML `Model.await` API claim and documented actual
  completion events and external job inspection.
- Added the complete shell action helper table, especially previously absent
  `Review.approve_for`, `Result.reject_disclosure`, and `Audit.drop_field`, with
  scope and protected-audit-field boundaries.
- Extended the opt-in checker to cover both root READMEs and all docs heading
  links; added heading-parser regressions for Setext, linked titles, explicit
  anchors, duplicate headings, closing hashes and fenced examples. Repaired
  stale links and supplied stable compatibility anchors where needed.
- Repeated literal CLI-flag/environment-name scans and shell builtin-name scans
  against authored documentation. These verify mentions, not semantics. The
  semantic review followed source call sites for commands, tools, ChatML,
  permissions, transports, persistence, compaction, and TUI behavior.
- Navigation inspection found all 291 current Markdown pages reachable from
  each root README, with user guides/references within three links. External
  URLs were not fetched and historical examples were not all executed.

No provider request, browser login, user-assisted TUI, soak, or external MCP
server was used in this final review. Verification results are recorded in the
[documentation worklog](documentation-worklog.md).
