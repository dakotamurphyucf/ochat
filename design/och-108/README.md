# OCH-108: unified command and process contract

Design deliverable for M7-R1. Source baseline: `58e9d2c5567d596fbe52ba1041e37d12dbde102b`. This document chooses the command/process boundary and refines implementation work; it does not implement the unified executable or claim that a headless workflow is complete. [Source evidence](source-evidence.md), [typed boundary draft](run_contract.mli), [validation](validation.md) and [follow-up proposals](follow-ups.json) accompany the design.

## Command surface and beta cutover

Install one public `ochat` command for product runtime workflows. With no arguments, print concise top-level help and exit successfully; never implicitly launch a terminal, GUI, daemon or model. Explicit commands make script and service lifetimes reviewable.

| Command | Ownership and purpose |
|---|---|
| `ochat daemon run --config FILE` | Own the daemon process/listeners and its shutdown. |
| `ochat daemon config check\|show --config FILE` | Validate or print redacted effective daemon configuration; no listeners or session creation. |
| `ochat tui [--connect URI \| --prompt FILE]` | Interactive client or explicitly selected embedded host. Existing terminal flags are mapped deliberately. |
| `ochat sessions list\|info\|create\|resume\|send\|cancel\|stop\|export …` | Shared session/operator services; an explicit session stop is distinct from cancelling an operation or finishing a run. |
| `ochat run --mode single-turn\|workflow …` | Bounded headless observer of a host-owned run, targeting one existing session or captured ChatMD definition. |
| `ochat stdio --local … \| --connect URI` | Session protocol host or gateway; stdout is protocol frames only. |
| `ochat authoring …` | Shared diagnostics/check/test/trace/REPL services when available; no independent checker. |
| `ochat store inspect\|convert …` | Explicit universal-document inspection/conversion only when implemented; no implicit destructive migration during startup. |
| `ochat internal agent-helper` | Reserved invocation-scoped inherited-channel adapter, not a general management client. |

Keep useful existing `index`, `query`, `tokenize`, `html-to-markdown`/`h2md` and `shell` operations during the beta cutover, with their documented ownership. The indexing/embedding API family is separate from conversational providers. Developer examples, rendering/debug utilities and indexer binaries do not all need to become product commands. Record their keep/developer-only/retire disposition in the installation inventory. Maintained MCP tool integration does not select the deprecated prompt-serving MCP host for this cycle. Record that existing host's disposition separately; no new required `ochat mcp` server follows from this design. If retained temporarily as a utility, its protocol remains explicitly selected and is never autodetected from session JSON-RPC.

Use a documented beta cutover instead of indefinitely maintaining duplicate command parsers. Remove public `chat-tui`, `ochat-agent-server` and `ochat-agent-stdio` installation names once their supported workflows are covered by `ochat`; publish flag/command mappings and actionable errors. The public helper name is retired only after the confined-artifact migration passes. Developer/test stanzas may remain as test harnesses but must not become a second installed product surface. No frozen legacy storage readers follow from executable migration; required universal document conversions own data compatibility.

All command spellings above are the selected hierarchy for downstream implementation. Exact authoring/operator subcommands are refined by their service owners rather than manufactured here. Ordinary CLI/help/service operation must have no GPUIO runtime dependency. Desktop presentation remains separately gated and outside this ticket.

## Parse, initialize and dispatch

Create pure command parsing and validated execution plans first. Help/version, parser errors and internal-helper selection precede configuration loading, credential resolution, source capture, filesystem/store creation, listener setup and terminal initialization. Help/version use immutable build metadata, bounded output and no provider calls. A missing or unreadable default configuration cannot prevent help. Do not use help flags as an escape from otherwise ambiguous dispatch; parse the selected command consistently.

Move module-level runtime effects behind explicit entrypoint calls. An early branch in `main` alone is insufficient: OCaml initializes linked modules before that branch. Current provider modules capture environment at module initialization and daemon entry code initializes cryptographic RNG before command parsing. Extraction must audit transitive module/native startup effects, not merely remove the old final `let ()`. Read-only module metadata is different from fetching credentials or initializing a host. RNG initialization belongs in the host-start path before allocating IDs, not in help dispatch.

Entrypoint libraries receive explicit Eio capabilities and validated plans, return typed outcomes and never call process exit. The thin executable maps an outcome to output/exit status once. Reuse daemon/embedded composition, session actors, current operation workers, moderation/admission and native tool registration; do not introduce another executor or scheduler. Session event reduction has one owner per connection; headless/TUI consumers must not compete for notifications.

## Configuration and identity

Separate client transport selection, runtime-host configuration and captured session execution settings. `--connect` and embedded local options conflict rather than silently creating a local host after a connection failure. Daemon bearer credentials authenticate daemon access, not provider inference. Paths passed to a remote operation refer to the authorized host or an explicit upload/source reference; never assume client cwd is the daemon workspace.

Use the provider worker's execution precedence: explicit admitted execution/session override, captured ChatMD setting, referenced profile defaults, adapter defaults. Preserve unresolved provider-owned omission instead of inventing a local value. Preserve absent/null/value semantics through universal documents. Resolve credentials at host dispatch against the captured profile/account identity and opaque owner generation; no access token belongs in CLI plans, persisted session settings or fingerprints. Pending configuration affects the next preparation, not an immutable already-admitted request.

CLI control defaults use explicit argv over a selected command-specific nonsecret config over built-in defaults. Do not inject one flat TUI argument file into daemon/headless commands. Explicit config is strict; an absent optional default config is allowed. Repeated scalar/conflicting mode flags are rejected or resolved by documented typed precedence, not generic token concatenation. `config show` is redacted and provenance-aware; it must not reproduce arbitrary config file bytes. Shared profiles/configuration remain their existing milestone owner's contract.

Keep session ID, session generation, run ID, operation/turn ID, request/retry identity and provider response ID distinct. Run identity binds a durable scope spanning one or more operations, not the last provider response or a session's current idle state.

## Single-turn and workflow runs

For a run that submits user input, require exactly one explicit source: `--message`, `--input-file` or `--stdin`; consume it once with a documented byte bound. Single-turn mode admits exactly one root user operation, including its native tool/provider follow-up work; independently requested later workflow turns are not silently counted as that single turn. A workflow can instead select explicit `--start-orchestration`, with no user-input flag, to admit the authored startup intent. This supports ChatMD orchestration and recurring fresh sessions without manufacturing an empty user message. Admission validates that the captured definition has an authorized startup policy. Missing input without that explicit selection is an error. A prompt definition supplies configuration/instructions, not implicitly a new user submission. Delayed owned results and denied continuation must remain visible rather than fabricating a complete answer.

Workflow mode requires a qualified ChatMD-selected orchestrator. Reusable ChatML policy owns continue/wait/finish; the host commits/adopts these actions at its existing safe boundary, enforces budgets/permissions/cancellation and consumes durable intent once. The CLI subscribes to state and output; it never sends its own “continue” prompts when the session becomes idle. CLI mode selection chooses the execution envelope and hard host limits, not a second policy engine. Reject `workflow` when the definition lacks the necessary lifecycle/control capability. Reject incompatible CLI policy overrides rather than replacing an authored moderator. CAP-R4 owns helper composition and exact action/library spelling.

Add a narrow shared host run record/terminal receipt. Current `Request_turn` and `End_session` do not express successful completion of a run while retaining its session. A run is admitted against expected session generation/source and records associated operation/intent identities. Continue schedules through existing receipts; wait commits the reason and authorized wake condition; finish commits a terminal result reference after owned work reaches the agreed boundary. Completion requires neither stopping the session nor waiting for unrelated jobs/schedules. Owned work must be settled or explicitly relinquished under authority; unresolved external effects cannot be declared successful merely to exit.

Terminal statuses distinguish completed, failed, cancelled, limited and interrupted. A lost client connection is a client outcome until the host run is queried; it cannot mark the run failed/successful. Commit idempotent terminal state before publishing its event. Same-request retry returns the existing receipt; a conflicting terminal transition is rejected. Restore/reconnect reconstructs from local documents and event sequence, never silently repeats a model/tool effect. Transient hosts promise only process lifetime; a durable embedded root retains data but does not run jobs after its process exits.

## Output, EOF and signals

| Surface | stdout | stderr / lifecycle |
|---|---|---|
| Help/version | Requested text/build version | Diagnostics only on error; no host startup. |
| Human run/CLI | Selected answer/result | Progress, permission/status diagnostics and failures. |
| `--output json` | One bounded versioned terminal/client-outcome object | No progress interleaving; explicit unknown/interrupted result. |
| `--output jsonl` | Ordered versioned run/event records and one terminal/client-outcome record | Diagnostics stay separate; bounded backpressure and stable sequence IDs. |
| Session stdio | Only session protocol frames | Diagnostics never contaminate protocol output. |
| Helper | One bounded JSON response after disclosure checks | Fixed errors; no config/help/progress chatter. |

Do not reuse provider SSE events as the stable command output schema. Include run/session identity, committed terminal status, permitted result/artifact references and structured error/retry classification; secret-bearing fields and hidden reasoning are excluded. Large outputs remain admitted bounded references. Explicit refusal/incomplete/provider failure is not translated into fabricated success. Recommended process exits: 0 completed/successful inspection, 2 command/config/admission error before dispatch, 1 committed work/service failure, 3 host limit/interrupted or uncertain client outcome, 130 cancelled by SIGINT, 143 terminated by SIGTERM. Structured state remains authoritative; exit codes alone cannot distinguish every failure.

EOF completes one-shot input collection; it is not run completion. Closing session-protocol stdin closes that connection. A local process-bound stdio host then closes its host; a gateway detaches without stopping a detached daemon session. A headless client disconnect/closed output is handled as detach with an explicit uncertain client outcome unless a stop-on-disconnect ownership policy was selected. EPIPE cancels output consumption and releases the attachment; it must not trigger an automatic resend.

First SIGINT requests cancellation of the owned operation/run and waits for bounded committed observation; a second interrupt forces local client exit and reports unknown if confirmation is missing. SIGTERM performs bounded ownership-specific cleanup, without deleting saved data or stopping unrelated daemon sessions. Daemon SIGINT/SIGTERM invokes its own graceful shutdown; SIGHUP requests validated daemon config reload only. Do not treat SIGHUP as a global reload of source/provider credentials. Headless noninteractive permission requests follow existing policy and cannot widen authority; unresolved intervention is surfaced as waiting rather than blindly approved.

## Internal helper migration and completion gate

Keep inherited request/response descriptors 3/4, framing/limits and host invocation checks. No socket, environment token, credential fallback or argv caller identity is added. The operator grant pins the final executable digest, exact arguments, roots, environment and permission ceiling. `internal agent-helper` dispatches before normal config/host/terminal initialization, but still requires the inherited channel. Its external request cannot select another privileged mode.

Run the actual final installed artifact under existing confined grants. Verify argument/hash changes, revocation before disclosure, scope expiry, cancellation/reaping, descriptor closure, no network, bounded framing and required read roots. A larger binary's transitive initialization may invalidate confinement; do not widen permissions to hide that defect. If safe internal dispatch cannot meet the existing ceiling, retain the previously qualified private helper temporarily and report the unified-artifact migration incomplete. This is a documented beta migration limitation, not permission to claim OCH-111 complete.

Existing process checks establish reusable boundaries, not completion of the new executable. OCH-110 extracts effect-safe entrypoints; OCH-111 qualifies the confined final helper; OCH-112 implements headless observers after the shared run record exists. One distinct generated task owns that shared host record; CAP-R4 remains lifecycle policy/library owner. [Follow-up proposals](follow-ups.json) transfer these blockers before this research ticket closes.
