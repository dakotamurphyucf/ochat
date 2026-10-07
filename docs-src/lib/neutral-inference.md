# Selected inference and host-owned execution

Every supported conversational route receives an immutable selected context.
`Inference.Request.Target` records a nonsecret adapter/profile/account/endpoint
reference, model and effective settings. It never contains a credential or grants
permission. `Inference_runtime.Context` binds that target to the actual adapter;
`Inference_client.Identity` supplies host-owned attempt and accounting identities.
The OpenAI implementation is `Openai.Inference_adapter`, using the existing
[Responses driver](openai-responses-driver.md).

## Preparation, dispatch and completion

The turn owner prepares the final canonical history, tools, assets and guidance.
Preparation is pure: it validates the complete request and captures its effective
configuration and private admission fingerprint. No authentication or network
request happens here. A prepared value contains its adapter implementation; a
caller cannot pair it with another profile at dispatch.

`Inference_client.run` performs final host admission, allocates an actual scope,
and invokes the required attempt acknowledgement before running the attempt.
Each attempt is single use. Authentication resolves only at dispatch under the
caller's Eio cancellation scope. Credential ownership remains with that host.
There is no automatic retry or provider fallback after uncertain submission.

The required completion callback reports the attempt's safe terminal or actual
interruption independently of rendering callbacks. It never receives raw provider
text. A returned completion callback failure propagates without a second callback;
when execution already raised, interruption reporting preserves that original
exception and backtrace even if the reporting callback also fails. Persisted
accounting integration consumes these ports separately; the initial application
composition supplies explicit untracked observers.

The adapter emits validated evidence, not durable conversation commits.
`Candidate_ready` identifies an output occurrence and its exact canonical
payload. Eligibility permits the existing local admission/tool path to inspect
a call; it never grants execution authority. Unsupported callers, namespaces and
asynchronous calls remain evidence and are not executed.

The existing turn driver reconciles repeated candidate evidence, admits history,
executes registered tools through existing moderation and permission checks, and
publishes host-finalized entries. Response-array ordering is available in the
receipt; concurrent local tool completion keeps its existing completion-arrival
ordering. Provider aliases do not replace host-owned history IDs or call bindings.
A provider terminal ends one inference attempt. It does not finish a workflow,
complete pending jobs or assert a successful host turn commit.

## Captures and settings

Authored semantic history lowers directly through the selected adapter. Actual
wire captures retain unknown fields, exact strings and absent/null distinctions.
Opaque replay requires declared support, exact captured origin (including model)
and a fresh cross-check of the raw item against its semantic projection. A model
change does not itself authorize replay of another model's opaque captures. There
is no silent lossy fallback when replay is incompatible. Reconstructed legacy DTO data stays
explicitly reconstructed; loading it does not establish actual wire provenance.

Root configuration is captured once, including explicit host defaults. A child
inherits omitted settings; explicit child overrides produce a separate target
without changing its parent's captured selection. Replay does not re-read current
profile defaults. Explicit root revision changes preserve unknown target/setting
members while replacing the fields owned by the new root configuration.

Persisted sessions retain `Unresolved` when older data has no captured target.
Execution then requires an explicit host migration decision and durable capture;
loading a document does not silently choose a backend. Model jobs persist their
source selection and subsequently their recipe-effective execution selection.
Retries use that captured selection. Delegated children capture their effective
target before reservation and restore it independently of later parent changes.

## Initialization and recovery

A new or rebuilt runtime persists its chosen configuration and a private
`Pending { fresh_history }` state before script initializers can invoke a model.
Only successful host initialization changes this to `Ready`. Completion checks
its stable basis and overlays initialized history, moderator state and shell
reviewer checkpoints onto the current actor state. Jobs, reservations, shell
approval/manifest grants admitted during initialization survive. It does not
archive the session a second time.

Administration holds the runtime-owner fence through retiring old resources,
committing the selection, initializing the replacement and retiring it when the
session is stopped. Cleanup cannot enter between the commit and initialization.
An initialization failure does not roll back the committed configuration. Authored
read-file roots are resolved against the candidate runtime paths before that
commit. This preflight does not run scripts, discover MCP tools or promise that
subsequent initialization cannot fail; generated definitions retain their existing
admission checks.

An actor-issued, process-local initialization scope permits synchronous
`Model.call` during Pending initialization, including creation of a stopped
session. It owns the exact admitted job IDs, generations and attempts. Recipe
target capture and completion validate that ownership in the actor mailbox. An
authorized Stop revokes the scope even when the session is already stopped; an
unauthorized Stop does not. The factory ends the scope on every exit and interrupts
residual synchronous attempts while preserving queued asynchronous jobs. The
ordinary scheduler never uses this scope to activate a Pending runtime. Each
actual recipe inference rechecks that exact job and scope before the existing
strict attempt observer and provider dispatch; derived contexts retain the check.
A Stop after this admission can race an already admitted request, so cancellation
and completion continue to preserve submission uncertainty.

Failure retains the durable chosen configuration and actual admitted effects.
Recovery keeps Pending runtimes unloaded until explicit activation; it never
infers successful initialization from an empty transcript or automatically repeats
an initializer after a crash. These controls do not broaden stopped-session job
permissions or bypass the existing foreground/background ownership rules.

## Auxiliary requests and compatibility

`Inference_client.Execution` carries the same selected context and host ports for
summarization, prompt evaluation, grading, shell model review and other no-tool
requests. Model omission inherits the selected target. Its text helper removes
`tool_choice` and replaces an existing structured text format with plain text for
that auxiliary request, while preserving verbosity and the original selection.
Use `Execution.run` for intentionally structured completions.

Compaction reads canonical semantic history directly. Retained entries preserve
their complete payloads and IDs; an actual edited result or newly produced reminder
is Authored. Bound host occurrences keep parallel calls/results together even
when a provider reuses a call alias. Summary protocol failures retain the existing
three-attempt/one-bisection policy. Expected failures return errors; strict
observation callbacks and cancellation propagate without partial summary success.
Offline tests use explicit injected requests, never missing-key fallbacks.

The standalone TUI captures its session cache key and existing model-list retention
heuristic once with a new target, persists them before initialization, and preserves
resumed captured settings. This heuristic is host policy, not a support catalog.

Explicit session compaction also works while the runtime is unloaded. Its host
port captures the admitted operation and complete persisted selection, then runs
outside the actor mailbox after worker readiness. A cancellable ownership lease
retains an existing selected execution or resolves the exact captured target for
a short-lived auxiliary execution. It does not load the agent runtime, rerun
initializers, or select an alternative provider. Stop and administration join the
lease before releasing resources; primary errors survive cleanup failures.

Typeahead stays opt-in and requires an explicitly supplied execution. Its standalone
host derives a response-limited adapter without changing the captured target,
profile, credentials or identity allocator. Raw response/frame bounds can only
tighten; the existing deadline and insertion bound remain separate controls.
Typeahead is opt-in private editor inference. The CLI supplies a bounded selected
execution for embedded sessions and explicitly selects a client-local host from
its configured `API_URL`, credentials and suggestion model for attached sessions.
The remote conversation remains daemon-owned; private suggestions do not infer or
forward the daemon's target or credentials. Library callers without an explicit
typeahead execution report unavailable. Off mode creates no auxiliary execution.

The old `Chat_completion.run_agent` and `run_completion` executors are retired and
reject before filesystem, network or tool effects. Their old DTO conversion
helpers are not a second inference dispatcher. Embeddings remain a separate API
family. The low-level provider APIs are not implicit fallbacks for session routes.

## Evidence and diagnostics

Receipts distinguish a complete response array from an observed prefix. Usage
components separately record actual, estimated or unknown values; authoritative
revisions replace earlier observations rather than adding a second charge.
Configuration observations expose a closed safe vocabulary, omit private endpoint
and instruction values, and retain effective provenance. Provider completion,
host interruption and delivery uncertainty remain distinct.

Focused qualification uses a non-OpenAI-shaped synthetic adapter for neutral
execution, and loopback HTTP/SSE fixtures for the real OpenAI adapter. Tests cover
strict callbacks, cancellation, candidate reconciliation, exact raw replay,
configuration presence/provenance, output limits and independently selected
auxiliary requests. No live model or external credential is required.

## Entry-point inventory

All supported routes reach the same selected runtime. The following entry points
compose it with the existing host's tool, history and lifecycle owners.

| Route | Entry and ownership |
|---|---|
| CLI and headless | `Driver.run_completion` / `run_completion_stream` require selected context and tracking ports before constructing initializers. |
| Standalone TUI | `App.run_chat` reuses captured selection; `App_streaming` and `App_compaction` receive the same selected services. |
| Daemon, local stdio and embedded TUI | `Inference_composition.daemon_options` supplies explicit host policy; `Session_factory` resolves the persisted selection. |
| Generated/authored children and descendants | `Runtime_builder` receives the child's independently captured selection; `Driver.run_agent` applies only explicit nested prompt overrides. |
| ChatML `Model.call` / `Model.spawn` | `Model_executor` derives the recipe's effective target and acknowledges capture before model-capable initialization; retries use the persisted job binding. |
| Forks | `In_memory_stream` isolates child history and carries the actual invocation parent. The built-in returns its last assistant entry's text; `Fork.execute_entries` retains its all-new-assistant-text contract. |
| Compaction and relevance | A host-owned auxiliary lease reuses a loaded selected `Execution` or resolves the captured target without loading the runtime; canonical compaction preserves retained entry IDs and payloads. |
| Meta prompting and refinement | `Meta_prompting.Context` and `Inference_support` require explicit selected execution for every online model step. |
| Typeahead | An explicit response-limited execution preserves the captured profile, credentials and identity allocator. |
| Shell model review | `Agent_runtime.model_completion` uses selected no-tool execution and reports the actual effective model. |
| MCP prompt execution | `Mcp_prompt_agent` requires context, identity and tracking ports; registered prompt handlers use that selected path. |

Canonical entry wrappers remain useful: `Response_loop.run_entries`,
`Agent_response_loop.run_entries`, `Driver.run_entries` and `Fork.execute_entries`
delegate to the shared engine. Legacy response/agent wrappers preserve their
existing fork-depth bound. Provider-shaped `post`/`post_stream` injection and the
old agent DTO-result executor reject explicitly before execution. Pure DTO
conversion and descriptor helpers do not dispatch inference.

The current `Runtime_builder.parse_user_content` authoring bridge still converts
plain-text and ChatMD inputs through the existing Responses DTO converter, then
immediately creates a canonical Reconstructed payload. That local provenance is
preserved in storage; it does not select a backend or dispatch a model request.
Host producers that construct canonical content directly, such as new compaction
reminders, use Authored payloads.

The initial standard host has no remote image resolver. Local or already inline
image bytes are supported; an unresolved remote URL rejects before authentication
or network dispatch, including when it entered through reconstructed history.
A host can supply immutable assets explicitly through the selected request port.

The command entry points capture `API_URL` and `OPENAI_API_KEY` once at host
construction. `API_URL` retains the existing host/base-URL convention: a bare
host uses HTTPS, and `/v1/responses` is appended to the base path. Invalid
configured endpoints fail validation; they do not fall back to the public API.
