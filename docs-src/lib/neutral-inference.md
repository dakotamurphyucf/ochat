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
accounting integration consumes these ports separately. Daemon-managed resource
graphs use the [retained observation ledger](inference-observations.md); standalone
host compositions may supply explicit untracked observers.

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

## Host provider profiles and captured intent

`Inference_host.Provider_profiles` composes the existing neutral `Target`, durable
`Selection`, OpenAI `Responses_driver.Profile`, and `Inference_runtime.resolver`.
It does not introduce another conversation selection, secret store or login flow.
Registry administration is trusted host configuration: imports and agent-selected
profile labels do not authorize add, edit, disable, remove or reauthorize.

A profile owns nonsecret adapter/account/endpoint identity, a credential binding,
and current declared capabilities/defaults. The binding records an adapter method
label and host credential reference. `api_key` and `oauth_subscription` are separate
billing modes. The latter denotes the selected direct Codex subscription lifecycle,
not arbitrary OAuth against the public Responses API. Credential references must
identify the full host issuer/client registration/audience/account tuple. Future
login integrations install that binding explicitly. Actual OAuth acquisition and
route-specific dispatch headers await OCH-66 qualification; this registry does not
claim a working subscription request flow. Credentials never fall back
across accounts, subscription/API-key modes or endpoints. Provider references and
credentials are unrelated to daemon access tokens and MCP OAuth credentials.

Capture authorizes the principal/profile/account/binding and resolves settings once.
The captured target stores effective settings and their original provenance,
including omission and explicit null. Its profile revision records the provenance
of captured defaults; it is not a credential generation or immutable compatibility
identity. Restore retains that revision and settings. Resolution explicitly compares
adapter/profile/account/endpoint/binding, then constructs the adapter with the
captured revision and current capabilities. Preparation never consults current
defaults. Unknown target and binding fields remain in private storage and in target
model/settings edits; safe public configuration projections do not automatically
expose the binding or its unknown fields.

`auth_binding` absent means historical/unavailable identity. Null explicitly records
no binding. Dynamic resolution rejects both without guessing a billing mode.
Concrete bindings are validated and retain unknown JSON members. The explicitly
fixed single-profile legacy composition accepts only absent binding by default,
using its supplied profile and auth port; it rejects null or concrete bindings unless
the caller supplies the exact concrete binding to the adapter. This limited ingress
is not a dynamic migration/default inference rule.

Authorization and status ports return non-yielding host policy snapshots on the
registry's single Eio-domain owner; native callers deliver mutations to that owner.
Every actual dispatch rechecks authorization before credential access. The host
credential callback receives the exact current credential identity and may refresh
only that binding; it cannot start interactive login. Reauthorization strictly
advances the host generation and may replace the host auth owner. Prepared requests
retain their captured profile/account/endpoint/mode; same-binding refresh resolves
fresh owner/generation. The result of a yielding lookup is rejected if authorization,
owner/generation, disable/remove state or profile configuration changed. The opaque
lease carries nonsecret owner/generation plus a currentness guard, rechecked after
DNS/TLS connection acquisition immediately before writing bearer headers. Once
submitted, an HTTP attempt retains its lease and identity rather than switching
accounts. Lease identity also supplies a channel invalidation seam for optional WS.

Profile edits preserve compatibility identity and can replace defaults/capabilities/
revision. They conservatively invalidate previously resolved contexts at dispatch:
`Profile_changed` requires explicit resolve/reprepare of the same captured target;
no retry occurs automatically. Current capabilities can then reject newly unsupported
settings or input. Account/endpoint/method/reference switching requires a new profile
ID and host-approved `Selection.change`. Removed IDs cannot be reused during the
registry lifetime. Disable returns redacted `Disabled` status and prevents dispatch;
it does not claim an external environment variable or secret store was erased.

The typed registry errors distinguish authorization, unavailable historical binding,
missing profile, incompatible identity, disabled state and reauthorization. The adapter
resolver port preserves recovery categories: unavailable/disabled/missing selections
return `Target_unavailable`, authorization returns `Target_denied`, incompatible
identity returns `Target_mismatch`, and login-needed status returns
`Reauthorization_required`. Detailed registry `resolve` and `status` retain finer
profile-management errors. Source and registry lease guards compose; conflicting
source auth-owner/generation labels reject, so wrapping never removes revocation. Authentication terminals
include `Profile_changed` and `Reauthorization_required`; failures before headers remain
`Definitely_not_submitted`. No status/lease serializer includes bearer tokens, and no
raw credential diagnostic is emitted.

Validation lives in `test/provider_profiles`: independent profile bearer/settings
selection using a loopback HTTP fixture, restore against edited defaults, missing/null
bindings, preserved future binding metadata, revoked authorization before lookup,
stale capability cancellation and revalidation, owner/generation rotation during
lookup, redacted disabled status, and incompatible account edits. Existing neutral
inference and OpenAI adapter tests cover the shared codecs and dispatch contracts.

### Optional session-owned WebSocket transport

The host explicitly selects `Http_sse`, `Prefer_websocket`, or
`Require_websocket` on an inference context. SSE remains the default. A declared
WebSocket capability is required; capability declarations do not prove live
provider qualification. The public Responses API with API keys is the initial
route. Subscription OAuth acquisition and route-specific headers belong to
OCH-66.

A runtime graph owns a neutral `Inference_runtime.Session`, and the adapter
creates a private bound preparation closure on that context. No host-wide
adapter cache is shared between sessions. `Context.derive` detaches resources;
`derive_in_session` explicitly retains them for sequential turns in the same
graph. Detaching retains the requested policy. Child and auxiliary completions
use a per-attempt ephemeral WebSocket when requested, so `Require_websocket`
never silently becomes SSE. Graph teardown first cancels and drains workers,
then closes and joins its channels. Channel lifetime and request lifetime are
separate.

Configuration records the requested policy and initial nomination. A distinct
validated transport-selection observation records the actual selected route,
its designated accounting identity, and a closed fallback reason. Public attempt
queries disclose it with configuration. A preference may fall back before any
`response.create` bytes could be submitted. Authentication failures never fall
back. After partial write, response evidence, or uncertain submission there is
no automatic resend, including on a provider continuation-cache error. Attempt
cancellation evidence remains conservatively possibly submitted until a normal
terminal establishes more precise evidence.

Each request retains its complete local history and sends `store=false`.
Connection-local continuation is an optimization: exact full wire-history prefix,
tools, effective settings, immutable assets, model, endpoint, profile, account and
authentication identity must match. Only validated completed output can seed the
bounded cache. Edits and incompatible prefixes cause full input to be sent without continuation.
Child execution, connection loss or changed credentials additionally require a
fresh channel. Every new
inference authorizes and acquires a fresh host lease. Authorization
owner/generation identifies login/logout/replacement; the separate nonsecret
credential operation revision changes on silent refresh. Both must match to
reuse an authenticated channel. Missing credential revision disables reuse;
token bytes or token hashes must never serve as that revision.

The private [RFC6455](https://www.rfc-editor.org/rfc/rfc6455) client framer uses the existing verified TLS connector and
bounds inbound frame/message bytes independently from outbound request bytes,
fragment and control counts, cumulative event bytes, deadlines and retained
history. It validates handshake acceptance, masking, reserved bits, fragmentation,
control frames, UTF8 and close payloads without per-frame fibers or unbounded
queues. Protocol failure and cancellation retire and join the channel.

Host embeddings opt in through the immutable `transport_policy` argument on
`Inference_host.create` or `Provider_profiles.create`; contexts resolved by that
host retain the policy. Operator-facing CLI/UI selection belongs to OCH-67.
A runtime build that rejects before dispatch may retain its empty neutral owner
until the enclosing switch closes; it has acquired no connection or credentials.

Dynamic provider plans capture the current authorization owner and generation
at preparation, after pure request validation and without credential access.
Each plan holds its own resolver. Logout or replacement invalidates previously
prepared plans before lookup; a new plan on the same unchanged graph context
may capture the new generation. Silent refresh within that generation remains
permitted. Dispatch freshly authorizes and composes host and source lease guards;
preparation alone does not grant dispatch authority. Static explicit hosts use
`Inference_adapter.Auth_source.Static`; dynamic hosts use `Capture` without a
static fallback.

## Retained history and model changes

Captured provider items retain their original provenance and JSON when the session
changes models. `Openai.Responses_replay` declares a finite set of directed model
transitions in a trusted host profile. Declarations do not come from ChatMD
settings and do not authorize another account or endpoint. No transition is
inferred from a model name, the reverse direction, or a chain of earlier changes.
The default profile requires the original model.

A declaration selects the item classes that have been qualified for that pair:
assistant text, function calls, custom calls, or reasoning. Cross-model admission
requires a closed supported wire shape as well as the target's feature support.
Unknown fields, annotations that have not been qualified, unknown phases and
other opaque item classes require the original model. Reasoning declarations
require separate same-family qualification; accepting ordinary text does not
establish encrypted reasoning compatibility. See the
[OpenAI reasoning guide](https://developers.openai.com/api/docs/guides/reasoning?api-mode=chat)
for the provider's model-family boundary. Original raw fields and tool
argument strings are never stripped or re-encoded to make a transition succeed.

`Inference_runtime.Context.preflight_history` checks retained history compatibility
without allocating an attempt or resolving credentials. Authored and reconstructed
items reuse the provider's feature checks; local asset references remain unresolved
at this stage, with media capability checks still required. The provider reuses this
check during final preparation, including an independent raw/semantic integrity
check. Session administration invokes preflight before changing the selected
model or replacing the runtime. A refusal preserves the current session. Only an
explicit history reset checks the replacement empty history; a model change never
silently resets or drops conversation data. Full request settings, tools and asset
resolution remain preparation responsibilities.

The offline tests qualify directed declarations, exact replay bytes, unknown-field
refusal, and administration rollback using synthetic models. They do not establish
live endpoint support for any model pair. Public API and direct OAuth qualification
must record supported pairs separately before a host enables them.


## Persistence workload and measurements

`load.storage-defaults` in the existing agent-server E2E executable exercises a
real daemon with the production 100-event / 5000-ms checkpoint cadence and real
clocks. It sends eight 1-KiB messages, executes `run_chatml` calling `read_file`
with a 17,500-byte UTF8/quote/backslash/newline payload, and completes 120 scheduled
notifications through an extensibility-v1 moderator. Each notification measurement
includes acknowledgement, durable delivery, and the corresponding moderator state
update. The scenario also forces the time-based checkpoint, verifies retention of
two snapshots, closes the daemon before an independent pruning check, and restarts
it. Restart must preserve exact canonical history and nested invocation identities
without another inference call. Provider responses are synthetic; tool execution,
HTTP commands, storage, orchestration, timers and restart are real.

Run this workload without other local builds or performance tests:

```sh
opam exec --switch=default -- dune exec --root . \
  --build-dir _build-storage-measurement --cache=disabled \
  test/agent_server_e2e/agent_server_e2e.exe -- \
  --scenario load --case load.storage-defaults
```

Set `OCHAT_E2E_REPORT_ROOT` to retain the JSON phase report. Optional workload
variables are `OCHAT_E2E_STORAGE_COMMITS` (default 120, maximum 1000),
`OCHAT_E2E_STORAGE_SEED_MESSAGES` (default 8, maximum 128), and
`OCHAT_E2E_STORAGE_FRAGMENT_REPEATS` (default/maximum 2500). The report records
actual validated checkpoint settings and script execution/compilation budgets.

A single isolated macOS 14.5 ARM64 run on 2026-10-08 completed with these results:

| Phase | Samples | p95 | Maximum |
| --- | ---: | ---: | ---: |
| Seed message turn | 8 | 262 ms | 709 ms |
| Nested large tool turn | 1 | — | 2.304 s |
| Notification acknowledgement | 120 | 119 ms | 163 ms |
| Notification completion | 120 | 247 ms | 3.252 s |
| Mutation after time checkpoint becomes due | 1 | — | 996 ms |
| Snapshot and journal replay | 1 | — | 309 ms |
| Independent validated pruning, no deletion needed | 1 | — | 43 ms |
| Restart to daemon readiness | 1 | — | 79 ms |
| Restart, history verification, replay check and shutdown | 1 | — | 1.409 s |

The nested tool turn completed within the configured 30-second script budget;
its exact 17,543-byte decoded file result, including file metadata, survived.
Restart restored 13 canonical entries and made zero provider calls. These numbers
characterize this workload, rather than guaranteeing latency for arbitrary history
sizes or platforms. Checkpoint and pruning work is included in synchronous phase
timings; the report does not attribute separate internal spans. This run is not a
before/after optimization comparison or qualification of native watcher deadlines.
Controlled-clock helper correctness tests are separate from production timing
qualification. Full-suite wall time must not be used as a latency estimate.

Snapshot pruning now returns its retention floor from the same fully validated
retained set. The serialized checkpoint owner uses that floor for journal pruning,
which avoids an immediate second decode of all retained snapshots. Every call still
validates stored frames and digests and verifies `CURRENT` before deleting files;
there is no long-lived file or timestamp cache. The floor is returned only after
successful deletion and required directory synchronization. Independent callers
can still request a fresh retention-floor validation.

## Native and confined-helper watcher timing

`load.native-watch-defaults` uses the maintained watcher, probe and request
scripts with real daemon clocks, the same 100-event / 5000-ms checkpoint cadence,
eight retained 1-KiB turns and a 2,500-fragment escaped child response. It runs
native session operations and the confined `ochat-agent-helper` request channel.
Each variant verifies the exact child payload, one committed notification, then
interrupts another child attempt by shutting down and reopening the daemon.
Recovery must preserve the original subscription, deliver the interrupted outcome
once, and issue no replacement provider request.

Build the E2E executable and helper in an isolated directory, then run with no
competing local builds or tests:

```sh
opam exec --switch=default -- dune build --root . \
  --build-dir _build-watch-measurement --cache=disabled -j 2 \
  test/agent_server_e2e/agent_server_e2e.exe bin/ochat_agent_helper.exe
OCHAT_E2E_HELPER_EXE="$PWD/_build-watch-measurement/default/bin/ochat_agent_helper.exe" \
  _build-watch-measurement/default/test/agent_server_e2e/agent_server_e2e.exe \
  --scenario load --case load.native-watch-defaults
```

An isolated macOS 14.5 ARM64 run on 2026-10-08 passed both variants:

| Variant | Watch registration through verified delivery | Interrupted attempt recovery through delivery |
| --- | ---: | ---: |
| Native session operations | 6.625 s | 24.631 s |
| Confined helper channel | 7.145 s | 25.383 s |

The maintained watch timeout is 30 seconds and subscription deadline is 31 seconds.
The recovery interval includes registering the second watcher, daemon shutdown,
reopening and the committed recovery delivery; it is not startup latency alone.
Both variants retained the original subscription and produced exactly one delivery
with zero provider replays. The complete case, including setup and final teardown,
took 82.38 seconds.

The transport harness uses a 120-second HTTP logical-connection idle timeout to
permit long local observation periods. The helper fixture explicitly authorizes its
reviewed manifest, while retaining executable-hash binding, required sandboxing
and restricted session operations. These fixture settings do not relax the watcher
or script deadlines. Provider responses are synthetic; native tool operations,
helper process confinement, persistence and restart are real. This single workload
qualifies those deadlines on the measured host, not every workload or platform.

## Runtime-host credential composition

`Inference_host.Credential_bridge` binds approved profiles to the shared
`Credential_registry` authority. Each mapping identifies an exact provider,
billing mode, runtime host, account and credential binding. Its profile projection
contains no secret material. Metadata synchronization does not probe secret files
or environment variables; authorized operator status may inspect only the selected
binding. Capture, preparation and dispatch retain independent authorization checks.

Dispatch admits the exact mapped identity before borrowing access material and
preserves the registry owner, authorization epoch and credential revision guards.
An expired OAuth grant remains dispatchable only when a qualified renewal port
exists for that exact identity. The registry then performs demand-based refresh;
status still reports renewal required until it succeeds. Uncertain refresh never
falls back to an old token or starts interactive login. Environment sources are
explicit host capabilities with per-lookup currentness guards. Disabling their
binding prevents admission without claiming to erase the external variable.

`Provider_configuration` opens the private-file backend and registry under the
host switch, outside session storage roots. `Existing` refuses missing or invalid
authority. `Initialize` is explicit create-only provisioning; it does not enroll
credentials. Applications report setup required when ordinary startup cannot open
that authority. Operator setup, protected key enrollment and OAuth login are
separate explicit operations. MCP defers provider-host acquisition until a model
operation needs it, so non-model tools do not require provider setup.

The CLI, TUI, daemon, stdio service, refinement commands and model-using MCP tools
compose this shared host. Legacy network entry points in `Openai.Responses` are
removed; that module retains wire types/codecs. Embeddings use the common inference
runtime and driver instead of acquiring ambient keys through codec functions.
