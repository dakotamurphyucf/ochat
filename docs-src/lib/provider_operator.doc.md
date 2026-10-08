# Runtime-owned provider administration

Provider administration uses the same registry and backend as inference.
`Provider_runtime_host` composes the private credential store, lifecycle registry,
approved profile templates, bridge, operation receipts and `Provider_operator`
service. `Provider_runtime` makes that composition available before setup and
keeps a stable backend facade as setup opens the registry. The daemon advertises
`provider.operator` only when a real operator port is installed.

## Setup and credentials

An unconfigured host supports authorized status and explicit setup. Status reports
`setup_required`; it does not implicitly initialize the registry or enroll an
environment key. Inference remains unavailable until setup and an approved
credential binding are ready. Setup retains its original operation identity so
an interrupted reply can be reconciled without creating another authority.
Corrupt state and missing enrolled secrets are not treated as fresh setup.

Credentials belong to the runtime host. Clients can initiate login, inspect
nonsecret status, cancel their login, select an approved profile and disable a
binding. Remote requests never contain API keys, OAuth tokens, arbitrary provider
URLs, client filesystem paths or arbitrary environment variable names.
`configure-environment` selects a host-declared source; the local protected-file
entry point reads an owned private file through the shared storage backend.

The beta stores credentials in private files. The directory and immutable secret
revision policies are described in [provider secret storage](provider_secret_store.doc.md).
[Credential lifecycle](credential_registry.doc.md) owns refresh, epoch/revision
checks, publication and logout. The operator service does not decode tokens or
implement a second credential registry.

## Commands and authorization

| Protocol command | Scope | Result |
| --- | --- | --- |
| `provider.setup` | `provider.manage` | Original setup revision |
| `provider.status` | `provider.view` | Bounded nonsecret profiles, owned flows and selection |
| `provider.login.begin` | `provider.manage` | Host-qualified flow reference |
| `provider.login.challenge` | `provider.manage` and original owner | Private live challenge |
| `provider.login.cancel` | `provider.manage` and original owner | Original flow state |
| `provider.logout` | `provider.manage` | Disabled binding and drain outcome |
| `provider.select` | `provider.select` | Compare-and-set selection result |
| `provider.configure_environment` | `provider.manage` | Declared source configuration result |

The host also checks its configured policy. The
[original actor proof](operator_authorization.doc.md) is retained across
asynchronous work; another login with the same principal cannot extend it.
Private challenges have a separate authorized transport representation. Generic
results, receipts, status, history and debug output cannot serialize their URI
parameters or device code.

Login workers belong to the runtime host, independently of the initiating RPC
or session. Disconnecting a remote client does not cancel a login. Explicit
cancellation joins the owned worker; host shutdown interrupts it. Reopening the
host reconciles the original receipt and does not restart an interrupted exchange.
A flow reference includes host, profile, identifier and expiry, and cannot transfer
ownership between principals.

Mutating commands require stable idempotency keys. Retry an uncertain command
with its original parameters and key, or query `command.receipt`. An unavailable
receipt does not authorize repeating the effect with a new key. Selection retains
bounded original-operation proofs: retrying selection A after selection B returns
A's original result without selecting A again. New fresh inference captures use
the selected profile; an existing explicitly bound target keeps its profile.

## CLI workflow

The main executable exposes the `provider` command group. Start with status,
then explicitly provision the host if needed:

```sh
ochat provider status
ochat provider setup -key initial-provider-setup
ochat provider configure-environment -key enroll-declared-api-source
ochat provider status
```

The environment command requires the runtime host's declared `OPENAI_API_KEY`
source. To use the subscription route, run `ochat provider login -key account-login -mode device` or choose `-mode browser`; the command displays the private challenge
and waits for terminal status. It does not automatically open a browser.

`select` requires the current selection revision from status. `logout` disables
the selected binding and drains admitted requests. `cancel` takes the original
profile, flow identifier and expiry shown by login/status. Consult each command's
`-help` for required flags.

All commands accept an optional `-connection-profile` pointing to a named daemon
connection profile with a pinned server identity. Without that flag they use the
local runtime. `-provider-home` chooses a local trusted home anchor and cannot be
combined with a remote connection. Local `configure-key-file` accepts only a
protected file and is unavailable through a remote connection.

The standalone login process owns its local worker and must remain running while
login completes. For a login that survives client disconnection, connect to the
daemon that owns the worker. Provider credentials are distinct from credentials
used to authenticate that daemon connection.

## Host inference transport policy

The completion CLI, local TUI, durable daemon and local stdio host accept
`--inference-transport sse|prefer-websocket|require-websocket`. The default is
`sse`. Remote TUI and stdio connections reject this host option: clients cannot
change the remote host's transport policy. Store maintenance also rejects it.
The existing default constructors remain SSE; explicit constructors receive the
policy through the same owned platform, backend and credential bridge. Local
TUI typeahead uses a bounded view of that host rather than opening a second
credential authority.

`Provider_runtime_host.Profile_policy` supplies the shared pure route baseline
and strict policy parser. Public API and direct Codex are distinct routes with
exact canonical endpoints. Protocol declarations do not authorize credentials,
prove account access or replace exact identity, epoch and revision checks. Direct
Codex retains its explicit unsupported temperature, top-p and max-output-token
settings; the application never removes a selected setting to obtain success.

The shipping WebSocket catalog supports exactly `gpt-6-luna` on the public API
route at `https://api.openai.com/v1/responses`. Unlisted models, model prefixes,
endpoint overrides (including a trailing slash) and direct Codex retain unknown
WebSocket support. This declaration does not establish account eligibility.
Required WebSocket refuses unknown support before credential acquisition or
connection. Preferred WebSocket records an observable SSE fallback for unknown
support; uncertain delivery never authorizes resubmission.

## Live qualification recorded 2026-10-08

The actual public API route and `gpt-6-luna` passed the following bounded probes
on Darwin arm64, separately with SSE and required WebSocket. Each accepted
inference recorded its actual selected transport without fallback. The runner
used the real runtime host, credential registry and durable admission ledger.
Only allowlisted manifests are published as completion evidence. Private
snapshots remain local; credentials, challenges and provider captures are not
publication artifacts.

| Probe, on each transport | Accepted requests | Evidence and limit |
| --- | ---: | --- |
| Journey | 4 | Three completed host turns, one native `apply_patch` effect, restore, history continuity, exact captured identity/configuration |
| Strict JSON schema | 1 | Exact captured fixed closed schema and parsed output |
| Inline image | 1 | Fixed red PNG; answer identifies its color without an answer in the prompt |
| Reasoning | 1 | Captured `effort=low`, `summary=auto` and correct arithmetic answer; not every reasoning metadata field |
| Inline PDF | 1 | Actual auxiliary `Execution.run` plus graph tracking; fixed PDF marker, zero host turns and native effects; not frontend attachments |
| Function call | 1 | Actual auxiliary API wire call, exact named strict function and fixed arguments; zero host turns and native effects, no function execution |
| Logout | 0 new inference requests | Disabled binding/drain proven on both journey roots; manifests retain four prior attempts and the prior effect, and do not themselves prove transport/configuration |

The local allowlisted manifest names are `gpt6luna-api-sse-journey07.json`,
`gpt6luna-api-ws-journey01.json`, and the corresponding `json01`, `img01`,
`reason01`, `doc01`, `fn01` and `logout01` manifests for each transport.
These are dated observations rather than latency or universal capability claims.
They do not qualify temperature, top-p, cache controls, every media variant,
model-pair replay or direct Codex. Subscription OAuth browser login, saved
credential registration, host restart and subsequent login cancellation passed.
The first direct Codex SSE request returned HTTP 400; no OAuth inference or
renewal success is claimed here.

The standalone harness requires explicit live opt-in and either a protected
API-key file (`-key-file`) or an explicitly named local environment input
(`-key-env`); neither silently falls back to another source. Build it with the
repository's isolated toolchain, then use fresh short absolute roots for each
transport and feature. Short roots avoid macOS Unix socket path limits. Replace
the key-file placeholder locally; never place a key value in argv or evidence.
The observed API runs selected `-max-output-tokens 1024`. These commands make
paid provider requests and are not normal tests. Normal `runtest` executes only
the offline `-self-check` path, without host setup, credential or environment
input reads, provider requests, or browser launch:

```sh
opam exec --switch=default -- dune exec --root . --build-dir _build-qualification --cache=disabled -j2 test/provider_live_qualification/main.exe -- -self-check
qualification_root=$(mktemp -d /tmp/oq-sse-XXXXXXXX)
opam exec --switch=default -- dune exec --root . --build-dir _build-qualification --cache=disabled -j2 test/provider_live_qualification/main.exe -- -live -auth api -model gpt-6-luna -account-alias api-qualification -transport sse -phase journey -max-attempts 4 -phase-seconds 300 -max-output-tokens 1024 -root "$qualification_root" -key-file /absolute/protected/api-key -output "$qualification_root/evidence.json"
```

Repeat the journey with `-transport require-websocket` and a fresh root. For each
isolated feature, use `-phase feature -feature json-schema|image|reasoning|document|function-call`
(select one value), `-max-attempts 1` and a new root/output path. To qualify
logout, reuse the corresponding journey's exact model, transport, root and
persistent budget and `-max-output-tokens 1024` with `-phase logout`. Reusing a completed feature or journey
root never authorizes replay. The harness enforces no native tool approvals for
feature phases; the journey approves only its exact synthetic patch.

Process host policy governs newly prepared work, including work from restored
targets. It is not a newly persisted session setting. An already prepared
attempt retains its transport configuration; uncertain delivery never permits
automatic replay, route fallback or credential/account substitution.
