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
