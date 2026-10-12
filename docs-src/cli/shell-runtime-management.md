# `ochat shell` runtime management

Host integration: see [native/legacy/daemon authorization and administration](../guide/chatmd-shell-host-integration.md).
The declaration language is shared, but bootstrap grants, approvers, persistence
owners and management commands differ. `--authorize-shell-manifest` authorizes a reviewed prompt in native
`--local` or legacy interactive TUI mode; it is not a daemon flag. Legacy `Session_store`
management does not accept daemon IDs as a way to select daemon state.

The `ochat shell` command group inspects ChatMD shell authority, manages
persisted grants, and validates/replays audit logs. Inspection and replay never
execute commands.

## Inspect a runtime

```console
$ ochat shell inspect agent.chatmd
$ ochat shell inspect agent.chatmd -canonical
```

Legacy text inspection retains the v1 resource-manifest encoding. A configured
hook needs its captured project graph; raw text inspection rejects it rather than
printing a body-only hash as executable authorization.

```console
$ ochat shell inspect -config agent-server.sexp -prompt ci-agent -canonical
$ ochat shell inspect -config agent-server.sexp -prompt ci-agent -revision REVISION_ID -invocation headless -canonical
$ ochat shell inspect -config agent-server.sexp -prompt ci-agent -signature-payload -issuer example.org -audience ochat -issued-at 1791691200
```

Configured inspection opens an explicit `Operator_only` daemon owner using the
selected configuration. Startup performs normal source admission and catalog,
artifact and project-history preparation. It does not implicitly update locked
Git selections. The subsequent inspection query compiles that captured revision;
it does not recapture sources, create a session, initialize hooks, set up native
tools or call an inference provider. Retained inspection requires an invocation
(`ui`, `headless` or `delegated-ui`) matching the stored program contract. It can
inspect that graph after the original files have been removed; there is no fresh
file fallback.

Output distinguishes `execution manifest` from `resource manifest`. The execution
hash covers the v2 canonical encoding of the original resource manifest and every
actually compiled prepared hook entrypoint's complete graph fingerprint,
including custom matcher entrypoints. A dependency-only code change therefore
requires a new grant or signature. Empty prepared projection keeps the exact
legacy v1 bytes. Secret values remain excluded.

`-canonical` prints the requested execution identity. Configured inspection reports
security acceptance or rejection separately, so a missing required signature
does not prevent obtaining the identity to sign. `-signature-payload` requires
`-issuer` and prints the exact canonical Ed25519 payload for external signing; it
never generates or loads a private key. `-audience` is repeatable, the issue time
defaults to the host clock, and `-expires-at` sets optional Unix-second expiry.
The signature and runtime grant must cover this same complete manifest. Inspection
output grants no authority.

## Validate and replay audit

```console
$ ochat shell audit validate PATH
$ ochat shell audit replay PATH
$ ochat shell audit request PATH REQUEST_ID
```

- `validate` loads rotations, validates schema, sequence continuity, and
  integrity-chain digests.
- `replay` renders the read-only event timeline.
- `request` reconstructs one request’s safe decision/result summary.

None of these commands reruns a request or recovers redacted fields.

## Manage command grants

```console
$ ochat shell grants list SESSION_ID
$ ochat shell grants explain SESSION_ID GRANT_ID
$ ochat shell grants revoke SESSION_ID GRANT_ID -confirm
$ ochat shell grants revoke SESSION_ID GRANT_ID -reason "retired workflow" -confirm
```

`list` shows ID, scope, active/expired/revoked status, runtime, and command
digest. `explain` prints non-secret identity bindings: manifest, command,
executable, cwd, environment, stdin length/digest, script digest, session/user/
host binding, reviewer, and timestamps.

Revocation is a mutation and refuses to proceed without `-confirm`. It updates
the session and appends a chained management audit event. If revocation
succeeds but audit persistence fails, the command reports that partial outcome
and exits nonzero.

## Manage manifest grants

```console
$ ochat shell manifest-grants list SESSION_ID
$ ochat shell manifest-grants explain SESSION_ID GRANT_ID
$ ochat shell manifest-grants revoke SESSION_ID GRANT_ID -confirm
```

Manifest explanations include manifest and source digests, canonical source
root, repository identity, signer/issuer, session/user/host binding, schema,
built-in profile versions, and imported source digests.

Revocation requires `-confirm` and emits a management audit event.

## Inspect interrupted requests

```console
$ ochat shell interrupted list SESSION_ID
```

This prints redacted interrupted-request metadata. Requests are never resumed;
retry creates a new plan and repeats authorization.

## Interactive manifest authorization

```console
$ chat-tui -file agent.chatmd --authorize-shell-manifest
```

The flag grants the exact canonical manifest for that interactive process. It
does not authorize future changed manifests or create global path-based trust.
Without an authorizer, shell manifests fail closed before tools are exposed.
The flag applies only to interactive mode and cannot be combined with selector
commands that do not run the interactive UI.

## Host security environment

| Variable | Meaning |
|---|---|
| `OCHAT_SHELL_ADMIN_POLICY` | Administrative ceiling policy path. |
| `OCHAT_SHELL_TRUSTED_SOURCES` | Trusted-source policy path. |
| `OCHAT_SHELL_REPOSITORY_IDENTITY` | Optional repository identity. |
| `OCHAT_SHELL_MANIFEST_SIGNATURE` | Canonical manifest signature path. |
| `OCHAT_SHELL_MANIFEST_PUBLIC_KEYS` | Ed25519 public key set. |
| `OCHAT_SHELL_SIGNATURE_AUDIENCE` | Signature audience, default `ochat`. |

Missing required security material is a startup error. Values are loaded with
Eio and are never silently ignored when policy requires them.

## Exit behavior

Parse, reference, manifest, policy, trust/signature, session, audit, and
management errors print safe diagnostics and exit nonzero. Configured inspection
prints a rejected security result alongside the requested identity for signing;
that successful read-only inspection does not authorize runtime startup. Revocation without
`-confirm` exits with refusal. Commands do not print raw secrets.

## Related documentation

- [Shell runtime reference](../overview/chatmd-shell-runtime.md)
- [Security guide](../guide/chatmd-shell-security.md)
- [Persistence and audit](../guide/chatmd-shell-persistence-and-audit.md)
- [`chat_tui` guide](../guide/chat_tui.md)
