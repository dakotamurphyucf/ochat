# `ochat shell` runtime management

The `ochat shell` command group inspects ChatMD shell authority, manages
persisted grants, and validates/replays audit logs. Inspection and replay never
execute commands.

## Inspect a runtime

```console
$ ochat shell inspect agent.chatmd
$ ochat shell inspect agent.chatmd -canonical
```

Output includes requested/live manifest digest, administrative-policy source,
signature status, trusted-source count, and effective runtimes/profile
versions. `-canonical` prints canonical requested manifest JSON. Secret values
are excluded.

Inspection parses and compiles ChatMD, resolves live paths/executable metadata
needed for inspection, and applies host security validation. It does not grant
authority or spawn a command.

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
| `OCHAT_SHELL_RESOURCE_RUNNER` | Optional structured OS-resource-limit helper. |

Missing required security material is a startup error. Values are loaded with
Eio and are never silently ignored when policy requires them.

## Exit behavior

Parse, reference, manifest, policy, trust/signature, session, audit, and
management errors print safe diagnostics and exit nonzero. Revocation without
`-confirm` exits with refusal. Commands do not print raw secrets.

## Related documentation

- [Shell runtime reference](../overview/chatmd-shell-runtime.md)
- [Security guide](../guide/chatmd-shell-security.md)
- [Persistence and audit](../guide/chatmd-shell-persistence-and-audit.md)
- [`chat_tui` guide](../guide/chat_tui.md)
