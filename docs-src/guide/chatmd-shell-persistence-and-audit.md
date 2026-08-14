# ChatMD shell persistence, administration, and audit

Shell security state is explicit, typed, versioned, and separate from
conversation history. This guide covers manifest grants, command grants,
extension snapshots, interrupted requests, administrative policy, trusted
sources, signatures, audit durability, and read-only replay.

## Session shell state

Current session snapshots include a typed `shell_state` containing:

- exact canonical manifest grants;
- command approval grants;
- serializable ChatML shell extension snapshots;
- the last durable audit sequence;
- interrupted request metadata.

Migrations from older session versions create empty shell trust. Ochat never
infers approval from old command history. Reset clears user/session shell
grants and extension state by default. An in-flight process is never resumed
after restart; retry creates a new request, repeats resolution/fingerprinting,
and re-enters policy/approval.

## Manifest grants

A manifest grant authorizes one exact canonical requested runtime. Its identity
includes the manifest digest plus authority-relevant source information such
as canonical source root/digest, repository identity, schema/encoding version,
built-in versions, imported source digests, signer/issuer evidence, and
configured session/user/host bindings.

Any covered edit invalidates the match. List, explain, and revoke grants with:

```console
$ ochat shell manifest-grants list SESSION_ID
$ ochat shell manifest-grants explain SESSION_ID GRANT_ID
$ ochat shell manifest-grants revoke SESSION_ID GRANT_ID -confirm
```

Revocation is persisted and emits a chained management audit event.

## Command approval grants

Command grants bind runtime/manifest, executable fingerprint, exact command or
selected prefix, request kind, cwd/environment identity, input or script
digest, identity bindings, timestamps, reviewer source, and revocation state.
Raw secrets are not stored.

Stores:

- memory: compiled-runtime lifetime;
- session: session snapshot lifetime;
- durable file: versioned integrity-protected file with atomic replacement and
  restrictive permissions.

Once-only approval is never persisted. Durable stores reject or redact
secret-bearing identity that cannot be persisted safely.

Management commands:

```console
$ ochat shell grants list SESSION_ID
$ ochat shell grants explain SESSION_ID GRANT_ID
$ ochat shell grants revoke SESSION_ID GRANT_ID -reason "no longer needed" -confirm
```

Expiration and revocation make a grant inactive without erasing its audit
history.

## Extension snapshots

Session/runtime-lifecycle ChatML shell extensions can snapshot data-shaped
state. Restore requires matching extension ID, kind, source SHA-256, lifecycle,
runtime, and manifest identity. Failed, timed-out, or malformed transactions do
not commit state. Invocation-lifecycle state is not persisted.

## Interrupted requests

When shutdown or recovery finds an unfinished shell request, ochat stores a
redacted record with request/runtime/manifest identity, reason, retryability,
and safe command display. It does not restore the process or continue an old
execution plan.

```console
$ ochat shell interrupted list SESSION_ID
```

The TUI’s Interrupted tab repeats the same guarantee: retry always creates and
authorizes a new request.

## Administrative policy

Set `OCHAT_SHELL_ADMIN_POLICY` to a host policy file. The loader uses Eio and
applies the policy after canonical compilation but before runtime
instantiation. Controls can restrict capabilities, roots, limits, backends,
hooks, reviewer kinds, approval scopes, raw prefix grants, YOLO, executable
trust/hashes, audit durability, trusted sources, signatures, programs,
arguments, and effects.

Administrative policy rejects incompatible manifests and reports all relevant
violations. It never silently rewrites requested behavior.

## Trusted sources

Trusted-source policy binds allowable authority to source identity:

- canonical source root;
- optional repository identity;
- exact source digests;
- optional signer IDs;
- a capability ceiling for network, child processes, arbitrary code,
  privilege change, external backends, executable hooks, model reviewers, and
  durable approvals.

Environment variables:

| Variable | Purpose |
|---|---|
| `OCHAT_SHELL_TRUSTED_SOURCES` | Trusted-source policy file. |
| `OCHAT_SHELL_REPOSITORY_IDENTITY` | Optional repository identity presented for matching. |

Imported source digests are included in the canonical manifest, so editing an
import invalidates source evidence.

## Signed manifests

Manifest signatures use Ed25519 over canonical manifest identity plus encoding
version, concrete built-in versions, issuer, audience, issue/expiry time, and
imported source digests. Runtime configuration loads public verification keys
only; private signing keys do not belong in an agent runtime.

| Variable | Purpose |
|---|---|
| `OCHAT_SHELL_MANIFEST_SIGNATURE` | Signature document. |
| `OCHAT_SHELL_MANIFEST_PUBLIC_KEYS` | Public-key set. |
| `OCHAT_SHELL_SIGNATURE_AUDIENCE` | Required audience; defaults to `ochat`. |

A source, import, profile, schema, audience, expiration, or manifest mismatch
fails before runtime creation.

## Audit configuration

```xml
<audit
    format="jsonl"
    path="${session_dir}/shell-audit.jsonl"
    content="redacted"
    failure="deny_start"/>
```

Formats are `none`, `stderr`, `jsonl`, and `session`. Content levels:

- `metadata`: no command arguments or output;
- `redacted`: safe command/result fields, the default;
- `full`: bounded full content after secret redaction; security-sensitive.

Failure policy:

- `continue`: report diagnostics and continue;
- `deny_start`: do not start new commands while durable audit is unavailable;
- `terminate`: cancel in-flight work on durable failure.

The runtime also supports chained JSONL, rotating JSONL, session sinks,
organization collectors, and fan-out. Sequence is assigned before fan-out.
Integrity chaining can detect truncation, reordering, and altered events.

## Audit events

The event stream covers the complete decision path, including:

- manifest authorization/rejection and management revocation;
- executable resolution and replacement detection;
- effects, policy, and capability failures;
- approval request/answer and grant use;
- interceptor/analyzer/reviewer invocation and response;
- execution plan, backend, process start, output counts, finish;
- timeout, limits, cancellation, audit failure, and rejection.

Events include a schema version, monotonic sequence, timestamp, session,
runtime, manifest, request, optional plan identity, event name, redacted fields,
and optional previous/event digests.

## Audit filters

ChatML or executable filters receive already-redacted events. They may remove
or redact mutable fields but cannot alter execution or immutable identity.
Their output is validated and redacted again. Executable filters run under a
separate worker runtime and `shell-hook-json-v1`.

## Read-only validation and replay

```console
$ ochat shell audit validate PATH
$ ochat shell audit replay PATH
$ ochat shell audit request PATH REQUEST_ID
```

Replay loads numbered rotations from oldest to newest, validates supported
schema, sequence continuity, and integrity digests, then reconstructs a safe
timeline. It can summarize resolution, effects, policy, approvals, rewrites,
interceptors, backend, byte counts, completion, timeout/cancellation, and
rejection.

Replay never executes a command, resumes a process, or recovers redacted
secrets.

## TUI management

Open `:shell` in `chat_tui` to inspect:

- effective runtime posture and canonical digest;
- administrative and signature status;
- runtime backend/network/audit summaries;
- command grants and revocation;
- validated audit requests/timelines;
- interrupted requests.

Management loading and revocation run asynchronously through Eio. Only
generation-matching immutable results are applied by the UI domain; stale
workers cannot overwrite newer state.

## Operational checklist

- Inspect the manifest before first authorization and after imports change.
- Require trusted sources/signatures where opening arbitrary ChatMD is not
  sufficient authority.
- Prefer redacted chained audit for durable security workflows.
- Use `deny_start` when missing audit must prevent execution.
- Explain a grant before revoking it; record a reason for operational changes.
- Review expired/revoked grants rather than deleting their evidence.
- Treat interrupted records as diagnostics, not resumable jobs.
- Back up session and audit files according to their sensitivity.
