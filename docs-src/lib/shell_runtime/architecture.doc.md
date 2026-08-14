# `Shell_runtime` architecture

`ochat.shell_runtime` materializes canonical ChatMD shell manifests into live
Eio-backed registries and integrates host policy, persistence, extensions,
audit, moderator processes, and management surfaces.

## Main responsibilities

- `Host`, `Environment`: explicit Eio capabilities, paths, process environment,
  secret loading, and canonical live values.
- `Lowering`, `Runtime`, `Registry`: convert resolved specs into one immutable
  `Shell_access.Executor.config` per runtime and publish only after complete
  instantiation.
- `Manifest_authorizer`, `Manifest_grant_store`: exact manifest authorization.
- `Admin_policy(_loader)`, `Trusted_source`, `Manifest_signature`,
  `Manifest_security`: organization ceilings and source/signature evidence.
- `Approval_broker`, `Approval_store`: UI callback queue and memory/session/
  durable grant persistence.
- `Chatml_extension` and typed `Chatml_*_value` codecs: purpose-built surfaces,
  transactions, limits, lifecycle, snapshots, and model review.
- `Hook_protocol`, `Hook_worker`, executable reviewer/interceptor/analyzer/
  audit filter: bounded `shell-hook-json-v1` workers.
- `Audit_sink`, `Audit_replay`: redacted/chained/rotating/session/fan-out sinks
  and non-executing validation/reconstruction.
- `Moderator_process_adapter`, `Interrupted_store`, `Result`: shared moderator
  process routing, recovery metadata, and safe tool results.

## Lifecycle

Preparation applies administrative policy to the canonical requested
manifest. Security verification checks trusted source/signature requirements.
Authorization grants the exact digest. Instantiation uses Eio to resolve paths,
load secrets, open stores/audit, compile extensions, fingerprint helpers, and
select backends. One immutable registry generation is then shared by tool and
moderator calls.

See [the contributor runtime guide](../../guide/chatmd-shell-runtime-internals.md).
