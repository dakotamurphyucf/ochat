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
Authorization grants the exact digest. Preparation compiles authorized extensions
without initializing them. Instantiation uses Eio to resolve paths, load secrets,
open stores/audit, fingerprint helpers, and select backends. One immutable
registry generation is then shared by tool and moderator calls.

Hosts that must admit additional script contracts before any native hook
initializer use `Registry.instantiate_prepared_after`. Its callback receives an
abstract `Registry.Staged.t` containing the actual runtime and tool identities.
`Chat_response.Shell_tool.prepare` constructs the real functions against that
view. Those functions, and borrowed executor configurations, reject execution
until the callback succeeds, every hook activates, and snapshot restore finishes.
For example, a host can retain a prepared shell function while validating another
tool; if that tool rejects, invoking the retained function returns
`shell.tool_inactive` before decoding its input or performing native effects.

Each native hook instance moves from pending to ready once. Activation failure
is terminal; closing the borrowed switch revokes even previously ready access.
Callback exceptions and cancellation propagate with their original backtraces.
The host's switch owns the native handles throughout, including unsuccessful
preparation: rejection does not close an unrelated caller switch. Initializers
that completed before a later failure may have external effects; this boundary
does not roll those effects back. The ordinary registry and standalone runtime
constructors retain their immediate behavior.

## Fresh specialist code over retained resources

`Registry.rebind_code` accepts a prepared manifest and its exact authorization
grant, then compares the effective native declarations with the original registry.
Tool bindings, worker wiring, administrative ceilings, lifecycles and failure
policies must agree. Hook code and diagnostic provenance may change. A declaration
digest alone cannot authorize a different compiled dependency graph.

The resulting `Registry.Code_view` borrows the original native resources: path
bases, loaded secrets and environment, executable workers, selected model services
and raw audit sinks. It does not reopen those resources. Fresh hook state belongs
to the new code owner and complete execution manifest; the original state remains
unchanged. Repeated worker names retain their original occurrence order.

Use `Code_view.with_execution` for each dispatch. Its `Execution.registry` and
`Execution.prepare_executor` select the admitted code while preserving actual
caller services. Tool calls, moderator Process calls and executable hook workers
share `Call_context.prepare_executor_for_call`, so nested processes use the current
session and approval store/provider. Results identify the manifest actually used.
An unbound original owner retains its original services.

An execution admission expires when its callback returns. Borrowed configs and
registries cannot be revived by applying another code view, and both the resource
owner and code owner must remain live. Join workers before returning their loan.
Deferred work must enter a new admission using its recorded view and current
caller services; the code-view primitive does not select or persist queued-job
source revisions.

The caller's logical work ceiling covers comparison, temporary indexes and code
construction. Byte comparisons poll in chunks of at most 1024 bytes; temporary
budgeted comparators do not escape into retained maps. This bounds logical work,
not CPU instructions, disk use or native process execution. Matcher construction
is stack-safe; nested matcher execution retains its existing depth limits.

See [the contributor runtime guide](../../guide/chatmd-shell-runtime-internals.md).

## Agent-host integration and complete API index

See [host integration](../../guide/chatmd-shell-host-integration.md) for daemon
versus legacy ownership and [environment](../../agent-server/environment.md) for
host-specific configuration. Compiling a manifest is not authorization or OS
confinement. A detached viewer disconnect does not cancel daemon-owned runtimes.

| Module | Contract | Code |
|---|---|---|
| `admin_policy` | [Interface](../../../lib/shell_runtime/admin_policy.mli) | [Implementation](../../../lib/shell_runtime/admin_policy.ml) |
| `admin_policy_loader` | [Interface](../../../lib/shell_runtime/admin_policy_loader.mli) | [Implementation](../../../lib/shell_runtime/admin_policy_loader.ml) |
| `approval_broker` | [Interface](../../../lib/shell_runtime/approval_broker.mli) | [Implementation](../../../lib/shell_runtime/approval_broker.ml) |
| `approval_store` | [Interface](../../../lib/shell_runtime/approval_store.mli) | [Implementation](../../../lib/shell_runtime/approval_store.ml) |
| `audit_replay` | [Interface](../../../lib/shell_runtime/audit_replay.mli) | [Implementation](../../../lib/shell_runtime/audit_replay.ml) |
| `audit_sink` | [Interface](../../../lib/shell_runtime/audit_sink.mli) | [Implementation](../../../lib/shell_runtime/audit_sink.ml) |
| `call_context` | [Interface](../../../lib/shell_runtime/call_context.mli) | [Implementation](../../../lib/shell_runtime/call_context.ml) |
| `chatml_approval_value` | [Interface](../../../lib/shell_runtime/chatml_approval_value.mli) | [Implementation](../../../lib/shell_runtime/chatml_approval_value.ml) |
| `chatml_audit_value` | [Interface](../../../lib/shell_runtime/chatml_audit_value.mli) | [Implementation](../../../lib/shell_runtime/chatml_audit_value.ml) |
| `chatml_codec` | [Interface](../../../lib/shell_runtime/chatml_codec.mli) | [Implementation](../../../lib/shell_runtime/chatml_codec.ml) |
| `chatml_context_value` | [Interface](../../../lib/shell_runtime/chatml_context_value.mli) | [Implementation](../../../lib/shell_runtime/chatml_context_value.ml) |
| `chatml_effect_value` | [Interface](../../../lib/shell_runtime/chatml_effect_value.mli) | [Implementation](../../../lib/shell_runtime/chatml_effect_value.ml) |
| `chatml_extension` | [Interface](../../../lib/shell_runtime/chatml_extension.mli) | [Implementation](../../../lib/shell_runtime/chatml_extension.ml) |
| `chatml_interceptor_value` | [Interface](../../../lib/shell_runtime/chatml_interceptor_value.mli) | [Implementation](../../../lib/shell_runtime/chatml_interceptor_value.ml) |
| `chatml_policy_value` | [Interface](../../../lib/shell_runtime/chatml_policy_value.mli) | [Implementation](../../../lib/shell_runtime/chatml_policy_value.ml) |
| `chatml_result_value` | [Interface](../../../lib/shell_runtime/chatml_result_value.mli) | [Implementation](../../../lib/shell_runtime/chatml_result_value.ml) |
| `chatml_value` | [Interface](../../../lib/shell_runtime/chatml_value.mli) | [Implementation](../../../lib/shell_runtime/chatml_value.ml) |
| `environment` | [Interface](../../../lib/shell_runtime/environment.mli) | [Implementation](../../../lib/shell_runtime/environment.ml) |
| `executable_analyzer` | [Interface](../../../lib/shell_runtime/executable_analyzer.mli) | [Implementation](../../../lib/shell_runtime/executable_analyzer.ml) |
| `executable_audit_filter` | [Interface](../../../lib/shell_runtime/executable_audit_filter.mli) | [Implementation](../../../lib/shell_runtime/executable_audit_filter.ml) |
| `executable_interceptor` | [Interface](../../../lib/shell_runtime/executable_interceptor.mli) | [Implementation](../../../lib/shell_runtime/executable_interceptor.ml) |
| `executable_reviewer` | [Interface](../../../lib/shell_runtime/executable_reviewer.mli) | [Implementation](../../../lib/shell_runtime/executable_reviewer.ml) |
| `hook_payload` | [Interface](../../../lib/shell_runtime/hook_payload.mli) | [Implementation](../../../lib/shell_runtime/hook_payload.ml) |
| `hook_protocol` | [Interface](../../../lib/shell_runtime/hook_protocol.mli) | [Implementation](../../../lib/shell_runtime/hook_protocol.ml) |
| `hook_worker` | [Interface](../../../lib/shell_runtime/hook_worker.mli) | [Implementation](../../../lib/shell_runtime/hook_worker.ml) |
| `host` | [Interface](../../../lib/shell_runtime/host.mli) | [Implementation](../../../lib/shell_runtime/host.ml) |
| `interrupted_store` | [Interface](../../../lib/shell_runtime/interrupted_store.mli) | [Implementation](../../../lib/shell_runtime/interrupted_store.ml) |
| `lowering` | [Interface](../../../lib/shell_runtime/lowering.mli) | [Implementation](../../../lib/shell_runtime/lowering.ml) |
| `manifest_authorizer` | [Interface](../../../lib/shell_runtime/manifest_authorizer.mli) | [Implementation](../../../lib/shell_runtime/manifest_authorizer.ml) |
| `manifest_grant_store` | [Interface](../../../lib/shell_runtime/manifest_grant_store.mli) | [Implementation](../../../lib/shell_runtime/manifest_grant_store.ml) |
| `manifest_security` | [Interface](../../../lib/shell_runtime/manifest_security.mli) | [Implementation](../../../lib/shell_runtime/manifest_security.ml) |
| `manifest_signature` | [Interface](../../../lib/shell_runtime/manifest_signature.mli) | [Implementation](../../../lib/shell_runtime/manifest_signature.ml) |
| `model_reviewer` | [Interface](../../../lib/shell_runtime/model_reviewer.mli) | [Implementation](../../../lib/shell_runtime/model_reviewer.ml) |
| `moderator_process_adapter` | [Interface](../../../lib/shell_runtime/moderator_process_adapter.mli) | [Implementation](../../../lib/shell_runtime/moderator_process_adapter.ml) |
| `registry` | [Interface](../../../lib/shell_runtime/registry.mli) | [Implementation](../../../lib/shell_runtime/registry.ml) |
| `result` | [Interface](../../../lib/shell_runtime/result.mli) | [Implementation](../../../lib/shell_runtime/result.ml) |
| `runtime` | [Interface](../../../lib/shell_runtime/runtime.mli) | [Implementation](../../../lib/shell_runtime/runtime.ml) |
| `trusted_source` | [Interface](../../../lib/shell_runtime/trusted_source.mli) | [Implementation](../../../lib/shell_runtime/trusted_source.ml) |
