# ChatMD shell runtime implementation architecture

This guide is for contributors. It describes current package boundaries and
security invariants; it is not a future implementation roadmap.

## Dependency direction

```text
ochat.chatmd_shell_spec  -> Core, Jsonaf
ochat.chatmd             -> chatmd_shell_spec
ochat.shell_access       -> Core, Core_unix, Digestif, Eio, Jsonaf, Optint, Re
ochat.shell_runtime      -> chatmd_shell_spec, shell_access, chatml
ochat.chat_response      -> chatmd, shell_runtime
ochat.chat_tui           -> chat_response, shell_runtime UI adapters
```

The specification package contains only serializable data and pure manifest
compilation. Live filesystem/process/store/UI values enter in `shell_runtime`.

## Four explicit boundaries

1. **Parse:** ChatMD AST nodes become strict serializable declarations with
   source provenance.
2. **Compile:** imports, references, inheritance, defaults, profiles, features,
   and canonical manifest are resolved deterministically.
3. **Authorize:** administrative/trust/signature policy and a manifest
   authorizer accept the exact digest.
4. **Instantiate:** Eio capabilities, paths, environment, secrets, executables,
   backends, stores, extensions, audit, and executor configs become live.

Inspection stops before command execution. A missing boundary cannot fall back
to legacy spawn.

## Package ownership

### `chatmd_shell_spec`

- source refs, diagnostics, path expressions, duration/byte units;
- closed nested shell element vocabulary;
- runtime, tool, and ChatML script specifications;
- conservative defaults and versioned built-in expansion;
- inheritance/merge and deterministic canonical manifests;
- required feature IDs and live-material identity.

It contains no Eio handles, callbacks, stores, process execution, or host
probing.

### `shell_access`

- structured command and conservative chain AST;
- bounded input, limits, capabilities, resolver, executable fingerprints;
- effects, matchers, policy, approvals, interceptors, and audit envelopes;
- backend confinement classes and immutable execution plans;
- Eio pipelines, timeouts, cancellation, reaping, output finalization, and
  immediate pre-spawn verification.

### `shell_runtime`

- explicit host capabilities and Eio path resolution;
- environment/secret loading and lowering to executor values;
- manifest authorization, admin policy, trusted source, signatures;
- immutable registry preparation/instantiation and inspection;
- approval broker and memory/session/durable stores;
- ChatML host extensions, typed codecs, model reviewer, snapshots;
- executable hooks/worker runtimes and `shell-hook-json-v1`;
- JSONL/session/rotating/fan-out audit and replay;
- moderator `Process.run` adapter and interrupted-request persistence.

### `chat_response` and `chat_tui`

`Chat_response.Agent_runtime` centralizes tool construction for main drivers,
nested agents, and TUI setup. Tools receive an instantiated registry; they do
not compile manifests or open files during invocation.

The TUI owns local approval input, Shell Security management, startup
authorization, and session persistence. Workers return immutable
generation-tagged data; the UI domain alone mutates model/render state.

## Parsing architecture

`Chatmd_ast` and `Chatmd_lexer` recognize `shell_access`, `moderator_runtime`,
and shell child tags. Shell child tags are recognized only within shell scope.
`Chatmd_parser` constructs the complete nested AST. `Chatmd_shell_declaration`
then validates allowed attributes, child cardinality, enums, values, and
source-qualified diagnostic paths.

`prompt.ml` remains the compatibility/dispatch layer. New grammar and
validation belong in focused modules; manifest/runtime logic must not move into
Prompt.

Each declaration carries file, source/prompt directory, namespace, offsets,
and source digest. Import expansion attaches provenance before qualification.

## Canonical manifest

The compiler:

1. qualifies imported IDs;
2. rejects duplicate/unresolved references;
3. resolves inheritance with complete cycle diagnostics;
4. applies merge semantics and built-in profile expansion;
5. evaluates platform predicates as requested data;
6. normalizes path expressions without execution;
7. preserves ordered rule/backend/reviewer/interceptor collections;
8. records schema/source/import/profile/feature versions and digests;
9. resolves tool/runtime and moderator/runtime references;
10. emits deterministic canonical JSON and SHA-256.

The manifest describes requested behavior. Live inspection separately records
selected canonical paths, executable fingerprints, backends, and unavailable
alternatives.

## Registry lifecycle

`Registry.prepare_with_policy` validates the manifest against administrative
policy and prepares effective runtime/script material. Instantiation receives
an explicit `Host.t` with Eio environment, workspace, prompt/session/source
directories, process environment, session ID, approval provider/store,
authorizer, and persistence callbacks.

All referenced runtimes and extensions instantiate before the immutable map is
published. An invocation captures one registry generation and manifest digest.

## Mapping to `Shell_access.Executor.config`

`Shell_runtime.Lowering` converts resolved serializable fields:

- capabilities and canonical roots -> `Capabilities.t`;
- resolver search/trust/aliases -> `Resolver.t` plus live executable identities;
- limits -> executor and backend bounds;
- matcher AST -> `Matcher.t`;
- policy rules/default -> `Policy.t`;
- approvals/reviewers/stores -> reviewer and approval-store callbacks;
- interceptors/analyzers -> typed ChatML or executable callbacks;
- backend declarations -> available backend values and confinement class;
- secrets -> `Secret_filter.t`;
- audit -> `Audit.t` sink/failure behavior.

Effective cwd/environment, Eio clock/process manager/switch ownership, audit
sequence, manifest/runtime/session identity, and live file fingerprints are
runtime-only values.

## Command lifecycle invariants

- Model arguments are literal argv unless mode is explicitly raw.
- Capability checks happen before command approval.
- Host/manifest deny precedence is deterministic.
- Rewrite restarts the entire preparation pipeline.
- Required confinement never falls back to direct.
- Target, wrapper, resource runner, script, interpreter, and hook are
  reverified immediately before use.
- Every custom output transformation is followed by bounds, UTF-8, terminal,
  and secret finalization.
- Cancellation closes process, pipes, approval waits, hook workers, and audit
  fibers under the owning Eio switch.
- Stable typed `shell.*` diagnostics cross CLI/TUI/audit/test boundaries;
  exception strings do not.

## ChatML extensions

The generic host runtime compiles purpose-specific surfaces and typed bridge
values. Stateful instances serialize calls with Eio mutexes; transactions
commit one valid terminal action and state together. Session snapshots are
data-only and source/manifest bound. The moderator facade retains conversation
semantics, while shell kinds do not inherit moderator authority.

## Executable hooks and backend templates

Hooks are one-process-per-request in protocol v1. Parent and worker use Eio for
stdin/stdout/stderr, deadlines, cancellation, and reaping. Exact JSON schema,
request kind, bounded fields, zero exit, and one response are mandatory.

External backends compile to an argv AST. Command argv occurs exactly once;
scalar atoms produce one element and repeated-root atoms produce structured
pairs. No shell interpolation is allowed.

## Persistence and management

Session shell state is versioned. Older sessions migrate to empty trust.
Durable stores use Eio, atomic replacement, restrictive permissions, schema
versioning, and integrity checks. Revocation and manifest management append
audit events. Audit replay is read-only.

## Core and Eio conventions

All implementation modules use Core as the standard-library layer. Use Eio
for files, process execution, pipes, clocks, timeouts, cancellation, mutexes,
queues, and flow copying whenever available. Reserve Core_unix for facilities
Eio does not expose, such as selected stat metadata, uname, or resource-limit
setup.

Do not introduce `In_channel.with_file`, `Out_channel.with_file`,
`Unix.create_process`, `Sys.command`, or blocking sleeps where Eio provides the
operation. Follow the repository coding guidelines for short focused
functions, explicit errors, `_exn` naming, `.mli` documentation, and minimal
implementation comments.

## Conformance checklist

- strict parser and source provenance;
- deterministic inspectable manifest;
- exact bootstrap authorization;
- fixed/structured/chain/raw/script modes with typed schemas;
- resolver fingerprint and replacement defense;
- conservative effects and capability-before-approval;
- deny precedence and complete rewrite restart;
- no required-sandbox fallback;
- Eio concurrent pipelines, limits, cancellation, and reaping;
- fail-closed ChatML/executable extension protocols;
- separate cycle-checked worker runtimes;
- repeated output finalization;
- manifest/executable-bound approvals;
- structured redacted audit and non-executing replay;
- moderator process routing through the registry;
- no legacy direct-spawn fallback;
- Core/Eio and coding-guideline compliance.
