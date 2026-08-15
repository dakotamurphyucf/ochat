# ChatMD shell runtime reference

ChatMD can describe a complete shell-capable agent runtime without custom
OCaml code. A document may declare process authority, executable resolution,
sandboxing, command policy, approvals, reviewers, interceptors, limits,
environment values, secret redaction, and audit behavior. Ochat compiles those
declarations before it exposes any shell tool to a model.

This page is the language and lifecycle reference. Related pages provide more
focused guidance:

- [Shell tool declarations](chatmd-shell-tools.md)
- [Shell runtime security](../guide/chatmd-shell-security.md)
- [ChatML and executable extensions](../guide/chatmd-shell-extensions.md)
- [Persistence, administration, and audit](../guide/chatmd-shell-persistence-and-audit.md)
- [Worked examples](../guide/chatmd-shell-examples.md)
- [`ochat shell` management commands](../cli/shell-runtime-management.md)
- [Implementation architecture](../guide/chatmd-shell-runtime-internals.md)

## Source-of-truth model

The expanded ChatMD declaration is the requested source of truth. Ochat uses
three representations:

1. The parser produces serializable runtime, script, and tool specifications.
2. The manifest compiler resolves imports, namespaces, inheritance, built-in
   profile versions, variables, platform predicates, and defaults into one
   deterministic canonical manifest.
3. The runtime registry authorizes that exact manifest and materializes live
   Eio resources, executable fingerprints, sandboxes, stores, extensions, and
   `Shell_access.Executor.config` values.

A runtime is either instantiated as declared or rejected with a diagnostic.
Ochat does not silently remove authority, insert a direct backend, or fall back
to the legacy command runner. Host administrative policy may reject requested
authority, but it does not rewrite the manifest.

Shell declarations, shell scripts, and runtime metadata are host-managed. They
are not sent to the model as conversation history.

## Minimal example

```xml
<shell_access id="readonly" extends="builtin:workspace-readonly@1"/>

<tool name="search" type="shell" mode="fixed" command="rg --json"
      runtime="readonly" description="Search repository files"/>
```

Before `search` is published, ochat expands the built-in, compiles a canonical
manifest, applies host security controls, authorizes the manifest, and creates
the runtime registry.

## Top-level declarations

The shell runtime feature adds these host-managed top-level forms:

```xml
<shell_access id="...">...</shell_access>
<script id="..." language="chatml" kind="...">...</script>
<moderator_runtime shell_runtime="..."/>
<tool type="shell" ...>...</tool>
```

`<shell_access>` declares a named runtime. `<tool type="shell">` binds a
model-visible tool to one runtime. Shell-specific `<script>` kinds implement
dynamic matchers, reviewers, interceptors, effect analyzers, or audit filters.
`<moderator_runtime>` routes moderator `Process.run` through a named shell
runtime.

Nested shell tags are parsed by the main ChatMD lexer and parser. They are only
recognized while inside a `<shell_access>` declaration or a shell tool. The
dedicated shell declaration converter then applies a strict closed schema.
Unknown attributes, unknown children, duplicate singleton sections, and mixed
message content fail closed; there is no second permissive XML parser.

## IDs, references, and namespaces

Runtime, script, rule, reviewer, interceptor, analyzer, executable, and sink
IDs are case-sensitive and must be non-empty. Prefer:

```text
[A-Za-z_][A-Za-z0-9_.-]*
```

References may be local, imported, or built-in:

```text
development
team:development
builtin:workspace-development@1
```

An unresolved, ambiguous, or duplicate reference is a configuration error.

### Imports

```xml
<import src="../runtime/common.chatmd" namespace="team"/>

<shell_access id="development" extends="team:development-base">
  <limits wall_time="10m"/>
</shell_access>
```

Imported declarations retain their source file, source directory, namespace,
span, and source digest. Relative paths resolve against the file containing
the declaration. `${prompt_dir}` continues to name the root prompt directory.
Import cycles and namespace collisions are rejected. Editing an imported file
changes the manifest digest and invalidates grants bound to the old manifest.

### Inheritance

A runtime may extend exactly one runtime or built-in profile:

```xml
<shell_access id="build" extends="builtin:workspace-development@1">
  <limits wall_time="10m"/>
</shell_access>
```

Merge rules are deterministic:

- An absent section inherits unchanged.
- A present scalar overrides the inherited scalar.
- `none` clears fields that explicitly accept it, such as optional timeouts.
- Ordered collections append by default.
- `merge="replace"` replaces an inherited collection.
- Named inherited entries require explicit supported override behavior; silent
  collisions are errors.
- Inheritance cycles are rejected with the dependency path.

Inheritance is resolved before authorization, and the canonical manifest
contains the complete expansion.

## Values, units, and variables

### Booleans

Use `true` or `false`. Bare flag attributes are accepted only where the
surrounding ChatMD declaration documents them.

### Durations

Durations are positive decimal values followed by `ms`, `s`, `m`, or `h`:

```text
250ms  30s  2.5m  1h
```

The value `none` disables an optional timeout.

### Byte sizes

Byte sizes use a non-negative integer with an optional `B`, `KiB`, `MiB`, or
`GiB` suffix:

```text
4096  512KiB  2MiB  1GiB
```

### Standard variables

Path expressions may use:

| Variable | Meaning |
|---|---|
| `${prompt_dir}` | Directory of the root ChatMD file. |
| `${source_dir}` | Directory of the file containing the declaration. |
| `${tool_dir}` | Working directory selected by the host invocation. |
| `${workspace}` | Workspace root. |
| `${session_dir}` | Session-owned directory. |
| `${cache_dir}` | Ochat cache directory. |
| `${home}` | Current user home directory. |

Unknown variables are fatal. Expansion happens before canonicalization and
manifest hashing. Ordinary attributes do not perform arbitrary `${env:KEY}`
substitution; use explicit environment and secret declarations.

Configured [`read_file`](tools.md#configuring-read_file-roots) roots use the
same path-expression variables. In `chat-tui` and `ochat chat-completion`,
`${workspace}` and `${tool_dir}` are the process launch directory,
`${prompt_dir}` is the root prompt directory, and `${source_dir}` follows the
file containing the declaration. The host may supply different values when
Ochat is embedded as a library.

## `<shell_access>` root

```xml
<shell_access
    id="runtime-id"
    extends="optional-reference"
    cwd="${workspace}"
    pipefail="true">
  ...
</shell_access>
```

`id` is required. `cwd` defaults through the expanded profile/defaults; a
relative literal resolves against the declaration source directory.
`pipefail="true"` makes a pipeline unsuccessful when any stage fails.

The following singleton sections are accepted:

| Section | Purpose |
|---|---|
| `<capabilities>` | Requested process and filesystem authority. |
| `<resolver>` | Search paths, trusted roots, aliases, and hashes. |
| `<environment>` | Environment inheritance and mutations. |
| `<limits>` | Time, input, output, and OS resource bounds. |
| `<backends>` | Seatbelt, bubblewrap, direct, or external execution. |
| `<policy>` | Static and dynamic allow/ask/deny classification. |
| `<approvals>` | Provider, unavailable behavior, and scopes. |
| `<reviewers>` | Ordered UI, ChatML, model, or executable reviewers. |
| `<interceptors>` | Before/after command extensions. |
| `<effect_analysis>` | Additional or replacement effect analyzers. |
| `<secrets>` | Values that must be redacted. |
| `<audit>` | Audit sink, content level, failure policy, and filter. |

Omitted sections receive concrete conservative defaults before manifest
authorization. Defaults are visible in the canonical manifest; they are not a
hidden host profile.

## Capabilities

```xml
<capabilities
    sandbox="required"
    network="false"
    child_processes="false"
    arbitrary_code="false"
    privilege_change="false">
  <read path="${workspace}"/>
  <write path="${workspace}/_build"/>
  <read path_env="SDK_ROOT" optional="true"/>
</capabilities>
```

`sandbox` is `required`, `preferred`, or `direct_unsafe`. Read/write roots are
canonicalized with symlink awareness. A write root also permits reads beneath
it. A path reached through an escaping symlink is not inside the root.

Capabilities are ceilings, not policy decisions. Effects are inferred first;
an effect outside the declared capability set is rejected before approval.
Approval cannot add a missing capability.

See the [security guide](../guide/chatmd-shell-security.md) for complete
semantics.

## Resolver

```xml
<resolver allow_relative_search_path="false">
  <search_path path="/usr/bin"/>
  <trusted_root path="/usr/bin"/>
  <executable
      id="project-linter"
      path="${workspace}/tools/lint"
      sha256="..."
      trusted="true"/>
</resolver>
```

Unqualified programs are searched in declaration order or through the
sanitized process path when no explicit search path exists. Explicit aliases
support hash pinning and `executable_ref` from fixed tools. Resolved targets
are fingerprinted and reverified immediately before spawn.

## Environment

```xml
<environment inherit="selected">
  <set name="PATH" value="/usr/bin:/bin"/>
  <pass name="CI"/>
  <pass name="API_TOKEN" required="true" secret="true"/>
  <unset name="SSH_AUTH_SOCK"/>
  <unset_prefix value="AWS_"/>
  <path prepend="${workspace}/.tools/bin"/>
  <path_env name="OPAM_SWITCH_PREFIX" suffix="bin" position="append" required="true"/>
</environment>
```

Inheritance modes are `none`, `safe`, `selected`, `all_sanitized`, and `raw`.
The environment is fixed when the runtime is instantiated. Secret values are
registered for output and audit redaction.

## Limits

```xml
<limits
    wall_time="2m"
    idle_time="30s"
    max_stdin="1MiB"
    stdout="1MiB"
    stderr="1MiB"
    total_output="1500KiB"
    cpu_time="90s"
    memory="2GiB"
    file_size="64MiB"
    open_files="128"/>
```

Required limits must be enforceable. Ochat rejects a runtime rather than
claiming an unavailable limit is active.

## Backends

```xml
<backends>
  <seatbelt when="macos"/>
  <bubblewrap when="linux" executable="/usr/bin/bwrap"/>
</backends>
```

`when` accepts `macos`, `linux`, `windows`, or `any`. Nonmatching backends are
excluded. Required sandboxing never falls back to direct execution.

External backends use a structured argv template:

```xml
<backends accept_declared_confinement="true">
  <external_backend
      id="org"
      when="linux"
      executable="/opt/org/bin/sandbox-run"
      sha256="..."
      confinement="declared">
    <arg value="--cwd"/><cwd_value/>
    <read_roots flag="--read"/>
    <write_roots flag="--write"/>
    <network_flag value="--network"/>
    <resource_limit_args/>
    <arg value="--"/><command_argv/>
  </external_backend>
</backends>
```

Template atoms never invoke a shell or concatenate untrusted syntax.

## Policy and approvals

```xml
<policy default="ask">
  <rule id="allow-rg" action="allow">
    <all>
      <basename value="rg"/>
      <trusted_executable/>
      <no_unknown_effects/>
    </all>
  </rule>
  <rule id="deny-rm" action="deny">
    <basename value="rm"/>
  </rule>
</policy>

<approvals
    provider="ui"
    unavailable="deny"
    scopes="once,exact_session,prefix_session,durable_exact"
    durable="true"/>
```

All matching rules participate, with `deny > ask > allow`. Hard denial cannot
be overridden by a reviewer or interceptor. `ask` enters the reviewer system.
Supported approval scopes are `once`, `exact_session`, `prefix_session`, and
`durable_exact`; only declared and administratively permitted scopes are
offered.

For explicit reviewer chains:

```xml
<reviewers strategy="first_terminal">
  <reviewer id="static" kind="chatml" script="review" failure="deny"/>
  <reviewer id="model" kind="model" agent="security-reviewer" failure="deny"/>
  <reviewer id="human" kind="ui"/>
</reviewers>
```

The only shipped strategy is `first_terminal`. A rewrite restarts resolution,
effect analysis, capability checks, policy, interception, approval identity,
planning, and executable verification.

## Effects, interceptors, secrets, and audit

These sections are described in detail in the security and extension guides.
Their core guarantees are:

- Effect analysis is conservative; failure never widens access.
- A before-interceptor may continue, rewrite, synthesize a result, or reject.
- An after-interceptor receives finalized output and its replacement is
  bounded, sanitized, and redacted again.
- After-interceptors cannot declare a matcher in schema version 1.
- Secret values never enter canonical manifests and are redacted from model,
  UI, progress, reviewer, and content-bearing audit surfaces.
- Audit filters cannot change execution behavior or immutable event identity.

## Canonical manifest

The canonical manifest includes every authority-relevant resolved value:

- source and import digests;
- profile and feature versions;
- canonical paths and platform selection;
- capabilities, roots, resolver settings, hashes, and backend definitions;
- environment policy without secret values;
- limits, policy matcher AST, approvals, reviewers, hooks, and audit settings;
- shell tool modes and constraints that affect authorization.

Serialization has deterministic field ordering, preserves argv boundaries,
uses normalized units and enums, resolves imports/inheritance, includes script
and executable content identity, and excludes timestamps, random request IDs,
and raw secrets. The current encoding is versioned and hashed with SHA-256.

Inspect without execution:

```console
$ ochat shell inspect agent.chatmd
$ ochat shell inspect agent.chatmd -canonical
```

See the [CLI guide](../cli/shell-runtime-management.md).

## Compilation lifecycle

Before a shell tool is published, ochat:

1. Parses ChatMD through the closed grammar.
2. Expands imports and records provenance.
3. Validates declarations and unique IDs.
4. Resolves references and dependency cycles.
5. Expands built-ins and inheritance.
6. Resolves variables and canonical path expressions.
7. Compiles the deterministic manifest and required feature set.
8. Applies administrative ceilings and trust/signature requirements.
9. Requests authorization for the exact manifest when needed.
10. Materializes environment, secrets, stores, audit sinks, extensions,
    executable identities, backends, and executor configurations.
11. Publishes one immutable runtime/tool registry generation.

Any failure prevents dependent tools from being exposed. Parsing and
inspection do not execute commands.

## Per-command lifecycle

Each invocation captures one registry generation and manifest digest. It then:

1. validates the tool payload and input bounds;
2. builds a structured request or conservative chain/raw/script request;
3. resolves and fingerprints each executable;
4. infers effects;
5. applies administrative hard-deny and capability checks;
6. evaluates manifest policy;
7. runs eligible before-interceptors;
8. restarts preparation after any rewrite;
9. obtains a matching grant or runs the reviewer chain for `ask`;
10. selects a backend and creates an immutable execution plan;
11. reverifies targets, wrappers, scripts, and helpers at the last safe point;
12. executes under Eio cancellation, timeout, pipe, and output ownership;
13. finalizes output, runs after-interceptors, and finalizes again;
14. emits the safe result and completes the structured audit trail.

Skipped conditional-chain branches are not prepared and do not prompt.

## Dependency cycles

Compilation checks imports, inheritance, tool/runtime references, moderator
runtime bindings, ChatML extension operations, executable hooks, worker
runtimes, and model-reviewer dependencies. A cycle such as

```text
main -> interceptor filter -> worker -> extends main
```

is rejected before runtime creation.

## Errors and compatibility

Configuration errors include invalid schema, references, cycles, unavailable
required backends, unsupported features or profile versions, security-policy
violations, missing host capabilities, failed hashes/signatures, and rejected
manifest authorization.

Invocation errors include resolution/fingerprint failure, capability or policy
denial, unavailable approval, hook protocol failure, sandbox/spawn failure,
timeouts, bounds, audit failure, cancellation, and nonzero behavior selected by
the tool.

Expected shell-tool invocation failures are returned to the model as structured
JSON text instead of aborting response streaming:

```json
{
  "error": {
    "code": "idle_timed_out",
    "message": "request was idle for 60.000s"
  }
}
```

The stable code supports programmatic recovery while the safe message explains
the failure. Host cancellation and unexpected host exceptions retain their
normal runtime behavior.

Diagnostics use stable `shell.*` categories, safe messages, source paths where
available, and redaction markers. Exception text is not a public protocol.

Legacy `<tool name="x" command="..."/>` remains a parser compatibility form.
It is desugared into a fixed shell tool and uses the shell registry. There is
no fallback to a separate direct process spawner. ChatMD without shell tools
retains its previous behavior.

## Built-in profiles

The currently shipped profiles are:

| Profile | Purpose |
|---|---|
| `builtin:workspace-readonly@1` | Sandboxed read-oriented workspace access with conservative policy and UI review. |
| `builtin:workspace-development@1` | Sandboxed development authority suitable for builds/tests, with review for unfamiliar behavior. |
| `builtin:yolo@1` | Unrestricted direct local process authority, allow-by-default policy, and no command approval. |

Unversioned aliases resolve to the current compatible version, and the
concrete version enters the manifest. Upgrading an alias changes the digest.
YOLO is intentionally dangerous; read the [security guide](../guide/chatmd-shell-security.md#yolo-profile)
before using it.

## Grammar summary

```text
document     ::= top_level*
top_level    ::= message | config | import | script
               | moderator_runtime | shell_access | tool

shell_access ::= <shell_access id extends? cwd? pipefail?>
                   capabilities? resolver? environment? limits?
                   backends? policy? approvals? reviewers?
                   interceptors? effect_analysis? secrets? audit?
                 </shell_access>

tool         ::= fixed_tool | structured_tool | chain_tool
               | raw_tool | script_file_tool
```

The tables and examples on this page and the shell tools page are normative
for the shipped schema; unknown fields are not extension points.
