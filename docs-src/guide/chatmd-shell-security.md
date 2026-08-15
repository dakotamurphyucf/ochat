# ChatMD shell runtime security

This guide explains how ochat decides whether a shell request may run and what
the operating system actually confines. It complements the
[language reference](../overview/chatmd-shell-runtime.md).

## Four separate security questions

For every request, keep these concepts separate:

1. **Manifest authority:** did the user or host authorize this exact expanded
   runtime configuration?
2. **Capabilities:** may this runtime perform the inferred filesystem,
   network, process, arbitrary-code, and privilege effects?
3. **Policy and approval:** is this particular command automatically allowed,
   denied, or reviewable?
4. **Backend confinement:** does Seatbelt, bubblewrap, or another wrapper
   enforce the declared boundary at the OS level?

An allow rule does not add a capability. Approval does not override a hard
deny or administrative ceiling. A direct backend does not enforce filesystem
or network roots merely because the manifest declares them.

## Bootstrap manifest authorization

Opening a ChatMD file is not automatically authorization to exercise its host
authority. Ochat first compiles a canonical manifest and may require a grant
bound to its SHA-256 digest.

The authorization summary includes security-relevant details such as:

- readable and writable roots;
- network, child-process, arbitrary-code, and privilege-change authority;
- sandbox requirement and applicable backends;
- direct or user-declared confinement warnings;
- passed environment keys and secret source descriptors;
- custom executable hooks and model reviewers;
- durable approval storage and full-content audit;
- moderator process access;
- imported source and built-in profile versions.

Any authority-relevant edit—including an imported file, script, executable
pin, path derived from the environment, profile version, policy rule, or tool
mode—changes the digest and invalidates a grant for the previous manifest.

Inspect without executing:

```console
$ ochat shell inspect agent.chatmd
$ ochat shell inspect agent.chatmd -canonical
```

Interactive `chat_tui` normally fails closed when shell authority has not been
authorized. `--authorize-shell-manifest` grants only the exact manifest for
that process. Persistent manifest grants remain bound to source, manifest,
profile/import versions, and configured session/user/host identity.

## Administrative ceilings

Host policy is loaded before runtime instantiation. It may:

- restrict read/write roots and resource limits;
- disable network, child processes, arbitrary code, or privilege changes;
- require sandboxing or durable audit;
- forbid direct or declared confinement and external backends;
- forbid hooks, reviewer kinds, approval scopes, raw prefix grants, or YOLO;
- require trusted executables, hashes, trusted sources, or signatures;
- hard-deny programs, argument patterns, or effects.

Precedence is:

```text
host hard deny / capability ceiling
> manifest deny
> manifest ask / allow
> reviewer result
```

When a manifest exceeds the ceiling, startup reports the requested value, the
ceiling, the policy source, and remediation. Ochat does not silently convert a
network-enabled direct runtime into a network-disabled sandboxed runtime.

Set the host policy path with `OCHAT_SHELL_ADMIN_POLICY`. Trust/signature
configuration is described in the
[persistence and audit guide](chatmd-shell-persistence-and-audit.md).

## Capabilities

```xml
<capabilities
    sandbox="required"
    network="false"
    child_processes="true"
    arbitrary_code="true"
    privilege_change="false">
  <read path="${workspace}"/>
  <write path="${workspace}/_build"/>
</capabilities>
```

### Filesystem roots

Roots are canonicalized with symlink awareness. A write root also permits
reads beneath it. Relative paths resolve against the declaration source
directory unless they explicitly use a standard variable. `path_env` includes
the environment key and resolved path in the manifest; a missing required key
is fatal.

These roots authorize filesystem effects made by processes launched through a
shell runtime. They are independent of the built-in model-facing `read_file`
tool:

```xml
<tool name="read_file">
  <read id="source" path="${workspace}/lib"/>
</tool>
```

Nested `<tool><read/></tool>` roots determine what `read_file` can return to
the model. Nested `<shell_access><capabilities><read/></capabilities>` roots
determine what a spawned process may read. Declaring either one does not grant
the other. Configure both when an agent needs direct file inspection and shell
commands over the same tree. See
[configuring `read_file` roots](../overview/tools.md#configuring-read_file-roots).

### Process powers

- `network` allows a backend to expose networking.
- `child_processes` covers shells, build systems, VCS helpers, package
  managers, interpreters, and other programs that spawn children.
- `arbitrary_code` covers interpreters, compilers, shells, build systems,
  unknown executables, and opaque behavior.
- `privilege_change` covers identity, ownership, permission, mount, and system
  state changes. It remains subject to hard denies.

### Sandbox modes

- `required`: only a backend recognized for confinement may run. No direct
  fallback exists.
- `preferred`: prefer confinement and allow a separately declared direct
  fallback. Authorization highlights this possibility.
- `direct_unsafe`: explicitly request direct execution. Bootstrap
  authorization and host policy must accept it.

Effect analysis precedes capability checking. A command whose effects exceed
the manifest is rejected before an approval prompt appears.

## Executable resolution and identity

The resolver prevents PATH ambiguity from becoming invisible authority:

```xml
<resolver>
  <search_path path="/usr/bin"/>
  <search_path path="/bin"/>
  <trusted_root path="/usr/bin"/>
  <executable id="lint" path="${workspace}/tools/lint" sha256="..." trusted="true"/>
</resolver>
```

If a requested program contains `/`, it resolves relative to the runtime cwd
unless absolute. Otherwise search paths are checked in order. Relative or
empty search entries are rejected unless the manifest explicitly enables
`allow_relative_search_path`.

“Trusted” means the resolved regular executable satisfies configured root,
mode/ownership, optional SHA-256, and runtime trust checks. It does not mean
the program is harmless.

Ochat fingerprints the canonical path and file identity. The target,
sandbox/resource wrapper, interpreter, script, and hook executable are checked
again at the last safe point before use. Replacement invalidates approval and
prevents spawn.

## Effect analysis

Built-in analysis recognizes effects including:

- `read_path`
- `write_path`
- `network`
- `child_processes`
- `arbitrary_code`
- `privilege_change`
- `unknown`

Unknown behavior is conservative. ChatML or executable analyzers may add
effects. Replacing built-in effects requires explicit `replace="true"`
authority, and analyzer failure resolves to `unknown` rather than removing a
restriction.

## Environment security

Prefer `inherit="safe"` or `selected` and enumerate required values:

```xml
<environment inherit="selected">
  <set name="PATH" value="/usr/bin:/bin"/>
  <pass name="CI"/>
  <pass name="API_TOKEN" required="true" secret="true"/>
  <unset_prefix value="AWS_"/>
</environment>
```

Inheritance modes are `none`, `safe`, `selected`, `all_sanitized`, and `raw`.
Raw inheritance is security-sensitive because environment values can alter
program loading and behavior. The effective environment is fixed at runtime
instantiation and participates in approval identity through a digest.

Secret environment values are not printed in manifests or approval displays.

## Resource limits

Use finite wall, idle, stdin, per-channel, and total-output limits. Optional OS
limits cover CPU time, memory, file size, and open files. Input is rejected
before approval when it exceeds `max_stdin`. Pipelines stream through bounded
Eio flows rather than accumulating unbounded upstream output.

Cancellation, timeout, or limit failure closes pipes, kills and reaps the
owned process tree, and records the terminal audit event. If requested OS
limits cannot be enforced, runtime instantiation fails.

## Execution backends

### macOS Seatbelt

Seatbelt profiles are deny-by-default and are generated from declared roots,
system runtime reads, executable paths, process creation, and networking.

```xml
<seatbelt when="macos" executable="/usr/bin/sandbox-exec" allow_system_reads="true"/>
```

### Linux bubblewrap

Bubblewrap uses namespaces, read-only system binds, declared read/write binds,
a private temp directory, and configured `/proc`, `/dev`, process, and network
behavior.

```xml
<bubblewrap when="linux" executable="/usr/bin/bwrap" private_tmp="true" proc="true" dev="minimal"/>
```

### Direct

```xml
<direct id="local"/>
```

Direct execution provides no OS filesystem or network confinement. Policy,
approval, fingerprinting, limits, cancellation, output protection, and audit
still run, but declared roots are not kernel-enforced boundaries.

### External wrappers

External backends use structured argv atoms, never string interpolation. Ochat
fingerprints both wrapper and target. `confinement="declared"` means the user
asserts confinement; ochat cannot prove it. Required sandboxing may accept it
only with `accept_declared_confinement="true"` and an explicit authorization
warning.

## Policy

```xml
<policy default="ask">
  <rule id="allow-git-status" action="allow">
    <all>
      <basename value="git"/>
      <trusted_executable/>
      <argv_prefix values="git,status"/>
    </all>
  </rule>
  <rule id="deny-helper-override" action="deny">
    <any>
      <argument value="--exec-path"/>
      <argument_contains value="core.sshCommand="/>
    </any>
  </rule>
</policy>
```

Every rule requires an ID, action, and matcher. The matcher vocabulary is:

| Matcher | Meaning |
|---|---|
| `<any_command/>` | Every command. |
| `<program value="..."/>` | Requested program text. |
| `<basename value="..."/>` | Resolved command basename. |
| `<resolved_path value="..."/>` | Canonical executable path. |
| `<trusted_executable/>` | Resolver trust classification. |
| `<program_regex value="..."/>` | Program regex. |
| `<argv_prefix values="a,b"/>` | Exact leading argv sequence. |
| `<argument value="..."/>` | Exact argument present. |
| `<argument_contains value="..."/>` | Argument substring present. |
| `<effect name="..." under="..."/>` | Inferred effect, optionally beneath a path. |
| `<no_unknown_effects/>` | No unknown effect. |
| `<raw_shell/>` | Raw-shell request kind. |
| `<chatml_match .../>` | Dynamic ChatML matcher. |
| `<all>`, `<any>`, `<not>` | Boolean composition. |

All matching rules are collected. Precedence is always `deny > ask > allow`;
an allow rule cannot cancel a matching deny.

Dynamic matcher failure is conservative: it cannot accidentally make an allow
rule match or make a deny restriction disappear.

## Approval identity and scopes

An approval binds at least:

- canonical manifest digest and runtime;
- executable canonical path and SHA-256;
- exact argv boundaries or selected prefix;
- request kind;
- cwd and relevant environment identity;
- input/script digest and length where applicable;
- session/user/host binding required by the scope.

Changes to any binding invalidate the grant.

Scopes:

- `once`: current execution only; never persisted.
- `exact_session`: same exact command identity in the session.
- `prefix_session`: explicitly selected argv prefix in the session.
- `durable_exact`: exact identity persisted across permitted sessions.

The manifest and administrative policy limit which choices the UI can offer.
Raw prefix grants require explicit administrative permission.

## Reviewer chains

`ask` requests pass through `first_terminal` reviewers in order. A reviewer may
defer, approve once, approve a permitted scope, deny with a reason, or rewrite
structured argv. Denial is terminal. Rewrite restarts the complete command
pipeline and is depth-limited.

Reviewer kinds:

- `ui`: interactive local approval;
- `chatml`: typed purpose-built ChatML script;
- `model`: strict JSON response from a configured reviewer agent;
- `executable`: `shell-hook-json-v1` process under a separate worker runtime.

Failures never become approval. Model reviewers have tools disabled by
default and cannot use the shell being reviewed.

## Secrets and safe output

```xml
<secrets replacement="[REDACTED]">
  <from_env name="API_TOKEN"/>
  <from_file path="${source_dir}/.secrets/key"/>
  <literal value="test-only-secret"/>
</secrets>
```

Missing required sources are fatal. Empty values are ignored with a warning
instead of becoming a global replacement pattern. File sources remove one
trailing newline by default.

Before output reaches a model, UI, history, progress observer, reviewer, or
content-bearing audit field, ochat:

1. enforces per-channel and total bounds;
2. validates or replaces invalid UTF-8;
3. removes terminal controls and unsafe characters;
4. redacts configured secrets;
5. runs after-interceptors over the finalized value;
6. repeats validation, sanitization, redaction, and bounds after every custom
   transformation.

`stream="finalized"` is the default. `sanitized` is valid only when every
filter supports streaming-safe cross-chunk matching. Raw process bytes are
never sent to an agent-visible observer.

## YOLO profile

```xml
<shell_access id="local" extends="builtin:yolo@1"/>

<tool name="shell" type="shell" mode="structured" runtime="local"/>
```

`builtin:yolo@1` intentionally requests:

- direct unsandboxed execution;
- unrestricted host filesystem scope;
- network, child-process, arbitrary-code, and privilege-change authority;
- unrestricted executable search;
- allow-by-default policy;
- no per-command approval.

It gives the model the user’s local process privileges. It is not equivalent
to a reviewed direct-development profile. Ochat still preserves runtime
integrity controls such as manifest inspection, structured argv in structured
mode, process cancellation/reaping, executable observation, output bounds,
terminal sanitization, and configured secret redaction.

Host policy may forbid YOLO. It must reject the profile, not silently rewrite
it into a safer profile.

## Security checklist

- Prefer fixed or structured tools.
- Require confinement unless direct execution is a conscious requirement.
- Keep roots narrow and environment inheritance explicit.
- Pin project-owned executables and hook workers.
- Deny destructive/privileged behavior statically before reviewers.
- Offer only the minimum approval scopes.
- Use once-only approval for raw shell and deployments.
- Put hooks in separate minimal worker runtimes.
- Keep audit redacted unless full content is explicitly required.
- Inspect the canonical manifest after every authority-relevant change.
- Revoke durable grants that are no longer needed.
- Treat all process output as untrusted even after a command succeeds.
