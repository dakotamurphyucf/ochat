# `Chatmd_shell_spec` architecture

`ochat.chatmd_shell_spec` owns serializable ChatMD shell declarations and pure
canonical manifest compilation. It uses Core, Jsonaf, Digestif and Uutf and contains
no Eio handles, callbacks, mutable stores, host probing, or process execution.

## Module map

- `Source_ref`: declaring file/directory, root prompt directory, namespace,
  source span, and digest.
- `Diagnostic`: stable source-qualified `shell.*` errors.
- `Path_expr`, `Duration`: standard variables, durations, and byte sizes.
- `Shell_element`: closed nested element vocabulary recognized by ChatMD.
- `Shell_spec`: runtime capabilities, resolver, environment, limits, backends,
  policy, reviewers, hooks, secrets, and audit values.
- `Shell_tool_spec`: fixed/structured/chain/raw/script-file tool declarations.
- `Chatmd_script_spec`: legacy moderator and six shell ChatML script kinds.
- [`Extension_spec`](../../../lib/chatmd_shell_spec/extension_spec.mli): additive
  versioned moderator/standalone tools, pinned schemas, scripts and authoring policy.
- [`Tool_schema`](../../../lib/chatmd_shell_spec/tool_schema.mli): bounded, pure
  extension-tool JSON-schema compilation and value validation. Parsed declarations
  use this service; invocation admission integration remains unfinished.
- `Manifest_defaults`, `Builtin_profile`: explicit defaults and versioned
  profile expansion.
- `Manifest_merge`, `Manifest_compiler`: inheritance/reference/cycle checking,
  feature collection, deterministic canonical JSON, SHA-256, and live material.

## Boundaries

The ChatMD parser creates nested AST nodes. `Chatmd_shell_declaration` converts
them into these types with strict validation. Manifest compilation then
qualifies imports, resolves inheritance, applies defaults/profiles, and hashes
the requested behavior. Live path/executable checks belong to `Shell_runtime`.

Ordered rule/backend/reviewer/interceptor lists remain ordered. Map-like data
is canonicalized deterministically. Secret descriptors enter the manifest;
secret values do not.

## Built-ins and features

Shipped profiles are `workspace-readonly@1`, `workspace-development@1`, and
`yolo@1`. Unversioned aliases resolve to concrete versions recorded in the
manifest. Required feature IDs prevent unsupported declarations from being
ignored or lowered to weaker behavior.

See [the contributor runtime guide](../../guide/chatmd-shell-runtime-internals.md).

## Agent-host integration and complete API index

See [host integration](../../guide/chatmd-shell-host-integration.md) for daemon
versus legacy ownership and [environment](../../agent-server/environment.md) for
host-specific configuration. Compiling a manifest is not authorization or OS
confinement. A detached viewer disconnect does not cancel daemon-owned runtimes.

| Module | Contract | Code |
|---|---|---|
| `builtin_profile` | [Interface](../../../lib/chatmd_shell_spec/builtin_profile.mli) | [Implementation](../../../lib/chatmd_shell_spec/builtin_profile.ml) |
| `chatmd_script_spec` | [Interface](../../../lib/chatmd_shell_spec/chatmd_script_spec.mli) | [Implementation](../../../lib/chatmd_shell_spec/chatmd_script_spec.ml) |
| `diagnostic` | [Interface](../../../lib/chatmd_shell_spec/diagnostic.mli) | [Implementation](../../../lib/chatmd_shell_spec/diagnostic.ml) |
| `duration` | [Interface](../../../lib/chatmd_shell_spec/duration.mli) | [Implementation](../../../lib/chatmd_shell_spec/duration.ml) |
| `feature` | [Interface](../../../lib/chatmd_shell_spec/feature.mli) | [Implementation](../../../lib/chatmd_shell_spec/feature.ml) |
| `manifest` | [Interface](../../../lib/chatmd_shell_spec/manifest.mli) | [Implementation](../../../lib/chatmd_shell_spec/manifest.ml) |
| `manifest_compiler` | [Interface](../../../lib/chatmd_shell_spec/manifest_compiler.mli) | [Implementation](../../../lib/chatmd_shell_spec/manifest_compiler.ml) |
| `manifest_defaults` | [Interface](../../../lib/chatmd_shell_spec/manifest_defaults.mli) | [Implementation](../../../lib/chatmd_shell_spec/manifest_defaults.ml) |
| `manifest_merge` | [Interface](../../../lib/chatmd_shell_spec/manifest_merge.mli) | [Implementation](../../../lib/chatmd_shell_spec/manifest_merge.ml) |
| `path_expr` | [Interface](../../../lib/chatmd_shell_spec/path_expr.mli) | [Implementation](../../../lib/chatmd_shell_spec/path_expr.ml) |
| `shell_element` | [Interface](../../../lib/chatmd_shell_spec/shell_element.mli) | [Implementation](../../../lib/chatmd_shell_spec/shell_element.ml) |
| `shell_spec` | [Interface](../../../lib/chatmd_shell_spec/shell_spec.mli) | [Implementation](../../../lib/chatmd_shell_spec/shell_spec.ml) |
| `shell_tool_spec` | [Interface](../../../lib/chatmd_shell_spec/shell_tool_spec.mli) | [Implementation](../../../lib/chatmd_shell_spec/shell_tool_spec.ml) |
| `source_ref` | [Interface](../../../lib/chatmd_shell_spec/source_ref.mli) | [Implementation](../../../lib/chatmd_shell_spec/source_ref.ml) |
