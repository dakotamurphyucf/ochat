# `Chatmd_shell_spec` architecture

`ochat.chatmd_shell_spec` owns serializable ChatMD shell declarations and pure
canonical manifest compilation. It depends on Core and Jsonaf only and contains
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
- `Chatmd_script_spec`: moderator and six shell ChatML script kinds.
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
