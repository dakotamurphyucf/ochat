# Prepared shell authorization

The declaration compiler produces the unchanged v1 resource manifest and its
private live material. `Registry.prepare_with_compiler_bounded` first validates
that pairing and the administrative ceiling, then compiles actual referenced
scripts without initialization. `Registry.prepared_manifest` is the execution
identity used by security verification, grants, activation, audit and capability
publication.

A prepared hook contributes its declared ID/kind, every actually selected
entrypoint (including the default and custom matcher alternatives), and the
complete immutable Prepared fingerprint. The `ochat.shell.execution.v2` canonical
JSON encloses the original resource JSON plus sorted hook facts. Length admission
precedes output allocation; comparison, copying, serialization and hashing poll
bounded chunks under the existing startup owner. Startup admits these additional
v2 metadata bytes. An empty prepared projection returns the original v1 manifest
with identical bytes, hash and legacy text-budget behavior.

The resource digest validates captured declaration/material ownership. It does
not authorize prepared execution. Session Factory retains the original root
source, workspace, principal and permission-profile checks, while operator grants
and Ed25519 signatures bind the complete execution digest. Old raw-manifest grants
reject before native setup, preflight or hook initialization. No source path or
reported digest can substitute a compiled program.

`Agent_runtime.inspect_resources` provides capture-time declaration/admin checks.
`Runtime_script_preparation.inspect_shell` borrows the same startup policy and
compiles an actual captured revision for complete inspection. Required-signature
rejection is returned separately from the requested canonical identity. The
configured `ochat shell inspect` route opens an explicit Operator_only daemon
owner; its startup performs ordinary configured capture and storage, while the
query performs no source acquisition, tool setup, initializer or provider call.
Retained inspection validates the requested invocation against genuine old
custody and works after source removal. See the
[operator inspection guide](../../docs-src/cli/shell-runtime-management.md).

Signature, trusted-source and administrative policy loaders preserve caller
cancellation and unexpected exceptions. Their existing operator-selected
whole-file reads and JSON parse are not a new bounded policy-file ingestion API.
This implementation does not rebind fresh child native resources or choose a
source-launch policy.
