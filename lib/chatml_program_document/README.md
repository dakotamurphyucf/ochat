# Complete provided-program metadata

`Chatml_program_document` captures and restores metadata for an explicitly supplied
immutable `Chatml_prepared_program.t`. It has no compiler, runtime, artifact-store,
filesystem or source-provider dependency. Source bodies remain with the caller's
payload custody; this document describes their expected exact identities.

The named document kind is `chatml.provided-program`, version 1, inside the shared
`ochat.document` envelope. Opaque package namespaces and environment names use
lowercase hexadecimal bytes (`namespace_hex` and `name_hex`), preserving even NUL
and invalid UTF-8 accepted by the existing domain constructors. Portable paths keep
their validated UTF-8 representation. IDs are objects with `namespace_hex` and
`path`; module identities are their implementation source IDs.

Payload fields are `fingerprint`, `compiler_contract`, `entry`, `environment`,
`provenance`, `sources`, `definitions` and `scopes`. Sources contain `id`, `digest`
and `byte_length`. Environment contains `name_hex`, `required_contract` and optional
`declared_at`. Provenance is `implicit` or `project`; project provenance records
`declaration` and an optional `lock` ID without interpreting either source.
Definitions record `implementation`, optional `interface`, and `declared_at`.
Scopes record `source`, optional `selected_at`, and bindings of `name`,
`implementation` and `declared_at`. Optional fields are absent, never null.
Locations contain `source`, `digest`, `start_byte` and `end_byte`. They do not
serialize line/column coordinates; restore recomputes those from exact captured
bytes with `Chatml_source.locate`.

Capture runs the same schema traversal twice. The first pass accounts for exact
JSON bytes, nodes, fields and depth directly over Prepared before cloning metadata
lists or expanding opaque labels. Ordinary scalar sizes use shared document JSON
measurement; hex expansion uses checked arithmetic. Construction follows only
after this preflight. Standard document admission owns final JSON validation.

Admission retains the complete immutable current named document, including unknown
payload/envelope fields and order. No mutable editing API can reassign those fields.
Unsupported versions/conversions and unknown required semantics fail closed.
The `sources` accessor exposes immutable expected descriptors so a custody adapter
can admit bodies using `Chatml_source.create` with an expected digest. It does not
interpret private payload paths or attest that those bodies exist.

Restore first bounds metadata and supplied inventory before allocating reference
maps. It rejects duplicate, missing, surplus and mismatched revisions; reconstructs
locations, environment, definitions and scopes through current constructors; then
runs current Prepared admission with its exact source-plus-canonical-metadata cap.
It compares the current caller-owned authentic compiler contract and expected
Prepared fingerprint. JSON document limits separately bound the named wire format.
No lookup, parser, initializer, network/cache fallback or callback is retained.
Checkpoint and unexpected exceptions propagate unchanged.

Actual selected-project receipts are restored by `Chatml_project_build` with their
exact project/lock reconciliation. This codec cannot substitute for that owner.
Full prompt publication, payload custody, restart/retention and execution adapters
remain above this pure boundary.
