# Project declaration format v1

This document describes pure declaration decoding and scope selection. Explicit
host preparation performs directory capture, pinned Git acquisition, compiler
compatibility and native admission before handing an immutable project to the
existing graph compiler and runtime. See the working example in
`docs-src/examples/chatml/projects/shared-counter`; decoding itself performs no
acquisition or execution.

Declarations are flat JSON objects with `schema_version: 1`. Duplicate object keys,
unknown fields, invalid JSON and explicit null selectors reject. JSON integer
version literals are required; strings, fractional/exponent spellings and booleans
are not coerced. Errors retain the supplied captured declaration, even when its
size is rejected. Capture admission is the caller's responsibility. JSON field
paths accompany whole-declaration locations; no token offsets are invented.
Host presentation uses the existing bounded diagnostic renderer.

```json
{
  "schema_version": 1,
  "namespace": "demo",
  "source_dirs": ["src"],
  "entries": {"main": {"source": "main.chatml", "target": "one_off_v1"}}
}
```

```text
demo/
├── chatml.project.json
├── main.chatml
└── src/
    ├── graph.chatml
    ├── graph.chatmli
    └── format.chatml
```

`namespace` is required and nonempty, retaining exact JSON string bytes. Module
and interface member paths use `Source.Portable_path`: already-normalized relative
paths, no rewriting/case folding. Host local dependency locators are different:
relative or absolute paths on the selected runtime host, resolved only under that
host's explicit authority. No path string grants root authority.

| Field | Omitted | Present value |
|---|---|---|
| `source_dirs` | empty | unique directory strings; `.` means project root, otherwise portable member path; nonrecursive |
| `exclude` | empty | unique exact portable member paths removed from directory discovery/automatic companions |
| `modules` | empty | object of module-name overrides, each requiring `implementation`, optionally `interface` |
| `entries` | empty library catalog | object of entry-name objects requiring `source` and `target`, optionally `environment` |
| `dependencies` | empty | named explicit local/Git catalog declarations, described below |
| `environments` | one implicit `default` | object of named scope/requirement objects; empty object has no environment/default |
| `default_environment` | literal `default` if present | exact existing own environment name; absence without `default` needs explicit selector |
| `file_environments` | empty | object of exact portable member path to exact existing own environment name |
| `capabilities` | empty | unique nonempty exact host tool names; no native permission or implementation |

Module binding/override names use existing UIDENT syntax: ASCII uppercase first,
then ASCII letters, digits or underscores. Environment and entry names are nonempty
exact strings; they are not module names. Dependency aliases are ASCII identifiers
(letter/underscore first, then letters/digits/underscores). `self` is reserved for
the current catalog and cannot be declared as a dependency alias. No alias or
namespace is silently rewritten to avoid conflicts.

Automatic discovery is performed later by the host: `graph.chatml` yields `Graph`,
with optional exact `graph.chatmli`. Explicit module definitions override their
catalog name. Omitted `interface` selects an optional conventional companion of
the chosen implementation; explicit `interface` must later be captured exactly.
Exclusions affect discovery/automatic pairing, not explicitly selected entry or
module/interface paths. This gives deterministic explicit exceptions rather than
search-order precedence. An explicit nonconventional implementation path is valid;
if it has no conventional `.chatml` suffix, no companion is guessed.

Example selection fragment:

```json
{
  "dependencies": {
    "Shared": {
      "source": {"kind": "local", "path": "shared"},
      "project": "chatml.project.json"
    }
  },
  "environments": {
    "app": {
      "modules": {"Graph": "self.Graph", "Queue": "Shared.Queue"},
      "requires": {"compiler": "ochat.chatml.compiler.v1"}
    },
    "none": {"modules": {}}
  },
  "default_environment": "app",
  "file_environments": {"src/graph.chatml": "none"}
}
```

An environment object supports only `modules` and `requires`. Omitted `modules`
means that environment's package-owned complete catalog, including its own aliases;
an explicit empty object means no configured source bindings. A present module map
is the complete selected set, never additive to another environment. Each reference
is exactly `self.Module` or `DependencyAlias.Module`; extra/missing components or
unknown dependency aliases reject. Existence of a discovered/dependency module is
validated after host catalog capture, not guessed during manifest decoding.

`requires` supports optional `compiler` (nonempty compiler-family name), `targets`
(unique nonempty target names), and `contract_sha256` (validated lowercase SHA256).
Omission means no added constraint. Actual recognized compiler/target compatibility
is checked by the existing compiler adapter. Routine authors do not supply opaque
digests. No declaration synthesizes or combines native surfaces.

Selection order: an explicit request environment overrides an entry's declared
environment; otherwise use the project's declared default or its literal `default`.
Library requests use their explicit selector or that package's own default. Missing
selectors produce a located error, never first-map-entry selection. File overrides
replace the whole chosen scope with another environment from the same package;
implementation and interface paths select independently. A dependency's omitted
selector uses THAT project's default, never the root/importer environment name.

Dependency objects support `source`, `project`, optional `environment` and optional
`namespace`. `project` is a portable member path. `source` is one of:

```json
{"kind": "local", "path": "../shared"}
```

```json
{"kind": "git", "repository": "https://example.invalid/library.git",
 "object_format": "sha1", "commit": "<40 lowercase hexadecimal bytes>"}
```

Local path/repository strings are nonempty and contain no NUL/line breaks; host
acquisition checks authority, transport and authentication. Git `object_format`
is explicit `sha1` or `sha256`, with full lowercase 40/64-byte hexadecimal pin.
No branch/tag/HEAD resolution or format inference. Namespace override is explicit
nonempty identity, not inferred from dependency alias/repository/path. Same effective
namespace conflicts and real source/module collisions are host/admission errors.

Entries are launch roots, and source directories supply catalogs. An entry MAY
also lie in a declared directory: discovery does not filter it. The same Source.Id
can be entry and module implementation; compiler/runtime graph integration must
use one canonical node/initialization and ordinary actual cycle rules. Examples
place main outside src to make roles clear. Availability alone creates no edge.

Pure decode/selection performs no source discovery, initializer execution or native
selection. `capabilities` is only exact tool-name intent; host adapters reuse
`Tool_capability.select` against the actual effective registry, then normal live
revalidation/permission checks. No random capability ID, callback or saved reference
is created from a name. Inherited prepared implementations keep original authority.

JSON decode reuses `Document_schema.Limits.t` and bounded `Json.decode`. Polling
occurs before/after that stage and within domain walks; Jsonaf parsing has no
checkpoint API, so the bounded parse stage is not internally interruptible.
Exceptions from supplied checkpoints escape unchanged; no callback is retained.

Chatml_project.Lock validates primitive receipt metadata and canonical encoding.
OCH-192 reconciles expected bytes/origins/own-package selections against captures;
complete portable collisions and source/scopes/association admission reuse OCH-183
with those actual captures. A lock excludes its own digest/bytes and final Prepared
fingerprint; composition encodes and captures it before183 admission.


## Chatml_project.Lock selected-build wire v1

The lock is a flat object with required `schema_version: 1`, `root`, `entry`,
`environment`, `compiler`, `required_tools`, and `catalogs`. All collections must
be present, including empty collections. Unknown fields, duplicate JSON keys and
explicit null reject. Version uses an integer number lexeme, as the declaration
codec does. No self digest, final Prepared fingerprint or error context is encoded.

A source ID is `{ "namespace": string, "path": string }`, validated by the
existing exact Source.Id constructor. The compiler is
`{ "family": string, "target": string, "contract_sha256": string }`;
family/target/environment are nonempty intent names and SHA-256 is validated
lowercase hexadecimal. `required_tools` is a unique array of nonempty exact names.

`root` is the exact selected project declaration source ID. `entry` contains
`source` (the source-entry metadata below) and `selection`. Selection is
`{ "kind": "declared", "name": string }` for a nonempty entry name in the root
project, or `{ "kind": "captured" }` for an explicitly supplied host source.
A captured entry uses the selected root environment, without inventing a project
entry, file override or containing-document position. Composition must reconcile
these facts and every overlapping catalog source before admission.

Each catalog has required `namespace`, `origin`, `declaration`,
`selected_environment`, `dependencies`, `sources`, `modules`, and `scopes`.
Origin is `{ "kind": "local" }` or a Git object with `kind: git` and the exact
`repository`, `object_format`, full `commit`, and required `package_root`.
`package_root: null` means the repository root; a string is an exact validated
portable repository directory prefix. Absence, empty strings, dot/parent paths
and other non-string values reject. Local origins reject this field. Source IDs
remain relative to the selected package root, so prefix plus declaration ID path
identifies its repository member. Explicit Git dependencies select repository
root. A Git-owned declaration's relative Local dependency resolves from that
repository member's containing directory into the same repository/full pin;
escaping the repository root rejects. An absolute Local dependency instead needs
separate host Local admission. No resolution here proves native filesystem identity.
Local locators stay in incoming
captured dependency declarations: different relative locators can select the same
host-admitted root, so the receipt cannot choose one arbitrary incoming path as
canonical provenance. No native device/inode/acquisition key is serialized.

Each dependency association is `{ "name": string, "declaration": source-id }`.
The containing catalog owns the name, using the declaration's dependency alias
syntax (`self` is reserved and rejected). Different aliases may select one declaration;
duplicate names reject even if they select the same target. Composition checks
each association against its exact owner's source intent, namespace/project member
and selected environment. Capture-only reconstruction verifies these immutable
facts; it does not re-prove native path identity or acquire authority.

A source entry is `{ "id": source-id, "sha256": string, "byte_length": integer }`;
length is nonnegative. These are EXPECTED values: no capture is verified here.
A module selection is `{ "implementation": source-id }` with optional non-null
`interface: source-id`; distinct IDs required. A file scope has required `source`,
`environment`, `bindings`; each binding is `{ "name": UIDENT, "module": source-id }`.
Module identity is the implementation ID, with no other encoded identity wrapper.

Catalog namespaces are unique. Declaration, source entries, module implementation
and optional interface, and file-scope OWNER IDs must match their catalog namespace.
Binding and dependency TARGET IDs may cross catalogs. Source/module/scope/binding/dependency keys reject
exact duplicates. No source membership, portable collision, graph, capture digest
or declared environment membership proof is inferred from this receipt.192 checks
expected bytes/origins/own-package selections against exact captures, then183
admits the same complete input;193 consumes/qualifies it. Entry may share a module
implementation ID. A decoded lock cannot list its own captured ID in sources or as its entry;
authored construction requires the host to exclude its future selected lock ID.

Canonical output fixes object field order as written above, omits absent module
interfaces, and sorts catalogs by exact namespace, source entries/scopes by typed
Source.Id, modules by implementation Source.Id, bindings/dependencies by exact name and tools
by exact name. Namespace/path remain separate strings. Encoding first bounds its
invocation-local JSON construction, then uses existing exact
`Document_schema.Json.validate_and_measure` before rendering compact JSON.
Existing JSON validation/measurement/rendering stages are bounded but have no
internal checkpoint API; poll around them and during domain traversal/sorting.
Checkpoint exceptions escape, and no callback is retained. Only the one original
error-owning declaration/lock capture is retained separately from receipt identity.

Independent handwritten qualification should use a fixture with the entry also a
module implementation, zero-length EXPECTED source entries with deliberately
unrelated digests, and a scope alias pointing into a different catalog namespace.
Pure decoding succeeds without actual captures;192 later rejects discrepancies.
Permute every nested collection and require the same canonical bytes. Change one
expected digest/length/scope/environment and require changed bytes. Reject exact
duplicate keys (even equal values), wrong owner namespace, malformed versions/pins/
IDs/null/unknown fields, own lock ID inventory, escaped output over limit; interrupt
each checkpoint and require the same exception. Source-owned errors must retain
the exact supplied capture, including output errors after authored creation.

The public lock API is nested `Chatml_project.Lock` in the SAME wrapped
`ochat.chatml_project` library. Its explicit signature references the outer
Error/Declared_at/Git_pin contracts; private declaration, decoder, receipt,
codec and lock facade modules expose no paths or representations to callers.
Both codecs share the existing invocation-local Reader directly, with ONE
validated whole-source location per decode and pure field-path updates. No
cross-library private access, duplicated parser policy or error-type adapters.
