# Captured project composition

`Chatml_project_build` connects already captured project catalogs to the compiler's
immutable `Chatml_prepared_program`. It performs no file reads, Git requests,
initialization, or tool execution. Acquisition and durable publication belong to
the host; this library admits the exact material that host supplies.

For a new build, construct a `Selection_gate` before acquiring selected
dependencies. Check root Git intent before its acquisition, then each newly
selected dependency against that gate before acquisition. Use
`selected_dependencies` and `Dependency` projections to reuse the same captured
scope and owner-relative path rules as composition. Pass explicit remaining work
limits to each call and charge its immutable receipt exactly once, then
pass the same gate to `compose`. Root validation returns an immutable prior-owner
association; each dependency check takes that association and returns the prior
target association for the next package. These values belong to the exact gate
instance. Traverse prior owner-local edges explicitly, checking every incoming
route when aliases converge. Namespace or project basename alone never locates
a previous owner. Each `Catalog_input` carries its captured catalog,
origin, attempt-local acquisition identity, and owner-local dependency selections.
The acquisition identity is used to reconcile repeated aliases and does not enter
the completed result or lock. Different aliases may reference one catalog;
conflicting identities cannot be resolved by traversal order.

Composition selects the root's environment and every captured file's own package
environment, including companion interfaces. It resolves their configured module
bindings, checks their requirements against the actual compiler contract, and
unites requested tool names. Those names are requests for subsequent host
validation, not saved permissions. Available modules do not imply runtime edges
or initialization; the compiler and linked runtime own those decisions.

A declared entry uses its actual project declaration and file scope. An explicitly
captured entry uses the root library's selected environment without inventing a
file override or containing-document origin. An entry overlapping a catalog source
must agree in exact revision, bindings, and source-owned scope locations. The
result contains one source for that identity.

The selected lock records root/entry selections, Git origin intent, source
digests and lengths, module associations, file scopes, and dependency selections.
The lock excludes its own source identity and content hash, avoiding a recursive
hash. Composition encodes and captures the completed lock before admitting the
whole inventory through `Prepared_program.create`.

## Restoration

`restore` takes the exact lock capture plus all other source captures, each once.
It decodes the actual project declarations and rebuilds their catalogs through the
existing discovery planner using only the supplied captured members. It then
reconciles the complete reconstructed receipt against the decoded lock and admits
the compiler input. Missing, duplicate, surplus, or inconsistent material rejects.
Omitted optional companions describe the saved selection; they do not prove which
files existed on the original machine.

Valid JSON whitespace and key order are retained in the original lock capture.
Receipt comparison uses canonical metadata, but restoration does not replace the
saved bytes with a newly generated document. Consequently, different valid lock
formatting can have the same selected metadata and a different complete Prepared
fingerprint. Queries and restoration never refresh source files or fetch packages.

Git source identities remain package-relative. A Git origin separately records an
optional repository package-root prefix. A relative Local dependency inside a Git
manifest resolves from that manifest's repository directory within the same pin;
escaping the repository rejects. Absolute Local paths still require host admission.
These portable receipt facts do not prove native identity or authenticate Git
objects; those checks remain acquisition responsibilities.

## Captured role recomposition

`recompose` takes an already admitted Build with a captured entry and root Library
selection. It reconstructs the same exact catalogs and entry through restoration's
shared validation, then generates a distinct lock under the supplied authentic
contract. The caller chooses a lock ID different from the old lock and every
retained source ID. Sources, environments, module scopes and dependency selections
stay fixed; the original Build remains unchanged. Declared entry builds reject,
and authored compiler/target/contract requirements still apply.

For example, a retained moderator library can be recomposed for the delegated
moderator contract before the host compiles and instantiates that actual role.
This operation needs no original files and executes no initializers. It does not
retarget the old lock or weaken restore. A subsequent restore verifies the new
lock normally against the new contract. Outer artifact custody checks collisions
against its other programs and continues to own source persistence.

## Bounds and validation

Composition and restoration share an invocation-owned cooperative work budget.
Input byte accounting includes the finished lock and Prepared metadata; document
limits also bound lock decoding/rendering. Checkpoint exceptions propagate without
publishing a partial result or retaining a callback. Selection-gate calls have
explicit remaining limits and return their observed work receipts. The immutable
gate retains no limits or counter. The host charges those receipts across its
one aggregate preparation budget and deadline, including acquisition and final
composition. A failed projection has no partial accounting receipt: abort the
host attempt rather than retrying with a reset allowance.

Normal Build permits different selected environments and effective namespaces
while preserving Git repository, object format, full commit, package root and
project member. Newly selected Git dependencies and source-kind/code changes
require Update. Actual declarations/scopes still validate configuration; this
permission does not grant native access or bypass compiler compatibility.
For an authoritative new script slot or a known prior Provided input,
`Build_without_previous` permits a new Local root and selected Local closure.
It rejects every selected Git root/dependency before acquisition and repeats that
check during complete composition. It is not a recovery mode for missing or
corrupt historical metadata; the host must distinguish those states. Initial
admission and explicit dependency updates keep their separate operations.

Selected source subsets can differ at the same pin. Overlapping package-relative
members retain their digest and byte length despite namespace changes. A prior
omission does not prove a member was absent; authentic current Git object reads
remain the acquisition owner's responsibility. Complete composition independently
walks every current/prior owner pair, including finite declaration cycles and
converging aliases, under its bounded invocation budget.

The public tests in `test/chatml_project_build` use actual declarations and
catalogs with independently stated scope and identity expectations. They exercise
canonical permutations, shared requirements, exact capture restoration,
noncanonical lock bytes, rejected inventory mutations, bounds, and interruption.
They establish these tested invariants, not a proof of historical filesystem state
or correctness of an external Git transport.
