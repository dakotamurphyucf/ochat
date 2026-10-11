# Configured compiler dependencies

This library records the compiler dependencies of an admitted
`Chatml_prepared_program.t`. It performs no filesystem discovery, initializer
execution, tool call or provider call. A catalog entry being available creates
no edge. The existing single-file compiler entrypoints retain their behavior;
project compilation and runtime linkage are separate adapters.

The existing checker resolves local, parameter, recursive, opened and builtin
bindings first. On a genuine miss it consults the current captured file's selected
scope. Different configured names for one `Module_id` share one checked node.
Imported files receive authentic builtins and their own selected scope and
lexical definitions; they cannot see importer-local bindings. An entry that is
also catalogued is one canonical node with its ordinary optional companion.

`Chatml_configured_lookup` creates immutable file contexts and located reference
facts. Contexts and references carry the admitted Prepared fingerprint, canonical
owner, source identity/revision and implementation/interface phase. The dependency
session checks this affinity and its active owner before loading a target or
recording an edge. It checks targets on demand, caches only completed units and
rejects active targets at the closing reference. Related sites contain only the
cycle segment in chronological order; longer witnesses explicitly show the last
eight preceding cycle sites. The positive depth limit is a caller parameter.
Checkpoint exceptions propagate unchanged. Rejection and cancellation publish no
partial graph or checked-unit cache.

Configured `open` introduces raw public schemes and immutable canonical member
provenance without requiring an initialized module. Actual imported value reads,
including reads inside uncalled functions, create value edges. A later local
binding clears imported provenance; earlier checked closure reads keep their
original target. Existing inline and first-class module opens retain their
semantics. Alias conversion uses the same static lexical lookup and captures
immutable defining fallback namespaces, never the dependency callback. Private
alias expansion does not replay defining-source references as caller references.

The accepted runtime convention distinguishes compiler lookup from initializer
selection:

- `Open` and `Qualified_type` edges require compiler checking.
- `Value_reference` edges also select runtime dependencies.
- Runtime selection follows only value edges reachable from the entry, then
  filters the completed compiler order. Value edges in a subtree reached only
  through types or opens do not enter that runtime closure.

`Chatml_dependency_graph.runtime_dependency_order` computes this pure selection;
it executes nothing. All compiler edges still participate in cycle rejection
and compiler order, irrespective of their runtime classification.

`Chatml_configured_check` borrows the existing inference/export/signature paths,
with one shared inference identity and weak-cell owner and fresh owned annotation
maps per file. Each independent file starts at inference root level, restoring
its importer's level on every exit so mutable exports cannot gain polymorphism
through an importing let or closure. Raw schemes remain compiler-owned and can
share weak cells; they are not immutable semantic snapshots or transferable
compiled artifacts. Discard the engine and its exports after invocation failure.
Checked annotations identify canonical value reads and inert configured opens.

OCH-185 supplies these actual checker hooks, provenance, graph facts and pure
value-closure selection. OCH-186 supplies companion matching, entrypoint and host
contract validation, canonical resolver lowering and the immutable linked
artifact. OCH-187 supplies execution/session registries and initializer execution
under that artifact's policy. This boundary alone does not enable configured
project execution in existing host entrypoints.

The tests in `test/chatml_graph` combine actual parsed checker fixtures with an
independent bounded fixed-point graph model. They cover compiler/value reachability,
canonical alias/entry overlap, owned cycle segments and truncation, source/input/
phase affinity, active callback admission, every observed cooperative checkpoint,
lexical shadowing, nested imported paths, private alias scopes, raw polymorphism,
shared weak storage and source/configuration-owned lookup failures.
