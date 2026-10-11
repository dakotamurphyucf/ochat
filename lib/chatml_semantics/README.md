# Compiler-owned semantic queries

`Chatml_semantics.analyze` checks one admitted `Prepared_program` through the same
`Checked_graph` driver used by linked compilation. It freezes all recorded type
groups together after companion, importer and selected host requirements settle.
Analysis does not initialize modules or call native implementations. The
`Compilation.analyze_prepared` adapters run this path in the existing joined Eio
compiler worker, with the same source admission and cooperative elapsed policy.

A `Complete` snapshot retains immutable source captures, the admitted inventory,
actual compiler facts, the dependency graph and one frozen type graph. It contains
no inference cells, recorder, callback, runtime surface or execution state. Type
IDs preserve generalized quantifier groups, shared unquantified cells, equality
constraints, row layers and recursive binders. They are local graph coordinates,
not a cross-compilation type identity or rendered-string equivalence.

A symbol describes its principal implementation definition. A lexical or exported
binding can refer to that same symbol while selecting a companion's scheme and
restricted members. Queries preserve both views and specification links. Host
symbols retain their actual selected contract and member path, without fabricated
source locations. Configured module availability comes from the exact file scope
and is separate from actual lexical candidates and dependency edges. Type and
open-only edges do not imply initialization; runtime order uses the entry-rooted
value dependency closure.

Points are validated zero-width **byte** locations, including EOF, carrying a
fresh snapshot owner witness. Points, symbols and contexts from another snapshot
are rejected, even for identical source bytes. Reference queries use authored
parser selections. Type queries can use the smallest containing compiler expression
region. Context queries use actual reference sites, expression starts and statement
or unit anchors; a containing expression does not certify visibility at an inner
whitespace gap. Unsupported gaps are explicitly unavailable. No query loads source,
rechecks types, guesses a scope from nearby syntax or updates a global cache.

Invalid programs return a separate `Failed` partial result containing only owned
diagnostics and syntax tables from units actually parsed in that same invocation.
A failed parse has no recovery table. The failure phase comes from the actual
parse, implementation, companion, dependency or host-requirement boundary.
Contaminated raw types, contexts, exports and initialization graphs are never
published through this result. Explicit admission and limit failures are typed errors;
cancellation and unexpected exceptions remain exceptions.

Every analysis has explicit input, recorder, freezer, publication and cumulative
work limits. Phase capacities add to a bounded retained plan; they are not a heap
sandbox or reset per module. Publication admits facts, edges and label bytes before
retention, and query pages have their own result/work limits and explicit
truncation. The fixed standard contract projection in the domain adapter runs under
the elapsed policy before the semantic work ledger begins. Fingerprinting, checking,
freezing and indexing then share that one work ledger. Work is cooperatively polled,
so an uninterrupted parser step is not claimed to be preemptible. No output is
returned until its final checkpoint succeeds and the compiler worker has joined.
