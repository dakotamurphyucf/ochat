# Captured configured-module compilation

`Chatml_linked_program.compile` consumes an already admitted `Prepared_program`
and an actual `Chatml_compiler_contract`. It reads only captured bytes, checks
implementation/companion files through the existing checker and signature probe,
and lowers the completed graph through the existing resolver. Compilation never
executes an initializer, opens a file or calls a host tool.

An implementation and its companion each use their own configured scope. A
companion restricts external exports; selected host entrypoints still use the
entry's final private lexical scope, matching single-file behavior. All checked
units share inference identity and weak cells. Lowering waits until every check
and host contract succeeds, so an importer can specialize a dependency's weak
annotations before immutable frame slots are selected.

For example, `open Catalog` alone checks Catalog and introduces its public raw
schemes, but does not select its initializer. `let use () = Catalog.number` or a
read of an imported `number` selects Catalog even if `use` is never called.
Type references and opens in compiler-only subtrees likewise do not select those
subtrees' value dependencies for execution. The runtime order is the entry-rooted
value-edge closure in dependency order, with a canonical implementation identity
checked once even when several configured names or the entry share that identity.

The linked artifact is separate from legacy `compiled_script`. It retains exact
immutable input/captures, resolved AST, visibility, graph, selected descriptors
and static compiler metadata. Static projection deliberately drops builtin
`impl` callbacks; no raw inference type, compiler callback, runtime value or
installed capability escapes into it. Its compiler contract is computed from
actual names/types/aliases/selected requirements, then compared with Prepared's
contract before parsing. Standard `compiler.v1` identity bytes remain unchanged.
The private identity codec polls its actual conversion/escaping/copying work and
is checked against the pinned generated type schema and Sexplib machine encoding.

`Chatml_compilation.compile_prepared` reuses the existing joined Eio policy driver.
Total admitted source plus canonical metadata is checked before projection or
starting a domain. The same elapsed budget/checkpoint covers pre-domain static
projection, hashing, parsing, checking, settled lowering and bounded diagnostics.
Cancellation waits for compiler-domain cleanup. Existing text/single-capture
APIs retain their source-byte limits, runtime surface getter and execution APIs.

OCH186 delivers compilation only. OCH187 supplies execution with caller-authorized
runtime installers and an execution-owned completed-module map. Only compiled
host descriptors can be invoked through that prepared API; unselected names
cannot trigger retrospective inference or initialization. Today's evaluator
handles a canonical configured read through an explicit typed unavailable
registry boundary, producing the usual source-owned runtime failure. It never
falls back to configured names or synthetic globals. Project/file capture,
refresh and custody remain with their host adapters.
