# Bounded frozen type display

`Chatml_type_display` renders the immutable `Chatml_type_snapshot` graph for
authoring responses and developer tools. `chatml-display-v1` is an IDE display
format, not ChatML annotation syntax, a semantic equality key, or a host
authority token. Callers must retain the snapshot and use roots from that
same owner; a foreign root is rejected before rendering.

One `Session` belongs to one invocation. Its unique-reachable-node and
structural-edge scan ceilings, traversal-work allowance and output-byte limit
cover all renders in that invocation, including renders from different
snapshots. Later row/payload visits, label hashing and comparisons consume the
same work allowance. Each item also has a text-byte cap. A complete item means
the renderer visited its reachable frozen graph within the limits; a
truncated item carries the first limit it met. With a tiny byte allowance the
text may be empty while the typed truncation remains truthful. Cancellation
and unexpected checkpoint exceptions propagate unchanged. The session owns
only accounting; per-graph cycle and sharing state is rebuilt for each render.

Ordinary types use familiar help text: `fun(int, string) -> bool`,
`(int * string)`, `{name: string}`, and ``[`Ready | `Next(int)]``. Nested
functions and recursive binders are parenthesized when nested. Closed record
and variant rows with no shadowed labels display as fields and cases. Duplicate
raw row layers stay explicit as
`record_raw(row_layer{...} over ...)`; internal shape, payload and receiver
nodes also carry explicit fallback names. The text keeps function argument and
tuple arity, raw override order, module/record/variant distinctions,
quantified and free variable identity, and lexical recursive binders. A
a repeated compound node rendered explicitly, or a cycle, can get a
snapshot-local `#index` anchor;
scalar and variable leaves display without extra anchors. These marks only
explain sharing inside one display and never compare types across snapshots.
Independent quantified binding schemes may each use `'a` as their local name.
Labels from compiler-owned data are escaped into valid UTF-8 and never emit
terminal control characters.
Consumers should use the typed completeness and source/graph identity from the
surrounding analysis result rather than parsing this text.

This library has no protocol, UI, host, Eio or runtime dependency. The semantic
query adapter can use it to render type schemes under the same request budget;
the terminal REPL can use it for a value's known frozen type without executing
or rechecking source to obtain presentation text.
