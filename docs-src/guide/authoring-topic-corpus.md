# Source-derived authoring topics

[Authoring_corpus](../../lib/authoring_sources/authoring_corpus.mli) assembles
stable topics from the [installed reference sources](authoring-source-bundle.md).
A trusted topic specification names exact Markdown headings, source documents,
compiler surfaces and prerequisite topics. The implementation retrieves only
installed bytes. It performs no provider, tool or filesystem operation.

Section extraction preserves original bytes, including code fences and line
endings. Headings inside backtick or tilde fences do not define topic boundaries.
An excerpt can include nested subsections or stop at the next heading. Missing
or ambiguous headings and unclosed fences reject the source instead of returning
a misleading fragment. This is extraction for maintained ATX-heading documents,
not a general Markdown renderer or HTML parser.

Corpus construction checks dependency existence, cycles and surface compatibility.
Every prerequisite must be available on every surface declared by its dependent
topic. Assembly takes an actual host-selected surface and unique topic roots,
visits prerequisites first and includes a shared prerequisite only once. Declared
dependency and root order determine the result. An unavailable topic fails
explicitly; the assembler does not silently substitute another target or omit a
required topic. These checks do not grant the compiler surface's operations.

Topics retain their source-document hashes and exact excerpt hashes. Their
identity also includes the specification, prerequisites, surface set and review
metadata. The corpus identity binds the installed source bundle and its topics,
independently of manifest declaration order. A retrieval host must still bind the
runtime build, effective tool capabilities and resolved authoring policy.

`Pending` records unfinished semantic/example review. `Audited` records evidence
references and pins the ordered excerpt hashes; changed excerpts fail construction
until reviewed again. Evidence metadata is supplied by trusted maintainers, not
by model requests, and is not itself proof of a full-feature audit. An empty
pending list says only that the entries in that manifest were reviewed. Missing
public features still require a separate coverage manifest and CI checks.

The initial `language_foundation` provides these topics from the
[checked OCaml-differences guide](chatml-ocaml-differences.md):

| Topic | Contents |
|---|---|
| `chatml.introduction` | Language identity and the examples' one-off contract |
| `chatml.syntax.calls` | Explicit calls, arity and wrappers |
| `chatml.syntax.containers` | Array, record and variant syntax |
| `chatml.types` | Record joins, match closure, recursive types and mutation |
| `chatml.modules` | Module exports and qualified access |
| `chatml.operators` | Operators and builtin calling conventions |
| `chatml.tasks` | Bind/map, deferred failures, catch and JSON |

All use the shared introduction as a prerequisite. The examples remain labelled
as one-off candidates even when retrieved for an author writing a standalone tool
or moderator; each target still needs its own entrypoint guidance. The seven
topics partition that guide, not the full language specification or runtime API.
They do not yet provide complete task packages, native tool schemas, feature
coverage, token budgeting, pagination or model-context insertion.

The [topic tests](../../test/authoring_sources/topic_tests.ml) cover fenced-source
boundaries, shared dependency order, invalid graphs, incompatible surfaces and
stale review pins. The documentation gate checks source parity, whole-guide topic
coverage and the 18 actual compiler/behavior examples. Review pin updates must be
accompanied by semantic review and the relevant checks, rather than automatically
accepting new hashes after a failure.
