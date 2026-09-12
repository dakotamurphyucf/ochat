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
They do not yet provide complete task packages, native tool schemas, full feature
coverage or model-context insertion. The separate
[authoring query tool](authoring-context-tool.md) adds flat feature orientation,
retrieval budgets and pagination over this foundation.

`runtime_foundation` includes those seven language topics and seven topics from
the [execution-contract reference](chatml-authoring-runtime.md):

| Topic | Available authoring targets |
|---|---|
| `runtime.invocations.contracts` | All four versioned script surfaces |
| `chatmd.declarations.schemas` | Standalone tools and ordinary/delegated moderators |
| `runtime.authority.tool-selection` | All four versioned script surfaces |
| `runtime.invocations.validation` | All four versioned script surfaces |
| `runtime.invocations.one-off` | One-off scripts |
| `runtime.invocations.standalone` | Standalone tools |
| `runtime.invocations.moderator` | Ordinary/delegated moderators |

The target-specific topics include their language, authority, validation and,
where applicable, declaration/schema prerequisites. Requesting the standalone
entrypoint topic on a one-off target fails with the requested topic's identity.
The same corpus adds five topics from the
[persisted-child reference](chatml-authoring-children.md), available as reference
context on all four script surfaces:

| Topic | Contents |
|---|---|
| `runtime.delegation.generated` | Captured ChatMD, inherited declarations and validation |
| `runtime.delegation.creation` | Retry identity, stopped default, lifetimes and authority |
| `runtime.delegation.submissions` | Message receipts, status and terminal waits |
| `runtime.delegation.output` | Non-consuming reads, output waits, fragments and cursor recovery |
| `runtime.delegation.stop-helper` | Stop receipts, outcome decoding and scoped helper access |

These form a prerequisite chain after shared language/authority/validation
context. A surface's ability to assemble this reference does not establish that
its host has installed any lifecycle tool. All ten validation routing IDs now
resolve to installed topics with matching human-document sources. These are
reviewed foundations, not complete background-workflow or child-agent packages.
The full feature/native-schema inventory, policy filtering and public helper
remain separate requirements.

Eight additional topics partition the
[background reference](chatml-authoring-background.md):

| Topic | Available targets and contents |
|---|---|
| `runtime.jobs.owned` | All four surfaces: owned starts, status, results and cancellation |
| `runtime.recovery.background` | All four surfaces: staging, authority, interruption and retries |
| `runtime.jobs.acknowledgement` | Moderators: pending outcomes and actual completion events |
| `runtime.jobs.subscriptions` | Moderators: lifetime, epochs, links and terminal winners |
| `runtime.jobs.timers` | Moderators: one-shot checks, misfire and bounded polling |
| `runtime.delivery.notifications` | Moderators: acknowledgement ordering, safe input boundaries and wakes |
| `runtime.delivery.ingress` | Moderators: scoped external producers and data events |
| `runtime.jobs.shell-example` | Moderators: the complete checked X03 coordinator |

Job guidance includes transaction/recovery prerequisites. Notification guidance
includes acknowledgement rules, and the shell example depends on that full
notification context. Moderator-only topics reject assembly for one-off or
standalone surfaces. These 27 topics still do not cover every language/native
schema construct or implement the five task packages and public retrieval service.

The [topic tests](../../test/authoring_sources/topic_tests.ml) cover fenced-source
boundaries, shared dependency order, invalid graphs, incompatible surfaces and
stale review pins. The documentation gate checks source parity, whole-guide topic
coverage, the 18 language examples and exact source/entrypoint checks for the
four runtime integration fixtures. The invocation and background moderator fixtures
also compile against the delegated surface. Their actual tool/state/restart behavior is checked by the
linked integration tests. Review pin updates must be
accompanied by semantic review and the relevant checks, rather than automatically
accepting new hashes after a failure. The child reference separately checks its
complete JSON creation request against the source fixture, actual creation
decoder and non-executing generated-definition validator with an unusable file
tool fixture. This checks captured imports and delegated moderator compilation;
it does not claim session creation, provider execution or the full lifecycle matrix.
