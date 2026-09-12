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

## Checking feature coverage

`Authoring_corpus.Coverage.compiler_targets` enumerates every module, global,
export, type alias and required entrypoint on explicitly selected compiler
surfaces. Identifiers include the surface and namespace, so identically named
operations on different targets remain distinct. Structural type schemes produce
contract hashes without executing a builtin or candidate script.

A reviewed mapping names one exact target, its contract hash, a topic, the hash
of that topic's complete prerequisite closure, and example or behavioral test
references. `Coverage.audit` rejects duplicate or obsolete mappings, changed
contracts, missing or incompatible topics, unaudited prerequisites and missing
evidence. A prerequisite change invalidates the mapping even if the root topic's
own text did not change. There is no wildcard that automatically considers new
module members documented.

The report lists mapped and missing targets. `Coverage.require_complete` fails
if any supplied target lacks a valid mapping. It establishes coverage only for
the supplied inventory: maintainers must also inventory language constructs,
ChatMD declarations, native tools and semantic boundaries before claiming full
public-feature coverage. Evidence references do not substitute for running tests
or reviewing explanations.

`Coverage.grammar_targets` supplies a separate inventory derived from the actual
compiled parser. The 143 regular productions are scoped to each of the four
extension surfaces. `Coverage.grammar_mappings` accounts for all 572 targets with
literal production/action pins and reviewed program/task topic closures. The
normal offline test gate rejects missing mappings and changed production
contracts. The private `grammar_coverage_data.ml` records these maintained pins;
CI does not regenerate them. The maintainer `review_coverage --grammar` command
prints candidates for review, not an automatic approval.

This covers grammar accounting, including structural and rejection branches. It
does not prove that every production is reachable or executable, nor that lexer
rules, precedence, inference or runtime semantics are completely documented.
Those remain separate requirements of the full public-feature manifest.

`Coverage.semantic_features` maintains 30 language-rule groups covering lexical
boundaries, delimiters, inference, scope, evaluation and representation. Each
names its implementation sources, a reference topic and evidence.
`semantic_targets` scopes these groups to the four extension surfaces (120
targets). `semantic_mappings` binds literal reviewed source hashes and topic
closures, so an implementation or prerequisite change requires documentation
review. The normal offline gate requires every inventoried rule to have a valid
mapping. The combined language/declaration gate requires every embedded
implementation source to participate in at least one maintained rule group.
The docs checker also requires each named example to run and appear in the
mapped topic's prerequisite closure. File references identify separate behavior
suites; their existence does not mean the docs checker ran those suites.

This is a maintained taxonomy with coarse source drift detection. It cannot
automatically discover a newly introduced semantic feature. Reviewing changes
must include deciding whether to add or split rules. Full ChatMD and native-tool
contract accounting remains separate from this language inventory.

`Coverage.declaration_features` adds 20 groups for ChatMD markup/source capture,
generated messages/configuration/tools, lifecycle scripts, extension bindings,
schemas, selected dependencies, authoring policy/help and authored persistence.
Its 60 mappings cover the three surfaces that use ChatMD declarations. Surface
IDs select reference compatibility; they do not make authored tool declarations
legal in a generated child. The guide explicitly distinguishes those contexts.
Thirty-three implementation source pins and nine topic-closure pins are maintained in
`declaration_coverage_data.ml`. Normal tests require complete mappings, and the
docs checker verifies named XML evidence against the actual checked guide.
File evidence names separately executed behavioral suites. This extends the
maintained taxonomy with root file/shell/agent/MCP bindings, resource-bearing
messages and stored traces, including their rejection in generated children.
The six complete root examples use actual parser and generated-admission checks
without instantiating any resource. File-reader, shell, MCP and authored-agent
behavior remains qualified by the separately referenced suites. This does not
claim that every existing shell security configuration is a new E09 feature.

The runtime's [native extension catalog](../../lib/agent_session/extensibility_native_tools.mli)
constructs the nine standard execution, child-session and authoring registrations.
Runtime construction and the [native contract test](../../test/chatml_composition/native_contract_tests.ml)
use that same exhaustive catalog. The test pins each actual description, input
schema, strictness and result convention together with its reviewed reference
closure. New catalog entries need an explicit reference mapping; changed tool
contracts or prerequisite text require reviewing the corresponding expectation.
Host-dependent implementation revisions and private reference keys are excluded
from this public API snapshot. Registration inspection does not invoke tools or
borrow a session. This check detects API/reference drift; it does not replace
native lifecycle, authority, persistence or failure-behavior qualification.

`Coverage.native_features` separately records 17 reviewed native behavior groups:
one-off requests/ownership/results, validation requests and deferred checks,
reference operations and paging, captured creation/retries/lifetimes, management
authority, status/submission/output/wait/stop and the shared helper adapter.
Its 68 mappings cover readable guidance on the four extension surfaces, with
35 implementation source pins and 28 prerequisite-closure pins. These include
actual native adapters, request decoders, delegation authority, receipt/output
code and the shared session factory. Hashes detect changes to these sources;
they do not automatically discover new semantics or prove all behavior correct.
The source participation gate now covers 74 distinct language, declaration and
native implementation files. Query-service construction checks native mappings
alongside the other maintained inventories before serving documentation.

The native request guide closes the field-level gaps for `run_chatml` and all
four `ochat_validate` targets. Its complete JSON requests run through actual
native tools with a fake provider; the moderator's intentionally failing
initializer remains unevaluated. Separately run child/session helper tests cover
the lifecycle contracts. The helper-only fixture explicitly uses manual authoring
policy so automatic native documentation helpers cannot invalidate its premise.

The initial maintained `Coverage.entrypoint_mappings` covers `main` on
`one_off_v1`, `run` on `tool_v1`, and `initial_state`/`on_event` on the ordinary
and delegated moderator surfaces. The normal offline tests compare these literal
reviewed pins to the current compiler and source-derived topics.
`Coverage.task_mappings` adds the Task module and its five exports on each surface,
with the complete [task semantics guide](chatml-task-effects.md) and checked examples.
`Coverage.string_mappings`, `array_mappings`, `option_mappings`, `json_mappings`
and `hashtbl_mappings` add all exports with the checked [String](chatml-strings.md),
[collections](chatml-collections.md), [JSON](chatml-json.md) and
[table](chatml-tables.md) references. `global_mappings` covers the ten shared
globals plus ordinary moderator `print`; `json_alias_mappings` covers the common
recursive type. `moderator_data_mappings` adds Item, Context and Tool_call plus
their five data aliases on the two moderator surfaces.
`host_effect_mappings` adds Log, Turn and each surface's exact Tool exports.
Runtime control, background work and invocation-context mappings complete
`Coverage.reviewed_mappings`: all 609 compiler-binding targets across the four
extension surfaces. To update a pin, review the changed contract or
topic closure and its relevant behavior tests first; regenerating pins on each
build would defeat this check. Authoring-context service construction also audits
this maintained subset against the installed compiler and corpus; incompatible
reviewed guidance fails before queries are served. Service construction now
requires complete mappings for all four maintained inventories: bindings,
grammar productions, language semantics, ChatMD declarations and native operation
semantics. Packages remain labelled
incomplete while the other required feature mappings are missing.

After reviewing a module's implementation, writing its full reference, and running
the checked examples, maintainers can inspect candidate pins with:

```sh
opam exec -- dune exec test/authoring_sources/review_coverage.exe -- Array chatml.collections
```

The utility prints candidates and `review_required: true`; it does not edit the
maintained mappings. Review every emitted export against the guide before copying
literal pins into `Coverage`. Normal CI never regenerates those expectations.
The module coverage test compares each reviewed module against its full current
export inventory on every surface, so a new export cannot silently remain outside
that module's documentation check.

Use `--globals TOPIC_ID` or `--alias NAME TOPIC_ID` to inspect those candidate
families. `--semantics` prints the maintained language rules, source hashes and
candidate topic contracts. `--native` reports native behavior groups, their source
contracts and reference closures. `--declarations` does the same for ChatMD, defaulting
to its three compatible surfaces. `--changed-docs` lists changed topic closures
only after verifying that reviewed binding, grammar, semantic, declaration and native contracts still match;
it never updates pins. Optional trailing surface IDs select a narrower exact set;
for example:

```sh
opam exec -- dune exec test/authoring_sources/review_coverage.exe -- Item chatml.moderator-data moderator_v1 delegated_moderator_v1
```

A separate completeness test derives the core API from the compiler's
actual core inventory, checks that each extensibility surface includes that API
with its explicit `print` exclusion, and requires every resulting target to have
a reviewed mapping. That test covers core modules, globals and the shared `json`
alias; it does not claim coverage of all syntax constructs or runtime extensions.
The moderator data test requires every export of those three modules and the
five documented aliases, and checks that both the APIs and topic are unavailable
on one-off/standalone targets. The complete binding inventory adds checks for
the other runtime families; broader semantic qualification remains separate.

## Initial topic corpus

`chatmd.capabilities` explains root declarations for file roots, shell runtimes,
agent tools and MCP catalogs, plus resource-bearing messages and stored traces.
It distinguishes what a user-authored root can request from a generated child's
inherited-only selection. The flat feature orientation links to it; native
retrieval returns the complete guide without activating the described resources.
Its six complete root examples are checked in the shared
[capability guide](chatmd-authoring-capabilities.md).

`runtime.native.requests` explains native computation inputs and limit overrides,
shared deadlines and outcome handling, plus the four validation request shapes
and interpretation of reports. It is readable on all four extension surfaces;
the computation and validation tool descriptions link directly to it. This does
not make native tools available outside the invoking scope's selection.

`authoring.reference` explains the query helper itself: its single strict request
shape, task and feature IDs, search versus full-topic retrieval, selected schema
and signature references, completeness flags, budgets and continuation recovery.
It is readable on all four extension surfaces. The native tool description links
directly to it. Its three source-pinned sections come from the
[query guide](authoring-context-tool.md); operator configuration stays outside
these model-facing excerpts. Offline integration checks run both installed JSON
requests through pagination and retrieve the complete topic through the native
tool in a persisted session using a fake provider.

`chatmd.definitions` is a flat agent-definition reference for instructions,
generation settings, literal markup, captured sources, inherited tools and
lifecycle moderation. It is directly prepared for standalone tools, moderators,
child agents and background workflows. Generated-validation diagnostics link
back to it. Nine complete XML examples pass through non-executing generated
validation, including rejected configurations and a failing initializer that
must remain unexecuted. The native repair fixture retrieves an actual XML
example from this topic and uses it to repair an inherited-tool declaration.
Broader ChatMD declaration accounting and native-tool semantic coverage remain
separate qualification requirements.

The flat `chatml.inference` and `chatml.evaluation` references explain annotation
arity, structural types, generalization, lexical boundaries and execution order.
They are included in every prepared authoring task and directly linked from the
language orientation. Checked examples include expected diagnostics and eager
runtime failures, including the documented generic-equality limitation.

The flat `chatml.task-effects` topic explains sequencing, repeated interpretation,
nested tasks, task versus pure failures and local/external rollback boundaries.
It is included in every prepared task package and in the language orientation's
direct reading routes. Six examples are executed offline, including two expected
runtime failures; those failures are tested rather than advertised as recoverable.

`chatml.strings` adds complete byte-oriented string operations, literal search and
replacement, UTF-8 boundaries and immediate errors. `chatml.collections` adds
arrays and options: transformations, shallow mutation, callback ordering, searches,
eager defaults and explicit interpretation of task arrays. Both are flat topics
available on every extensibility surface with shared language/task prerequisites.

`chatml.json` covers all JSON operations and the recursive type, including borrowed
mutable payloads, duplicate-key policies, syntax-only validation and nonfinite
export failures. `chatml.tables` covers every Hashtbl operation and local mutation
recovery boundaries. `chatml.globals` explains value rendering, reflection and
surface availability. Eleven further checked examples accompany these guides.

`chatml.moderator-data` covers all Item, Context and Tool_call exports plus the
item/tool_desc/tool_call/tool_result/context aliases. Three complete moderator
examples each execute on ordinary and delegated surfaces, verifying state returned
from `on_event` after `Session_start` with synthetic context and no host operations.
The checker honors explicit `also_run` metadata and reports additional executions
separately; existing integration-fixture `also_check` means compilation only.
This proves data-helper behavior, not daemon projection or permission enforcement.

`runtime.effects` documents Log, Turn and Tool with explicit per-target availability
and actual host boundaries. A linked moderator fixture runs on both targets with
a fake probe tool, checking logs/external effects surviving recovery or failed
commit, discarded staged edits, decision conflicts, turn aliases and halt intent.
Separate daemon composition exercises the versioned Tool.spawn alias through the
owned job service. Readability of the guide does not install its host services.

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
The topic corpus alone does not provide complete task packages, native tool
schemas, full feature coverage or model-context insertion. The separate
[authoring query tool](authoring-context-tool.md) adds flat feature orientation,
retrieval budgets and pagination over this foundation.

The additional broad `chatml.programs` topic covers the complete
[program-writing guide](chatml-authoring-language.md): source/operators,
functions/loops/modules, matching/types, structured-data utilities and effect
boundaries. It includes the differences/tasks/modules prerequisites and is
included in every `prepare` package. Its four complete examples run with no host
operations: nested comments/precedence, lexical captures and iteration, recursive
data with open record matching, and a string/array/table/JSON pipeline. This is
one flat guide, with whole sections preserved during pagination.

`runtime_foundation` includes those eight language topics and seven topics from
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
standalone surfaces. The corpus still does not cover every language/native schema
construct; retrieval and model-context integration use the separate query and
materialization services. Public enablement awaits complete qualification.

The [topic tests](../../test/authoring_sources/topic_tests.ml) cover fenced-source
boundaries, shared dependency order, invalid graphs, incompatible surfaces and
stale review pins. The documentation gate checks source parity, whole-guide topic
coverage, language examples (including expected errors), additional
moderator surface executions, and exact source/entrypoint checks for the
runtime integration fixtures. The invocation and background moderator fixtures
also compile against the delegated surface. Their actual tool/state/restart behavior is checked by the
linked integration tests. Review pin updates must be
accompanied by semantic review and the relevant checks, rather than automatically
accepting new hashes after a failure. The child reference separately checks its
complete JSON creation request against the source fixture, actual creation
decoder and non-executing generated-definition validator with an unusable file
tool fixture. This checks captured imports and delegated moderator compilation;
it does not claim session creation, provider execution or the full lifecycle matrix.
