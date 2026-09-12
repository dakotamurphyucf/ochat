# Query authoring documentation from an agent

`ochat_authoring_context` retrieves installed ChatML, ChatMD and runtime reference
text without network access or model calls. Internally qualified extension hosts
now supply this helper and `ochat_validate` automatically for declared authoring
tools such as `run_chatml` and `agent_create`. You can also explicitly declare
`<tool name="ochat_authoring_context"/>`. General public enablement remains pending;
the current corpus is a reviewed foundation, not complete feature coverage.

The default `auto` policy inserts one shared primer before the first model request
that can author code, with deeper retrieval available through the helpers.
`<authoring_context policy="manual"/>` adds neither prose nor helper tools; declare
the helpers yourself when wanted. `preload` adds complete requested topic closures,
for example `<authoring_context policy="preload" topics="chatml.tasks"/>` alongside
`<tool name="run_chatml"/>`. Shared prerequisites appear once, and incompatible or
over-budget preloads fail admission. Agents without authoring tools gain no
automatic tools or prose. Intent comes from explicit implementation/help metadata,
not from a tool name containing “script.”

The shared [authoring primer](chatml-authoring-primer.md) is now included in the
installed corpus as `authoring.primer`. Its `let*` example is compiled and run by
the offline documentation checks. It introduces the language, entrypoint kinds,
effect boundaries and a flat map of useful features; references to deeper topics
do not claim that their complete contents have already been supplied.

For runtime integration, [Authoring_materialization](../../lib/chat_response/authoring_materialization.mli)
turns an admitted policy into source-labelled user-role reference messages. It
uses the same installed corpus and task/surface resolver as retrieval, deduplicates
preload prerequisites and rejects a batch that exceeds its configured budget.
It binds messages to the corpus, runtime, selected capabilities, policy and owning
session/generation scope. Refresh checks actual effective history rather than a
cached claim that guidance was once sent. Protocol and provider-history roundtrips,
edited/redacted context and cross-session invalidation are covered offline.

The assembler does not itself install tools or commit model input. The shared
foreground worker now accepts an optional host-owned guidance factory. Before
each provider request, the actor checks the session/generation and canonical
history snapshot, then reserves IDs and appends missing guidance in one
transaction. The driver supplies the exact effective entries and moderator
provenance captured with that request. A same-text moderator replacement cannot
impersonate installed guidance. Failed persistence prevents the request and saves
neither the reference entries nor their ID reservation.

Successful additions enter both canonical history and provider input, without a
new user-submission event or a second append by the stream callback. They are
inspectable through history/export and survive persistence. Provider retries do
not repeat insertion, and legacy fork calls do not inherit the root hook.
Root registration now installs these factories after resolving the actual native
and managed tool registry. Generated agents use their admitted policy and their
own session scope. Automatic helper selection stays within the parent's requested
delegation ceiling: omitting required helper bindings rejects admission rather
than widening the child's authority. A child merely consuming an ordinary tool
does not inherit implementation-authoring prose. Runtime reload rechecks the
effective history before inserting anything again.

Durable session schema 18 retains a bounded reference index alongside history:
at most 64 recent receipts and 64 KiB of encoded metadata with the current host
defaults. The index records history IDs, topic hashes and source provenance,
without retaining topic prose. History appends and compaction update it in the
same transaction as the history change. Compaction also captures references from
older snapshots that had no index; restore validates its version and owning
session/generation. Eviction is explicit through a sticky truncation flag.
Explicit history deletion forgets the deleted occurrences, and generation resets
clear the index. Rediscovery pointers do not count as new documentation reads.

The model-input boundary uses these receipts for presence checks, but a receipt
never satisfies missing, modified or stale documentation. Missing historical
references now produce a visible, metadata-only rediscovery message with stable
topic IDs, remembered/current hashes, source labels and current entrypoints.
Unavailable authored packages and compiler targets are omitted. Manual mode lists
only explicitly selected helpers, adds no primer or tools, and suppresses optional
guidance with unavailable package dependencies. Ordinary agents get no pointer.
Topics being preloaded again do not also need a pointer, and current effective
pointers prevent duplicate metadata. Changed or redacted entries cannot satisfy
presence checks.

Pointer defaults allow 32 topics and 8192 content bytes, retaining whole recent
entries with an explicit truncation flag. Existing current pointers consume the
same aggregate allowance, with their serialized payload bytes charged
conservatively; repeated turns cannot keep expanding a truncated pointer.
This metadata is a bounded selection of remembered references, never retained
topic prose or a complete retrieval audit. See the
[index contract](../../lib/chat_response/authoring_reference_index.mli) and
[pointer contract](../../lib/chat_response/authoring_rediscovery.mli).

The lookup service now offers `Authoring_context.query_with_receipt` to runtime
integrators. It returns the unchanged strict JSON response plus separate,
host-produced metadata for its exact emitted items: topic/source hashes, fragment
indexes and totals, response digest, caller scope, host, surface and capabilities.
Selected tool/schema and compiler-signature references have their own complete
item-sequence hashes. Search excerpts and rejected or empty pages produce no
read receipt. A final page is not marked as containing a complete topic unless
all of that topic's fragments occur on that page.

These fresh query records have no model-input decoder or public constructor.
Native and helper lookups record them through an expiring invocation scope. The
actor retains a validated annotation atomically with the final successful outcome
only when its response digest matches the disclosed value. Failed, cancelled or
replaced results receive no annotation. Session schema 19 introduced these annotations;
invocation JSON uses schema 12 when one is present. Nested script reads retain
their own invocation identity and do not mark the enclosing script's summary as
read documentation. Retained helper borrows expire when their invocation ends.

Moderator handlers use the same receipt rules at their earlier commit boundary:
the disclosed result, receipt and proposed moderator checkpoint are committed
together. Dispatchers retain the annotated value they submitted, including across
execution-context handoffs. Replacing the response, returning a failure or losing
the commit prevents a new receipt. Later observation preserves the original read
and does not open a fresh documentation-read scope for the completed invocation.

Effective-history checks support version-2/3 reference provenance with bounded part
indexes, hashes and total counts. A topic counts as present only when all parts
remain under matching source/version, context and policy identities. Duplicate
pages do not fill gaps; conflicting hashes or counts prevent combining the pages.
Preload refresh and rediscovery both use this coverage. Version-1 complete guidance
remains readable, and metadata-only pointers never supply topic content. The offline
integration test exercises real query pages through history restore, missing and
altered pages, changed context/source versions, and conflicting fragment evidence.

An invocation annotation alone does not prove delivery to a model. Publication
now attaches version-3 provenance to the actual model tool-output occurrence when
the verified lookup matches its owning authoring context. The result and its
history/index update commit together. Internal script reads remain internal.

Version 3 also preserves the queried compiler surface. After compaction,
`reference.signatures` and `reference.tools` pointers name a currently enabled
query task and carry hashes from the same compiler/schema inventories as real
lookups. Different compiler surfaces remain separate; all pointers share the
topic/byte allowance. Narrowing tools recomputes the selected schema hash, and
withdrawing a target removes its pointer. Explicit reference-only configurations
can rediscover documentation without acquiring execution tools or a primer.
When a reference helper is selected, rediscovery covers the same host-enabled
compiler targets as direct queries. Selecting an execution tool does not narrow
that readonly access. Without a reference helper, rediscovery uses the selected
authoring tools' supported tasks. Neither path grants execution capabilities.
Older version-1/2 provenance remains readable, but virtual references without a
recorded surface are omitted from rediscovery rather than assigned a guessed target.

Session schema 20 retains a compact authoring policy/context binding established
before the model request, including for explicitly declared helper-only tools.
Manual mode adds no documentation prose for this bookkeeping. Crash recovery can
publish a saved outcome with the same provenance without rebuilding a runtime or
rerunning tools. Existing output occurrences are validated and reused. Legacy or
different-context outcomes remain ordinary outputs when no matching binding is
available; the runtime does not invent author-policy metadata. Annotated outputs
must match their original receipt, and model call occurrences remain canonical.

An offline daemon integration now retrieves a paginated background-workflow
package, compacts its actual conversation, restores the persisted snapshot/index,
and checks the next model input for a manual-policy rediscovery pointer. It then
retrieves the coordinator example again, validates the retrieved moderator, and
executes its completion parser through `run_chatml`. The fake provider supplies
the transcript; the compactor uses its offline summary branch. This proves the
runtime flow, not a model's independent ability to author correct code. The broader
X10 coverage also includes an actual narrowed generated child running that same
flow on its delegated moderator surface. Parent-only tools and instructions stay
out of the child's queries and context. Simulated runtime, capability and surface
changes reject old continuation cursors, classify old pages as stale, and require
fresh full-page coverage before another moderator authoring check.

Transient embedded hosts explicitly report that persisted child execution is
unavailable. Preparing a child-agent package or requesting the `child_sessions`
feature returns that incompatibility; the feature map disables the suggestion and
the selected creator description explains the limitation. Direct readonly topic
retrieval and static ChatMD validation remain possible. Successful validation
cannot make `agent_create` available: its normal execution check still rejects
the operation. The limitation participates in host identity and survives
compaction; it does not silently change manual policy.

General public extension qualification remains open.
Manual mode inserts no automatic documentation prose; it can preserve the compact
metadata above for earlier reads. The current complete serialized primer payload
measures 953 estimated tokens using UTF-8 bytes divided by three, rounded up;
this exceeds the initial 800-token engineering target and is not a tokenizer or
model-quality measurement.

An explicitly authorized shell helper can query the same service using
`operation: "reference"` in the [private helper envelope](../bin/ochat_agent_helper.doc.md#authoring-requests),
with the strict request below as `arguments`. This lets a ChatML moderator implement
its own authoring tool through an admitted shell integration. The helper can also
request non-executing validation with `operation: "validate"`. Each operation needs
its own host grant and a configured authoring target; neither requires a native
authoring tool in the agent's tool list.

For a repair loop, submit the candidate to `ochat_validate` with its intended
target and explicit tool selection. Each diagnostic includes `topic_ids`: retrieve
those topics with the same authoring task, follow any continuation, and use the
documented syntax and entrypoint contract to revise the candidate. Submit the
revision for validation again. Reference responses include prerequisites and
label complete examples by name and compiler surface; rejected examples are
explicitly marked as expected rejections.

The `validation_id` identifies the checked source and context. Editing the source
or changing the selected tools produces a different identity. It is not a token
to pass to `run_chatml`, and it cannot authorize execution. A script can pass
static validation yet fail when it calls an unselected tool or supplies a path
outside the tool's allowed roots. Validation does not evaluate initializers,
run the submitted tool calls or create a session from a submitted ChatMD bundle.

Start with `prepare`. Its first item is a flat feature map explaining what each
feature enables, when to use it and the guides to read before implementing it:

| Feature | Why an author would use it |
|---|---|
| ChatML language and tasks | Write deterministic transformations and compose tool effects with `let*` |
| One-off scripts | Combine tools and logic without starting another agent conversation |
| ChatMD and standalone tools | Package a computation behind a reusable schema and handler |
| Stateful moderators | Respond to session events and implement custom tool behavior with retained state |
| Background jobs | Acknowledge long-running work and deliver the result later |
| Subscriptions and timers | Poll for results, enforce deadlines and reject stale completion events |
| Notifications and external events | Deliver data, request a model turn or integrate an external producer |
| Persisted child agents | Delegate ongoing work with separate instructions and tracked session outputs |
| Authority and validation | Check generated code and understand inherited tool, shell and file constraints |
| Recovery | Design cancellation, retries and restart behavior around actual commit guarantees |

The map encourages the author to consider capabilities useful to its task, then
retrieve their contracts. It links directly to substantial sections; prerequisites
are included automatically. It does not require navigating a deep documentation
tree. Whole sections and complete examples remain intact when paged.
The [String reference](chatml-strings.md), topic `chatml.strings`, covers all
fourteen `String` exports with checked normalization, UTF-8 byte-offset and error
examples. Its per-surface compiler mappings are audited; adding or changing an
export requires reviewing both the contract and documentation pins.
The [collections reference](chatml-collections.md), topic `chatml.collections`,
covers all twenty-two `Array` exports and five `Option` exports, including shallow
aliasing, short-circuit searches, eager defaults and explicit task sequencing.
The [JSON reference](chatml-json.md), topic `chatml.json`, distinguishes syntax
validation, schema checks, missing/null values, duplicate keys, shared payloads and
floating-point export. [Mutable tables](chatml-tables.md), topic `chatml.tables`,
cover string-keyed lookup and mutation; [global helpers](chatml-global-helpers.md),
topic `chatml.globals`, cover rendering, reflection and exact `print` availability.
On moderator targets, [conversation data](chatml-moderator-data.md), topic
`chatml.moderator-data`, covers Item, Context and Tool_call operations and their
data aliases. Its examples run on both ordinary and delegated moderator surfaces.
It distinguishes projected snapshots and read-only inspection from admitted effects.
The [host-effects guide](chatml-host-effects.md), topic `runtime.effects`, explains
Log, Turn and Tool, including target restrictions, single tool decisions,
transactional edits versus external effects, and the versioned owned-job spawn alias.
Every task package includes the broad `chatml.programs` guide and its checked
OCaml-differences prerequisites, covering source syntax, control flow, matching,
types, structured-data utilities and effect boundaries before the task's runtime
contract. Read all pages when the expanded package needs continuation.

The `agent_create` tool description also explains why to create a persisted
specialist, reviewer or ongoing worker, describes the follow-up management tools,
and points to `prepare` with `task: "child_agent"` when the reference helper is
available. This gives the model a reason to discover the authoring guides before
it attempts an unfamiliar ChatMD definition or moderator.

These discovery pointers are derived from the actual selected capability metadata
and supported host targets, using the same task-to-surface resolver as retrieval.
Automatic/preload configurations expose both helpers; manual configurations mention
only the helpers explicitly exposed. With neither helper, the authoring tool keeps
its entrypoint description and stable package/topic identifiers. Selecting a narrower
tool set recomputes those pointers. Model tool descriptions and retrieved tool
inventories use the same presentation, without changing schemas, capability IDs or
the implementations behind them. Custom authored description text is preserved.

Custom conventions can now be captured through the host library API:
`Authoring_context.create ~authored_packages`. Each package includes explicit help
metadata and captured topic text with source labels, supported compiler surfaces
and prerequisites. Topic IDs use `custom.<package>.<topic>` and cannot overwrite
installed topics. Lookup performs no file reads or execution of embedded snippets.

The query service scopes these packages to the invoking tools' actual help
metadata. `prepare` includes selected custom roots and their dependencies;
`topic`, `search` and continuation use the same scope. Omitting a package cannot
silently restore it through another package's dependency. Changing captured text,
metadata or authority invalidates continuation. Responses label custom text as
`authored_conventions`, with package and source hashes; these conventions cannot
count as audited compiler/runtime documentation. The aggregate captured-text
budget defaults to 4 MB and can be configured independently from response budgets.

Qualified hosts can install the captured packages with
`Authoring_validation.configure_authored` and pass that immutable host through
the daemon's `authoring_validation_host` option or local runtime extension
services. Native reference helpers and automatic/preload materialization consume
the actual calling host snapshot, including when a helper is inherited by a child.
Custom preloads remain labelled authored conventions and deduplicate by their
actual payload/source identity. Declaring `authoring_help` alone does not install
a custom package's text. The daemon's normal configuration loader can capture
package files with `server.authoring_packages`, as described below. Local TUI and
stdio hosts accept the same files through `--authoring-package`. General extension
exposure remains pending qualification.

For a configured daemon, add the file list inside its `server` record:

```lisp
(authoring_packages ("./report-conventions.json"))
```

Paths resolve relative to the server configuration. Each file is a closed
version-1 JSON object, for example:

```json
{
  "version": 1,
  "packages": [{
    "help": {
      "version": 1,
      "package": "reports",
      "tasks": ["one_off_script"],
      "topics": ["custom.reports.rules"],
      "required_helpers": []
    },
    "topics": [{
      "id": "custom.reports.rules",
      "title": "Report conventions",
      "prerequisites": ["chatml.syntax.calls"],
      "surfaces": ["one_off_v1"],
      "source_name": "report-conventions.md",
      "text": "Keep source file names in every report.\n"
    }]
  }]
}
```

All shown fields are required; unknown and duplicate fields are rejected. Task
and helper names use their public string identifiers. `source_name` labels the
captured `text`; the loader never opens it. A file may hold multiple packages,
and dependencies may cross configured files. The complete set must satisfy the
same namespace, dependency, surface and ownership rules as the library API.
Files are limited to 1 MiB each and 128 files / 4 MiB in aggregate; the corpus
also checks its package, topic and captured-text budgets.

Configuration validation reads and captures the bytes, including in the server
CLI's `-validate-only` path. Running sessions use that immutable snapshot and do
not reread the files during queries. File changes require a daemon restart;
explicit reload detects changed package content at the same path and returns
`config.restart_required`. A failed reload leaves the existing snapshot intact.
Normalized configuration contains the captured text. Supply packages through
either server configuration or an explicitly configured host's corpus; combining
both is rejected to avoid silently replacing either source of conventions.
Loading packages does not register tools, grant permissions, change manual policy
or enable the currently gated extension rollout.

For local hosts, `chat-tui --local -file agent.chatmd --authoring-package conventions.json`
and `ochat-agent-stdio --local --prompt agent.chatmd --authoring-package conventions.json`
use the same bounded loader. Repeat the flag to supply multiple files. Relative
paths resolve against the process working directory. The default embedded TUI
mode also accepts it without `--local`; daemon connections, legacy file-backed
sessions and one-shot administration modes reject the flag instead of ignoring it.
Configure a connected daemon through its own server configuration.

Embeddings can pass absolute paths as `Embedded.start ~authoring_package_files`.
The entire set is captured and validated before creating the local store, then
installed through the same daemon composition. Transient and durable local hosts
retain their existing execution/lifetime differences. Neither rereads package
files during queries, and private packages still require matching selected tool
metadata. These flags supply configuration; they do not bypass the current
extension qualification gate.

Admission checks the authored owners of every requested topic's full dependency
closure. A preload cannot access a private package merely because its topic
supports the same compiler surface. Captured hosts have separate ordinary and
delegated catalogs, and child creation/restoration use the delegated catalog.
An ordinary-only moderator package cannot be presented as valid child guidance.
Replacing captured sources rebuilds both catalogs and changes host identity;
their catalog metadata cannot be replaced independently of the captured source.

Offline composition checks create a persisted child with one selected custom
package. Its native helper reads that package and denies the parent's other
package. After stopping, unloading and restarting the child runtime, its guidance
deduplicates, its helper reads the same captured text, and its inherited managed
handler still works. This qualifies runtime reload; the fixture does not simulate
a whole daemon process restart.

## One strict request schema

Every operation uses the same eight required fields. Unused fields must be `null`;
extra properties are rejected. `version` is always `1`.

```json tool=ochat_authoring_context
{
  "version": 1,
  "operation": "prepare",
  "task": "moderator_tool",
  "query": null,
  "topic_id": null,
  "features": ["background_tools", "timers", "notifications"],
  "cursor": null,
  "max_tokens": null
}
```

| Operation | Required non-null fields | Optional non-null fields |
|---|---|---|
| `prepare` | `task` | `features`, `max_tokens` |
| `topic` | `task`, `topic_id` | `max_tokens` |
| `search` | `task`, `query` | `max_tokens` |
| `continue` | `cursor` | `max_tokens` |

Task IDs are `one_off_script`, `standalone_tool`, `moderator_tool`, `child_agent`
and `background_workflow`. Feature IDs are `background_tools`, `subscriptions`,
`timers`, `notifications`, `external_events` and `child_sessions`. A task cannot
enable a target absent from the invoking host. Requesting a topic incompatible
with the selected target fails explicitly.

For example, read timer semantics and their prerequisites:

```json tool=ochat_authoring_context
{
  "version": 1,
  "operation": "topic",
  "task": "background_workflow",
  "query": null,
  "topic_id": "runtime.jobs.timers",
  "features": null,
  "cursor": null,
  "max_tokens": null
}
```

`search` matches words, symbols and topic IDs against surface-compatible installed
topics. Results contain stable IDs, titles, excerpts, hashes and prerequisites.
Fetch a result with `topic` to read the actual reference; an excerpt is not a
replacement for its contract. No semantic model or vector service is involved.
Search also matches compiler symbol names/signatures and selected tool names and
descriptions. These matches point to the corresponding flat reference topic;
results identify matching symbols and a bounded excerpt. Prose matches appear
first to retain their semantic guidance, followed by matching reference inventories.

Two direct reference topics complement the prose guides:

- `reference.signatures` returns the selected compiler surface's globals, module
  exports, aliases and required entrypoints. Its first item explains the readable
  signature notation. Fetch all continuation pages so alias definitions are included.
- `reference.tools` returns the invoking scope's exact selected tool descriptions,
  input schemas, strictness and output contract. It does not discover or grant tools
  outside that selection. `native_output` means native output remains opaque;
  `invocation_v1` identifies the runtime's structured invocation outcome contract.

Both are also included after the prose in `prepare`, using the same budget and
continuation mechanism. Schemas and signature declarations are never truncated.
Signatures are grouped into whole sections for entrypoints, aliases, globals and
each module, keeping navigation flat and avoiding a separate page per function.

## Interpret availability and completeness

The orientation reports the invoking scope's selected tool names/descriptions and
enabled authoring targets. Each guide identifies whether its topics are readable
on the current surface and whether its suggested authoring task is enabled.
These are distinct from runtime effect availability: compiler/reference support
and successful validation do not prove that a host service is installed, or grant
permission to use a tool. Execution still checks current bindings and authority.

Responses identify the runtime, surface, corpus and capability fingerprint.
`coverage` currently reports `reviewed_foundation_not_full_feature_coverage`, and
`prepare` reports `package_complete: false`. Selected native schemas and compiler
signatures are included, but broader language/ChatMD/runtime semantic coverage and
context lifecycle integration remain unfinished. `topic_sequence` identifies the assembled reference topics, including
ones on later pages; `items` contains only this page. `complete` describes pagination
of this query, not completion of the full authoring reference.

## Budgets and continuation

The default target is 12,000 estimated tokens, with a configurable 32,000-token host
ceiling. `max_tokens: null` selects the default. The current estimate is
`ceil(UTF-8 bytes of the full JSON response / 3)` and is labelled
`utf8_bytes_div_3_estimate`. It is not an exact tokenizer, a token upper bound or a
provider billing measurement. The host rejects requests above its ceiling.

Configure a daemon's immutable authoring budgets in its `server` record:

```scheme
(authoring_budget
 ((default_tokens 12000)
  (max_tokens 32000)
  (preload_tokens 32000)))
```

All fields are optional and use the values shown when omitted. Estimates must be
positive and at most 1,000,000; `default_tokens` must not exceed `max_tokens`.
`preload_tokens` bounds the combined automatic primer and prerequisite-complete
preload, independently of query pages. An oversized batch fails admission instead
of silently omitting required material. Manual policy still inserts nothing.
These are documentation budgets, separate from ChatML execution resource limits.

OCaml embeddings can construct a budget with
`Chat_response.Authoring_validation.context_budget` and pass it as
`Embedded.start ~authoring_budget`, or use `configure_context_budget` on their
trusted authoring host. Native queries and helper queries use the actual calling
host's budget; delegated sessions inherit it. A per-call materialization budget
can lower an explicit host budget, but cannot raise it. Supplying conflicting
budgets through both the daemon configuration and an explicit authoring host is
an error. Changing server budgets requires a restart and invalidates old context
identities and continuation cursors. Local TUI/stdio command-line budget flags
are not exposed yet; the embedding API supports the same configuration.

When `complete` is false, pass `next_cursor` to `continue`, setting `task`, `query`,
`topic_id` and `features` to null. If the next intact section cannot fit, the page
may be empty and `budget.minimum_next_tokens` reports the estimated space needed.
Increase the budget within the host ceiling; no source text is silently truncated.

Cursors are signed and bound to the original query, corpus, host target, selected
capabilities and invoking session/generation. Changing that context or the host's
signing key invalidates them; repeat the original query. A continuation can change
its response budget, but cannot switch to another session's authority or corpus.

See the [corpus reference](authoring-topic-corpus.md),
[query interface](../../lib/chat_response/authoring_context.mli) and
[native registration](../../lib/agent_session/authoring_context_tool.mli).
