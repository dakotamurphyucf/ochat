# Ochat: purpose, architecture, and direction

Ochat is a text-first toolkit for custom agents, LLM workflows, and retrieval.
It is aimed at developers and power users who want to own their instructions,
tools, and control logic rather than adopt a single fixed assistant workflow.
The usual starting point is a local ChatMD file opened in the TUI in your project.
No daemon is required.

## Why text-first?

Prompt definitions, tool declarations, scripts, and exported transcripts are
engineering artifacts: inspect them, diff them, review them, version them, and
reuse them. The design favors explicit tools, auditable orchestration, and
shell authority that is inspected and authorized rather than implicit.

Reproducibility here means preserving definitions, configuration, and evidence.
It does not mean deterministic model output or exactly-once external effects.
ChatMD can contain conversation history and imported documents or images as
well as instructions. Exported conversations can be inspected and branched;
a transcript export is not a backup of all durable host state.

## Where Ochat fits

| Compared with… | Ochat's focus |
|---|---|
| A fixed coding-agent application | Build your own planning, coding, review, and documentation roles as prompt files. |
| A code-first orchestration framework | Keep agent definitions and optional ChatML logic in reviewable text, with OCaml libraries available for custom hosts. |
| A hosted prompt-management or observability platform | Author and execute inspectable workflows; transcripts and audit help explain runs, but a hosted dashboard is not the core product. |
| An MCP tool wrapper | MCP tools are one integration alongside prompt composition, scripting, retrieval, and multiple execution hosts. |

Ochat is useful for repository assistants, documentation and research agents,
internal prompt packs, planning/review/test pipelines, CI requests, prompt
refinement, and background services. If you want only a ready-made chat window
with minimal setup and no interest in customizing workflows, this toolkit may
be more than you need. See [examples](../examples/README.md).

## Architecture in brief

ChatMD declares the agent. ChatML supplies optional typed workflow logic.
Shell manifests compile requested authority and bind authorization to a
canonical digest, so security-relevant changes require reconsideration.
The shared agent host separates serialized session state from model/tool
workers; clients own presentation, drafts, and selections, not daemon execution.

A typical shared-host execution resolves and pins supported prompt sources,
checks authority, admits work through a session actor, runs model/tools in
workers, applies moderation at safe boundaries, and publishes committed state
and live progress to clients. Stores retain journals, snapshots, pinned sources,
and artifacts—not an executing process or arbitrary continuation.

Canonical conversation occurrences have application-owned `History_entry.Id`
values. Provider item IDs and tool call IDs are correlation metadata, not their
replacement. Moderator projections retain provenance, TUI rows retain stable
identity, and provider payloads are explicit projections of canonical entries.
See [history and UI behavior](../guide/chat_tui.md),
[safe-point semantics](../chatml-safe-point-and-effective-history.md), and the
[architecture specification](../design/ochat-agent-server-spec.md).

Native local and daemon hosts share the agent core. Legacy file-backed
completion/TUI hosts and deprecated MCP prompt serving have separate ownership
and storage contracts. Consult [execution modes](../agent-server/concepts.md),
[embedding](../agent-server/embedding.md), and
[backup and recovery](../agent-server/operations.md) rather than treating all
hosts as interchangeable.

## OCaml and contributor orientation

OCaml suits Ochat's parsing, structured data, symbolic transformations, typed
workflow state, and compiler/test-driven repair loops. Core is the standard
library; Eio owns application I/O and concurrent resources. Agents themselves
are language-agnostic and tools exchange JSON.

For an OCaml project, compiler diagnostics and `dune runtest` failures can be
fed back into an agent after a proposed patch. Configure command authority
explicitly before automating that feedback loop. Ochat also offers OCaml source
indexing without an LSP dependency, odoc search, and reusable retrieval libraries.
See [search and indexing](../guide/search-and-indexing.md),
[library integration](../lib/embedding.md), and
[development setup](../../DEVELOPMENT.md).

ChatML is an experimental expression-oriented ML dialect with Hindley–Milner
type inference and row-polymorphic records and variants. Its integrated
moderation/orchestration runtime is separate from the small `dsl_script`
demonstration binary. See [ChatML](../chatml/README.md).

| Location | Contents |
|---|---|
| `bin/` | [Executables and command references](../bin/README.md). |
| `lib/` | Agent libraries, ChatMD/ChatML, shell execution, tools, and retrieval. |
| `docs-src/` | Markdown guides and API sidecars; not automatically included in odoc output. |
| `docs-src/design/` | Agent-server architecture and implementation specifications. |
| `docs-src/examples/` | Tracked prompts, shell declarations, and example clients. |
| `test/` | Normal tests and separate opt-in documentation/E2E runners. |
| `prompt-examples/` | Additional authored prompts to inspect and adapt. |
| `dune-project` | Package, dependency, and build metadata. |

A user-created `prompts/` directory is ignored by this checkout's default
configuration. Choose version control deliberately for shared definitions;
keep credentials, private transcripts, and daemon data out of public commits.
Daemon data belongs in its configured private root. Read the
[testing guide](../agent-server/testing.md) for opt-in suites and evidence;
normal `dune runtest` does not include E2E, soak, or live-provider runs.

## Status and future directions

Ochat is evolving: APIs, tool schemas, and higher-level choices may change.
Budget for occasional migrations. Bug reports, examples, documentation, and
code contributions are welcome.

Ideas retained from the project roadmap include a declarative ChatMD rules
layer for control flow and policy, richer branching/archives/evaluation,
additional provider backends, broader ChatML roles, and custom OCaml tools
through Dune plugins. These are directions, not delivery commitments or claims
of implemented interfaces. Irmin-backed storage was an earlier experiment;
the current daemon uses journals, snapshots, and artifacts.

Durable multi-client hosting, detached execution, owner leases, jobs, and
schedules are implemented, not future work. Restart-free growing-history
memory testing remains postponed; see [testing](../agent-server/testing.md).

The runtime currently uses OpenAI Responses behavior. A compatible proxy needs
validation of the request, stream, and tool features your workflow uses; changing
`API_URL` does not establish interoperability. See
[provider environment settings](../agent-server/environment.md) and
[transport/security limitations](../agent-server/permissions-and-security.md).

Return to the [documentation home](../README.md).
