# Beginner README content and navigation audit

This historical audit compared the detailed README with its beginner-friendly
replacement. That draft has now been promoted to [Readme.md](../../Readme.md);
there is no separate draft file. References below to the original and draft
describe the comparison at the time, not two current documents. The record
explains where material shortened or omitted from the new README can be found
without putting implementation detail back into a first-time reader's path.

## Method and boundary

Read both documents, compare their sections and examples, and inspect the
destination documentation rather than treating a working link as proof of
content coverage. Count the root draft as depth 0. Following one link from it
is depth 1; following two more is depth 3. Only explicit document links count,
not repository browsing, search, guessed filenames, or a trip through the
original README. Intermediate pages must remain under `docs-src`.

The check covers substantive topics, examples, caveats, and reference links,
not preservation of repeated marketing sentences or identical prose. The
draft intentionally replaces the old quick-start sequence with its own
self-contained local-first sequence. Roadmap statements remain labeled ideas,
and stale claims are corrected rather than copied as current behavior.

## Section coverage

The destinations below are reachable within three links from the root draft.
Depth is the shortest path to the named destination, not a folder depth.

| Detailed README material | Destination in `docs-src` | Depth |
|---|---|---|
| Introduction, text-first positioning, local-first hosting | [Project overview](../overview/project.md), [ChatMD introduction](../chatmd/README.md) | 1 |
| Why Ochat exists; design principles | [Purpose and principles](../overview/project.md#why-text-first) | 1 |
| Ochat in one minute; what is Ochat | [ChatMD introduction](../chatmd/README.md), [language reference](../overview/chatmd-language.md) | 1 |
| What makes Ochat different; how it compares; who it is for | [Positioning and audience](../overview/project.md#where-ochat-fits) | 1 |
| Quick start; first ten minutes; build from source | [Local TUI tutorial](../agent-server/tutorials/local-tui.md), [build troubleshooting](../guide/build-troubleshooting.md) | 1 |
| Prompt configuration, instructions, imported documents/images, transcripts | [ChatMD language](../overview/chatmd-language.md) | 1 |
| Built-in tools, root-scoped reads, aliases, schemas and path resolution | [Tool reference](../overview/tools.md) | 1 |
| All seven host path variables; client cwd versus daemon workspace | [Sessions and workspaces](../agent-server/sessions-and-workspaces.md) | 2 |
| Prompt packs, agent-as-tool input/output, nested sources and pinning | [Tool reference](../overview/tools.md), [sessions and workspaces](../agent-server/sessions-and-workspaces.md) | 1 / 2 |
| Grounding in a corpus; code and documentation search | [Search and indexing](../guide/search-and-indexing.md) | 1 |
| Iterative prompt refinement | [Meta-prompting](../lib/meta_prompting.doc.md) | 1 |
| Export, branch, resume; host-specific persistence boundaries | [Completion CLI](../cli/chat-completion.md), [TUI guide](../guide/chat_tui.md), [sessions](../agent-server/sessions-and-workspaces.md) | 1 / 1 / 2 |
| Common use cases | [Project overview](../overview/project.md#where-ochat-fits), [examples](../examples/README.md) | 1 |
| Minimal, refactoring, moderated prompt examples and Item helper fragment | [Longer prompt patterns](../examples/prompt-patterns.md) | 2 |
| Deprecated MCP prompt-serving example; maintained MCP tools distinction | [Compatibility reference](../bin/mcp_server.doc.md), [longer examples](../examples/prompt-patterns.md) | 1 / 2 |
| Item, Tool_call, Context, Turn and Model helper behavior | [Preserved helper walkthrough](../examples/prompt-patterns.md#writing-moderator-scripts-with-item), [runtime guide](../guide/chatml-moderator-runtime.md) | 2 / 1 |
| Developer-role behavior of compatibility system helpers | [Runtime guide](../guide/chatml-moderator-runtime.md#item) | 1 |
| Canonical history identity, provider correlation IDs and projections | [Project architecture](../overview/project.md#architecture-in-brief), [TUI history](../guide/chat_tui.md), [architecture spec](../design/ochat-agent-server-spec.md) | 1 / 1 / 2 |
| TUI ownership, progress animation, startup, resizing, caches and diagnostics | [TUI guide](../guide/chat_tui.md) | 1 |
| Legacy versus shared-host safe points, steering and controller ownership | [Controller contract](../chatml-host-session-controller-contract.md), [safe-point semantics](../chatml-safe-point-and-effective-history.md) | 2 |
| Spawn completion, idle wakeups, budgets and UI-only approvals | [Async lifecycle](../chatml-async-completion-lifecycle.md), [budget policy](../chatml-budget-policy.md), [UI capabilities](../chatml-ui-host-capabilities.md) | 2 |
| Background timer example and durable orchestration | [Background tutorial](../agent-server/tutorials/background-agent.md), [host orchestration](../agent-server/chatml-orchestration.md) | 1 / 2 |
| Choose how to run; durable daemon walkthrough | [Host modes](../agent-server/concepts.md), [Unix daemon tutorial](../agent-server/tutorials/unix-daemon.md) | 2 / 1 |
| Sessions, workspaces, ownership, permissions and automation | [Sessions](../agent-server/sessions-and-workspaces.md), [permissions](../agent-server/permissions-and-security.md) | 2 |
| Shell manifest, modes, authorization, confinement, extensions and audit | [Shell topic index](../shell/README.md) and its directly linked references | 1–2 |
| Unrestricted YOLO profile and warning | [Security guide](../guide/chatmd-shell-security.md#yolo-profile) | 2 |
| Architecture overview, actor/worker/store flow and backup boundary | [Project architecture](../overview/project.md#architecture-in-brief), [architecture spec](../design/ochat-agent-server-spec.md), [operations](../agent-server/operations.md) | 1 / 2 / 2 |
| Binaries and compatibility mode warnings | [Command index](../bin/README.md) | 1 |
| Project layout, ignored prompt directory and private host data | [Contributor orientation](../overview/project.md#ocaml-and-contributor-orientation) | 1 |
| Why OCaml, compiler/test feedback, source indexing and embedding | [Contributor orientation](../overview/project.md#ocaml-and-contributor-orientation), [library embedding](../lib/embedding.md) | 1 / 2 |
| Experimental ChatML language, dsl_script and public runtime wrappers | [ChatML index](../chatml/README.md), [runtime layers](../guide/chatml-moderator-runtime.md#runtime-layers), [demo reference](../bin/dsl_script.doc.md) | 1 / 1 / 2 |
| ChatML language, match, interpreter, parser and resolver references | [ChatML index](../chatml/README.md) and its directly linked references | 1–2 |
| Core/Eio conventions; normal versus opt-in docs/E2E/soak tests | [Contributor orientation](../overview/project.md#ocaml-and-contributor-orientation), [testing](../agent-server/testing.md) | 1 |
| Future directions, Irmin caveat, provider limitations, project status | [Status and direction](../overview/project.md#status-and-future-directions) | 1 |
| Documentation index, specifications and library sidecars | [Documentation home](../README.md) and its directly linked references | 1–2 |

The Asciinema preview and license link remain in the draft. External installation
resources, source/build metadata, `DEVELOPMENT.md`, and the license remain at
their existing authoritative locations; the docs explain and link to them rather
than duplicate those files.

The original README's real-world session link is restored in the
[examples index](../examples/README.md): root → examples → historical session
index → full or compacted transcript (three links). Those transcripts and the
additional prompt collection remain outside `docs-src` as example artifacts;
the explanatory documentation and entry point are inside it.

## Corrections and improvements

- Added a project overview to preserve positioning, principles, architecture,
  contributor context, and roadmap without expanding the beginner narrative.
- Added a command index instead of leaving readers to find individual binary
  reference files by browsing the repository.
- Preserved longer prompt examples and exact helper walkthroughs in a dedicated
  examples page, with directory setup and execution caveats.
- Added explicit ChatML links for controller, budget, UI, async, and language
  internals; these are now two links from the root, not dependent on incidental
  cross-references.
- Restored the real-world session discovery path and clarified transcript-as-
  artifact behavior in the draft.
- Corrected the built-in catalog's obsolete claim that `fork` is only a
  placeholder. Current host drivers handle nested fork execution; it is not
  an independently administered root daemon session.

## Verification

Historical results before promotion:

- All 55 distinct `docs-src` Markdown destinations linked by the detailed
  README are reachable from the draft: 21 at depth 1 and 34 at depth 2.
- All 35 distinct destination pages linked by this coverage record are reachable
  within the three-link limit without traversing the original README.
- All 317 local links and anchors across the draft and 11 reviewed navigation,
  example, overview, and audit pages passed validation.
- `dune build @agent-docs-check` passed: 288 documentation pages and 37 protocol
  methods; no live provider calls. `git diff --check` also passed.
- The original `Readme.md` content hash is unchanged.

For subsequent reviews, verify the full Markdown link graph from the current
README, stopping after three edges, and require each destination in this table
to be reachable. Validate local anchors in the README, topic indexes, and this
audit. Run the opt-in
`dune build @agent-docs-check` after refreshing the documentation inventory.

Link reachability is mechanically checkable. Topic coverage is an editorial
review, not a proof that every sentence is equivalent or that every historical
example was rerun. This audit does not claim live-provider, external website,
or runtime regression testing. See [testing](../agent-server/testing.md) for
those separate checks.
