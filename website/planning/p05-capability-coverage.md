# P05 capability coverage

All fourteen Section 5.6 requirements have searchable website entry points.
The same checked configuration supplies the documentation home’s framework map.
Deferred/repository detail remains linked in the canonical guides and carries
its own disposition; these entries do not certify live execution.

| Capability | Reader destinations | Qualification |
| --- | --- | --- |
| Imports and reusable prompts | `/docs/reference/chatmd/`, `/docs/library/chatmd/source-loader/` | Imported paths and captured source closures follow the declaring source and selected host. |
| Built-in tools | `/docs/reference/tools/` | ChatMD read_dir and model-visible read_directory are distinct names; declarations do not grant all host authority. |
| MCP tools and authentication | `/docs/library/mcp/client/`, `/docs/library/mcp/http/`, `/docs/library/mcp/oauth/` | Maintained outbound MCP tools and OAuth are separate from deprecated prompt serving and daemon authentication. |
| Custom OCaml tools | `/docs/library/custom-tools/` | Progress and traces are transient and need their own disclosure policy; the tracked example has companion Dune dependencies. |
| Retrieval and indexing | `/docs/guides/search-and-indexing/`, `/docs/reference/commands/md-index/`, `/docs/reference/commands/odoc-search/` | Agent retrieval uses configured embeddings; test vectors are not semantic retrieval and website search uses a separate index. |
| Prompt refinement and evaluation | `/docs/reference/commands/mp-refine-run/` | Local template and paid online strategies differ; append behavior and model costs matter. The broad older library overview needs reconciliation. |
| Compaction and retained history | `/docs/guides/context-compaction/`, `/docs/reference/compaction/` | Summaries are lossy; visible history, canonical history, exports, and archived states are distinct representations. |
| ChatML capabilities and budgets | `/docs/guides/moderator-runtime/` | Capabilities depend on the host. Turn, event-drain, and job limits are not dollar caps; detailed phase contracts retain their historical scope. |
| Jobs, timers, and restart | `/docs/guides/agent-orchestration/` | Recovery does not resume arbitrary continuations or guarantee exactly-once external effects. |
| Terminal controls and drafts | `/docs/guides/tui/` | Attachments, exports, keybindings, and optional draft suggestions follow host-specific behavior; suggestions may send unsent text and incur costs. |
| Shell grants and audit | `/docs/guides/shell-hosts/`, `/docs/reference/commands/shell-management/` | Legacy Session_store commands do not administer daemon IDs; a declaration alone does not authorize execution. |
| Clients and synchronization | `/docs/reference/agent-server/protocol/`, `/docs/guides/permissions/` | Protocol 1.0 synchronization does not promise replay of live deltas; reconnect restores authoritative projections. |
| OCaml integration and older APIs | `/docs/integrations/ocaml/`, `/docs/library/embedding-components/`, `/docs/compatibility/prompt-sessions/` | Current actor-owned hosts and older file-backed session APIs have different storage and lifetime owners. |
| Commands and utilities | `/docs/reference/commands/`, `/docs/reference/commands/ochat/`, `/docs/reference/commands/terminal-utilities/` | Installed names, utility subcommands, and source-only demos are distinguished explicitly. |

## Explicit detail remaining outside publication

- `docs-src/lib/meta_prompting.doc.md` (deferred): P05 explicit deferral: The broad overview mixes older Meta_prompt.Make/Chatmd.Prompt examples with current strategies and an obsolete mp-refine-run invocation missing explicit strategy context. Publish the reviewed mp-refine-run command; reconcile API examples against interfaces before promoting the overview.
- `docs-src/chatml-budget-policy.md` (repository-only): P05 repository scope — ChatML Budget Policy: Detailed phase-era shared-host contract retained for contributors; the moderator runtime and agent orchestration pages provide the scoped reader entry. This contract is not a universal native/daemon capability promise.
- `docs-src/chatml-ui-host-capabilities.md` (repository-only): P05 repository scope — ChatML UI host capabilities: Detailed phase-era shared-host contract retained for contributors; the moderator runtime and agent orchestration pages provide the scoped reader entry. This contract is not a universal native/daemon capability promise.
- `docs-src/chatml-host-session-controller-contract.md` (deferred): P05 explicit deferral: Historical phase contract contradicts itself about deferred user notes becoming canonical versus leaving canonical history unchanged. Current moderator/host guides publish; reconcile the detailed old contract against implementation before promotion.
- `docs-src/chatml-safe-point-and-effective-history.md` (deferred): P05 explicit deferral: Phase-era host contract requires a coordinated deferred-steering review and correction of its finalized-output UTF-8 claim; use current moderator runtime and shell security guides.
- `docs-src/chatml-async-completion-lifecycle.md` (repository-only): P05 repository scope — ChatML Async Completion Lifecycle: Detailed phase-era shared-host contract retained for contributors; the moderator runtime and agent orchestration pages provide the scoped reader entry. This contract is not a universal native/daemon capability promise.
