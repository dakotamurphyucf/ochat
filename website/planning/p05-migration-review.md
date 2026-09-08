# P05 documentation migration review

Date: 2026-09-06. Source revision: `bc76b6c72a280b4bad48a793a63373e6db26d2b4`.
Status: first publication group implemented; P05 remains in progress.

This report records the first group. The [subsequent hosting group](p05-hosting-review.md)
brings the current total to 52 rendered docs and 245 deferred sources.

## Publication scope

The manifest accounts for all **297 tracked Markdown sources** in `docs-src/`.
This increment adds 15 reviewed sources to the 22-page foundation: **36 publish,
1 bridge, 0 compatibility, 0 repository-only, 260 deferred**. There are 37
rendered documentation routes, plus the homepage and error page.

Review here means reading the canonical prose, preserving its qualifications,
choosing reader-facing metadata and navigation, and validating the web
transformation. It is not a fresh audit of every runtime claim. All new pages
retain `verification: not-checked`; illustrative search outputs remain labeled
as such. No live provider calls or new clean-machine installation were run.

The remaining 260 sources are not editorially approved. Their existing
per-source deferrals remain explicit; a generic initial-spike deferral does
not mean someone has reviewed the full document. P05.01–P05.08/P05.10–P05.11
remain open for the complete migration, even where infrastructure now exists.
P05.09's continuously regenerated inventory/report is implemented.

## Published routes

Stable manifest IDs are unchanged, including IDs derived from source paths.
Canonical prose remains in `docs-src/`; no source files or semantic fixtures
were moved or changed in this increment.

| Source relative to docs-src | Website route | Review focus |
| --- | --- | --- |
| `agent-server/README.md` | `/docs/guides/agent-server/` | Optional host; transport and ownership distinctions |
| `agent-server/environment.md` | `/docs/reference/agent-server/environment/` | Host/client environment; embedding stub and policy scope |
| `agent-server/sessions-and-workspaces.md` | `/docs/concepts/sessions-and-workspaces/` | Attachments, workspace paths, source pinning, recovery |
| `agent-server/operations.md` | `/docs/operations/` | Supervision, reload, stopped backup, qualified durability |
| `bin/README.md` | `/docs/reference/commands/` | Installed names; source-only and deprecated commands |
| `bin/chat_tui.doc.md` | `/docs/reference/commands/chat-tui/` | Native/connected/legacy flags; client-owned typeahead |
| `guide/build-troubleshooting.md` | `/docs/start/build-troubleshooting/` | Canonical platform workaround and upstream-change caveat |
| `shell/README.md` | `/docs/concepts/shell-access/` | Requested authority versus granted execution |
| `guide/chatmd-shell-host-integration.md` | `/docs/guides/shell-hosts/` | Host-specific grants and missing daemon grant-creation RPC |
| `guide/search-and-indexing.md` | `/docs/guides/search-and-indexing/` | Setup, stubs, dense/hybrid ranking, package limitations |
| `guide/search-examples/README.md` | `/docs/guides/search-and-indexing/examples/` | Illustrative output hub |
| `guide/search-examples/md-search.md` | `/docs/guides/search-and-indexing/examples/markdown/` | Explicitly illustrative result format |
| `guide/search-examples/ochat-query.md` | `/docs/guides/search-and-indexing/examples/code/` | Nested fence preservation; illustrative code output |
| `guide/search-examples/odoc-search.md` | `/docs/guides/search-and-indexing/examples/odoc/` | Local odoc retrieval, independent of website API hosting |
| `overview/project.md` | `/docs/concepts/project/` | Architecture, host boundaries, roadmap versus implementation |

The existing `/docs/start/troubleshooting/` URL remains owned by the host
troubleshooting page. Its label is now **Host troubleshooting**, under Agent
hosting. Build troubleshooting receives the distinct route above. This
explicitly resolves the initial proposed mapping without breaking an existing
preview URL. No redirects or alias declarations were introduced.

## Navigation and reading

The docs home adds four static learning paths: first local agent, tools, project
retrieval, and persistent hosting. Technical instructions continue to come from
the canonical README below them. The cards are excluded from Pagefind content
and work without JavaScript.

The sidebar adds Shell access, Search and indexing, Agent hosting, and Commands.
Groups have deliberate ordering; unknown sections fail generation rather than
silently hiding a page. Current-group expansion remains Starlight-owned.
Three search output pages are reachable from their hub and the guide, searchable
and in the sitemap, but omitted from the sidebar to keep it concise. Each new
page has an explicit next step; there is no automatic global pagination.

Unpublished linked documents still resolve to real tracked GitHub source URLs
at the source revision. Some advanced hub links therefore leave the website;
this is an intentional partial-migration state, not completed local coverage.
The graph check validates local output and source URL construction, not remote
HTTP availability or unpublished GitHub heading fragments.

## Migration evidence and source corrections

Every content generation transaction writes `.generated/migration-report.json`
and `.generated/migration-report.md`, alongside the existing content report.
They enumerate every source, counts, dispositions, route ownership, deferral
reasons, compatibility policy, effective verification, rendered-source hashes,
local modified published sources, and highlighting fallbacks. These files are
not deployable assets. CI is configured to upload them; remote CI has not run.

The report describes the current snapshot, not a cumulative Git change log.
The existing canonical corrections from P01–P04 remain:

- `agent-server/quickstart.md`: complete installation/provider onboarding.
- `agent-server/tutorials/local-tui.md`: tracked first-agent prompt, launch,
  interactions, persistence, and source-path qualifications.
- `lib/webpage_markdown/md_render.doc.md`: valid outer nested-code fence.
- `lib/chatmd/chatmd_parser.doc.md`: XML-like error prose marked as inline code;
  this source remains deferred, so it is not in the report's modified-published
  subset.

New editorial issue recorded without publication: `cli/chat-completion.md`
references `prompts/hello.chatmd` and expects an `echo` call, while the local
prompt at that path declares `odoc_search`. That path is also outside the
tracked dependency-complete first-agent fixture. Replace the batch smoke test
with a tracked example, verify transcript-directory setup and host semantics,
and run the semantic gate before publishing it. Do not copy the current sample
into a polished headless tutorial unchanged.

No new Markdown corrections were needed for these 15 imports. The existing
forced offline Dune gate from the P01–P04 audit remains applicable to the
unchanged canonical source bytes. Website build success does not replace that
independent gate when technical prose or examples change next.

## Capability coverage checkpoint (specification 5.6)

This is a coverage inventory, not a claim that P05.10 is closed.

| Capability | Published entry | Remaining work |
| --- | --- | --- |
| Imports/source paths | ChatMD reference; sessions and workspaces | Review deeper source-loader/library prose |
| Built-in tools | Tools introduction and catalog | Preserve declaration/model-name search behavior in P07 |
| Maintained MCP tools/auth | Tools introduction/catalog | MCP library/authentication guides deferred |
| Custom OCaml tools/progress/traces | OCaml integration; example index | Registration sidecar and dependency-complete downloads deferred |
| Retrieval/indexing | Search hub and three illustrative outputs | Indexer/search command references deferred; website-search evaluation is P07 |
| Prompt refinement/evaluation | Command overview links to repository sources | CLI and library prose deferred |
| Compaction/history | TUI, sessions/workspaces, operations | Compactor/library sidecars deferred |
| ChatML host capabilities/budgets | ChatML concept/reference | Runtime, budget, and UI-host contracts deferred |
| Background jobs/timers | Background tutorial | Orchestration reference deferred |
| TUI attachments/exports/keys/typeahead | TUI guide and command reference | P07 query evaluation and tutorial polish remain |
| Shell inspection/grants/audit | Shell hub, host matrix, permissions | Management CLI, policy, persistence and interruption guides deferred |
| Protocol/events/reconnect | Stdio tutorial; sessions and permissions | Protocol/types, transports, Unix/HTTP tutorials deferred |
| Current/older embedding APIs | Current OCaml integration | Legacy embedding/library ownership review deferred |
| Executables/utilities | Command overview and chat-tui reference | Remaining command references selectively deferred |

Supplemental ownership remains documented in `planning/decisions.md`: README
example extraction, allowlisted screenshot, explicit public assets, repository
links to interfaces/build files, and repository-only historical transcripts and
prompt collections. A machine-readable supplemental manifest and dependency
closure review are still required by P05.10/section 10.17. Nothing automatically
publishes those directories or scratch memory.

## Verification

- `npm run check`: 38 tests pass; Astro reports no errors, warnings, or hints.
- `npm run build`: passes; 39 HTML pages, 225 files, 7,830,088 bytes. Every built
  local link/fragment and indexing policy passes. Existing deferred Mermaid
  chunk-size warning remains a performance/release task.
- Browser evidence: `scratch/ochat-website-evidence/p05-browser.log` and
  `p05-browser-recheck.log`. The first run exposed a test trying to select a
  desktop-only theme control at mobile width; the corrected test changes the
  system color scheme and asserts the applied theme before checking layout/axe.
- Final browser outcome: 70 passed and 2 intentional non-Chromium clipboard
  skips across the full run plus targeted recheck; no unresolved failures. The
  first run had 67 passes and 3 timeouts; only those three changed tests reran.
- Visual evidence: `scratch/ochat-website-evidence/p05/` contains desktop light,
  desktop dark, and mobile docs-home screenshots. Inspected all three.
- Existing canonical code-fence parity now covers all 37 rendered sources.
- No publication, domain purchase, repository commit, or hosted release checks.

## Next implementation group

Review the Unix/HTTP tutorials and configuration/protocol references together
so the hosting hub becomes locally complete. Then correct the batch example,
review shell management/security/persistence and selected library families,
complete older TUI bridge mappings, and close the supplemental/capability
inventory. Preserve generated-from-code ownership and rerun semantic checks
whenever their canonical dependencies change.
