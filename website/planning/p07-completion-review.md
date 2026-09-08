# P07 review and Milestone B content beta

Status: **P07 and Milestone B complete, 2026-09-07.** Base source revision: `bc76b6c72a280b4bad48a793a63373e6db26d2b4`; local implementation remains uncommitted.

The content beta combines the approved documentation corpus, connected tutorials, inline examples, source provenance, compatibility routes, and measured search. Review it at [the local preview](http://127.0.0.1:4321/), [documentation](http://127.0.0.1:4321/docs/), or [the example catalog](http://127.0.0.1:4321/docs/examples/). This is an optimized `astro build` artifact with preview/noindex deployment settings. Custom-domain hosting and HTTPS rehearsal remain later release phases.

## What P07 delivers

| Task | Implementation and evidence |
|---|---|
| P07.01 | `scripts/search-index.mjs` owns one `astro:build:done` pass. The built-in Starlight indexing pass is disabled; an existing `pagefind/` output makes the owned pass fail instead of silently indexing twice. Output records its owner, options, and processed HTML count. |
| P07.02 | Actual index enumeration equals manifest `search` ownership: 106 pages, comprising 102 published pages and four explicitly approved compatibility pages. All nine bridges, deferred/repository-only content, homepage, 404, source readers, provenance controls, and repeated navigation are excluded. |
| P07.03 | One `Search.astro` component is used on homepage and docs. It provides a labeled native dialog, immediate input focus, Ctrl/Cmd+K outside text inputs, Escape/Close, backdrop closing, explicit Tab/Shift+Tab cycling, and trigger/original-focus restoration. Opening it closes the mobile docs menu. |
| P07.04 | Initial, loading, results, pagination, empty, and error/retry states retain navigation alternatives. A no-JavaScript link opens the docs overview. Generation IDs reject stale responses; clear removes old results. Local browser history preserves the query when returning from a result. |
| P07.05 | `config/search-queries.json` owns all 18 Section 11.4 queries and concrete approved expected routes. `npm run evaluate:search` checks top-five ranks, context/excerpts, indexed ownership, and actual heading destinations, then emits reports tied to the built artifact SHA-256. |
| P07.06 | Measured changes: retain `${}` during indexing, reduce compatibility content weight, weight authored descriptions, and clarify two canonical headings/introductions. No query-specific redirects, synonym service, result-order override, or speculative filters were added. |
| P07.07 | Search starts no worker/index requests until a nonempty query. Results use plain-text rendering with safe entity decoding and local documentation URL checks. The evaluator checks 1,752 indexed/result fragment destinations. Browser tests cover failed worker/index/fragment requests and successful retries. |
| P07.08 | This review, the approved corpus and example reports, the local optimized preview, and final browser/search evidence form Milestone B. No later phase or external publication is implied. |

The full corpus remains 300 canonical documents: 102 published, four compatibility, nine bridges, 175 repository-only, and ten deferred. There are 115 documentation routes and 117 total HTML pages. Ten tutorials link nine downloadable source bundles; the catalog has four complete examples, five configurable templates, and one illustrative reading sample. Inline readers expose the 23 selected file destinations (15 unique files), and ChatML continues to use OCaml highlighting.

## Measured decisions

The baseline Pagefind index returned no results for `${workspace}`. “Save session” put three compatibility pages first, while “MCP” ranked the deprecated prompt server ahead of maintained client/tool integration. The baseline default widget had no homepage integration; a blocked Pagefind module left a loading message, an unhandled error, and no navigation alternative. These observations are preserved in `scratch/ochat-website-evidence/p07-baseline-search.json` and `p07-baseline-failure.json`.

The supported Pagefind `includeCharacters` option retains `${}` when indexing. Starlight's built-in pass does not expose that option, so the website now owns the single final-build Pagefind pass and pins Pagefind 1.5.2 directly. The shared frontend uses Pagefind's normal search/data APIs and ranking. Compatibility documents remain discoverable for explicit identifiers such as `Prompt_session`; their content weight is 0.1, while authored current descriptions use weight 4. Section and maturity/scope metadata are displayed with every result. These use Pagefind's supported [content weights](https://pagefind.app/docs/weighting/) and [metadata](https://pagefind.app/docs/metadata/).

The canonical sessions/workspaces page now introduces saving/resuming with the correct native-local, daemon, and local-stdio distinctions. The ChatMD reference names its `${workspace}` path-variable section and corrects an overly broad launch-directory statement to distinguish selected workspaces from materialized prompt/source directories. The offline documentation gate passed, and no tutorial/example verification hashes were refreshed or invalidated.

A further fault-injection test showed that Pagefind 1.5.2 catches failed index-chunk fetches internally and can report them as zero matches. A small dedicated worker wraps the supported API and observes fetch failures in its own scope. It propagates failed network/HTTP requests, including swallowed failures, to the dialog and is terminated on failure before a fresh retry. It does not patch page-global fetch, edit generated vendor JavaScript, or replace the search algorithm. The worker also keeps search work off the main thread.

Unquoted Pagefind searches retain its default prefix/fuzzy behavior; unrelated-looking strings can still find partial matches. The interface explains quoted exact phrases, and empty-state tests use a quoted absent phrase. No broader exact-match or synonym guarantee is claimed.

## Query and excerpt review

The final automatic evaluation meets the proposed 90% target with **18/18 expected families in the top five (100%)**. The gate also requires current expected destinations ahead of compatibility results for installation, first-agent setup, MCP, and saving a session. An expected family means one of the explicitly listed approved routes; it does not mean every possible relevant page must appear.

The table below records the expected destination found and a manual assessment of its excerpt/context. Detailed top-five titles, excerpts, status labels, subheadings, and local warm timing samples are in `.generated/search-report.json`; timings are diagnostic, not release latency guarantees.

| Query | Rank | Expected destination found | Excerpt/context review |
|---|---:|---|---|
| `install` | 1 | /docs/start/installation/ | Useful: names the toolchain, build, and provider setup. |
| `first agent` | 1 | /docs/start/first-agent/ | Useful: identifies opening an agent in the terminal and the first request. |
| `read_file` | 1 | /docs/reference/tools/ | Useful: shows the short read declaration and root context; literal tags remain text. |
| `workspace` | 1 | /docs/concepts/sessions-and-workspaces/ | Partial: identifies the right concepts page; its Workspaces and paths subheading provides the more specific entry. |
| `${workspace}` | 4 | /docs/reference/chatmd/ | Useful: names the exact variable and the complete path-variable list. |
| `ChatMD` | 3 | /docs/reference/chatmd/ | Useful: identifies the language reference and native/daemon host qualification. |
| `ChatML` | 4 | /docs/reference/chatml/ | Useful: identifies language values, tasks, and moderator scope; Experimental is visible. |
| `specialist` | 1 | /docs/tutorials/specialist/ | Useful: names the parent/specialist pair and file-reading example. |
| `--local` | 2 | /docs/start/first-agent/ | Useful: identifies the native first-agent terminal path; the stdio result is separately titled. |
| `save session` | 2 | /docs/concepts/sessions-and-workspaces/ | Useful: explains that persistence depends on the host and native local is transient. |
| `daemon reconnect` | 1 | /docs/tutorials/unix-daemon/ | Useful: describes creating, disconnecting, and reattaching to a durable session. |
| `shell_access` | 3 | /docs/reference/shell-runtime/ | Useful: displays the declaration and cwd context with readable literal tags. |
| `MCP` | 2 | /docs/library/mcp/client/ | Useful: explicitly identifies maintained clients/tools and shutdown/cancellation ownership. |
| `stdio` | 2 | /docs/tutorials/stdio-client/ | Useful: identifies the client tutorial and required data-root setup. |
| `HTTP authentication` | 2 | /docs/reference/agent-server/transports/http/ | Useful: states authenticated RPC scope and the plain-HTTP/loopback boundary. |
| `compaction` | 1 | /docs/reference/compaction/ | Useful: names resulting-history limits and the process that owns compaction. |
| `Prompt_session` | 1 | /docs/compatibility/prompt-sessions/ | Useful: explicitly says legacy file-backed compatibility and different daemon/native owners. |
| `apply_patch` | 1 | /docs/library/ | Partial: the library overview is a navigation index; the tool catalog at rank two has the behavioral detail. |

No historical result outranks installation or the first-agent tutorial. MCP places maintained transport/client/tool material ahead of the labeled legacy prompt server. The intentional `Prompt_session` query still finds its compatibility API first, with its legacy scope visible in the title, excerpt, and status.

## Browser, loading, and content evidence

Final validation passed against one optimized build:

- `npm run check`: 56 unit tests; zero Astro errors, warnings, or hints.
- `npm run build`: 117 HTML pages, 411 files, 15,696,746 bytes; local links, fragments, publication/indexing policy, and capacity pass.
- `npm run evaluate:search`: 18/18 expected families in the top five; exact 106-page index; 1,752 valid fragment destinations; no gate failures.
- `npm run test:browser -- --workers=1`: one complete 165-case run, **163 passed and two existing non-Chromium clipboard skips**, across Chromium, Firefox, and WebKit in 5.7 minutes. All 33 new search cases pass; no new skips.
- `dune build --force @agent-docs-check`: 300 canonical pages and 38 protocol methods pass offline; no provider calls.
- Changed implementation files pass Prettier; Git whitespace checks pass. Both specifications retain identical task states.

Artifact SHA-256: `08a12bbb70f28d8d6165e969f162a34678c787260062b3980ab952db0c1c7a75`. Matching build, content, migration, example, index, and query reports are saved in `scratch/ochat-website-evidence/p07/`; final command logs use the `p07-` prefix in the parent evidence directory. The final browser log is `p07-browser.log`. Earlier failed runs remain diagnostic evidence and are not the completion result. Handoff-document edits do not change the public artifact.

Six reviewed screenshots cover homepage initial search, desktop results, mobile results, empty state, error fallback, and 200% text at 320px. Both page and dialog have no horizontal overflow. A 480px-high viewport models reduced space for a mobile keyboard; physical-device keyboard behavior and screen-reader sampling remain P10 manual release checks.

A cold local Chromium sample requested no Pagefind or search-worker assets before interaction. The first `install` query loaded a subset of the index and five result fragments, totaling 160,226 decoded HTTP body bytes in the recorded sample. This is neither compressed transfer size nor a public-host timing measurement. Search queries are not sent to a server, analytics endpoint, or provider; only static asset requests leave the worker. Returning from a result can retain the query in that page's local history entry.

Firefox's Playwright instrumentation does not intercept dynamic imports inside module workers. Portable loading tests therefore gate/block the worker entry and test index/fragment fetches directly; renderer tests supply synthetic worker messages. Pagefind-module import failure itself was also exercised successfully in Chromium/WebKit during development. No new test is skipped for this limitation.

## Maintenance and handoff

From `website/`, install dependencies, run `npm run check`, then `npm run build`. Install Playwright browsers and run `npm run evaluate:search` and `npm run test:browser`. `npm run preview` serves the optimized artifact. Search indexes are generated at build time; development-server search may show the navigation fallback until a built artifact is served.

CI now runs the search evaluator after browser installation and uploads index/search reports alongside existing content/build reports. The evaluator owns and stops its own ephemeral local preview, so it does not depend on a stale development server. Remote GitHub Actions has not been run from this session.

Keep `pagefind: false` in the Starlight configuration while the owned indexing integration is present. Preserve the character option, exact corpus check, and current-before-compatibility gate when changing dependencies or relevance. Do not remove the dedicated-worker error boundary without repeating real index-chunk failure tests. Retain both source-reader and Markdown ChatML-to-OCaml mappings.

P08 presentation/media/metadata work is next. P09's API inclusion/deferral decision, P10 hosted rehearsal and release checks, and P11 domain/GitHub launch remain open. The actual implementation commit in P01.01 remains pending. No domain was bought, no site was published, and no user changes were committed or pushed.
