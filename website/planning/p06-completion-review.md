# P06 completion review

Date: 2026-09-06. Base revision: `bc76b6c72a280b4bad48a793a63373e6db26d2b4`.
Status: P06 complete. All eleven phase tasks and the final local verification gates pass.

## Delivered curriculum

Current inventory: 300 canonical documents (102 publish, 4 compatibility, 9 bridges, 175 repository-only, 10 deferred), 115 rendered documentation routes, 1,260 classified supplemental sources. The three new canonical tutorials account for the increase from P05.

All ten first-class tutorials now provide an outcome, prerequisites and host, working-directory context, complete starting sources or linked setup, actions and expected observations, troubleshooting, cleanup/persistence, and next steps. T01–T04 form the beginner progression; T05–T10 are scoped advanced paths. Previous/next links are driven by `config/tutorials.json`, and every tutorial exposes its source bundles and an expandable verification record.

| Lesson | Canonical source | Host | Verification |
| --- | --- | --- | --- |
| T01 — Run your first local agent | [docs-src/agent-server/tutorials/local-tui.md](../../docs-src/agent-server/tutorials/local-tui.md) | Native local TUI | offline-checked |
| T02 — Give an agent a file tool | [docs-src/tutorials/file-tool.md](../../docs-src/tutorials/file-tool.md) | Native local TUI | offline-checked |
| T03 — Add a specialist reviewer | [docs-src/tutorials/specialist.md](../../docs-src/tutorials/specialist.md) | Native local TUI | offline-checked |
| T04 — Run a request from a script | [docs-src/cli/chat-completion.md](../../docs-src/cli/chat-completion.md) | File-backed batch CLI | offline-checked |
| T05 — Stop after three completed turns | [docs-src/tutorials/workflow.md](../../docs-src/tutorials/workflow.md) | Native local TUI; experimental ChatML | offline-checked |
| T06 — Run a narrow shell agent | [docs-src/agent-server/tutorials/shell-agent.md](../../docs-src/agent-server/tutorials/shell-agent.md) | Legacy local TUI or configured daemon | offline-checked |
| T07 — Run a private Unix daemon | [docs-src/agent-server/tutorials/unix-daemon.md](../../docs-src/agent-server/tutorials/unix-daemon.md) | Durable Unix daemon | offline-checked |
| T08 — Run a background agent | [docs-src/agent-server/tutorials/background-agent.md](../../docs-src/agent-server/tutorials/background-agent.md) | Detached daemon; experimental ChatML | offline-checked |
| T09 — Use a local stdio client | [docs-src/agent-server/tutorials/stdio-client.md](../../docs-src/agent-server/tutorials/stdio-client.md) | Local stdio with explicit data root, or gateway | known-limitation |
| T10 — Connect an HTTP client | [docs-src/agent-server/tutorials/http-client.md](../../docs-src/agent-server/tutorials/http-client.md) | Authenticated loopback HTTP daemon | offline-checked |

T02 and T03 add complete file-reader and specialist workspaces; T05 adds a prompt with an external three-turn ChatML script. T01 gains the concrete Esc / `:w` / Enter alternate submit path. T04 retains initialization-versus-continuation semantics and its historical anchor. Advanced tutorials keep their maintained commands and gain explicit setup, outcomes, failure handling, and cleanup. No canonical source or semantic fixture moved.

## Catalog and downloads

The canonical catalog is `docs-src/examples/catalog.json`. Keeping it in the existing Dune docs source tree lets the semantic gate validate the exact chosen bundles while preserving the root exclusion of `website/`. Website curriculum metadata remains in `website/config/tutorials.json`.

Ten selected catalog entries distinguish **four complete source examples**, **five configurable templates**, and **one illustrative reading sample**. Nine deterministic POSIX `.tar` bundles contain all selected source/data/build companions and the original MIT license. Twenty-three individual destinations represent fifteen unique source files, with the shared license included in every archive.

| Example | Kind | Required local files | Additional prerequisites |
| --- | --- | --- | --- |
| First local agent | complete | `hello.chatmd`, `LICENSE.txt` | Installed Ochat, configured provider, and access to the selected model. |
| File reader | complete | `reader.chatmd`, `reference/project.txt`, `LICENSE.txt` | Launch inside the extracted directory with the configured opam/provider environment. |
| Specialist reviewer | complete | `explorer.chatmd`, `docs-reviewer.chatmd`, `reference/project.txt`, `LICENSE.txt` | Keep both prompts together and launch from the extracted directory. Both selected models require provider access. |
| Three-turn workflow | complete | `three-turns.chatmd`, `three-turns.chatml`, `LICENSE.txt` | Keep the script beside the prompt; each completed request can incur charges. This is not a spending cap. |
| Background timer | template | `timer.chatmd`, `LICENSE.txt` | Generate a separate private daemon configuration using tutorial setup, select this prompt, and keep the daemon alive. Do not submit a model message. |
| Narrow shell command | template | `pwd.chatmd`, `LICENSE.txt` | Supported OS sandbox backend and resource helper, inspected manifest, explicit host authorization, configured provider. Do not combine native --local with --authorize-shell-manifest. |
| Protocol discovery | template | `discover.ndjson`, `LICENSE.txt` | Installed host/gateway plus the tutorial private configuration. Keep stdin open for ongoing work; this stream ends after discovery. |
| Custom OCaml tool | template | `custom_tool.ml`, `dune`, `LICENSE.txt` | Use this configured Ochat checkout and its library dependencies. The bundle is a source example, not an independent opam package; build/run at docs-src/examples/tools as linked in the guide. |
| Moderator contract | template | `moderator.chatml`, `LICENSE.txt` | Embed this script in a ChatMD moderator declaration and choose a compatible host. This file alone is not an agent prompt. |
| Search output samples | illustration | No download | Use the search setup guide for current execution instructions. |

A complete source example still requires the installed runtime and stated provider configuration. Templates explicitly name their missing host/backend/configuration or checkout dependencies. The custom OCaml tool includes its Dune stanza but is not presented as an independent opam package. Larger root prompt packs, the unbundled general-agent template, and old mixed prompt patterns remain repository-only/deferred references; they are not silently promoted into this catalog. P05's ten documented deferrals remain deliberate.

Every copied source needs both exact catalog selection and exact supplemental `example-download` approval. Neither a directory rule nor a repository source link grants copy authority. Generation enforces tracked ownership, real-path containment, unique paths/IDs, declared dependency edges, licenses, and no accidental public-file collisions. Copies and archives are staged with pages as one atomic snapshot. Output checks verify every download SHA-256 and reject unowned download artifacts.

Individual extensionless files use a `.txt` URL because the static preview treats extensionless paths as routes; their download attribute and archive retain the actual filename, such as `dune`. Plain `.tar` was selected after actual browser tests caught automatic HTTP decompression for `.gz`, followed by Firefox adding an extra gzip layer when saving `.tgz`. The final contract is verified against both HTTP response bytes and files actually saved by the browser.

The catalog works without JavaScript, has links to its three example categories, and provides an immediate jump from its page introduction to downloads. It has desktop columns and a single-column mobile layout in both themes. Verification hashes/details are excluded from Pagefind while descriptive example, capability, and host text remains searchable. There is no simulated Run button or in-browser agent execution.

## Verification and provenance

Records include the base Git revision, macOS 14.5 arm64, OCaml 5.3.0 / Dune 3.21.1 / Node 22.22.1, exact source hashes, exercised commands, observations, provider-use status, and limitations. A source-hash or revision mismatch downgrades visible verification to `not-checked`. New local source files show their source path and pending-commit state instead of a nonexistent immutable GitHub link. Production generation rejects uncommitted canonical/example bytes. No commit or live-provider verification is implied by the base revision.

Nine tutorials are `offline-checked`; T09 is `known-limitation`, with the explicit private data-root workaround verified against the standalone binary. No record claims live provider success. Archive/hash checks do not certify model outputs or sandbox confinement.

### T01 — Run your first local agent

The canonical hello fence matches its tracked source; parsing and batch fixture checks pass. Launch/key instructions were reviewed against the current TUI source.

Limits: No live provider response or physical terminal interaction was verified.

### T02 — Give an agent a file tool

The exact prompt parses from a captured bundle. Its declared workspace root reads the bundled Lantern sample and rejects an attempted parent-directory escape through the real file tool.

Limits: No live provider response or physical terminal interaction was verified.

### T03 — Add a specialist reviewer

The captured loader resolves the companion beside the parent and fails when it is missing. The parent reads bundled data; the specialist declares no tools. No live parent-to-specialist model exchange was performed.

Limits: No live provider response or physical terminal interaction was verified.

### T04 — Run a request from a script

The exact preparation and follow-up messages parse with one template, one initial user message and then two users after continuation. The CLI flags, append semantics and output-source directory were reviewed against the implementation.

Limits: No live provider response or physical terminal interaction was verified.

### T05 — Stop after three completed turns

The real parser resolves the external script. The runtime compiles and instantiates it; synthetic completed-turn events preserve state and request session end on the third turn only.

Limits: No live provider response or physical terminal interaction was verified.

### T06 — Run a narrow shell agent

The prompt parses, its shell manifest compiles for macOS, and the actual checkout CLI produces canonical inspection containing /bin/pwd. No sandboxed command execution was performed.

Limits: Inspection does not verify OS sandbox confinement, interactive authorization, or a model-driven shell call. Configure the host/backend before use.

### T07 — Run a private Unix daemon

The actual daemon validates a fresh private config, accepts Unix gateway discovery, and shuts down gracefully. Existing offline host tests check session/observer authorization; this increment did not drive TUI detach/reattach.

Limits: Local private discovery and authentication checks only; no paid response, physical TUI interaction, public listener, or production hosting verification. HTTP used an available loopback port.

### T08 — Run a background agent

The real embedded host delivers the ten-second timer once and reaches the stopped state. No model message or provider request was sent.

Limits: Timer execution is verified in the real embedded host. Physical TUI detach/reattach, restart misfires, and external effects were not checked in this increment.

### T09 — Use a local stdio client

Typed request codecs validate the five envelopes. Actual checkout local stdio, Unix gateway, and authenticated loopback HTTP gateway return five matching successful discovery responses before EOF.

Limits: The explicit private --data-root workaround remains necessary. No model message, TUI input, or public deployment was exercised.

### T10 — Connect an HTTP client

The actual loopback HTTP daemon validates generated private configuration, authenticates initialization, returns a connection ID, serves session.list, closes the logical connection, and supports gateway discovery. Existing offline observer tests verify restricted session visibility.

Limits: Local private discovery and authentication checks only; no paid response, physical TUI interaction, public listener, or production hosting verification. HTTP used an available loopback port.

## Checks and evidence

- `npm run check`: 51 unit tests pass; Astro reports zero errors, warnings, and hints. Coverage includes source-fence parity, native tar extraction and exact bytes, policy/closure rejection cases, source/revision invalidation, safe provenance, and curriculum navigation.
- `npm run build`: **117 HTML pages, 411 files, 15,456,846 bytes**, representing 115 documentation routes plus homepage/404. Artifact SHA-256: `03419bc7851054b4f09f8cc864be1bc01d95420be967361cba765127cb621752`. Route/fragment/heading, source attribution, search/sitemap/noindex, download ownership/bytes, and artifact capacity checks are enabled.
- Forced `dune build --force @agent-docs-check`: 300 pages and 38 protocol methods pass. `Docs_tutorials` loads the actual catalog through the captured source resolver, traverses nested local agents, fails missing companions, materializes bundled data, reads the scoped sample and rejects an escape, checks specialist tool isolation, and compiles/executes three synthetic completed-turn events. Existing timer, observer, protocol JSON, generated-source, exact excerpts, batch, shell, and custom-tool checks remain.
- `python3 test/agent_docs/check_tutorial_hosts.py`: five provider-free checks pass using actual checkout binaries: private configuration validation, standalone local stdio with explicit data root, Unix daemon/gateway discovery and shutdown, authenticated loopback HTTP initialization/connection header/session listing/close/gateway, and canonical fixed-pwd shell inspection. Each discovery transport returns five matching successful responses. The helper removes generated credentials and state after stopping its own processes.
- Final full browser suite: **124 passed, 2 expected non-Chromium clipboard skips**, in one complete Chromium/Firefox/WebKit run (4.6 minutes), against the final plain-tar artifact. It includes five added scenarios per engine for T01–T10 navigation/context, no-JavaScript catalog reading, every served download plus a saved archive, narrow accessibility in both themes, and preserved runtime qualifications.
- Visual review: five final screenshots inspected after palette/label refinement: desktop/light catalog, mobile/dark specialist card, desktop/light file-tool steps, 320px/dark workflow introduction, and mobile/light expanded file/provenance details. No document overflow. Catalog links use the shared site palette, and status badges wrap as whole labels.

Local evidence: `scratch/ochat-website-evidence/p06-{check,build,docs-check,browser}.log`, `p06-host-check.json`, and `p06/` screenshots/reports. Early download failures are retained as diagnostic history; final results must not be inferred from those partial runs. `.generated/examples-report.json` is included in the configured CI evidence upload. Remote GitHub Actions has not run.

No model message/provider call, sandboxed shell command, physical TUI key sequence, public listener, deployment, account setting, domain purchase, commit, or push was performed. Provider TLS limitations, host-specific shell authorization, stdio startup workaround, scripting lifecycle differences, and Apple Silicon/OpenBLAS setup links remain visible. Public hosting/header behavior, physical accessibility, performance, and live operational checks remain later release gates.

## Phase gate mapping

| Task | Evidence |
| --- | --- |
| P06.01 | T02 complete file declaration, bundled sample, workspace-root read, authority explanation and observed checkpoint. |
| P06.02 | T03 complete parent/specialist/data layout, declaration-relative resolution, missing-companion rejection and tool isolation. |
| P06.03 | T04 complete prompt preparation, output/continuation semantics, source-root context, transcript inspection and cleanup. |
| P06.04 | T05–T10 published with scoped prerequisites, current host commands, reviewed qualifications and explicit verification limits. |
| P06.05 | T01–T10 normalized outcomes, context, actions, expected observations, failures, persistence/cleanup and next steps. |
| P06.06 | Ten-entry typed catalog distinguishes four complete, five template and one illustrative source. |
| P06.07 | Exact tracked-source selection, readable filenames/provenance, 23 individual downloads and nine source archives. |
| P06.08 | Offline semantic checks, exact-file/hash records, feasible provider-free actual binary checks; no invented live run. |
| P06.09 | T01–T04 beginner journey and T05–T10 optional progression have real previous/next links and source-bundle panels. |
| P06.10 | Real parser/captured-loader closure validation; complete companion/data/build/notice inventories and explicit template dependencies. |
| P06.11 | Stdio private data root, provider TLS, native/legacy shell flags, host scripting lifecycle and platform setup preserved and reviewed. |

P07 is next: evaluate search against this complete approved curriculum/catalog, exercise relevance and failure states, and produce Milestone B. Working search smoke tests here do not close P07. P01.01 remains open for the actual repository commit; P08–P12 remain separate presentation/release/operations work.

## Follow-up: inline example source and ChatML highlighting

The user requested inspecting connected ChatMD examples inside the UI without downloading them, then specified OCaml syntax highlighting for ChatML. The shared `ExampleSource.astro` reader now renders all 23 selected file destinations (15 unique sources) in the catalog and on all ten tutorials, plus the associated custom-tool and ChatML reference pages. The first tutorial/reference entrypoint is open initially; each companion, sample, build file, and license has a native expandable filename. Catalog entries provide an explicit View source control and use the full content width when expanded. Downloads and their provenance remain available.

Displayed text comes from the exact approved source bytes through strict UTF-8 decoding. Astro escapes tags, and a Shiki preprocessing hook restores the final newline that Astro normally removes from snippets. Nothing is fetched or executed to open a source file; basic reading works without JavaScript. Code regions are named, keyboard-focusable, horizontally scrollable, and styled for both themes. Source bodies stay out of Pagefind to avoid duplicate content. The catalog’s illustrative search-output entry still links its existing reading source and has no downloadable source selection.

Both Markdown `chatml` fences and `.chatml` files use OCaml highlighting; XML highlights ChatMD tags, and Dune build files keep their Scheme grammar. This is display formatting, not a parser or runtime change. The two specs record the user’s additions in section 9.5. All 122 existing task states remain synchronized, and P06 remains complete.

Follow-up verification is recorded in `scratch/ochat-website-evidence/inline-source-*` and the `inline-source/` screenshots/reports. Final results are below. The earlier P06 full-suite evidence above remains historical.

Final follow-up checks: `npm run check` passes 53 tests with zero Astro errors/warnings/hints. `npm run build` passes all output checks (117 HTML pages, 411 files, 15,734,623 bytes; SHA-256 `766b7dca1a4247fdae1c5ea9c6b6adee95cfe315afaf7529fb7023b75108f565`). Forced `dune build --force @agent-docs-check` passes 300 pages and 38 methods offline. One final browser run covering tutorials, shared reading, and site behavior passes 79 tests with two existing non-Chromium clipboard skips across Chromium, Firefox, and WebKit (81 total, 1.8 minutes). This is the affected browser suite, not a rerun of the entire website suite. Four screenshots were inspected across both themes and 320–1440px widths; no document overflow. Every displayed selected file matches source text, including final newlines, and served/saved download bytes still match the catalog hashes. Formatter and Git whitespace checks pass. No runtime/provider calls, commits, publication, or domain changes were made in this follow-up.
