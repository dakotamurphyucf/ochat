# P05 batch and shell publication review

Date: 2026-09-06. Source revision: `bc76b6c72a280b4bad48a793a63373e6db26d2b4`.
Status: batch and shell group implemented; overall P05 remains in progress.

## Scope

Nine reviewed sources join the previous 52: **61 rendered docs** (**60 publish,
1 bridge**), **236 deferred**, **297 tracked sources accounted for**. Existing
source IDs and routes are retained. The shell hub now links to local runtime,
tool, security, persistence, extension, management, and architecture references.

| Canonical source relative to docs-src | Website route |
| --- | --- |
| `cli/chat-completion.md` | `/docs/reference/commands/chat-completion/` |
| `cli/shell-runtime-management.md` | `/docs/reference/commands/shell-management/` |
| `bin/ochat_shell_resource_runner.doc.md` | `/docs/reference/commands/shell-resource-runner/` |
| `overview/chatmd-shell-runtime.md` | `/docs/reference/shell-runtime/` |
| `overview/chatmd-shell-tools.md` | `/docs/reference/shell-tools/` |
| `guide/chatmd-shell-security.md` | `/docs/guides/shell-security/` |
| `guide/chatmd-shell-persistence-and-audit.md` | `/docs/guides/shell-persistence/` |
| `guide/chatmd-shell-extensions.md` | `/docs/guides/shell-extensions/` |
| `guide/chatmd-shell-runtime-internals.md` | `/docs/library/shell/architecture/` |

Navigation orders shell runtime → tools → security → persistence, with management
and resource-helper entries under Commands and implementation detail under Library.
The extensions guide carries experimental status for ChatML. All nine retain
conservative `not-checked` runtime verification metadata: editorial and offline
checks do not establish live-provider or deployment-backend verification.

## Canonical corrections and validation scope

The batch guide previously expected an `echo` tool from a prompt that did not
declare it. Its replacement copies the tracked tool-free hello prompt into a
private `mktemp` directory and appends one user message. Preparation is separated
from the billable completion command. A second example continues the output
transcript while omitting `-prompt-file`, since every invocation with that flag
appends the template again. The old `#130-second-smoke-test` fragment is retained.

The guide now explains output-parent creation, launch-directory artifacts,
failure diagnostics, single-writer use, the native TUI's separate host, and the
Unix `/dev/stdout` special case. The root-scoped example explicitly creates its
output parent. Relative batch imports and source variables use the output
transcript directory; the original template directory is not preserved. These
claims were checked against `bin/main.ml`, `lib/chat_response/driver.ml`, the
ChatMD parser, and `lib/Io.ml`.

The runtime reference now distinguishes batch output-root context from native
and daemon materialized prompt artifacts. Three misleading finalized-output
claims were corrected in the batch guide, shell tools overview, and runtime
internals. Finalized output has byte bounds and can end within a UTF-8 sequence;
the optional sanitized live stream has a distinct incremental UTF-8 and disclosure
contract. The existing detailed security guide already explained this correctly.

`Docs_smoke.batch` reads the documented template path and both literal user
messages from the canonical guide, copies/appends through the existing I/O
helpers in the checker-owned private directory, then parses the transcript. It
checks one config and developer message, one then two user messages, and no tool
or script declarations. `docs_check` invokes it alongside existing examples.
This validates the prepared ChatMD and continuation shape. It does **not** run
the shell fences, completion driver, model, or a live shell backend, and does not
assert a particular assistant response.

No runtime implementation, generated-document owner, dependency declaration,
canonical path, or existing semantic fixture was moved. Four canonical prose
files changed; the website importer still preserves every fenced-code byte.

## Deliberate deferral

`guide/chatmd-shell-examples.md` remains deferred after review. Its 17 partial
patterns include stale embedded ChatML calls in examples 5, 8, and 9: older
`Shell.context`/rewrite/defer surfaces and `Audit.keep(event)`. Current extension
hooks use purpose-specific modules, with `Audit.keep()` taking no argument.
The existing docs gate parses this page's XML but does not compile those script
bodies. Correct the snippets and compile dependency-complete fixtures before
publishing. External programs, deployment manifests, credentials, and platform
requirements also need explicit treatment; the existing complete narrow `pwd`
tutorial remains the starting path. The manifest records this specific reason.

## Verification

- Forced `dune build --force @agent-docs-check` passes: 297 pages, 38 methods,
  including the new batch preparation check and existing generated-source,
  protocol, example, and authorization checks. No live provider calls.
- Both changed OCaml test helpers pass `ocamlformat --check`.
- `npm run check`: 38 unit tests pass; Astro reports zero errors, warnings, and
  hints. Source fence parity now covers all 61 rendered docs.
- `npm run build`: 63 HTML pages, 276 files, 10,818,412 bytes. Output links,
  actual fragments, indexing policy, and static-host capacity checks pass.
  The existing dense protocol reference and deferred Mermaid chunk remain
  performance release work; these counts are not a hosted benchmark.
- Browser checks cover the batch historical anchor and preparation/continuation
  blocks, management host scope, no-JavaScript shell reading path, identifier
  search, 320px reflow and full axe scans for batch and extension pages in both
  themes. Existing migration/navigation/search checks were also rerun.
- Browser result: **27 passed across the selected run and targeted recheck**,
  covering Chromium, Firefox, and WebKit, with no unresolved failures. The initial
  run passed 24 checks; three anchor tests failed because a digit-leading ID was
  used as an unescaped CSS ID selector. All three passed after changing the test
  to an attribute selector; the page and preserved fragment were already correct.
  This result combines the initial run with the corrected test recheck.
- Inspected four screenshots: batch desktop/light and mobile/dark, security
  desktop at sanitized live progress, and the extension action table at 320px
  in dark mode. None had document overflow; wide source/table content retains
  its scroll containers.

Evidence: `scratch/ochat-website-evidence/p05-shell-{docs-check,check,build,browser,browser-recheck}.log`
and `scratch/ochat-website-evidence/p05-shell/`. These are local development
checks; remote CI, hosted performance, and assistive-technology release reviews
remain pending. The earlier full browser baseline is recorded in
[the hosting review](p05-hosting-review.md); it was not rerun in full for this
content-only group. Automatic migration reports remain outside public output.

## Historical next implementation work

Completed by [the P05 closeout](p05-completion-review.md): the three hook bodies
now pass real offline compilation/action checks, and corpus, bridges, and
supplemental coverage are complete. The following describes this earlier checkpoint.


Correct and validate the partial shell examples, then continue reviewed library
families and capability gaps. Complete older TUI bridges and the machine-readable
supplemental-source manifest. Dependency-complete tutorial downloads remain P06
work. No new P05 checkbox is closed merely because another nine pages render.
