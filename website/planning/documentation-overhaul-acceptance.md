# Documentation overhaul acceptance audit

Status: D01–D06 complete locally. Publication and hosted verification are separate,
conditional steps; this document does not authorize either. The implementation/evidence history and existing-page review
are in [documentation-learning-paths.md](documentation-learning-paths.md).

## Capability coverage

Every required capability has a human introduction, a worked learning step, an
exact reference and inspectable source. Paths in this table are relative to
`docs-src/`. Tutorial IDs remain stable; curriculum membership, not numeric ID,
defines learning order.

| Capability | Introduction and worked step | Exact contract | Complete composition |
| --- | --- | --- | --- |
| ChatMD instructions, tools and imports | `concepts/how-ochat-works.md`, `chatmd/README.md`; first-agent and file-tool lessons | `overview/chatmd-language.md` | Shared Lantern foundation and all three applications |
| Shell capabilities and guardrails | `shell/README.md`; shell-agent and shell-guardrails lessons | `overview/chatmd-shell-runtime.md`, `overview/chatmd-shell-tools.md` | Guarded engineering assistant |
| Custom hooks and model review | Shell concept; shell-customization lesson | `guide/chatmd-shell-extensions.md` | Engineering's deterministic and model-review entry points |
| One-off specialists | `guide/subagents.md`; specialist lesson | Agent declarations in `overview/tools.md` and ChatMD reference | Foundation specialist and engineering evidence reviewer |
| Fixed/optional persistence | Delegation guide; persistent-specialist lesson | `guide/chatmd-authoring-definitions.md`, `guide/chatml-authoring-children.md` | Three-role persistent review team |
| Generated child | Delegation guide; generated-specialist lesson | Children guide and `guide/chatml-native-requests.md` | Lab captured reviewer and validation/creation requests |
| Inherited authority | `guide/delegated-tools.md`; generated-specialist lesson | `guide/chatmd-authoring-capabilities.md`, children/definitions guides | Lab reviewer selects only inherited file access |
| One-off ChatML | `chatml/README.md`; chatml-program lesson | `guide/chatml-authoring-language.md`, runtime contracts | Report program and real lab checker logic |
| Reusable ChatML tool | Script-form guide; chatml-tool lesson | `guide/chatml-authoring-runtime.md`, ChatMD definitions | Engineering report summary and team collector |
| Conversation moderator | Script-form guide; workflow lesson | `guide/chatml-moderator-runtime.md` | Lab's single coordinator |
| Stateful custom tool | Script-form guide; stateful-workflow lesson | Runtime/native-request contracts | Lab run/watch state and report/close tools |
| Async work and delivery | Script-form guide; background-results lesson | `guide/chatml-authoring-background.md` | Background lab checks and staging |
| Child response watcher | Delegation/background guides; lab walkthrough | Children and background contracts | Timer/receipt polling with bounded deadline, retained result and notification |
| Authoring discovery/validation | Generated-specialist lesson and human reference framing | `guide/authoring-context-tool.md`, model primer and source-bundle contracts | Lab captures and validates a reviewer before creation |
| Host/session lifetime | How-pieces-fit introduction; local/Unix and persistent lessons | Agent-server configuration/operations and lifecycle guide | Private durable team/lab setup; process-bound engineering setup |
| MCP, search and external clients | Tools/operate destinations and existing focused lessons | Maintained MCP/search and stdio/HTTP protocol references | Optional integration paths; engineering's literal repository search |

The thirty existing-page requirements are individually accounted for in the
linked implementation review. D06 added the missing full capability/approval
maps to the lab and team walkthroughs, plus concrete narrower variants. These
changes preserve their existing source bundles and historical execution records.

## Application requirements and implementation decisions

All three bundles include their root, imported sources, scripts, schemas, sample
inputs, README and license through the catalog's actual dependency graph. Shared
Lantern files are assembled into complete archives; a raw application source
subdirectory is not advertised as the archive. Setup names the working directory,
host, provider access and sample dependencies. Source viewers expose root and
companion roles, including independently selected alternate entry points.

The engineering application provides bounded file/literal-search tools, distinct
inspection/check runtimes, deterministic review, a separate model-review variant,
approved report writes, a reusable report calculation and a tool-free evidence
reviewer. Its walkthrough includes denied/selective requests, repeated-report
approval, missing input/backend diagnostics and a human-applied correction.
The stock shell model-review adapter uses a fixed tool-free prompt: its `agent`
field is an identity label, not a path to an authored ChatMD. This verified runtime
behavior replaces the plan's proposed `command-reviewer.chatmd` packaging without
removing the contextual-review variant.

The review team uses two fixed-persistent declarations and one optional declaration.
It teaches default one-off, explicit persistence, new instance versus same-session
follow-up, distinct role/session/receipt/cursor identities, partial failures and
cleanup. Its stateless collector gathers observations through selected lifecycle
tools; the parent supplies assignments and preserves disagreement in the report.
It intentionally does not need a stateful moderator for that bounded collection.

The lab performs the complete inventory/check/review/proposal/stage/recheck/report
workflow. The inventory admits two known IDs, source paths and `verification-v1`;
it is not a source of arbitrary shell commands. Check results identify tutorial,
status, exit code, evidence path and diagnostic summary. Watch queries retain
tutorial, role, session and receipt. The report retains check/stage inputs and
canonical results, correlated reviewer/writer snapshots and unfinished work;
the main agent assembles those records into an evidence-linked explanation.

One moderator owns the lab's custom tools, run records and response watches.
Initial acknowledgement precedes terminal job publication. A timer-driven watcher
probes the exact receipt, reads a bounded terminal output page and records one
result; additional pages use the same query/cursor. A successful watch can carry
a failed child receipt. Generated source is actually captured and validated,
with only inherited `read_file`; the authored writer is a separate persistent
conversation. The fixed stager writes a copy, so only an executed stage and actual
recheck can establish the corrected mechanical result. Closing retires known work;
interrupted effects require inspection and deliberate recovery.

The lab is conversational; its moderator handles state and asynchronous delivery
while the model chooses assignments and proposals. The paired execution diagrams
also explain unattended coordination without claiming a separate ChatMD file type,
native child-response push, cross-daemon federation or transparent process resume.
The sample's eight-record limits and polling intervals are application choices,
not intrinsic language restrictions. Private host configuration preauthorizes the
fixed staging capability; it does not pretend to request per-command human approval.

## Reader tasks inspected

Actual built pages were walked at 1440px and 390px. The shell introduction is one
ordinary click from the documentation landing page. Its runtime/schema/authority
distinction leads to useful guardrails and custom decisions. The earlier specialist
lesson links directly to persistent and generated variants. All four script forms
lead to their complete lessons. Background results lead directly to the lab and
its acknowledgement, receipt-watching and staged-recheck explanation.

Every root/runtime/agent/script/schema in the three application readers was selected
and compared with maintained bytes. The long lab coordinator supports wrapping,
desktop expansion, exact clipboard copy and a permalink restoring its selected
file. Native-local, daemon and stdio pages retain their distinct lifetime guidance.
The baseline manifest's 149 published routes all retain their original URLs and
publication dispositions. All 1,493 historical source headings resolve in the
built pages, as do 25 explicit historical anchors; the renamed shell tutorial now
retains its earlier title anchor.
The contributor guide describes the supported explicit-anchor mechanism instead
of recommending the currently rejected `fragmentAliases` field.

All eight extracted lesson/application check batches passed, including the lab's
original check → literal-text staging → successful recheck. The original sample
remains unchanged. Narrower lab/team configuration variants passed static root
inspection and server validation; those optional variants were not model-tested.
Desktop and mobile screenshots of the long source reader were inspected.

The full framework build/install, ordinary test suite and PR-safe end-to-end suite
passed with live-provider opt-in and credentials excluded. The initial forced
ordinary run found eight stale reference-hash expectations after D05 formatting;
only the reviewed documentation hashes changed, and the follow-up suite passed.
The nine native API hashes were unchanged. The offline semantic documentation
gate also passed. Its recorded revision predates the final commit, but the exact
semantic-input hash still matches the qualified sources; the original report is
retained without rewriting its provenance.

All 309 browser cases were exercised across Chromium, Firefox and WebKit. The
initial run passed 298, skipped the two existing clipboard cases, and found three
stale navigation expectations in each engine. Updating the homepage destination,
direct persistent-specialist link and optional hosting-to-stdio path made all nine
affected cases pass with traces enabled. The result is 307 passing cases and two
existing skips, with initial failures and follow-up evidence retained separately.
The full local run disabled trace recording for speed; CI trace policy is unchanged.

Search passed all 33 queries across 154 indexed pages and 2,555 checked fragment
destinations. All twelve required feature queries reached an intended destination
in the top three. Seven throttled mobile performance samples passed the existing
JavaScript/CSS/font, layout-shift and reflow budgets. These diagnostic samples are
not field measurements or a performance claim for every application page.

## Qualified artifact and publication boundary

The retained local production artifact was built from
`f097ab6acd0e3503e4ded9fd8f67cf4470ca33e2` for `https://ochatlabs.com`:
165 HTML pages, 861 files, SHA-256
`3cb0a51100b92a325997b5408878581d74c3de19a69406a82aa86721993bcbc8`.
It lives at
`scratch/agents/docs-overhaul-root-20260913/d06-qualified-artifact-f097ab6a/`.
Retention and an independent verification both passed. Search/performance reports
identify that exact artifact hash. Browser, reader, source and semantic evidence
are retained beside its inventory. This final handoff changes tests and repository
planning prose only; it does not relabel the earlier artifact as a later revision.

The final branch review found no unrelated tracked paths, generated outputs,
new credentials or CI workflow changes. Per-batch review and meaningful runtime
tests cover the source changes; an added-line credential-pattern check supplements
that review. Pre-existing untracked local guidance/sample files remain untouched.

D06.09 and D06.10 remain conditional: after separate authorization, publish through
the protected PR/release-gate workflow, then inspect the actual deployed revision,
pages, search and downloads. The locally verified Git source objects are not proof
that an unpublished branch revision is available on GitHub. Nothing was pushed,
merged, deployed or sent to a live model provider during this overhaul.

No live provider quality, Linux confinement execution, physical TUI session or
hosted deployment is implied by local deterministic/browser evidence.

## Acceptance scenario evidence

These are reader outcomes from plan section 14, not counts of files written.
Local evidence is kept under `scratch/agents/docs-overhaul-root-20260913/`.

| Scenario | Observed path or execution | Evidence |
| --- | --- | --- |
| 1. Discover shell capabilities | `/docs/` → shell feature, one ordinary click; tool/runtime/schema distinctions visible | `d06-reader-journeys.json` |
| 2. Configure guardrails | Shell feature → guardrails lesson → inspection/check runtime files and request decisions | Reader journeys; `d06-extractions.log`; shell integration expect tests |
| 3. Customize decisions | Guardrails → customization → deterministic hook and model-review variant | Reader journeys; extraction; model-review adapter test uses a controlled response, not a live reviewer |
| 4. Continue a specialist | Old specialist lesson → persistent lesson, with a direct generated-specialist alternative | Reader journeys; persistent/generated lesson integration |
| 5. Generate a specialist | Parent declarations, captured source, validation and creation request; send/status/read/wait/stop | Reader journeys; lab generated-reviewer and authority integration |
| 6. Choose a script form | Concept decision table → each of the four complete form lessons | Reader journeys; program, standalone and stateful integration |
| 7. Understand async results | Background lesson → lab acknowledgement, completion, notification and receipt polling | Reader journeys; lab repeated-poll, failed-child and cleanup coverage |
| 8. Inspect full applications | Every root/runtime/agent/script/schema selected in all three source readers | Reader source text compared with maintained bytes, desktop/mobile |
| 9. Run a bundle | All eight lesson/application extraction batches; real sample commands from stated directories | `d06-extractions.log`; no provider run |
| 10. Read long code | Lab coordinator wrap, expanded desktop width, clipboard bytes and restored file permalink | Reader journeys and inspected desktop/mobile screenshots |
| 11. Search naturally | Required twelve feature queries in the top three; all 33 benchmarks pass | Retained `evidence/search-report.json` |
| 12. Trace the lab | Inventory → check → reviewer/watch → proposal → literal staging → recheck → retained report | Reader journeys, extraction and lab integration/recovery expect tests |
| 13. Understand hosts | Native-local, daemon and stdio instructions distinguish process lifetime, durable state and disconnect | Reader journeys; embedded/daemon runtime checks |
| 14. Follow old links | 149 retained routes, 1,493 old source headings, nine README website links and 869 immutable source objects | Historical-heading and source-link audits; public revision availability remains conditional on publication |

Verification records retain their actual revision, source hashes, observations and
limitations. All 25 catalog entries have complete record coverage. A changed HEAD
or recorded input invalidates the current status while leaving the historical
evidence readable; final qualification does not rewrite old captures or stamp
every example as live-checked. The original recorded documentation-review demo
retains its original provider/model and historical input evidence.
