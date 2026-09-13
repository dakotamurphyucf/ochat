# Documentation overhaul acceptance audit

Status: local D06 qualification in progress. D01–D05 are implemented. Publication
and hosted verification are separate, conditional steps; this document does not
authorize either. The implementation/evidence history and existing-page review
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

## Reader tasks inspected so far

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
built pages; the renamed shell tutorial now retains its earlier title anchor.
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
gate also passed. Search, full browser checks and artifact provenance remain in
progress; this is not yet a claim that D06 is complete.

## Remaining qualification

- Final canonical, website, search, semantics, full browser and presentation gates.
- Final application/lesson bundle and source/verification reconciliation.
- Old fragment/source links, README/homepage routes and artifact revision checks.
- Final branch review, evidence/limitations, committed handoff and local artifact.
- Separately authorized protected publication, followed by hosted revision checks.

No live provider quality, Linux confinement execution, physical TUI session or
hosted deployment is implied by local deterministic/browser evidence.
