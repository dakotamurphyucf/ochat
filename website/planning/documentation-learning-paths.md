# Capability-oriented documentation

The documentation overhaul makes shell customization, subagents, and ChatML
workflows visible learning destinations. Previously, substantial runtime features
were documented in references while the connected tutorials mainly introduced
small examples and transport setup.

## Navigation and source ownership

The public sidebar now groups pages into Start here, Tools and shell access,
Subagents and agent teams, ChatML workflows, Complete applications, Run and
operate, and Reference. Contributor material remains separately reachable.
Existing routes stay unchanged; installation, build troubleshooting and the first
agent retain their order. The manifest owns section/order metadata, while
`config/navigation.mjs` assembles the sections and landing-page paths.

The landing page exposes three practical feature choices and separate beginner,
application and reference links. The broader capability catalog remains available
below the introductory material. New human guides explain how definitions, tools,
scripts and sessions fit together, how to choose a delegation pattern, and how
inherited tools preserve authority.

## Human guidance and model context

The installed authoring primer remains canonical model-facing text. A website
component introduces its purpose and links human readers to feature guides.
`config/authoring-topics.mjs` maps known inline topic identifiers to the maintained
source excerpts represented by the runtime corpus. Markdown transformation adds
links only to those inline references; executable fences and installed guidance
are unchanged. Dynamic tool/signature topics point to their availability contract,
not a fictional universal capability list.

## Verification boundaries

The navigation tests preserve route ownership and onboarding order. Browser
journeys cover visible feature discovery, specialist onward links, primer topic
links, no-JavaScript reading and mobile layout. Search benchmarks retain the
existing queries and add natural feature terms such as “subagents,” “shell
permissions” and “background results.” Queries with `maxRank` are mandatory
individual checks in addition to the overall top-five benchmark threshold.

Canonical documentation checks, browser rendering and search do not establish
live model behavior. Historical tutorial verification is downgraded when its
source dependencies change; do not refresh recorded hashes without reviewing and
repeating the relevant checks. Production publication remains a separate protected
release workflow.

## Connected learning paths

The first connected scripting checkpoints are now available: **T11** summarizes
Lantern reports with `run_chatml`; **T12** exposes the same maintained source as
`summarize_checks`. Each bundle is independently inspectable and downloadable.
The program takes a JSON array; the named tool takes an object containing a
`files` array and validates its output. Neither example runs the checks reported
in its supplied data.

`config/tutorial-paths.mjs` now owns explicit primary learning paths. Every
registered tutorial must appear once. Stable IDs identify lessons; they are not
a global prerequisite sequence. Previous/next links stay within a path and return
to the curriculum at its end. Allocate new IDs after the last registered lesson,
and update the manifest, path membership, overview, catalog and approved sources
together.

Offline composition tests execute these exact sources through registered tools
in a real session with deterministic provider responses. They check aggregation,
schema rejection before reads, inherited file boundaries and malformed-report
failure without partial results. Captured bundle checks separately verify source
closure and missing companions. This does not establish live-provider behavior.

The paths now include independently runnable checkpoints for useful shell commands,
custom guardrails/review, persistent and generated specialists, stateful moderation
and background results. The next stage combines these capabilities in three
complete multi-file applications: a guarded engineering assistant, a persistent
review team and a living documentation lab.

Each application must include its root, imports, agents, scripts, schemas and
sample data in the existing website source reader, with exact host setup and
honest execution evidence. Generated children select inherited tools; response
notifications use the supported watcher/polling pattern. These are documentation
compositions of existing runtime features, not permission to invent broader
authority or unsupported session APIs.

## Native shell prerequisite correction

The shell learning path exposed a startup gap: native local TUI required a shell
manifest grant but rejected the interactive authorization flag. Native
`--local --authorize-shell-manifest` now explicitly admits the reviewed prompt's
compiled manifest for that process. The ordinary default remains fail-closed;
command policy, approval and required confinement continue to apply. Omitting
`--local` with this flag retains the existing legacy selection behavior.

The native regression also exposed a concurrent publication bug. A completed
tool could fail to publish its result while another tool in the same foreground
batch awaited permission. Publication now accepts that waiting state for the
matching running operation; execution admission and other safe points retain
their checks. The regression requires nonreviewed results before responding to
the pending approval, then verifies the subsequent model request. The runtime
source coverage pin was reviewed for this publication correction; lifecycle,
recovery, moderator and notification contracts did not change.

Local validation on macOS: native startup/exit without a model request; required
sandbox execution, hard denial and approval using deterministic provider fixtures;
the session, composition and authoring-source suites; canonical documentation
checks; website checks/build and six affected browser checks across Chromium,
Firefox and WebKit. No live-provider validation or remote publication is claimed.

Legacy retirement remains future work. It does not require importing, exporting
or migrating old alpha session stores. That decision does not remove legacy
functionality in this change.

## Useful shell learning checkpoints

T06 keeps its existing URL and now starts the Lantern learning project with a
fixed setup-file inspection tool and a reusable read-only shell runtime. T13
adds a real documentation checker, a separate runtime for project code, a scoped
report directory, selected environment, required confinement, and approval before
report writes. Both are independently complete downloads with every maintained
file visible in the browser, including Markdown sample inputs. Literal input
schemas, command effects, policy decisions and OS enforcement are explained as
separate parts of the same execution path. A deliberately missing verification
section produces useful failure evidence instead of a trivial successful command.

Canonical sample Markdown can be displayed as source only when its exact example
path has an approved download rule and a canonical manifest disposition. Ordinary
documentation is still not implicitly copied into downloads. Existing source
URLs and the earlier shell authorization fragment remain available.

Execution exposed a Seatbelt prerequisite defect: the child-process capability
allowed fork but did not admit helper executable paths. It now permits execution
inside the already admitted readable roots, retaining denial outside those roots.
The real-backend regression checks both outcomes and disabled child capability.
No separate resource-helper executable is required.

Local evidence on macOS includes the complete composition and authoring-source
suites, canonical documentation checks, real shell helper lifecycle regressions,
and the shell boundary expect tests. Deterministic provider fixtures drive actual
shell execution and verify both approval and denial while other tool outputs
complete. Both built archives were extracted, compared byte-for-byte with their
maintained sources, admitted through the shell inspector, and exercised with the
sample checker. Website checks passed 83 tests; the build checked 157 HTML pages
and 702 files; search passed 33 queries; 96 tutorial and reading browser tests
passed across Chromium, Firefox and WebKit. Desktop lessons and the mobile source
reader were visually reviewed. Catalog records identify exact source hashes and
the limits of this evidence; no live provider or Linux execution is claimed.

The following sections record subsequent shell and specialist checkpoints.
Stateful and background workflows, the three complete applications, and the final
presentation and reader-journey audit remain unfinished work in the full plan.

## Custom shell decisions

T14 completes the shell learning sequence with a real ChatML reviewer: reject a
selective report, automatically approve the first complete report request, and
defer later requests to the user. The lesson explains that retained state records
review decisions rather than successful writes or file existence. The complete
Lantern bundle includes a separately selected model-review entry point with the
same command and confinement configuration and an explicit hard report policy.

The model-review documentation now matches the stock adapter. Its `agent` field
labels a fixed tool-free reviewer; it does not look up an authored ChatMD agent
tool. Model failures configured as denial do not fall through to a human. The
former shell pattern's misleading agent declaration and fallback wording were
corrected while preserving its old fragment URL.

The source reader distinguishes an **Alternate entry** from an imported companion.
Bundle validation parses each entry point, compares the combined actual source
closure with declared dependencies, and verifies missing-source failure for each
entry that consumes a companion. This supports independently selected variants
without fabricating imports that would change runtime behavior.

Local macOS qualification covers the full composition suite and 349 canonical
documentation pages. The exact public ChatML sources execute through a real
session and required shell backend. A separate real Agent_runtime test replaces
only the nested model callback: policy denial avoids the callback, a valid stub
decision runs the checker, and malformed output preserves prior evidence. This
does not make a model API call or establish live model judgment. The extracted
13-file archive retains exact source bytes, both roots inspect correctly, and its
sample checker produces the expected report. Website checks passed 83 tests; the
build checked 158 HTML pages and 719 files; search passed all 33 queries. The
desktop lesson, alternate-entry label and mobile wrapped script were reviewed.
All 99 tutorial and reading browser checks passed across Chromium, Firefox and
WebKit, including complete source access with JavaScript disabled.

## Continuing and generated specialists

T15 and T16 add a visible agent-team learning path after the one-off specialist.
Both use Lantern's setup instructions and recorded check report, asking a reviewer
to diagnose the missing verification step and refine its proposal in the same
conversation. Optional and fixed authored persistence are taught alongside the
shared lifecycle tools. The generated variant discovers authoring help, validates
captured ChatMD sources, selects the parent's existing file tool and explicitly
starts its child before sending work. Both tutorials distinguish session identity,
submission receipts, output cursors, lifecycle state and retained output.

Each independently complete 14-file archive includes both parent entry points,
the authored reviewer, readable generated templates and matching creation and
validation requests, the shared sample project, and an exact private daemon
configuration. Prompt definitions and the store remain outside the read-only
project workspace. Readers can inspect every companion file in the browser.
The earlier specialist tutorial and delegation guide link directly to the new
lessons. Search accepts these human tutorials as useful destinations while
retaining the required top-three threshold.

A combined integration scenario executes the exact public files through a real
durable daemon and its dispatcher, replacing only model responses. It checks
retained conversation history, wrong-wrapper rejection, optional one-off output,
fixed persistence, authoring discovery, source parity and validation, rejection of
a broader native reader, idempotent generated creation, correlated follow-up
responses, scoped file reads and output retained after stop. Both built archives
were extracted, compared byte-for-byte, and validated from their extracted paths.
The canonical checks cover 351 documentation pages and 39 protocol methods; the
website checks pass all 83 tests. This evidence does not claim live model quality,
a physical TUI walkthrough, a wire-transport walkthrough of these bundles, or a
Linux run.

The initial concurrent runtime/browser run hit an existing compaction deadline
and Firefox test deadlines. The runtime and canonical suites passed when rerun
without browser load, with unchanged expectations. Heavy validation groups are
kept separate on this development machine to avoid counterproductive contention.
All 105 tutorial and reading browser checks passed across Chromium, Firefox and
WebKit across the initial run and targeted rechecks. Four Firefox rechecks passed
with two workers; the remaining existing shell-customization check passed alone
in 11.4 seconds, with its original 30-second deadline and assertions unchanged.
The final build checks 160 HTML pages and 755 files. Desktop lesson and mobile
source expansion were visually reviewed.

## Stateful tools and background evidence

T17 builds a retained review ledger behind one strict moderator-handled tool.
Recording a note replaces only that file's previous note; a rejected update leaves
the ledger unchanged. Resolving the invocation returns tool output, while the
handler's returned state retains the ledger for subsequent turns. T18 uses a
separate moderator to start Lantern's real checker through a constrained shell
binding, acknowledge its job, expose progress/cancellation, and deliver a
correlated terminal result with an explicit request for a model turn. Both
complete projects are available in the inline source reader and as independent
nine- and ten-file archives.

The ChatML path now runs from one-off programs through reusable tools, the
three-turn moderator, stateful tools and background results. The three-turn source
uses `let*` and its lesson distinguishes model decisions, event handling and
runtime execution. The original timer remains an operations example with a link
to useful background work. Website-only reference introductions lead directly to
the two practical lessons without changing the audited model-authoring corpus.

Offline native-host integrations execute the maintained ledger and coordinator
with controlled provider responses. They check retained/replaced findings, failed
updates, real checker exit 1, missing-input exit 2, a running-job progress query,
overlapping-start rejection and cancellation. The cancellation case adds a
filesystem readiness gate only to the temporary sample checker; it does not
change the published moderator, shell binding or schemas. Every background case
requires the initial acknowledgement to precede exactly one terminal notification
and observes the requested follow-up input without rewriting that acknowledgement.

The approval variant also exposed an existing host distinction worth teaching:
an unresolved shell permission pauses foreground model work. The model cannot
use its progress or cancellation tools during that permission wait; the reader
must resolve it through the host's permission controls. Cancellation tools apply
to admitted running work. The lesson retains this behavior explicitly.

The full composition and canonical-documentation checks passed on macOS, covering
353 documentation pages and 39 protocol methods. All 83 website checks passed;
the final build checks 162 HTML pages and 783 files. All 33 search queries reached
the top five, including the required feature queries' stricter rank thresholds.
The two new archives and the reformatted three-turn archive were extracted and
compared byte-for-byte, then inspected from their extracted working directories.
Both bundled checkers produced the expected failing report. These checks establish
local offline behavior, not live model quality, physical TUI interaction, Linux
execution or restart persistence for these process-bound examples.

All 111 tutorial and reading browser checks passed across Chromium, Firefox and
WebKit without retries or changed deadlines. Desktop/mobile review also followed
both reference introductions into their practical lessons and confirmed the
rendered pages have no document-wide overflow. The reference callouts and lesson
introductions were visually inspected. Browser projects ran after runtime checks;
Firefox used one worker and Chromium/WebKit used two to avoid local contention.

## D03 learning-path completion

The foundation now leads explicitly from file reading to either a specialist or
a guarded shell capability. The curriculum explains Lantern's progression from
a brief to real documentation/checks and directs readers to independently complete
bundles. It distinguishes recorded report inputs from actual checker execution.
Installation → build troubleshooting → first agent stays unchanged. Batch,
daemon, stdio and HTTP introductions explain why to choose those optional paths;
the batch page no longer inserts itself between specialist and ChatML lessons.
Persistent/generated lessons include the durable setup at the point of use.

All 18 tutorial IDs belong to exactly one primary path, with meaningful
previous/next links and matching overviews. The runtime evidence for the completed
checkpoint families is:

| Checkpoints | Behavioral evidence |
| --- | --- |
| First agent, file reader, one-off specialist | Canonical complete-source parity, actual scoped file reads with escape denial, companion capture and specialist tool isolation; no claim of a live model conversation. |
| Shell inspection, guardrails and custom review | Native confined tool execution, literal argument handling, separate read/write authority, report approvals and deterministic review state; model-review behavior uses a controlled callback. |
| Persistent and generated specialists | Real durable host with controlled model responses: continuing identities, source validation, selected inherited tools, receipts, follow-up and stop/read behavior. |
| One-off program and reusable tool | Real registered-tool execution: aggregation, strict schema rejection before reads, file boundaries and malformed-report failure. |
| Moderator, stateful tools and background results | Exact scripts execute retained state, invocation resolution, three-turn termination, real checker jobs, correlated notification and cancellation. |
| Batch, daemon and clients | Canonical batch fixtures, embedded timer/observer checks, private config validation and actual local/Unix/HTTP discovery. |

Canonical checks passed for 353 pages and 39 protocol methods. The maintained host
integration checker now covers both explicit durable and automatic transient
stdio data roots; both return the five discovery responses and exit cleanly at
EOF. Private Unix and authenticated loopback HTTP discovery also pass. The old
T09 verification note incorrectly retained a historical RNG limitation; it now
records the observed working behavior. These six host checks make no model calls.

The actual foundation, timer, stdio and program/tool archives were extracted and
compared byte-for-byte; ChatMD entries inspect from their extracted working
directories. Shell, specialist-team and stateful/background archive execution is
recorded in their sections above. No checkpoint relies on the untracked obsolete
Lantern preparation draft. Tutorial evidence was refreshed only for these
executed offline checks; historical live application recordings were retained.
Verification remains tied to its recorded revision and hashed dependencies; this
is not a claim to have tested live providers or every platform at a future revision.

Website validation passed all 83 checks; the build checks 162 pages and 783 files.
The targeted onboarding, all-lesson path and instruction browser checks passed.
The earlier full 111-case browser matrix remains applicable to unchanged reader
behavior; it was not repeated for these prose and evidence changes.
Three mobile journeys also followed the new file-tool → shell, batch → daemon
and stdio → learning-path links in the rendered site without document overflow.

## D04 — Guarded engineering application

The first complete application combines six useful capabilities: scoped file
reading, fixed inspection, literal text search, real checks with reviewed report
writes, a standalone ChatML report summarizer, and an authored evidence reviewer.
Its two entry points demonstrate deterministic ChatML review and the stock
tool-free model-review callback. The latter's identity label does not load the
application's authored specialist. The walkthrough explains authority, actual
check evidence, human correction, review-state lifetime and shutdown.

The 19-file archive shares maintained checker, schema and script sources with
the learning checkpoints. Both extracted roots pass shell inspection. Every
published archive file matches its canonical bytes; the actual checker saves
the expected failing report, and the documented human correction produces three
passing checks. This is mechanical evidence, not a judgment of prose quality.

Two focused expect scenarios qualify the composition. A native transient session
executes inspection, search, the real checker and report summarizer, with outside
access rejected and a later denied report write preserving evidence. A separate
explicitly injected agent callback checks the model-review policy and the exact
one-off specialist binding receiving real checker output. The ordinary one-off
agent runner has a separate transport from the native parent's model override;
the test does not claim that one override covers both. No provider call was made.

Canonical validation passed for 355 pages and 39 protocol methods; website checks
passed 83 cases and the build checked 163 HTML pages and 806 files. Nine Chromium
application/source cases passed, including archive byte parity and all 19 inline
files without JavaScript. Desktop and mobile review followed the root/source
links and opened companion ChatML without document-wide overflow. Search found
all 33 benchmark queries in the top five, across 152 indexed pages and 2,512
checked anchors. Long mobile code still benefits from the planned D05 reader
improvements. These are local macOS and preview results, not a live provider,
interactive TUI, Linux or deployed-site qualification.

## D04 — Persistent review team

The second complete application gives correctness, documentation and integration
reviewers distinct authored conversations. Two declarations are fixed persistent;
the documentation reviewer supports default one-off and explicit persistent use.
The walkthrough covers new-instance creation, same-wrapper continuation, new
follow-up receipts, output cursors, failure distinctions and explicit stopping.
Its private daemon exposes only the sample workspace through file tools.

A standalone ChatML collector performs bounded status, zero-time receipt wait and
output-read operations for up to three reviewers. It retains per-operation errors
and raw correlated evidence; it does not infer success from idle/caught-up status
or synthesize consensus. The caller retains role/session/receipt/cursor state.
The follow-up sample deliberately leaves the proposed correction unexecuted so
reviewers must distinguish better wording from proof of a passing release.

The complete durable integration passes with controlled provider responses:
distinct sessions, actual scoped evidence reads, one failed reviewer, receipt
filtering, consumed-cursor reads, optional one-off output, new instance on omitted
ID, same-ID retained history with a new receipt, and healthy output preserved when
another receipt query fails. Stop requests are admitted, desired state is stopped,
no child operation remains active at observation, and retained evidence is readable.
This does not claim the stop receipt itself joins all asynchronous cleanup.
The existing authored/generated lesson integration also passes after sharing its
host harness with the application test. Production runtime code is unchanged.

All 16 files in the published archive match canonical sources. From clean
extraction, the private server configuration validates and the root inspects.
Canonical checks pass for 357 pages and 39 methods, website checks pass 83 cases,
and the build checks 164 HTML pages and 827 files. Nine targeted Chromium cases
pass, including the complete inline bundle without JavaScript and all served
source/archive byte checks. Desktop/mobile review follows the specialist lesson
into the application, opens agents/schemas/scripts and reads wrapped ChatML
without document overflow. Search passes 33/33 queries across 153 indexed pages
and 2,523 checked anchors. No live provider, physical TUI, Linux, restart/recovery
or deployed-site qualification is claimed for this batch.

## D04 — Living documentation lab

The third application combines the earlier lessons into a complete 28-file
project. A conversational parent assigns a dynamically captured, validated,
read-only reviewer and an authored persistent writer. One ChatML moderator owns
background tutorial checks, retained evidence, stateful tools, timer-driven
receipt watchers, fixed sample staging, rechecks and cancellation. It is linked
from the background-results and generated-specialist lessons, the application
gallery and the main documentation navigation. Every companion is readable in
the existing browser source viewer and included in the complete archive.

Its private daemon profile explicitly preauthorizes the inspected shell manifest
and fixed staging capability. Required confinement, no network and a staging-only
write boundary remain active. `tool_default allow` supplies an automatic shell
approver; this application does not claim that a shell `ask` rule under that
profile produces a human prompt. The guarded engineering application teaches
interactive approval separately. The mechanical checker tests a verification
heading and expected-result line; it does not judge tutorial quality.

Controlled native durable integration executes the real pass/fail checker,
rejects an unsupported staging target, stages a copy, verifies the changed
result, and retains acknowledgments before canonical notifications. Reviewer
validation/creation, idempotent creation retry, an actual inherited file read,
timer arming, duplicate-watch rejection, same-session follow-up, a persistent
writer, failed child receipts and four distinct result notifications pass.
Closing a pending reviewer cancels its execution and watcher; repeated close
retains one cancellation. Watcher success remains distinct from child success.

A separate daemon-restart case uses the exact application sources with an Ask
profile to hold an admitted file read. After shutdown and recovery, job identities,
attempts and invocation counts are unchanged; one interrupted result is delivered
and the moderator's report retains it. Automatic model wakes are disabled in this
recovery fixture. This proves recovery of interrupted check evidence without
replaying the read, not continuation of an arbitrary program counter or recovery
of a partially written external file.

The larger scenario exposed an embedded transport shutdown bug: an unread bounded
notification queue could leave its publisher blocked after disconnect. Publication
now observes connection closure before session detachment. The full embedded host
suite and a zero-capacity backpressure/close regression pass; the application
scenario also joins all host cleanup normally.

Clean archive extraction verifies all 28 source files and exact captured reviewer
request bytes. Private configuration validation and root shell inspection pass.
Direct check/stage/recheck commands run from the configured sample-project working
directory, preserve the original tutorial and treat shell-like proposal text
literally. Canonical checks cover 361 pages and 39 methods. Astro reports no errors;
the 83 website tests pass after updating the two catalog-count expectations, with
the affected tests rerun. The build checks 165 HTML pages and 859 files, and all
39 Chromium application/source-reader cases pass, including the lab's complete
inline source without JavaScript. Desktop/mobile inspection follows a lesson into
the application, opens five companion types and uses wrapping without document
overflow. Search passes 33/33 queries across 154 indexed pages and 2,542 anchors.
These are local results. No live provider, physical TUI, Linux, power-loss,
mid-write recovery or hosted publication is claimed.
