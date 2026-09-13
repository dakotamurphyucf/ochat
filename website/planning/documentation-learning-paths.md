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

## Remaining learning work

The first connected scripting checkpoints are now available: **T11** summarizes
Lantern reports with `run_chatml`; **T12** exposes the same maintained source as
`summarize_checks`. Each bundle is independently inspectable and downloadable.
The program takes a JSON array; the named tool takes an object containing a
`files` array and validates its output. Neither example runs the checks reported
in its supplied data.

`config/tutorial-paths.mjs` now owns explicit primary learning paths. Every
registered tutorial must appear once. Stable IDs identify lessons; they are not
a global prerequisite sequence. Previous/next links stay within a path and return
to the curriculum at its end. Add new IDs after T12, and update the manifest,
path membership, overview, catalog and approved sources together.

Offline composition tests execute these exact sources through registered tools
in a real session with deterministic provider responses. They check aggregation,
schema rejection before reads, inherited file boundaries and malformed-report
failure without partial results. Captured bundle checks separately verify source
closure and missing companions. This does not establish live-provider behavior.

The remaining work extends the foundation through independently runnable checkpoints
for useful shell commands, custom guardrails/review, persistent and generated
specialists, stateful moderation and background
results. Those converge on three complete multi-file applications: a guarded
engineering assistant, a persistent review team and a living documentation lab.

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

Custom shell reviewers, persistent and generated specialist lessons, stateful and
background workflows, the three complete applications, and the final presentation
and reader-journey audit remain separate unfinished work in the full plan.

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
