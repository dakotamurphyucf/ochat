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

The next work extends the foundation through independently runnable checkpoints
for useful shell commands, custom guardrails/review, persistent and generated
specialists, one-off programs, reusable tools, stateful moderation and background
results. Those converge on three complete multi-file applications: a guarded
engineering assistant, a persistent review team and a living documentation lab.

Each application must include its root, imports, agents, scripts, schemas and
sample data in the existing website source reader, with exact host setup and
honest execution evidence. Generated children select inherited tools; response
notifications use the supported watcher/polling pattern. These are documentation
compositions of existing runtime features, not permission to invent broader
authority or unsupported session APIs.
