# ChatML extensibility examples

These bundles show the same composition sources used by Ochat's offline tests.
They use ordinary declarations, scoped tools and host services. All runnable
agent roots select `gpt-6-astra`. A live conversation uses your provider; Dune's
offline providers and clocks prove integration without model API calls.

Build the complete set with:

```sh
dune build @test/chatml_extensibility_fixtures/bundle
```

Copy the desired complete directory from
`_build/default/test/chatml_extensibility_fixtures/` out of the build tree.
Generated files there include companion scripts, report inputs and composed
moderators. Do not copy only a root ChatMD file or launch from a directory that
omits its companion imports/schemas. Each bundle's README gives its host and
launch instructions. Keep daemon data, configuration and credentials outside
the agent's data workspace.

| Example | Bundle | Purpose and implementation |
| --- | --- | --- |
| X01 | [Reports](x01-report/README.md) | One-off deterministic aggregation through an existing scoped file tool |
| X02 | [Reviews](x02-review/README.md) | Moderator-handled tools with session-owned review state |
| X03 | [Background shell](x03-background-shell/README.md) | Prompt acknowledgement, owned shell work and later notification |
| X04 | [Standalone tools](x04-standalone/README.md) | Reusable synchronous and asynchronous script tools |
| X05 | [Persisted specialists](x05-child-session/README.md) | Generated children and authored optional/persistent agent tools |
| X06 | [Response watcher](x06-response-watcher/README.md) | Timers and polling composed in ChatML, with receipt/cursor targets |
| X07 | [Helper session tools](x07-helper-session/bundle/README.md) | Scoped shell helper plus the same watcher; complete helper/native-polling roots |
| X08 | [External completion](x08-external-completion/README.md) | Authenticated producer data, deduplication and later conversation delivery |
| X09 | [Authoring discovery](x09-authoring-discovery/README.md) | Automatic primer, retrieval, validation and execution |
| X10 | [Authoring after compaction](x10-authoring-compaction/README.md) | Fresh retrieval after reference text leaves the conversation |
| X11 | [Diagnostic repair](x11-authoring-repair/README.md) | Invalid candidates, topic-linked repairs and non-executing validation |

The [authoring lab](authoring-lab/README.md) provides five captured task-specific
packages and curated source copies to inspect through `read_file`. It uses the
existing `authoring_help` and package-file configuration paths. Guides include
installed prerequisites and precise entrypoint/failure guidance, with explicit
authored-convention provenance.

Native `agent_*` tools are conveniences over the shared scoped session services.
X07 exercises the optional external helper through the normal shell boundary;
it is not an executable required by Ochat. Its native-watcher variant swaps only
the polling backend, while creation still uses the helper. Neither watcher uses
a native child-response push subscription.

Local sessions support process-bound script/moderator work. Persisted delegation
requires an admitted durable service; local discovery must report an unavailable
capability rather than promise it. Retained data and receipts do not guarantee
arbitrary external effects execute exactly once. X03 deliberately uses a trusted
`direct_unsafe` shell fixture and makes no OS confinement claim; X07's helper
uses a required sandbox and scoped grants.

The composition suite loads the report, shell, standalone, ingress, discovery,
compaction and repair sources directly. Dedicated review, watcher, helper and
authored-session fixtures check their corresponding exact sources; helper and
authored roots include real daemon restart coverage. Server configuration checks
validate packaged paths and imports. More focused lifecycle, permission,
cross-store recovery and protocol suites supply the wider qualification matrix.
See each README for the particular evidence instead of treating a bundle's
presence as proof of every host or failure mode.
