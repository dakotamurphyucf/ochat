# Task-specific authoring lab

This bundle publishes five version-1 authored packages for the X01–X11
compositions. Each package adds a flat, task-specific guide with exact entrypoints,
prerequisite installed topics and linked failure cases. It is labelled authored
conventions; it never replaces the audited language/runtime reference.

Build `dune build @test/chatml_extensibility_fixtures/authoring-lab/bundle` and copy
the complete built directory to a new location. The `public/` directory contains
curated source copies and reports generated from the maintained example files.
Keep configuration, package files, credentials and daemon data outside `public/`.
These source copies are for inspection; use each original complete bundle when
running its agent with all companion schemas and imports.

| Task | Root file | Captured package | Custom topic |
| --- | --- | --- | --- |
| `one_off_script` | `one-off.chatmd` | `one-off.json` | `custom.e10-one-off.guide` |
| `standalone_tool` | `standalone.chatmd` | `standalone.json` | `custom.e10-standalone.guide` |
| `moderator_tool` | `moderator.chatmd` | `moderator.json` | `custom.e10-moderator.guide` |
| `child_agent` | `children.chatmd` | `children.json` | `custom.e10-children.guide` |
| `background_workflow` | `background.chatmd` | `background.json` | `custom.e10-background.guide` |

For local authoring, run from `public/`:

```sh
chat-tui --no-config --local -file ../one-off.chatmd --authoring-package ../one-off.json
```

Select the corresponding root/package pair for another task. Each root imports
the same base and attaches one package to its existing `read_file` tool using
`authoring_help`; none replaces host metadata on reserved native authoring tools.
The configured package must accompany the declaration. `source_name` in the JSON
labels captured text; it does not load a Markdown file. Start a new host session
after changing package files to capture the revised text.

Ask the model to prepare the chosen task, read the returned custom guide and all
prerequisites, and inspect a relevant example through the `examples` read root.
For the first task, ask it to aggregate the two sample reports and validate the
source before running it. `prepare` incorporates the selected package; `topic`
can retrieve its custom topic directly. Follow `next_cursor` until complete and
retrieve missing content again after compaction. Other installed packages stay
hidden unless a selected tool binds their metadata.

For native child execution, use the included daemon configuration instead of a
transient local host. Change its prompt path from `./one-off.chatmd` to the root
you want, validate with `ochat-agent-server -config "$PWD/server.sexp" -validate-only`,
then launch the server with the same config. Connect from another terminal:

```sh
chat-tui --no-config --connect "unix://$PWD/agent.sock" --new-daemon-session --prompt lab --workspace examples
```

The daemon captures all five packages, while the selected root exposes only its
bound package. Approvals use the normal interactive profile. The local host
reports persisted delegation unavailable; installing documentation cannot enable
it. The authoring lab selects `gpt-6-astra`, and interactive use calls your provider.
Offline qualification is a separate deterministic provider transcript, not a
model-quality evaluation.
