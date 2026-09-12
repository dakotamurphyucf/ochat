# X10: author again after compaction

Build `@test/chatml_extensibility_fixtures/x10-authoring-compaction/bundle`, copy
the directory out of `_build`, and run
`chat-tui --no-config --local -file agent.chatmd`. Interactive use selects
`gpt-6-astra` and uses your provider. For retained daemon sessions, register this
root and its companion schema in your configured prompt directory.

Ask the agent to prepare `background_workflow`, then retrieve
`runtime.jobs.shell-example` for that task. A small `max_tokens` value such as
1500 deliberately exercises pagination. Keep following each returned cursor,
increasing the budget to at least `minimum_next_tokens` when needed. Ask it to
explain the exact event and outcome contracts before authoring a coordinator.

In the TUI, leave insert mode with Escape, enter `:compact`, and press Enter.
After compaction finishes, ask for another authoring step using the same contracts.
The reference text may have been
removed: the runtime's rediscovery pointer is a reason to query again, not proof
that the complete package is still present. Query the topic again, validate the
new source, and execute only admitted work. The `fixture_work` tool is an inert
standalone echo of an object, suitable for testing composition without a shell.
The installed shell-coordinator example describes how a real shell tool can
occupy that same selected-tool boundary; it does not install such a tool here.

This agent intentionally uses manual policy and explicitly declares both helpers.
Compaction must not silently switch it to automatic policy. After runtime or
tool-selection changes, discover the current target instead of reusing an old
continuation or assuming persisted delegation is available locally.

`test/chatml_composition/authoring_compaction_tests.ml` loads these exact files,
uses the actual offline compactor, retrieves before and after compaction,
validates the retrieved coordinator and executes through the public script tool.
The child and transient-host companion tests cover narrowed authority and absent
persistence. Run `dune build @test/chatml_composition/runtest`; no model API is used.
