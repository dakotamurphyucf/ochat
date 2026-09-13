# X09: discover, retrieve, validate, execute

Build `@test/chatml_extensibility_fixtures/x09-authoring-discovery/bundle` and copy
the built directory to a new workspace. Run
`chat-tui --no-config --local -file agent.chatmd`. The agent explicitly selects
`gpt-6-astra`; interactive use calls your configured provider.

This root leaves the authoring policy at its automatic default. `run_chatml`
causes the runtime to provide one shared primer and the reference/validation
helpers before the first authoring turn. No full reference is preloaded. Ask it
to read the primer's feature overview, prepare `one_off_script`, validate an identity
script, then execute it. Ask for the `standalone_tool`, `moderator_tool`, and
`child_agent` packages to compare entrypoints and capabilities. Follow `next_cursor`
with `continue` until `complete` is true; the last page does not contain earlier
pages. Retrieve again when their content is missing from the conversation.

Local sessions report persisted delegation as unavailable. For actual child
creation, register this root as a prompt in a configured daemon and connect to
that prompt; requesting a reference does not enable persistence or grant tools.
The root's child-creation tool is intentionally present so this distinction is
visible. Use X05 for a complete child lifecycle agent.

`test/chatml_composition/authoring_policy_integration_tests.ml` loads this exact
root and repeats automatic, manual, explicit-helper and preload policies. Its
ordinary-agent case confirms no unsolicited guidance. The broader authoring
composition tests retrieve complete packages and compile/execute examples through
native tools. Run `dune build @test/chatml_composition/runtest` for these offline
provider transcripts. They establish integration, not model authoring quality.
