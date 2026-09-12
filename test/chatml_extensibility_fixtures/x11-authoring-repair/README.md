# X11: repair diagnostics without executing the candidate

Build `@test/chatml_extensibility_fixtures/x11-authoring-repair/bundle`, copy the
complete directory into a new workspace, and run
`chat-tui --no-config --local -file agent.chatmd`. The root selects `gpt-6-astra`.
Interactive conversations call your provider; the offline suite does not.

Ask the agent to validate the source in `invalid-call.chatml` as a
`one_off_script` with an empty tool selection. You can paste its one line into
the conversation; the agent's file tool intentionally reads only `reports/`.
The OCaml-style application must fail. Have the agent inspect the returned
topic IDs, retrieve the relevant reference using `ochat_authoring_context`,
correct the syntax and validate again. Only then ask it to run the corrected
source. Validation itself does not evaluate the program or its initializers.

Repeat with a missing record field, a closed variant match, a non-JSON result,
and the wrong standalone/moderator entrypoint. For a generated ChatMD candidate,
try an import outside its captured bundle or a new native tool declaration.
Those must be rejected; selecting a tool name is not permission to replace the
parent's implementation. A passing report cannot bypass later admission or file
rules. Even after successful validation, an attempt to read `../secret.json`
through the `reports` root must fail.

`test/chatml_composition/authoring_repair_tests.ml` loads this exact root and the
invalid-call source. Its scripted provider submits the full candidate matrix,
retrieves the linked installed examples, repairs all four targets, and executes
four programs through real tool paths. It checks inert validation, changed
source/selection identities, current authority denial and forged-receipt rejection.
Run `dune build @test/chatml_composition/runtest`. These are integration assertions,
not a claim that a model independently discovered the repairs.
