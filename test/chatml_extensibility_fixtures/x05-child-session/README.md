# X05: persisted generated and authored specialists

Build `@test/chatml_extensibility_fixtures/x05-child-session/bundle` and copy the
entire `_build/default/test/chatml_extensibility_fixtures/x05-child-session/`
directory to a new location. Keep `public/` as the workspace; the configuration,
prompt definitions, socket and private daemon data stay outside it.

Validate with `ochat-agent-server -config "$PWD/server.sexp" -validate-only`, then
start `ochat-agent-server -config "$PWD/server.sexp"`. From another terminal in
that directory, connect with
`chat-tui --no-config --connect "unix://$PWD/agent.sock" --new-daemon-session --prompt generated --workspace examples`.
Use `--prompt authored` for the optional/persistent named-tool variant. Both
select `gpt-6-astra`; interactive use requires your provider credentials and
normal tool approvals. There is no helper executable in this native bundle.

Ask the generated coordinator to review `report-a.json`, then give the same child
a second request for `report-b.json`. Keep its ID, creation key and each send's
receipt. Restart the daemon between exchanges to check retained source and
output. An owned child stopped during shutdown must be resumed through the
authorized host session-start operation; `agent_send` never starts a stopped
session implicitly. Read and stop preserve historical data.

For the authored prompt, ask `researcher` for a one-off count, then for a retained
session and another count using its ID. Start a second retained instance to see
independent moderator state. `reviewer` always uses persistence. The actual
`test/agent_server_authored_test.ml` fixture loads these parent/specialist files
and schemas and qualifies native calls, private state and daemon restart over a
Unix socket with offline providers. The wider authored fixture family qualifies
timeouts, approvals, concurrency and generic lifecycle interoperability.

## Captured child authoring request

`create.json` is the complete version-1 creation request displayed in the shared
[child authoring guide](../../../docs-src/guide/chatml-authoring-children.md).
It captures a root definition, an imported inherited `read_file` declaration and
a pass-through delegated lifecycle moderator. It requests immediate start and
owned lifetime, without selecting a provider model.

`test/agent_docs/docs_child_authoring.ml` checks exact displayed bytes, the real
creation decoder and generated-definition validation. Its capability runner is
unusable: these checks neither read files through the declared tool, evaluate
the moderator, create a child nor call a model. This fixture qualifies the
authoring request, not the full X05 restart/lifecycle composition.

An actual invocation requires a durable qualified host and a parent with a
delegable `read_file` binding. The source paths name captured files, not ambient
host files. Use a different creation key for each intended new child, retain the
same key and request for retries, and submit the report task through `agent_send`
after creation. The selected model and any approvals remain host policy decisions.

## Persisted lifecycle composition

`helper-child.chatmd` is the generated source used by the real confined helper
integration fixture (`test/agent_server_helper_test.ml`). Both parent and child
select `gpt-6-astra`; the child requests high reasoning and the parent leaves
reasoning at the provider default. The child inherits only
`read_file`, and counts pre-tool events in its lifecycle moderator's persisted state.
The fixture uses deterministic provider responses, not a paid model endpoint.

The fixture exercises create retries, scoped lifecycle operations, a denied private
file read, completed response receipts, cursor watching, and stop. After a daemon
restart it reads the retained response, resumes the same child, and completes a
second exchange that reads an allowed report. It checks the moderator source hash
and counter, response isolation between receipts, and a cursor spanning the new
output. The existing interruption/restart watcher cases follow this successful
exchange. Actual outbound provider model/reasoning serialization is independently
checked by `test/agent_server_e2e/scenarios/generated_provider_scenario.ml` against
an isolated loopback server.

Both helper-only and native-watcher modes pass this composition in the normal
Dune helper test target. The static `create.json` guide fixture remains a separate
non-executing authoring check. Broader public host/transport exposure has its own
qualification requirements.
