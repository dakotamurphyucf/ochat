# Captured child authoring request

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
