# Create a task-specific specialist

An authored specialist has a role you chose ahead of time. A generated specialist
lets the parent choose instructions for the current task, capture its own ChatMD
definition, and keep the resulting conversation for follow-up.

In this Lantern lesson, the parent creates a reviewer focused on verification
guidance. The child receives the existing project file tool. It can choose its
instructions, model, and reasoning settings, but cannot broaden that tool's file
roots or invent a shell capability.

## Start with the complete bundle

Use the **Lantern generated specialist** download below. It includes both parents,
the shared sample project, a private Unix daemon configuration, and readable
generated-source examples. You can also use the unchanged bundle from
[specialist conversations](persistent-specialist.md).

Follow that lesson's [durable host setup](persistent-specialist.md#start-a-durable-local-host),
including provider access to `gpt-6-astra`. With the daemon running, select this
prompt from the extracted directory:

```sh
chat-tui --no-config --connect "unix://$PWD/agent.sock" \
  --new-daemon-session --prompt generated --workspace lantern
```

`generated.chatmd` declares `agent_create` and the five management tools, plus a
`read_file` binding restricted to the sample project. The runtime's default
authoring policy supplies the authoring-context and validation helpers and a short
primer. It does not give the child extra tools automatically.

The example permission profile allows the declared tools so the child can read
unattended. Configuration, prompt definitions and `private-data/` remain outside
the workspace. Neither parent nor child has a file writer or shell tool.

## Give the model the authoring context

Send the parent:

> Create a persistent reviewer specifically for the verification guidance missing
> from Lantern's setup tutorial. First consult the child-agent authoring docs and
> validate the captured definition. Give it only the existing project file reader.
> Have it inspect the setup, reference and bundled check report, then retain it
> for a follow-up about concrete wording.

The parent can request the feature overview and required contracts with:

```json
{
  "version": 1,
  "operation": "prepare",
  "task": "child_agent",
  "query": null,
  "topic_id": null,
  "features": null,
  "cursor": null,
  "max_tokens": null
}
```

This is the strict `ochat_authoring_context` schema; unused fields remain `null`.
Follow any returned continuation before relying on incomplete guidance. The
helper reads installed documentation, not the network. See the
[authoring-context guide](../guide/authoring-context-tool.md#one-strict-request-schema)
for topic lookup, search, and continuation calls.

## Capture a definition with selected tools

Open `generated/reviewer.chatmd` and `generated/tools.chatmd` in the source reader.
The candidate selects a model, states the reviewer task, and imports this tool
reference:

```xml
<tool type="inherited" name="read_file"/>
```

This means “use the parent's already admitted implementation.” It does not build
a new file reader. The `project` root stays the same sample directory even if the
child's instructions ask for more. Generated children cannot redefine that binding
with a new `<read path="/">` declaration.

`generated/create.json` shows the complete captured request. It includes:

| Field | Meaning |
| --- | --- |
| `root_file` | `reviewer.chatmd`, a name inside the captured bundle. |
| `sources` | The exact text of the root and imported `tools.chatmd`. |
| `tools` | The selected existing binding: `read_file`. |
| `start_immediately` | `true`, so the new child can receive work. |
| `lifetime` | `owned`, tying the child's active lifetime to its parent. |
| `idempotency_key` | The stable key for this one intended creation. |

The ordinary files and JSON request show the same source text. The runtime uses
the captured bytes supplied in the request, not those names as paths into the
host's filesystem. Editing the example files afterward does not update an already
created child's captured definition.

These files are a concrete candidate to inspect and adapt. The live parent may
generate different wording for your task. It should validate and create its own
same captured bytes, rather than claiming to have used the example file merely
because that file is visible on the website.

## Validate, then create and start

`generated/validate.json` is the corresponding `ochat_validate` request. It uses
`target: "generated_chatmd"` and the same root, sources and selected tools, leaving
out creation-only fields. Validation checks definitions, captured dependencies,
configuration and allowed declarations without starting a child or evaluating its
initializers. Read diagnostics and referenced topics before retrying a correction.

After validation succeeds, call `agent_create` with the full creation request.
Retain the returned `session_id`, definition revision and exact creation request/key.
Retry the same key with the same bytes after an uncertain result; changing the
payload conflicts, while a new key may create a second child.

Creation defaults to stopped when `start_immediately` is omitted. This example
sets it explicitly. `agent_send` does not start or resume a stopped session, and
retrying an old creation key does not restart one that has since stopped.

## Send work and read the particular response

Once the child is ready, the parent calls `agent_send` with its returned session ID,
this task, and a fresh message key:

> Read docs/setup.md, docs/reference.md and expected-report.json with root project.
> Explain the missing verification guidance and propose a concise section that
> accurately describes the expected failing check. Cite your evidence.

The send result is a submission receipt, not the answer. Retain `receipt_id` and
use it with `agent_wait`. A bounded timeout means keep checking or do other work;
it does not cancel the child. Inspect terminal status rather than assuming every
terminal receipt succeeded.

Use `agent_read` with the session ID and the same receipt filter. Read subsequent
pages with `next_cursor`. Ordinary records contain complete assistant output;
large records can be split into ordered fragments. The
[output contract](../guide/chatml-authoring-children.md#read-output-and-recover-cursors)
describes reconstruction and recovery from stale cursors.

Send a second message to that same child:

> Refine your earlier proposal so a first-time reader understands why exit status 1
> is expected here. State which wording is proposed and what still needs validation.

Give this message a new key and follow its new receipt. Reusing the first message's
key with different text is a conflict. The retained conversation lets the child
develop its earlier work; the parent should not create a fresh child for this
follow-up. The exact language of a live model response will vary.

## See how authority stays bounded

Ask the parent to validate a candidate that replaces the inherited reference with
a new native file-tool declaration pointing outside the sample project. Validation
should reject the generated declaration. Correct it back to `type="inherited"`;
do not add broader tools to work around the error.

A valid inherited declaration also cannot bypass the file tool at execution time.
Reading `../server.sexp` through `root: "project"` crosses its configured root and
must fail. Generated instructions and a higher reasoning setting do not change
that implementation. The selected child has no `agent_create` binding, so it
cannot start another generation of specialists through that native tool.

Management tools check the actual parent-child relationship. Session IDs and
cursors are references, not transferable authority. Current restrictions are
checked again when work runs; successful earlier validation is not an execution
grant that survives later revocation.

## Finish the investigation

Ask the parent to stop the child after collecting its response. `agent_stop` with
`mode: "graceful"` requests completion of admitted work; `cancel` requests
interruption. Keep the stop key for identical retries and inspect stop progress.
Neither operation deletes the stored conversation. A stopped child can remain
readable, but sending more work requires a separately authorized host restart.

Stop owned children before stopping the parent, then quit the TUI and stop the
daemon as described in the [persistent lesson](persistent-specialist.md#stop-retain-and-troubleshoot).
The private store stays in the extracted directory. The
[subagent decision guide](../guide/subagents.md) compares authored and generated
delegation, and [ChatML workflows](../chatml/README.md) explains
how to coordinate their lifecycle with scripts, background jobs, and notifications.
