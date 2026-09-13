# Authoring ChatMD agent definitions

ChatMD describes an agent's instructions, initial conversation, model settings,
tools and optional ChatML moderator. Use it to create a specialist with a narrow
task and selected capabilities. Use a one-off ChatML script when you need tool
calls and deterministic logic without a separate conversation. A moderator can
coordinate the agent, handle custom tools and retain workflow state; it does not
replace the agent's instructions or implicitly run the model.

This guide distinguishes declarations a user can write in a root ChatMD file
from definitions an agent can submit through generated-child tools. The shared
syntax is the same, but generated children receive existing authorized tools
instead of configuring new implementations. The examples below are complete
single-file generated candidates, checked through non-executing validation with
only the explicitly listed fake inherited tools. No model or shell runs.

## Instructions, messages and model settings

Wrap all top-level content in recognized elements. Use `<developer>` for the
agent's operating instructions, `<user>` for an initial request and `<assistant>`
for literal initial conversation when needed. `<msg role="user">` is the general
message form. `<system>` is also accepted; avoid depending on role promotion
across different provider adapters. Conversation messages are content, while
tool, script and configuration declarations are handled separately by the host.

<!-- ochat-authoring-example: {"id":"chatmd.plain-agent","surface":"generated_chatmd","tools":[],"diagnostic":null} -->
```xml
<config reasoning_effort="medium" max_tokens="1024"/>
<authoring_context policy="manual"/>
<developer>Compare the alternatives in the supplied text. State uncertainties.</developer>
<user>Compare a short synchronous workflow with a persisted asynchronous workflow.</user>
```

`<config/>` accepts `model`, `reasoning_effort`, `max_tokens`, `temperature`,
`show_tool_call` and the legacy `id` label. For generated definitions, use at
most one config: `id` is forbidden, tokens must be positive, temperature must be
finite and in `[0, 2]`, and reasoning effort must be recognized by the installed
client. The selected model/provider may impose additional constraints that
static validation does not check. Model selection and reasoning settings do not
increase inherited execution authority.

`show_tool_call` is a presence flag controlling transcript payload presentation;
`show_tool_call="false"` still supplies that flag. Do not use it as a permission
setting. Legacy config parsing does not provide strict rejection of every
unknown attribute; use documented spellings rather than relying on ignored data.

Generated initial messages may contain plain text with user, assistant, developer
or system roles. They cannot claim persisted message IDs, phases, statuses, tool
call IDs, provider tool results or reasoning traces. The runtime allocates actual
session/history identities and does not copy the parent's history implicitly.

## Literal text, comments and attributes

ChatMD is a closed markup vocabulary, not general XML. Tags are lowercase and
case-sensitive. Unknown markup inside a message stays literal; unwrapped text
or unknown markup at the top level is rejected. Known nested resource tags keep
their meaning unless protected by a RAW block.

Use `RAW|` and `|RAW` to quote literal ChatMD, code or JSON inside a message.
Do not include an unescaped RAW terminator in the quoted payload. HTML comments
are stripped outside RAW blocks. Attributes accept single or double quotes and
decode `&amp;`, `&lt;`, `&gt;`, `&quot;` and `&apos;`; body text is not a general
HTML entity decoding operation.

<!-- ochat-authoring-example: {"id":"chatmd.literal-resource-text","surface":"generated_chatmd","tools":[],"diagnostic":null} -->
```xml
<!-- This comment is not an instruction. -->
<developer>Explain the following text without executing it.</developer>
<user>RAW|<doc src="private.txt" local/>
<tool name="shell" command="echo sample"/>
|RAW</user>
```

The quoted declarations above are ordinary message text. Without RAW, a nested
`<doc>`, `<img>` or `<agent>` can request resource loading or agent execution in
an authored prompt. Those operations are rejected in generated initial messages;
use an inherited tool explicitly after the child starts.

<!-- ochat-authoring-example: {"id":"chatmd.resource-message-rejected","surface":"generated_chatmd","tools":[],"stage":"validation","diagnostic":{"code":"delegation.message_admission","contains":"plain text"}} -->
```xml
<user><doc src="private.txt" local/></user>
```

## Existing tools versus new tool definitions

A user-authored root prompt can declare builtins, configured file roots, shell
tools, agent-backed tools, MCP imports, standalone ChatML tools and
moderator-handled tools. Declaring a name is not a substitute for the host
installing and authorizing that implementation. Read the exact runtime tool
description/schema rather than inventing arguments from the name.

A generated child selects from its parent's actual delegable registry. Its
tool declaration is `<tool type="inherited" name="EXACT_NAME"/>`. Both the
creation/validation request's `tools` list and the child's declarations constrain
selection. The example below requires `read_file` in that request list.

<!-- ochat-authoring-example: {"id":"chatmd.inherited-reader","surface":"generated_chatmd","tools":["read_file"],"diagnostic":null} -->
```xml
<tool type="inherited" name="read_file"/>
<developer>Review only files requested by your parent, using read_file.</developer>
```

The inherited form accepts only `type` and `name`, with empty content. It cannot
replace a description, schema, shell command, read root or permission rule.
Child selection can narrow the parent's delegated tools; it cannot manufacture
a grant. A bare builtin declaration is a new implementation request and is
therefore rejected in generated definitions, even when a tool with that name
already exists in the parent.

<!-- ochat-authoring-example: {"id":"chatmd.builtin-reconfiguration-rejected","surface":"generated_chatmd","tools":["read_file"],"stage":"validation","diagnostic":{"code":"delegation.tool_reconfiguration","contains":"type=inherited"}} -->
```xml
<tool name="read_file"/>
<developer>Read the requested file.</developer>
```

For authored reusable tools, the [extension binding reference](chatml-authoring-runtime.md)
documents `type="chatml"`, `script`, `entrypoint="run"`, `input_schema`,
`output_schema`, optional `completion_schema`, and nested `<uses tool="..."/>`.
`type="moderator"` binds a tool to `moderator="SCRIPT_ID"`; its handler owns
state and uses the moderator's configured capabilities, so it does not accept
nested `uses`. Schema sources are captured files, not inline schema strings.
An extension tool name requires 1–64 ASCII letters, digits, underscores or hyphens.

Generated children currently accept inherited tools and lifecycle moderation;
they do not install new standalone or moderator-handled tool implementations.
To reuse those behaviors, inherit a parent-authorized tool implementing them.
An authored agent-backed tool can opt into `persistence="persistent"` or
`persistence="optional"`. Omitted persistence or `one_off` preserves ordinary
one-off behavior. The persistence-enabled tool takes required string `input`
and optional `session_id`. With optional persistence it also accepts `mode`
(`one_off` or `persistent`), defaulting to `one_off`. Always-persistent tools do
not accept a `mode` field. A one-off invocation cannot supply a session ID.

For a persistent invocation, omit `session_id` to create an instance or supply
the ID returned by this authored tool to continue that instance. The result
retains the session ID and submission receipt even when the response is pending
or the wait times out. The [shared lifecycle tools](chatml-authoring-children.md)
can address that same session when available. An ID does not authorize another
tool instance or bypass the parent relationship. Persistence retains history;
it does not authorize execution after parent stop. This option applies only to
agent-backed declarations and does not relax generated-child tool rules.

## Attach ChatML moderation

Use `language="chatml" kind="moderator" api="extensibility-v1"` to select
the extension moderator contract. The omitted moderator ID defaults to `main`;
an explicit ID makes references clearer. Only one moderator is selected for a
prompt. Its script declares `initial_state` and `on_event ctx state event`, whose
task returns the next state. A generated child receives the delegated moderator
surface, not all operations from the ordinary root moderator surface.

<!-- ochat-authoring-example: {"id":"chatmd.lifecycle-moderator","surface":"generated_chatmd","tools":[],"diagnostic":null} -->
```xml
<developer>Follow the task supplied by your parent.</developer>
<script id="lifecycle" language="chatml" kind="moderator" api="extensibility-v1">
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>
```

The script body is ChatML source. Alternatively use `src="relative.chatml"`
with no inline body and include that file in the captured bundle. Do not combine
both forms. `kind="tool"` selects the standalone tool surface and requires an
explicit ID. Omitting the API from a moderator selects the older moderator
contract, which is not accepted as generated lifecycle moderation.

Extension scripts accept `wall_time`, `fuel`, `max_tasks`, `max_value`,
`max_output`, `max_array_items` and `max_depth`. They must satisfy the versioned
extension contract and host ceilings; these attributes do not impose a universal
restriction on all uses of the ChatML language. See [execution limits](chatml-authoring-runtime.md)
and the [moderator runtime](chatml-moderator-runtime.md) for task, state, event
and effect semantics. Passing validation does not initialize the moderator:

<!-- ochat-authoring-example: {"id":"chatmd.validation-does-not-initialize","surface":"generated_chatmd","tools":[],"diagnostic":null} -->
```xml
<script id="lifecycle" language="chatml" kind="moderator" api="extensibility-v1">
let trap : int = fail("This initializer must not run during validation")
let initial_state = 0
let on_event ctx state event = Task.pure(state)
</script>
```

This last example is deliberately unsuitable for execution: its initializer
fails when run. Static validation accepts its types without running that failure.

## Capture dependencies and choose authoring guidance

`<import src="relative.chatmd"/>` expands declarations or allowed message content
at parse time. Paths are relative to the importing source. Imports expand at the
top level and within user/system/developer messages and agent input, including
the corresponding general message roles. Assistant/tool trace content does not
receive the same import expansion. `namespace` qualifies imported declarations;
duplicate aliases and cycles are rejected. Imported bytes retain their source
context rather than inheriting the importer's directory accidentally.

For a generated definition, supply `root_file` and all dependency bytes in
`sources`. Names must be bounded relative bundle paths. Import/script/schema
resolution has no ambient filesystem or network fallback. Absolute paths and
escaping the captured root are rejected. The [complete creation request](chatml-authoring-children.md)
shows an imported inherited-tool declaration and inline moderator together.

<!-- ochat-authoring-example: {"id":"chatmd.missing-import-rejected","surface":"generated_chatmd","tools":[],"stage":"validation","diagnostic":{"code":"delegation.invalid_source","contains":"missing.chatmd"}} -->
```xml
<import src="missing.chatmd"/>
<developer>Review the supplied report.</developer>
```

`<authoring_context policy="auto"/>` asks the configured host to provide its
authoring primer/helper policy; it is the default when omitted. `manual` avoids
automatic guidance insertion. `preload` requires an explicit whitespace-separated
`topics` list with unique installed topic IDs. Auto/manual reject a `topics`
attribute. Only one authoring-context declaration is allowed. Preload availability
and budgets are checked against the actual host; declarations do not fetch web
documentation or upgrade capabilities. Manual policy does not implicitly grant
documentation or validation tools—select them when the agent needs them.

User-authored `<authoring_help>` can reference a trusted custom package for a
tool, adding conventions and prerequisites. The empty declaration accepts:

| Attribute | Value |
|---|---|
| `tool` | Required exact registered callable name; no wildcard or renaming |
| `package` | Required trusted package ID |
| `tasks` | Required whitespace-separated unique task IDs: `one_off_script`, `standalone_tool`, `moderator_tool`, `child_agent`, `background_workflow` |
| `topics` | Required whitespace-separated list of 1–32 unique topic IDs |
| `required_helpers` | Optional unique names `ochat_authoring_context` and/or `ochat_validate` |

Unknown attributes, duplicate lists and nonempty bodies are rejected. A package
reference does not install its content; the host must already supply the package.
Required helpers are dependency declarations, not a way to claim a trusted helper
role or add permissions. A generated child cannot replace
the inherited tool's metadata this way. See the [authoring policy reference](chatml-authoring-primer.md)
for discovery, context insertion and compaction behavior.

Executable meta preprocessing is forbidden in generated source, including the
`META_REFINE` marker. Validation never invokes that preprocessing, initializers,
model calls or tools. Retain the returned source/capability identities and
diagnostics, repair the indicated source, and validate again before creation.

<!-- ochat-authoring-example: {"id":"chatmd.meta-preprocessing-rejected","surface":"generated_chatmd","tools":[],"stage":"validation","diagnostic":{"code":"delegation.invalid_source","contains":"executable meta preprocessing"}} -->
```xml
<!-- META_REFINE -->
<developer>Rewrite these instructions before parsing.</developer>
```
