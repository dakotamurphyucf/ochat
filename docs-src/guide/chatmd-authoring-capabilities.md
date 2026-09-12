# Authoring ChatMD capabilities and resource content

A user-authored root ChatMD file can declare tool implementations and resource
bindings. An agent-generated child submitted to `agent_create` cannot: it selects
existing delegated bindings with `<tool type="inherited" name="..."/>`. The
following examples explain root declarations, not a route around that restriction.
Reading this guide does not install a tool or authorize a resource.

Use `reference.tools` for the invoking agent's actual selected names, descriptions,
input schemas and result conventions. A declaration names requested capability;
the host must still capture sources, resolve dependencies and authorize runtime
resources before exposing its implementation. `ochat_validate`'s generated
target deliberately rejects the root-only forms shown below.

## Builtins and named file roots

Builtin declarations select registered functions by exact name. They do not make
an arbitrary OCaml function available. `read_file` additionally supports named
roots; its compatibility spelling `get_contents` exposes the same `read_file`
function. A complete declaration is:

```xml authoring=root
<developer>Read only the repository evidence needed for the task.</developer>
<tool name="read_file" description="Prefer focused source excerpts.">
  <read id="project" path="${workspace}" description="Selected workspace"/>
  <read id="docs" path="${workspace}/docs-src" description="Project documentation"/>
</tool>
```

Each `read` needs a nonempty unique `id` and a `path`; `description` is optional.
Only `read` children are allowed. Paths must resolve to existing directories at
runtime. The generated description retains the root usage contract and adds the
author's instructions. A self-closing `read_file` has one `cwd` root at
`${tool_dir}`. Relative root paths also resolve against `${tool_dir}`.

A call accepts `file`, optional `root`, nonnegative `offset` and nonnegative
`line_count`. With `root`, `file` is relative to that root. Without it, relative
paths use the tool directory; absolute paths remain subject to root confinement.
Canonicalization checks symlink and parent traversal escapes. The target must be
an existing regular text file. Choosing `/` as a root requests broad read access;
declaring a narrower root does not grant access outside it.

Host path variables are `${workspace}`, `${tool_dir}`, `${prompt_dir}`,
`${source_dir}`, `${session_dir}`, `${cache_dir}` and `${home}`. Unknown variables
fail. `${source_dir}` identifies the declaring source's directory, while
`${prompt_dir}` identifies the root prompt. In captured native sessions these
directories belong to the captured source tree. `${workspace}` selects the
workspace; do not substitute the agent's guessed current directory for it.

## Shell tools and moderator process binding

Shell tools select a named runtime. A narrow complete example is:

```xml authoring=root
<shell_access id="readonly" extends="builtin:workspace-readonly@1"/>
<tool name="git_status" type="shell" mode="fixed" runtime="readonly">
  <command program="git"><arg value="status"/><arg value="--short"/></command>
  <arguments mode="none"/>
</tool>
```

The runtime controls command resolution, effect/capability checks, sandboxing,
allow/ask/deny policy, approval, interceptors, resource limits and sanitized
output. A builtin profile is a versioned declaration template, not proof that
the requested runtime is authorized or supported on this host.

Long-form tools require `name`, `type="shell"`, `mode` and `runtime`. Choose:

| Mode | Meaning |
|---|---|
| `fixed` | Author-selected program and fixed argv, with explicitly configured model arguments |
| `structured` | Model-selected program plus a string-array argv |
| `chain` | A conservative parsed command/pipeline grammar |
| `raw` | Script text interpreted by an explicitly selected shell |
| `script` | A captured, verified script file plus literal arguments |

Fixed and structured arguments are literal argv; spaces, semicolons and shell
substitution syntax do not become shell control flow. Unsupported structured or
chain syntax fails instead of switching to raw execution. The compact legacy
`<tool name="search" command="rg"/>` is desugared through the same shell runtime
path. It is not an ungoverned process escape hatch.

Common options are `stdin` and `rationale` (`none`, `optional`, `required`),
`result` (`combined`, `stdout`, `structured`), `stream` (`finalized`, `sanitized`)
and `nonzero` (`result`, `error`). The generated description and exact published
schema explain the selected mode's arguments and output. `description` appends
task-specific guidance without replacing those usage rules.

`<moderator_runtime shell_runtime="readonly"/>` binds the ordinary moderator's
`Process.run` to that same runtime. Without a binding the operation is unavailable;
the delegated moderator surface does not expose `Process` at all. Shell-specific
ChatML script kinds use their own constrained matcher/reviewer/interceptor/effect/
audit surfaces, not the ordinary moderator's operations. They are distinct from
standalone `kind="tool"` and lifecycle `kind="moderator" api="extensibility-v1"`.

For the full declaration vocabulary, see the shared
[shell runtime](../overview/chatmd-shell-runtime.md),
[shell tool](../overview/chatmd-shell-tools.md) and
[shell extension](chatmd-shell-extensions.md) references. Host authorization and
runtime support still decide whether a syntactically valid declaration can run.

## Agent-backed tools

An authored agent tool chooses another ChatMD definition. This example assumes
the companion `reviewer.chatmd` is captured alongside the root:

```xml authoring=root
<developer>Delegate focused review work to the specialist.</developer>
<tool name="review" agent="reviewer.chatmd" local persistence="optional"
      description="Review supplied evidence and identify missing checks."/>
```

The `local` flag selects a local source reference; otherwise `agent` denotes a
remote source. Local relative references resolve from their declaring source.
Ordinary one-off agent tools take `{ "input": "..." }`, start a separate
conversation and return an answer. They do not copy the parent's conversation.
An authored definition chooses its own tools subject to the host's policy; do
not confuse that route with an agent-generated child's inherited-only definition.

Persistence is opt-in: `one_off` (the default), `persistent`, or `optional`.
Only agent declarations accept this attribute. Persistent calls produce a session
reference and use the same management tools as generated children. The exact
optional `mode`, `instance_id`, retry and follow-up contract is in
`chatmd.definitions`; using persistence requires the corresponding qualified
runtime and authority. A name or session ID is not management permission.

## MCP catalog bindings

A root definition can mount a selected MCP catalog over HTTP or stdio. This is a
nonexecuting declaration example; `example.invalid` is not a service to contact:

```xml authoring=root
<tool mcp_server="https://example.invalid/mcp" name="lookup"
      includes="lookup,search" strict client_id_env="EXAMPLE_MCP_CLIENT_ID"
      client_secret_env="EXAMPLE_MCP_CLIENT_SECRET"/>
```

`name` selects one remote tool and wins over `include`/`includes`. Otherwise a
present `include` wins over `includes`, even when empty; a comma-separated
selection is trimmed.
With no name or nonempty selection, the declaration imports the discovered
catalog. It does not namespace or rename remote functions. The `strict` flag
controls the wrapper's strict parameter handling. Remote descriptions/schemas
come from the catalog; the declaration's `description` does not replace them.

`client_id_env` and `client_secret_env` name host environment variables. Nonempty
values are supplied as URI query parameters during connection. Write variable
names in declarations, not secret values. The parser does not fetch the catalog;
runtime preparation connects/discovers and constructs wrappers. Child inheritance
keeps the parent's actual connected binding and captured schema identity, rather
than allowing a new endpoint, credentials or catalog under an existing tool name.

Catalog caches belong to connected declarations, not globally to a URI, and
expire after five minutes on access. Active advertised schemas are not hot
reloaded: recreate the runtime to refresh them. Invocation also checks that the
captured selected catalog entry still matches. Notification-based invalidation
currently has competing consumers and can miss a list-change event; do not rely
on immediate discovery of server changes. Catalog inspection grants no authority
to execute a newly discovered tool.

## Resource-bearing messages and stored traces

Plain messages are the appropriate starting point for generated children.
User-authored roots additionally support resource-bearing message helpers:

```xml authoring=root
<user>Inspect this supplied material:
<doc src="report.txt" local/>
<img src="diagram.png" local/>
<agent src="reviewer.chatmd" local>Summarize the supplied report.</agent>
</user>
```

`doc` loads document text; `local` selects a local file, otherwise the source is
remote. `strip` and `markdown` request HTML transformations, with `strip` taking
precedence. A local `img` becomes image content; an inline `agent` runs its
definition with the nested content as input and substitutes an answer. These
helpers can cause file/network/model work during materialization. Their presence
in a parsed document does not prove that the resources exist or are authorized.
They are not a substitute for explicit tools inside a generated child. RAW text
containing these tags is literal and triggers no helper.

Stored `tool_call` and `tool_response` entries correlate through `tool_call_id`;
calls also name `function_name`. Reasoning/history IDs and tool results are
runtime trace data, not a way to fabricate successful tool calls in a generated
definition. Generated admission rejects such history and implicit resource
loading. See `chatmd.definitions` for valid initial messages and captured imports.

For example, this is stored trace syntax, not a new child's initial conversation:

```xml authoring=root
<tool_call function_name="read_file" tool_call_id="call_example">
RAW|{"file":"report.txt"}|RAW
</tool_call>
<tool_response tool_call_id="call_example">RAW|Example stored result.|RAW</tool_response>
```

The complete examples above are checked with the real root parser and again at
the generated-child admission boundary. That check does not instantiate shell
runtimes, connect to MCP, load message resources, or run an agent.
