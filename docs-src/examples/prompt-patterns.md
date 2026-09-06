# Prompt patterns: minimal, editing, and moderation

These longer examples complement the [local TUI walkthrough](../agent-server/tutorials/local-tui.md).
Run commands from the Ochat checkout with dependencies and provider credentials
configured. Create `prompts/` and `.chatmd/` first; the commands below assume those
directories exist. Use a disposable project when testing editing tools. Live
model calls incur charges. These examples were moved from the detailed README;
the deprecated MCP example is retained only for existing integrations.

```sh
mkdir -p prompts .chatmd
```

## Example: minimal prompt

Create `prompts/hello.md`:

```xml
<config model="gpt-5.6-sol" reasoning_effort="medium"/>

<developer>
You are a helpful assistant.
</developer>

<user>
Say hello and explain what Ochat is in one sentence.
</user>
```

Run it:

```sh
dune exec bin/chat_tui.exe -- --no-config --local -file prompts/hello.md
```

Or:

```sh
dune exec bin/main.exe -- chat-completion \
  -prompt-file prompts/hello.md \
  -output-file .chatmd/hello-run.md
```

---

## Example: interactive refactor agent

Turn a `.md` file into a refactoring bot that reads files and applies patches under your control.

Use a disposable checkout first. The instruction to wait for confirmation is
model guidance, not an authorization mechanism. Enforce required approval through
host tool policy. A root-scoped `read_file` declaration does not also constrain
`read_dir` or `apply_patch`; each tool's authority must be reviewed separately.

Create `prompts/refactor.md`:

```xml
<config model="gpt-5.6-sol" reasoning_effort="medium"/>

<tool name="read_dir"/>
<tool name="read_file"/>
<tool name="apply_patch"/>

<developer>
You are a careful refactoring assistant. Work in small, reversible steps.
Before calling apply_patch, explain the change you want to make and wait for
confirmation from the user.
</developer>

<user>
We are in a codebase. Look under ./lib, find a small improvement and
propose a patch.
</user>
```

Open it in the TUI:

```sh
dune exec bin/chat_tui.exe -- --no-config --local -file prompts/refactor.md
```

From there you can ask the assistant to rename a function, extract a helper, or
update documentation. It will use `read_dir` and `read_file` to inspect the
code, then generate `apply_patch` diffs and apply them.

Press `Ctrl-G` while tools are active to open the Agent live view without
pausing the Chat stream or tool execution.

---

## Example: publish a prompt as an MCP tool

This is **deprecated prompt-serving compatibility**, not the new agent server.
It does not deprecate MCP-backed tools declared inside ChatMD.

For an existing stdio MCP integration, put the desired prompt in a dedicated
directory as `hello.chatmd` (the scanner uses the `.chatmd` extension), then
configure the MCP client to launch:

```sh
MCP_PROMPTS_DIR=/absolute/path/to/compatibility-prompts dune exec bin/mcp_server.exe
```

The client performs the MCP handshake, discovers tools, and invokes the exported
agent. This process remains attached to the client's stdio; do not type ordinary
chat messages into that stream. HTTP mode has separate authentication/setup
requirements—an unauthenticated curl request is not a complete example.
See the [compatibility reference](../bin/mcp_server.doc.md).

For new hosted agents, use [the daemon walkthrough](../agent-server/tutorials/unix-daemon.md).
Its session/attachment protocol is Ochat-specific rather than MCP.

---

## Example: moderated ChatMD prompt

You can attach one ChatML moderator script to a prompt and keep it host-managed.

Create `prompts/review.chatmd`:

```md
<config model="gpt-5.6-sol" reasoning_effort="medium"/>

<tool name="read_file"/>
<tool name="apply_patch"/>

<script language="chatml" kind="moderator" id="main">

type state =
  { reminded : bool }

type event =
  [ `Session_start
  | `Session_resume
  | `Turn_start
  | `Item_appended(item)
  | `Pre_tool_call(tool_call)
  | `Post_tool_response(tool_result)
  | `Turn_end
  ]

let initial_state : state =
  { reminded = false }

let on_event : context -> state -> event -> state task =
  fun ctx st ev ->
    match ev with
    | `Session_start ->
      let* () =
        Turn.prepend_system(
          "Before calling apply_patch, explain the change briefly."
        )
      in
      Task.pure(st)
    | _ ->
      Task.pure(st)

</script>

<developer>
You are a careful code assistant.
</developer>

<user>
Review lib/example.ml and suggest a small safe improvement.
</user>
```

This example prepends a developer instruction at session start. The compatibility
name `Turn.prepend_system` is retained, but it now creates a developer message.
It asks the model to explain changes; that instruction is not itself an enforced
permission gate. Configure the host's tool policy as well.

Run it in the TUI or CLI:

```sh
dune exec bin/chat_tui.exe -- --no-config --local -file prompts/review.chatmd
```

```sh
dune exec bin/main.exe -- chat-completion \
  -prompt-file prompts/review.chatmd \
  -output-file .chatmd/review-run.chatmd
```

For a richer end-to-end example, see:

- [General Assistant – agent workflow](../guide/general-agent-workflow.md)
- [prompt-examples](../../prompt-examples/readme.md)

## Writing moderator scripts with `Item.*`

By default, moderator scripts get the `Item`, `Tool_call`, and `Context`
helper modules on the installed moderator surface, so common transcript,
tool-call, and context queries do not need raw record or JSON plumbing.

Moderator scripts receive `ctx.items`, where each item has the shape:

```ocaml
type item =
  { id : string
  ; value : json
  }
```

The `Item` module provides helpers so scripts do not need to hand-author raw
JSON for common cases:

- `Item.id(item)` returns the stable item id used by overlay operations
- `Item.value(item)` returns the underlying structured JSON payload
- `Item.kind(item)` reads the serialized item `"type"` field when present
- `Item.role(item)` extracts a message role when the item has one
- `Item.text_parts(item)` collects text fragments from common message-like items
- `Item.text(item)` returns the first text fragment when one is present
- `Item.input_text_message(id, role, text)` builds a structured input message
- `Item.output_text_message(id, text)` builds a structured assistant message
- `Item.user_text(id, text)`, `Item.assistant_text(id, text)`,
  `Item.system_text(id, text)`, and `Item.notice(id, text)` are convenience
  constructors over those message shapes
- `Item.is_user(item)`, `Item.is_assistant(item)`, `Item.is_system(item)`,
  `Item.is_tool_call(item)`, and `Item.is_tool_result(item)` are predicate
  helpers over the serialized role/kind fields
- `Item.create(id, value)` wraps arbitrary structured JSON as an item

The default moderator surface also exposes pure inspector helpers for tool
calls and the current moderation context:

- `Tool_call.arg(call, name)` returns the raw JSON argument when it is present
- `Tool_call.arg_string(call, name)`, `Tool_call.arg_bool(call, name)`, and
  `Tool_call.arg_array(call, name)` return `Option.none()` when the argument is
  missing or has the wrong JSON shape
- `Tool_call.is_named(call, name)` and `Tool_call.is_one_of(call, names)` match
  against the serialized tool name using exact string equality
- `Context.last_item(ctx)`, `Context.last_user_item(ctx)`,
  `Context.last_assistant_item(ctx)`, `Context.last_system_item(ctx)`,
  `Context.last_tool_call(ctx)`, and `Context.last_tool_result(ctx)` return the
  last matching item in `ctx.items`
- `Context.find_item(ctx, id)` uses exact item-id equality
- `Context.items_since_last_user_turn(ctx)` and
  `Context.items_since_last_assistant_turn(ctx)` return the suffix beginning at
  the matching boundary item, or the full item list when no such boundary
  exists
- `Context.items_by_role(ctx, role)` uses exact role-string equality
- `Context.find_tool(ctx, name)` and `Context.has_tool(ctx, name)` inspect
  `ctx.available_tools` using exact tool-name equality

The following is a ChatML handler fragment, not standalone OCaml; combine it
with the moderator state/event declarations above:

```ocaml
let first_text : string array -> string =
  fun parts ->
    if Array.length(parts) == 0 then "" else Array.get(parts, 0)

let on_event : context -> state -> event -> state task =
  fun ctx st ev ->
    match ev with
    | `Item_appended(item) ->
      let summary =
        Item.id(item)
        ++ ":"
        ++ Option.get_or(Item.role(item), "unknown")
        ++ ":"
        ++ first_text(Item.text_parts(item))
      in
      Task.bind(Turn.append_item(Item.output_text_message("summary", summary)), fun ignored ->
      Task.pure(st))
    | _ ->
      Task.pure(st)
```

Prefer `Turn.append_item`, `Turn.replace_item`, and `Turn.delete_item`.
The older `append_message`, `replace_message`, and `delete_message` names are
still accepted as aliases.

Compatibility role helpers such as `Item.system_text` and notice constructors
now create developer messages; the corresponding compatibility predicates
recognize old system and developer entries. Raw item payloads and existing
history are not rewritten wholesale.

Additional script helpers:

- `Turn.replace_or_append(target_id_opt, item)` replaces when
  `target_id_opt` is `Option.some(id)` and appends when it is
  `Option.none()`
- `Turn.append_notice(text)` appends a synthetic developer notice item using a
  stable `system:`-prefixed id derived from the notice text
- `Model.call_text(recipe, text)` is shorthand for
  ``Model.call(recipe, `String(text))``
- `Model.call_json(recipe, payload)` is a named alias for
  `Model.call(recipe, payload)` when the payload is already structured JSON
- `Model.spawn_text(recipe, text)` is shorthand for
  ``Model.spawn(recipe, `String(text))``


For runtime ownership, steering, wakeups, and recovery, see the
[ChatML introduction](../chatml/README.md) and [TUI guide](../guide/chat_tui.md).
Return to the [examples index](README.md).
