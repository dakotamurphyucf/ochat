# Inspecting moderator items, context and tool calls

`Item`, `Context` and `Tool_call` provide immediate data construction and inspection
on `moderator_v1` and `delegated_moderator_v1`. They are absent from one-off and
standalone-tool surfaces. They do not create task effects, edit history, approve a
tool, run a model or grant a capability. Use the separate `Turn`/`Tool` operations
for admitted effects; see [moderator execution](chatml-moderator-runtime.md).

The examples are complete moderators. The documentation gate compiles and executes
each on both moderator surfaces, initializes its state, and delivers `Session_start`
with an empty synthetic context and no installed host operations. Each example
builds sample data and returns a JSON report as its new state. This qualifies data
helpers and entrypoint execution, not daemon history or tool execution.

## The data contracts

These structural aliases are distinct from the standalone tool's `tool_context`:

| Alias | Fields |
|---|---|
| `item` | `id: string`, `value: json` |
| `tool_desc` | `name: string`, `description: string`, `input_schema: json` |
| `tool_call` | `id: string`, `name: string`, `args: json` |
| `tool_result` | `call_id: string`, `name: string`, `result: json` |
| `context` | `session_id: string`, `now_ms: int`, `phase: string`, `items: item array`, `available_tools: tool_desc array`, `session_meta: json` |

The runtime supplies the actual context and event payload. `context.items` is the
current projected conversation view. It is not the entire canonical journal, and
selectors do not fetch omitted or compacted history. The phase string is not the
event constructor: for example, `Item_appended(item)` uses `"message_appended"`.
The arrays and nested JSON are borrowed values. Treat runtime snapshots as input;
mutating them is not an authorized history edit or a registry change.

`Pre_tool_call(call)` carries `tool_call`; `Post_tool_response(response)` carries
`tool_result`. A `tool_result` is a response-event record, not the `tool_outcome`
variant returned by a standalone handler. `Tool_invoked` has its own versioned
invocation record; do not substitute the simpler `tool_call` schema. Obtain exact
event and alias schemes from `reference.signatures` on the selected surface.

## Item constructors, roles and text

| Call | Meaning |
|---|---|
| `Item.create(id, value)` | Wrap an ID and raw JSON value without normalizing or validating it as a provider message. |
| `Item.id(item)` | Outer item ID. |
| `Item.value(item)` | Wrapped JSON value, without copying. |
| `Item.kind(item)` | String `type` field as an option. |
| `Item.role(item)` | String `role` field as an option. |
| `Item.text_parts(item)` | Array of recognized string text fields from content or output. |
| `Item.text(item)` | First recognized text part as an option, not all parts joined. |
| `Item.input_text_message(id, role, text)` | Input-text message. A case-insensitive `system` role is normalized to `developer`. Other role strings are retained. |
| `Item.output_text_message(id, text)` | Completed assistant output-text message. |
| `Item.user_text(id, text)` | Input-text message with role `user`. |
| `Item.assistant_text(id, text)` | Same assistant message construction as `output_text_message`. |
| `Item.system_text(id, text)` | Input-text message with role `developer`; the compatibility name remains. |
| `Item.notice(id, text)` | Instruction notice with role `developer`. Construction alone does not append or deliver it. |
| `Item.is_user(item)` | Whether the role is exactly `user`. |
| `Item.is_assistant(item)` | Whether the role is exactly `assistant`. |
| `Item.is_system(item)` | Whether the role is `system` or `developer`. |
| `Item.is_tool_call(item)` | Whether `type` is `function_call` or `custom_tool_call`. |
| `Item.is_tool_result(item)` | Whether `type` is `function_call_output` or `custom_tool_call_output`. |

Typed field inspection returns `None` for a missing or nonstring role/type field.
Predicates return false when their expected field is absent or does not match.
Raw `create` values are not rewritten: a legacy `system` item keeps that role.
Caller-supplied IDs are data, not newly allocated session IDs or an authority grant.

Text extraction first looks for `content`. If it exists, only an array is scanned,
collecting string `text` fields in order and ignoring other parts. An existing
nonarray `content` gives no text and prevents fallback to `output`. When content
is absent, a string `output` is one text part; an object output can supply an array
of `parts` with string text fields. This is not an OCR or general rich-content
renderer, and it does not stringify arbitrary JSON output.

<!-- ochat-authoring-example: {"id":"moderator-data.items","surface":"moderator_v1","also_run":["delegated_moderator_v1"],"result":{"id":"raw","kind":"message","raw_role":"system","text_parts":["first","second"],"first":"first","roles":["developer","assistant","user","assistant","developer","developer"],"predicates":[true,true,true,true,true],"output":"tool text","blocked_fallback":true,"wrapped_role":"system"}} -->
```ocaml
let initial_state : json = `Null
let on_event ctx state event =
  match event with
  | `Session_start ->
    let raw : item = Item.create("raw", Json.parse("{\"type\":\"message\",\"role\":\"system\",\"content\":[{\"text\":\"first\"},{\"image\":\"omitted\"},{\"text\":\"second\"}]}")) in
    let helpers = [
      Item.input_text_message("i", "SYSTEM", "instruction"),
      Item.output_text_message("o", "output"),
      Item.user_text("u", "input"),
      Item.assistant_text("a", "answer"),
      Item.system_text("s", "instruction"),
      Item.notice("n", "notice")
    ] in
    let call = Item.create("call", Json.parse("{\"type\":\"custom_tool_call\"}")) in
    let output = Item.create("result", Json.parse("{\"type\":\"function_call_output\",\"output\":\"tool text\"}")) in
    let blocked = Item.create("blocked", Json.parse("{\"content\":null,\"output\":\"ignored\"}")) in
    Task.pure(`Object([
      {key = "id"; value = `String(Item.id(raw))},
      {key = "kind"; value = `String(Option.get_or(Item.kind(raw), "missing"))},
      {key = "raw_role"; value = `String(Option.get_or(Item.role(raw), "missing"))},
      {key = "text_parts"; value = `Array(Array.map(Item.text_parts(raw), fun text -> `String(text)))},
      {key = "first"; value = `String(Option.get_or(Item.text(raw), "missing"))},
      {key = "roles"; value = `Array(Array.map(helpers, fun item -> `String(Option.get_or(Item.role(item), "missing"))))},
      {key = "predicates"; value = `Array([
        `Bool(Item.is_user(helpers[2])), `Bool(Item.is_assistant(helpers[3])),
        `Bool(Item.is_system(raw)), `Bool(Item.is_tool_call(call)), `Bool(Item.is_tool_result(output))])},
      {key = "output"; value = `String(Option.get_or(Item.text(output), "missing"))},
      {key = "blocked_fallback"; value = `Bool(Option.is_none(Item.text(blocked)))},
      {key = "wrapped_role"; value = Option.get_or(Json.get_field(Item.value(raw), "role"), `Null)}
    ]))
  | _ -> Task.pure(state)
```

## Select the current conversation view

| Call | Meaning |
|---|---|
| `Context.last_item(ctx)` | Last projected item, or `None` for an empty view. |
| `Context.last_user_item(ctx)` | Last item with role `user`. |
| `Context.last_assistant_item(ctx)` | Last item with role `assistant`. |
| `Context.last_system_item(ctx)` | Last item with role `system` or `developer`. |
| `Context.last_tool_call(ctx)` | Last function/custom tool-call item. |
| `Context.last_tool_result(ctx)` | Last function/custom tool-output item. |
| `Context.find_item(ctx, id)` | First item whose outer ID matches exactly. |
| `Context.items_since_last_user_turn(ctx)` | New outer array from the last user item through the end, including that user item. If absent, copy the entire view. |
| `Context.items_since_last_assistant_turn(ctx)` | Same inclusive suffix rule for the last assistant item. |
| `Context.items_by_role(ctx, role)` | New outer array of exact role matches in view order. This does not normalize `system` to `developer`. |
| `Context.find_tool(ctx, name)` | First exact name match in the supplied descriptor array, as an option. |
| `Context.has_tool(ctx, name)` | Whether the supplied descriptor array has that exact name. |

Last-item selectors return `None` when no item matches. Suffix helpers use item
positions, not submission IDs or persisted turn boundaries. Their new outer arrays
still share nested values. Tool discovery reads the snapshot only: it is not an
authorization check and does not invoke or install a tool. The runtime rechecks
actual authority when effects execute.

<!-- ochat-authoring-example: {"id":"moderator-data.context","surface":"moderator_v1","also_run":["delegated_moderator_v1"],"result":{"last":"a2","user":"u2","assistant":"a2","system":"d","call":"c","result":"r","found":"u1","user_suffix":["u2","a2"],"assistant_suffix":["a2"],"users":["u1","u2"],"legacy_role_count":0,"tool":"echo","available":true,"wrong_case":false,"empty":true,"no_user_suffix":["a"]}} -->
```ocaml
let initial_state : json = `Null
let id optional = match optional with
  | `None -> `Null
  | `Some(item) -> `String(Item.id(item))
let ids : item array -> json = fun items ->
  `Array(Array.map(items, fun item -> `String(Item.id(item))))
let on_event ctx state event =
  match event with
  | `Session_start ->
    let descriptor : tool_desc = {name = "echo"; description = "Echo input"; input_schema = Json.parse("{\"type\":\"object\"}")} in
    let view : context = {ctx with
      items = [Item.system_text("d", "instructions"), Item.user_text("u1", "first"),
        Item.assistant_text("a1", "answer"),
        Item.create("c", Json.parse("{\"type\":\"function_call\"}")),
        Item.create("r", Json.parse("{\"type\":\"custom_tool_call_output\"}")),
        Item.user_text("u2", "second"), Item.assistant_text("a2", "answer")];
      available_tools = [descriptor]} in
    let no_user : context = {view with items = [Item.assistant_text("a", "answer")]} in
    let empty : context = {view with items = []} in
    let tool = match Context.find_tool(view, "echo") with
      | `None -> "missing"
      | `Some(descriptor) -> descriptor.name
    in
    Task.pure(`Object([
      {key = "last"; value = id(Context.last_item(view))},
      {key = "user"; value = id(Context.last_user_item(view))},
      {key = "assistant"; value = id(Context.last_assistant_item(view))},
      {key = "system"; value = id(Context.last_system_item(view))},
      {key = "call"; value = id(Context.last_tool_call(view))},
      {key = "result"; value = id(Context.last_tool_result(view))},
      {key = "found"; value = id(Context.find_item(view, "u1"))},
      {key = "user_suffix"; value = ids(Context.items_since_last_user_turn(view))},
      {key = "assistant_suffix"; value = ids(Context.items_since_last_assistant_turn(view))},
      {key = "users"; value = ids(Context.items_by_role(view, "user"))},
      {key = "legacy_role_count"; value = Json.parse(to_string(Array.length(Context.items_by_role(view, "system"))))},
      {key = "tool"; value = `String(tool)},
      {key = "available"; value = `Bool(Context.has_tool(view, "echo"))},
      {key = "wrong_case"; value = `Bool(Context.has_tool(view, "Echo"))},
      {key = "empty"; value = `Bool(Option.is_none(Context.last_item(empty)))},
      {key = "no_user_suffix"; value = ids(Context.items_since_last_user_turn(no_user))}
    ]))
  | _ -> Task.pure(state)
```

## Inspect proposed tool arguments

| Call | Meaning |
|---|---|
| `Tool_call.arg(call, key)` | First JSON object argument with that key, as an option. |
| `Tool_call.arg_string(call, key)` | That field only if it contains a JSON string. |
| `Tool_call.arg_bool(call, key)` | That field only if it contains a JSON boolean. |
| `Tool_call.arg_array(call, key)` | That field only if it contains a JSON array; its payload is shared. |
| `Tool_call.is_named(call, name)` | Exact case-sensitive name comparison. |
| `Tool_call.is_one_of(call, names)` | Whether any name matches exactly; no patterns or namespace expansion. |

Missing arguments, nonobject arguments and incorrect JSON kinds yield `None`.
Explicit null yields `Some(Null)` through `arg`; typed accessors do not coerce.
These helpers do not enforce the tool's schema or permissions. A real moderator
must use the admitted rewrite/reject operations to change a proposed call; merely
constructing a different record does not replace the runtime's pending invocation.

<!-- ochat-authoring-example: {"id":"moderator-data.tool-call","surface":"moderator_v1","also_run":["delegated_moderator_v1"],"result":{"call_id":"c","name":"read_file","path":"notes.txt","dry_run":true,"values":2,"null":true,"missing":true,"wrong_kind":true,"named":true,"one_of":true,"wrong_case":false}} -->
```ocaml
let initial_state : json = `Null
let on_event ctx state event =
  match event with
  | `Session_start ->
    let call : tool_call = {id = "c"; name = "read_file";
      args = Json.parse("{\"path\":\"notes.txt\",\"dry_run\":true,\"values\":[1,2],\"nullable\":null}")} in
    let count = match Tool_call.arg_array(call, "values") with
      | `None -> 0
      | `Some(values) -> Array.length(values)
    in
    let explicit_null = match Tool_call.arg(call, "nullable") with
      | `None -> false
      | `Some(value) -> String.equal(Json.tag(value), "Null")
    in
    let response : tool_result = {call_id = call.id; name = call.name; result = `Object([
      {key = "call_id"; value = `String(call.id)},
      {key = "name"; value = `String(call.name)},
      {key = "path"; value = `String(Option.get_or(Tool_call.arg_string(call, "path"), "missing"))},
      {key = "dry_run"; value = `Bool(Option.get_or(Tool_call.arg_bool(call, "dry_run"), false))},
      {key = "values"; value = Json.parse(to_string(count))},
      {key = "null"; value = `Bool(explicit_null)},
      {key = "missing"; value = `Bool(Option.is_none(Tool_call.arg(call, "absent")))},
      {key = "wrong_kind"; value = `Bool(Option.is_none(Tool_call.arg_bool(call, "path")))},
      {key = "named"; value = `Bool(Tool_call.is_named(call, "read_file"))},
      {key = "one_of"; value = `Bool(Tool_call.is_one_of(call, ["echo", "read_file"]))},
      {key = "wrong_case"; value = `Bool(Tool_call.is_named(call, "Read_file"))}
    ])} in
    Task.pure(response.result)
  | _ -> Task.pure(state)
```
