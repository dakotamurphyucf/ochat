# Chat_tui.Types — shared display and command types

Keep small shared types independent of the UI model and runtime. These are
rendering/adapter types, not the canonical session history or protocol schema.
The complete [interface](../../../lib/chat_tui/types.mli) defines exact fields.

## Shell UI commands and pages

Shell Security, Agent and Chat page state belongs to `Model`. Approval,
revocation, management and moderator-input reactions belong to
[Controller_types](controller_types.doc.md), not to this module's `cmd`.

## Chat transcript helpers

`role = string` and `message = role * string` hold displayed role/text.
Roles are not validated by this alias; rendering also uses developer,
reasoning and tool labels. They do not replace identity-bearing
`History_entry.t` or `Projected_message.t`.

```ocaml
let seed : Chat_tui.Types.message list =
  [ "developer", "You are a helpful assistant."; "user", "Hello!" ]
```

## Streaming buffers

Streaming buffers belong to [Model](model.doc.md), not a public
`Types.msg_buffer` record. Deltas update the display incrementally before the
connection closes. Stable projected IDs identify rows; current array indexes
are layout positions, not durable identities.

## Commands

`Persist_session`, `Start_streaming`, and `Cancel_streaming` each carry a
`unit -> unit` thunk. [Cmd](cmd.doc.md) executes these with host-owned lifetime
and error handling. This compatibility abstraction is distinct from
`Agent_protocol.Command` and `Controller_types.reaction`.

## Patches

`Model.apply_patch` applies these in-place on the UI owner:

| Patch | Effect |
|---|---|
| Ensure_buffer | Ensure an ID-keyed streaming display buffer |
| Append_text | Append delta text and reflect it in the displayed row |
| Set_function_name | Associate tool name with buffer ID |
| Associate_tool_call | Correlate streaming item ID with tool call ID |
| Set_function_output | Record tool output text |
| Update_reasoning_idx | Record reasoning-summary correlation |
| Add_user_message | Add a display row only; **does not** append canonical history |
| Add_placeholder_message | Add a transient UI-only notice |

Callers maintaining canonical state must append an allocated history entry
separately; native clients instead receive authoritative server projections.
Do not persist a placeholder or send display-only live tool activity to a model.

## Tool-output classification

`Apply_patch`, `Read_file { path }`, `Read_directory { path }`, and
`Other { name }` select specialized display/highlighting. This metadata is
derived in the TUI and is not itself durable conversation state.

## Runtime settings

`settings` contains `parallel_tool_calls : bool`, default true through
`default_settings ()`. It is a legacy local execution setting; CLI flags
do not override native/daemon runtime policy.

## Known limitations

The UI model is mutable; these patches are not immutable-state transformations.
String roles and thunk commands do not enforce semantic authority, durability,
or cancellation. Keep those responsibilities in the host.
