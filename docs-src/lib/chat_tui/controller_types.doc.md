# Chat_tui.Controller_types

Share ordinary OCaml variant types between controller modules without cyclic
dependencies. These are not polymorphic variants or the agent wire protocol.

## Module purpose

`reaction` describes what the host must do after an input event.
`chat_destination` distinguishes `Earlier_conversation`,
`Search_result of Projected_message.Id.t`, and `Latest_conversation`.

## Reaction constructors

| Constructor | Host responsibility |
|---|---|
| Redraw | Present changed UI state |
| Refresh_messages | Rebuild effective projection after local canonical history change |
| Submit_input | Admit the draft through the selected host |
| Cancel_or_quit | Cancel active work or exit when idle; resolve pending permission appropriately |
| Compact_context | Request history compaction |
| Delete_history of History_entry.Id.t | Request authoritative deletion of a canonical occurrence and its matching tool pair |
| Quit | Close host/client resources and restore terminal |
| Chat_scrolled of bool | Redraw only when consumed scrolling changed the viewport |
| Prepare_chat_destination of chat_destination | Prepare an exact off-corridor destination asynchronously |
| Shell_approval_response of string * Approval_broker.ui_response | Resolve matching legacy shell approval |
| Shell_grant_revoke_requested of int * string | Start generation-tagged grant revocation |
| Shell_management_refresh_requested of int | Refresh generation-tagged security state |
| Moderator_input_response of string | Resolve current moderator interaction/agent permission choice |
| Unhandled | Ignore or pass to outer handling, including enabled typeahead admission |

The exact qualified payload types are in the
[interface](../../../lib/chat_tui/controller_types.mli).
`Types.cmd` is a separate thunk-carrying command type; do not confuse it with
controller reactions.

## Host integration

Legacy [App_reducer](app_reducer.doc.md) and native/daemon
[App](app.doc.md) own reaction handling. Every new constructor requires
deliberate host handling, not a catch-all redraw. Native history deletion routes
through actor authorization; destination preparation routes through
[Agent_history_layout](agent_history_layout.doc.md).

Controllers mutate local state only. Server-owned work requires an authorized
protocol request; a returned reaction by itself neither persists nor
authorizes a change.
