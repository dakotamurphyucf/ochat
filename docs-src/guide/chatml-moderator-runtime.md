# ChatML moderator runtime guide

This guide describes the current ChatML moderator runtime as exposed by the
repository today.

It is the consolidated entry point for:

- builtin surfaces,
- helper modules,
- event types,
- task/effect execution,
- history and overlay semantics,
- runtime requests and internal events,
- UI notifications and approval suspension,
- persistence boundaries,
- and the host/runtime split used by `chat_tui` and other embedders.

When this guide and the implementation disagree, the implementation is
authoritative.

## Runtime layers

The moderator stack is split into four public layers.

### `Chatml_runtime`

The generic host-side runtime for compiled ChatML scripts.

It is responsible for:

- compiling a script against a selected builtin surface,
- instantiating a per-session runtime,
- invoking `on_event`,
- interpreting returned task values,
- buffering transactional effects,
- committing or rolling back state and effects,
- exposing queued internal events,
- and, when the UI surface is installed, exposing suspended approval state.

### `Chat_response.Chatml_moderation`

The stable vocabulary shared across embedders.

It defines:

- event types,
- context, item, tool-call, and tool-result shapes,
- overlay operations,
- tool moderation results,
- runtime requests,
- UI notifications,
- and the host capability bundle used by the shared moderation manager.

### `Chat_response.Chatml_moderator`

The durable moderator boundary.

It is responsible for:

- caching compiled moderator artifacts,
- creating or restoring a moderator session,
- applying the durable overlay,
- exposing effective items and effective history,
- replaying queued internal events,
- extracting a persisted snapshot,
- and exposing the live `pending_ui_request` / `resume_ui_request` boundary.

### `Chat_response.Chatml_turn_driver`

The turn-boundary helper layer.

It is responsible for:

- preparing model inputs at turn start,
- applying pre-tool and post-tool moderation,
- handling turn-end runtime requests,
- and exposing the same pending-approval boundary to turn-oriented hosts.

It does not own host scheduling policy. Idle drains, wakeups, submit routing,
and automatic follow-up policy remain host-owned behavior.

## Builtin surfaces

ChatML uses composable builtin surfaces rather than one hard-coded builtin
universe.

### `Chatml_builtin_surface.core_surface`

The language core surface. It includes:

- global helpers such as `print`, `to_string`, and `length`,
- core modules such as `Task`, `String`, `Array`, `Json`, `Option`, and
  `Hashtbl`,
- and builtin type aliases such as `json`.

### `Chatml_builtin_surface.moderator_surface`

The default surface for moderator scripts.

It extends `core_surface` with moderator-oriented modules and aliases,
including:

- helper modules:
  - `Item`
  - `Tool_call`
  - `Context`
- effectful capability modules:
  - `Log`
  - `Turn`
  - `Tool`
  - `Model`
  - `Process`
  - `Schedule`
  - `Runtime`
- structural type aliases:
  - `item`
  - `tool_desc`
  - `tool_call`
  - `tool_result`
  - `context`

### `Chatml_builtin_surface.ui_moderator_surface`

The optional UI-capable surface for interactive hosts.

It extends `moderator_surface` with:

- `Ui`
- `Approval`

This surface split is deliberate:

- non-UI embedders can keep the default moderator surface unchanged;
- scripts compiled against `ui_moderator_surface` do not silently run on hosts
  that never opted into UI-only capabilities.

## Script entrypoints

The moderator runtime expects two convention-based entrypoints:

```ocaml
let initial_state = ...

let on_event : context -> state -> event -> state task =
  fun ctx st ev -> ...
```

`initial_state` provides the durable committed script state for a fresh
session.

`on_event` is invoked for each moderator event. It must return a task value.

## Event model

The script-visible moderation event constructors are:

```ocaml
type event =
  [ `Session_start
  | `Session_resume
  | `Turn_start
  | `Item_appended(item)
  | `Pre_tool_call(tool_call)
  | `Post_tool_response(tool_result)
  | `Turn_end
  ]
```

Hosts may also deliver arbitrary internal events by reinjecting raw ChatML
values such as:

```ocaml
`Queued("later")
`Tick
`Model_job_succeeded(job_id, recipe, result)
```

Those arrive through the `Internal_event` host path and are matched directly by
the script as ordinary variants.

### Context

Scripts receive a `context` record with:

```ocaml
type context =
  { session_id : string
  ; now_ms : int
  ; phase : string
  ; items : item array
  ; available_tools : tool_desc array
  ; session_meta : json
  }
```

`context.phase` is a string view of the current host phase, using names such
as:

- `session_start`
- `session_resume`
- `turn_start`
- `message_appended`
- `pre_tool_call`
- `post_tool_response`
- `turn_end`
- `internal_event`

The event constructor and the phase string are related but not identical. For
example, the event constructor is `` `Item_appended(item) ``, while
`context.phase` for that handler is `"message_appended"`.

## Helper modules on the moderator surface

### `Item`

`Item` provides constructors and accessors for common transcript items.

Useful helpers include:

- `Item.id`
- `Item.value`
- `Item.kind`
- `Item.role`
- `Item.text_parts`
- `Item.text`
- `Item.input_text_message`
- `Item.output_text_message`
- `Item.user_text`
- `Item.assistant_text`
- `Item.system_text`
- `Item.notice`
- `Item.is_user`
- `Item.is_assistant`
- `Item.is_system`
- `Item.is_tool_call`
- `Item.is_tool_result`

### `Tool_call`

`Tool_call` provides payload inspection helpers:

- `Tool_call.arg`
- `Tool_call.arg_string`
- `Tool_call.arg_bool`
- `Tool_call.arg_array`
- `Tool_call.is_named`
- `Tool_call.is_one_of`

### `Context`

`Context` provides selectors over projected history and tool availability:

- `Context.last_item`
- `Context.last_user_item`
- `Context.last_assistant_item`
- `Context.last_system_item`
- `Context.last_tool_call`
- `Context.last_tool_result`
- `Context.find_item`
- `Context.items_since_last_user_turn`
- `Context.items_since_last_assistant_turn`
- `Context.items_by_role`
- `Context.find_tool`
- `Context.has_tool`

## Task values and task syntax

Moderator scripts are effectful through task values.

The core task combinators are:

```ocaml
Task.pure  : 'a -> 'a task
Task.bind  : 'a task -> ('a -> 'b task) -> 'b task
Task.map   : 'a task -> ('a -> 'b) -> 'b task
Task.fail  : string -> 'a task
Task.catch : 'a task -> (string -> 'a task) -> 'a task
```

ChatML also supports task let-syntax:

```ocaml
let* x = task_value in ...
let+ x = task_value in ...
```

These desugar to `Task.bind` and `Task.map`.

## Effectful capability modules

### `Log`

Diagnostic logging:

```ocaml
Log.debug : string -> unit task
Log.info  : string -> unit task
Log.warn  : string -> unit task
Log.error : string -> unit task
```

### `Turn`

Transactional overlay operations:

```ocaml
Turn.prepend_system  : string -> unit task
Turn.append_item     : item -> unit task
Turn.replace_item    : string -> item -> unit task
Turn.delete_item     : string -> unit task
Turn.replace_or_append : string option -> item -> unit task
Turn.append_notice   : string -> unit task
Turn.halt            : string -> unit task
```

The older `append_message`, `replace_message`, and `delete_message` names are
accepted as aliases, but the item-oriented names are preferred.

### `Tool`

Tool moderation and synchronous/asynchronous host tool execution:

```ocaml
Tool.approve      : unit -> unit task
Tool.reject       : string -> unit task
Tool.rewrite_args : json -> unit task
Tool.redirect     : string -> json -> unit task
Tool.call         : string -> json -> [ `Ok(json) | `Error(string) ] task
Tool.spawn        : string -> json -> string task
```

### `Model`

Host-managed model recipes:

```ocaml
Model.call       : string -> json -> [ `Ok(json) | `Refused(string) | `Error(string) ] task
Model.spawn      : string -> json -> string task
Model.call_text  : string -> string -> string task
Model.call_json  : string -> json -> [ `Ok(json) | `Refused(string) | `Error(string) ] task
Model.spawn_text : string -> string -> string task
```

Recipe names are host-defined. They are not raw provider/model identifiers.

### `Process`

Host-managed subprocess execution:

```ocaml
Process.run : string -> string array -> string task
```

### `Schedule`

Host-managed delayed reinjection:

```ocaml
Schedule.after_ms : int -> 'e -> string task
Schedule.cancel   : string -> unit task
```

### `Runtime`

Runtime control and internal-event emission:

```ocaml
Runtime.emit               : 'e -> unit task
Runtime.request_compaction : unit -> unit task
Runtime.request_turn       : unit -> unit task
Runtime.end_session        : string -> unit task
```

## History and overlay model

There are three related transcript views:

- **canonical history**: the durable transcript stored by the host;
- **effective history**: canonical history projected through the durable
  moderator overlay;
- **visible history**: the host/UI presentation derived from effective
  history.

`Turn.*` operations do not directly rewrite canonical history. They update a
durable overlay that can:

- prepend synthetic system items,
- append synthetic items,
- replace projected items by id,
- delete projected items by id,
- halt the session with a reason.

Before the next model request, the host computes effective history by applying
that overlay to the projected canonical history.

## Safe-point and runtime semantics

The main safe boundaries are:

- session start and resume,
- turn start,
- pre-tool moderation,
- post-tool handling,
- turn end,
- idle internal-event drain,
- host-visible-history refresh.

The turn driver owns request-preparation and tool/turn boundary helpers.

The host owns:

- when idle drains happen,
- when follow-up turns are started,
- how queued async completions wake a session,
- how visible UI state is refreshed,
- and how user input is routed while an operation is already in flight.

### Runtime requests

Committed moderator execution may surface:

- `Request_compaction`
- `Request_turn`
- `End_session(reason)`

Hosts decide how to honor those requests. The shared runtime policy collapses
multiple requests rather than treating them as independent side effects.

### Internal events

`Runtime.emit(event)` buffers a ChatML value transactionally.

On successful commit:

- the buffered event is appended to the session queue;
- later, the host may replay queued events FIFO through the internal-event
  path.

If the task fails, buffered emitted events are discarded.

## Commit, rollback, and suspension

Moderator execution is transactional.

On successful task completion:

- the new state becomes committed,
- transactional local effects become visible in execution order,
- buffered emitted events are enqueued,
- and a pending end-session request halts the session.

On failure:

- the previous committed state stays in place,
- buffered local effects are discarded,
- buffered emitted events are discarded,
- and buffered runtime requests are discarded.

## UI-only capabilities

The UI surface adds two modules:

```ocaml
module Ui : sig
  val notify : string -> unit task
end

module Approval : sig
  val ask_text : string -> string task
  val ask_choice : string -> string array -> string task
end
```

### `Ui.notify`

`Ui.notify` emits a host-local notice.

It is:

- visible to the current embedding,
- not persisted as canonical history,
- not appended to transcript items automatically,
- and not part of the default non-UI surface.

### `Approval.ask_text` and `Approval.ask_choice`

Approval requests pause the current live script execution and later resume that
same execution with a validated response.

They do not:

- append a fake canonical user item automatically,
- create a second internal-event state machine,
- or alter the durable snapshot format.

The host-visible boundary is:

```ocaml
type pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of { prompt : string; choices : string array }

val pending_ui_request : session -> pending_ui_request option
val resume_ui_request : session -> response:string -> (unit, string) result
```

At the moderator wrapper level the same concept is exposed through
`Chat_response.Chatml_moderator` and `Chat_response.Chatml_turn_driver`.

### Suspension semantics

While approval is pending:

- ordinary `handle_event` progression is blocked,
- queued internal events may accumulate but are not drained,
- buffered local effects remain uncommitted,
- buffered emitted events remain uncommitted,
- `current_state` remains the last committed state,
- and `resume_ui_request` is the only supported continuation path.

Only one approval prompt may be pending per session at a time.

### Validation rules

- `Approval.ask_text` trims surrounding whitespace before returning the value
  to the script.
- An empty trimmed text response does not resume the script.
- `Approval.ask_choice` trims surrounding whitespace and exact-matches the
  normalized value against one of the declared choices.
- `Approval.ask_choice` with an empty `choices` array fails immediately.

### Persistence boundary

Pending approvals are live-session-only.

They are not stored in the moderator snapshot, and restoring a partially
suspended approval is unsupported.

## `chat_tui` behavior

`chat_tui` is the reference interactive embedding for the UI surface.

It reuses:

- the ordinary composer/input UI,
- the normal submit path,
- the moderator wakeup and idle-drain controller,
- and visible-history refresh built from effective history.

While approval is pending:

- the prompt is rendered through the existing input UI,
- submit is repurposed to approval submission,
- idle moderator drains for that session do not proceed,
- automatic follow-up turns for that session do not start,
- and no fake canonical user item is appended automatically.

## Choosing the right document

Use this guide for the consolidated runtime picture.

Use these focused documents when you need more detail on one topic:

- [ChatML safe-point and effective-history semantics](../chatml-safe-point-and-effective-history.md)
- [ChatML host session-controller contract](../chatml-host-session-controller-contract.md)
- [ChatML async completion lifecycle](../chatml-async-completion-lifecycle.md)
- [ChatML budget policy](../chatml-budget-policy.md)
- [ChatML UI host capabilities](../chatml-ui-host-capabilities.md)
- [ChatML language specification](chatml-language-spec.md)
