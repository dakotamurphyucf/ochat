# ChatML UI host capabilities

This document describes the optional UI-only capabilities available to
interactive ChatML embedders such as `chat_tui`.

For the consolidated moderator/runtime overview, including surfaces, event
types, helper modules, history semantics, runtime requests, and persistence
boundaries, see
[ChatML moderator runtime guide](guide/chatml-moderator-runtime.md).

This document focuses only on the UI-specific additions:

- `Ui.notify`
- `Approval.ask_text`
- `Approval.ask_choice`

It also records the capability families that remain intentionally deferred:

- `Ui.set_status`
- `Ui.clear_status`
- `Approval.request`
- `Store.*`

## Why these capabilities are optional

UI notifications and interactive approval prompts are not universal runtime
assumptions.

Some hosts have:

- a visible transcript,
- a notices area,
- an input composer,
- and a live pause/resume path.

Others are:

- batch runners,
- exporters,
- background services,
- or non-interactive embedders.

For that reason, these APIs live on a dedicated UI-capable builtin surface
rather than the default moderator surface.

## Builtin surface

The default surfaces remain:

- `Chatml_builtin_surface.core_surface`
- `Chatml_builtin_surface.moderator_surface`

Interactive hosts that opt into UI-only capabilities install:

- `Chatml_builtin_surface.ui_moderator_surface`

That surface extends `moderator_surface` with:

- `Ui`
- `Approval`

This keeps two guarantees in place:

1. non-UI embedders keep the default moderator surface unchanged;
2. scripts compiled against the UI surface do not silently run on hosts that
   never installed those capabilities.

## Script-visible API

```ocaml
module Ui : sig
  val notify : string -> unit task
end

module Approval : sig
  val ask_text : string -> string task
  val ask_choice : string -> string array -> string task
end
```

## `Ui.notify`

`Ui.notify text` emits a host-local notice.

The notice is:

- visible to the current embedding,
- not persisted as canonical history,
- not appended to transcript items automatically,
- and not part of the default non-UI runtime contract.

In `chat_tui`, `Ui.notify` is routed through the existing system-notice
plumbing rather than a separate notice channel.

## `Approval.ask_text`

`Approval.ask_text prompt` asks the host UI for free-form text input and
returns the submitted answer to the paused script.

The returned answer is script data. It is not a fake canonical user item.

If a script wants the prompt or answer to appear in the transcript, it must
append those items explicitly through `Turn.*` and `Item.*`.

## `Approval.ask_choice`

`Approval.ask_choice prompt choices` asks the host UI to choose one of the
declared strings and returns the selected string to the paused script.

As with `ask_text`, the result is returned directly to the script rather than
being written into canonical history automatically.

## Suspension and resume semantics

`Approval.ask_text` and `Approval.ask_choice` are true live-session
pause/resume operations.

They are not:

- deferred safe-point input,
- synthetic canonical user messages,
- or a second internal-event state machine.

When a script executes one of these builtins:

- the current live script execution pauses at that call site,
- the host exposes one pending approval prompt for the session,
- ordinary `handle_event` progression for that session is blocked while the
  prompt remains pending,
- queued internal events may accumulate, but they are not drained until resume,
- and the script continues from the paused call site only after the host
  resumes that same suspended execution with a validated response.

Only one approval prompt may be pending per session at a time.

Attempting to suspend again while an approval is already pending is an error.

## Host-visible runtime boundary

At the generic runtime layer:

```ocaml
type pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of { prompt : string; choices : string array }

val pending_ui_request : session -> pending_ui_request option
val resume_ui_request : session -> response:string -> (unit, string) result
```

At the durable moderator boundary:

```ocaml
type pending_ui_request =
  | Ask_text of { prompt : string }
  | Ask_choice of { prompt : string; choices : string array }

val pending_ui_request : t -> pending_ui_request option

val resume_ui_request
  :  t
  -> response:string
  -> (Chatml_moderation.Outcome.t list, string) result
```

This boundary makes the pending prompt host-visible without widening the
durable moderator snapshot format.

`resume_ui_request` is the only supported continuation path.

## Commit and state semantics while suspended

Approval suspension is transactional.

When `Approval.ask_*` suspends a handler:

- buffered local effects remain uncommitted,
- buffered emitted internal events remain uncommitted,
- `current_state` continues to expose the last committed script state,
- the pending approval prompt becomes visible to the host immediately,
- and a successful resume commits the suspended handler exactly once.

This prevents half-applied overlay changes or partial state mutation from
becoming visible while the script is waiting for UI input.

## Validation rules

`Approval.ask_text`:

- trims surrounding whitespace before the script receives the answer;
- does not resume the script for an empty trimmed response.

`Approval.ask_choice`:

- trims surrounding whitespace before comparison;
- exact-matches the normalized value against one of the declared choices;
- returns the matched string to the script;
- fails immediately if the `choices` array is empty.

In `chat_tui`, the fixed validation notices are:

- `Please enter a response before continuing.`
- `Please answer with one of the listed choices before continuing.`

## `chat_tui` behavior

`chat_tui` reuses its normal composer and submit flow for approval prompts.

While approval is pending:

- the prompt is shown through the existing input UI,
- submit is repurposed as approval submission instead of starting a normal
  user turn,
- no fake canonical user message is appended automatically,
- idle moderator drains do not proceed for that session,
- and automatic follow-up turns do not start for that session.

After a successful resume:

- the prompt clears,
- any newly committed moderator outcomes are applied,
- and normal submit behavior returns.

## Persistence boundary

Pending approvals are live-session-only.

That means:

- no `Session.Moderator_snapshot` schema change,
- no durable serialization of a partially suspended approval,
- no `Store.*` workaround for approval persistence,
- and no promise that restart can restore an in-flight approval prompt.

Ending a session while approval is pending drops the pending request.

Later resume attempts fail because the session is no longer waiting for UI
input.

## Deferred capability families

The following APIs remain intentionally out of scope:

- `Ui.set_status`
- `Ui.clear_status`
- `Approval.request`
- `Store.get`
- `Store.put`
- `Store.delete`

The current UI surface is intentionally narrow:

- UI-local notices,
- short text approval prompts,
- fixed-choice approval prompts,
- and live-session pause/resume.

It is not a general UI framework, status-bar API, human workflow engine, or
script-visible persistence layer.

## Relationship to the other runtime docs

Use these documents together:

- [ChatML moderator runtime guide](guide/chatml-moderator-runtime.md)
- [ChatML safe-point and effective-history semantics](chatml-safe-point-and-effective-history.md)
- [ChatML host session-controller contract](chatml-host-session-controller-contract.md)
- [ChatML async completion lifecycle](chatml-async-completion-lifecycle.md)
- [ChatML budget policy](chatml-budget-policy.md)

The runtime guide gives the consolidated picture. This document stays focused
on the UI-only additions layered on top of the default moderator runtime.
