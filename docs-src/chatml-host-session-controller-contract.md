# ChatML Host Session-Controller Contract

This document makes the current host-side session-controller behavior explicit
without introducing a new compiled API.

The concrete implementation today lives in `chat_tui`, primarily through:

- `lib/chat_tui/moderator_session_controller.{ml,mli}`
- `lib/chat_tui/app_runtime.{ml,mli}`
- `lib/chat_tui/app_reducer.{ml,mli}`
- `lib/chat_tui/app_submit.{ml,mli}`
- `lib/chat_tui/app_compaction.ml`

The contract is written in generic host terms so later work can implement
against the behavior described here, while still naming the current anchors.

## Purpose and scope

The ChatML runtime is responsible for:

- running compiled moderator scripts,
- keeping durable script state,
- decoding transactional local effects,
- maintaining queued internal events,
- and applying safe-point moderation hooks.

The host session controller is responsible for everything around that runtime:

- noticing wakeups,
- deciding when idle work may run,
- keeping at most one active turn or compaction job per session,
- refreshing visible history after moderator-visible changes,
- scheduling follow-up turns from current session state,
- holding deferred user steering until a safe model-input boundary,
- and running host-owned background work such as compaction.

This document describes that host contract. It is not a new runtime layer and
it is not specific to the `chat_tui` UI beyond the cited implementation
anchors.

For the optional Phase 3 UI-only capability layer built on top of this host
contract, see
[`docs-src/chatml-ui-host-capabilities.md`](chatml-ui-host-capabilities.md).
That document covers `Ui.notify` plus the live-session `Approval.ask_text` and
`Approval.ask_choice` flow. These remain host-owned capabilities layered on
top of the same wakeup, input-capture, visible-history, and follow-up
orchestration model described here.

## Responsibilities owned by the host

The host session controller owns responsibilities that should remain outside
the generic ChatML runtime:

1. **Single active foreground operation**

   The host serializes streaming turns and compaction work. In `chat_tui` this
   is enforced by `App_runtime.op`, `App_runtime.has_active_turn`, and the
   reducer rule that only one streaming or compaction operation may be active
   at a time.

2. **Idle-only moderator drains**

   Background moderator wakeups do not immediately mutate visible state during
   an active turn. The host records that moderator work is pending and drains
   internal events only when the session is idle.

3. **Visible-history refresh**

   The host owns the user-visible transcript projection. In `chat_tui`,
   `App_runtime.refresh_messages` rebuilds messages from moderator-effective
   history, rebuilds tool-output indexes, clamps selection, and clears image
   caches.

4. **Follow-up turn scheduling**

   Runtime requests such as `Runtime.request_turn()` do not directly start a
   new model call. The host decides when a follow-up turn may start and
   launches it from the current canonical session state.

5. **Deferred steering**

   If the user submits steering text while a turn is already streaming, the
   host records that note separately and injects it only at the next safe
   model-input boundary. It does not append a fake canonical user item.

6. **Compaction execution**

   Compaction is host-owned background work. The runtime may request it, but
   the host decides when to queue, start, complete, and reproject after it.

7. **Approval-blocked live sessions**

   Optional UI-only approval prompts are also host-owned. When a session is
   waiting for `Approval.ask_text` or `Approval.ask_choice`, the host shows the
   pending prompt through its normal input UI, routes submit to approval
   response instead of normal user submit, and suppresses idle drains and
   automatic follow-up turns for that session until resume succeeds or the
   session ends.

## Session-controller state

The current `chat_tui` contract is embodied by
`App_runtime.session_controller_state`:

```ocaml
type session_controller_state =
  { mutable moderator_dirty : bool
  ; deferred_user_notes : deferred_user_note Queue.t
  ; mutable pending_turn_request : turn_start_reason option
  }
```

Conceptually, those fields correspond to:

- pending background moderator work,
- deferred request-only user steering,
- and one pending follow-up turn request.

## Event and action vocabulary

The conceptual contract names below are the stable vocabulary for this phase.
Each name maps directly to current `chat_tui` anchors.

| Conceptual action | Current `chat_tui` anchor | Current behavior |
|---|---|---|
| `mark_dirty` | `App_runtime.mark_moderator_dirty` | Marks that moderator wakeup work is pending. It does not itself drain internal events. |
| `drain_internal_events_if_idle` | `Moderator_session_controller.drain_internal_events` plus the idle-drain path in `app_reducer` | Replays queued internal events only when the host is idle, then surfaces refresh, compaction, follow-up-turn, halt, notice, and internal-event outcomes. |
| `schedule_turn` | `App_runtime.request_turn_start` plus `App_submit.start_from_current_session` | Records a pending follow-up turn reason, then starts a turn later from the current canonical session state without appending a new user item. |
| `defer_user_note` | `App_runtime.enqueue_deferred_user_note` plus `consume_deferred_user_notes_for_safe_point` | Stores user steering submitted during streaming and exposes it only through safe-point input consumption. |
| `refresh_visible_history` | `App_runtime.refresh_messages` | Reprojects visible transcript state from moderator-effective history after moderator-visible changes. |
| `run_compaction` | compaction path orchestrated by `app_reducer` and `App_compaction.start` | Queues compaction behind foreground work, runs it as host-owned background work, replaces canonical history on success, and refreshes visible history before more work continues. |

## Required invariants

Every host embedding that follows this contract should preserve the following
rules.

### Single active turn

The host keeps at most one active foreground streaming turn per session. A
host may also serialize other foreground work such as compaction under the same
single-active-operation rule. `chat_tui` enforces this with `App_runtime.op`
and the reducer queueing policy.

### Idle-only drain

Moderator wakeups are drained only when no foreground operation is active.
Wakeups that arrive during streaming or compaction are represented by
`moderator_dirty` and handled later at an idle safe point.

### Deferred steering stays request-only

Deferred user notes are safe-point input only. They are not appended to
canonical history and are not spliced into an in-flight request.

### Approval waiting blocks ordinary session progression

While an approval prompt is pending, ordinary `handle_event` progression for
that session is blocked. Hosts may continue queuing internal events for later,
but they do not drain those events or start automatic follow-up turns until
the session resumes from the pending approval.

### Refresh after moderator-visible changes

When moderator-visible overlay state changes, the host refreshes visible
history from effective history. Pure runtime requests alone do not imply a
refresh.

### Follow-up scheduling is host policy

Runtime requests surface intent, not direct execution. The host decides when a
follow-up turn may run, how it interacts with queued work, and whether the
session is halted.

## Ordering and precedence rules

The host must apply surfaced moderator outcomes in a consistent order.

### Runtime-request precedence

`Moderator_session_controller.t` already defines the key precedence rules:

- `End_session` suppresses any scheduled turn.
- Compaction requests remain visible even when `End_session` is present.
- Pure runtime requests do not by themselves require a visible-history
  refresh.

### Wakeup and drain ordering

The host first records a wakeup through `mark_dirty`. It drains only if the
session is idle. If a wakeup arrives during active work, it remains pending
until a later safe point.

### Safe-point-before-next-work rule

After a streamed turn finishes, and after compaction completes or errors, the
host checks for pending idle moderator work before starting any queued submit,
compaction, or follow-up turn. In `chat_tui`, this is the
`handle_idle_safe_point ()` check that runs before `maybe_start_next_pending ()`.

### Follow-up turn ordering

Follow-up turns are recorded separately from the FIFO queue of submits and
compactions. They do not interrupt active work. They start only after the host
has become idle, confirmed the session is not halted, and decided it is safe
to continue from current session state.

## Safe-point boundaries

The host session controller is allowed to drain or continue work only at safe
boundaries.

### Turn-start safe point

`Chat_response.Chatml_turn_driver.prepare_turn_inputs` applies the explicit
turn-start boundary. Deferred steering is consumed here as request-only input
after the turn-start decision has been made.

### Post-tool safe point

`Chat_response.Chatml_turn_driver.handle_tool_result` applies the post-tool
boundary and drains moderator internal events surfaced at that boundary.

### Turn-end safe point

`Chat_response.Chatml_turn_driver.finish_turn` applies the end-of-turn
boundary. After the foreground stream completes, the host may then perform an
idle moderator drain and schedule a follow-up turn.

### Host idle safe points

The `chat_tui` reducer currently treats the following host boundaries as safe
points for draining pending moderator wakeups or continuing queued work:

- while idle after `Moderator_wakeup`,
- after streamed turn completion,
- after compaction completion,
- after compaction error,
- and after streaming error recovery.

These boundaries preserve the rule that background moderator work is never
injected into the middle of an active foreground turn.

## Follow-up turn contract

Follow-up turns are ordinary turns started from the current canonical session
history.

The host contract is:

- do not append a fake user item,
- carry an explicit start reason such as `Moderator_request` or
  `Idle_followup`,
- clear or consume any pending turn request when launching the follow-up,
- and refuse to start the turn if the session has already been halted.

In `chat_tui`, `App_submit.start_from_current_session` is the concrete launch
point for this behavior.

## Deferred user-note contract

When the user submits text during an active streamed turn, the host may record
that text as deferred steering instead of starting another turn immediately.

The host contract is:

- strip and ignore empty notes,
- store non-empty notes in a host-owned queue,
- render them as transient safe-point input,
- consume them only through the safe-point input source,
- and leave canonical history unchanged.

This keeps the current reasoning/tool workflow intact while still letting the
user steer the next request.

## Approval-blocked session contract

Phase 3 approval prompts are not a variant of deferred steering.

The host contract while approval is pending is:

- expose at most one pending approval request per session,
- keep the pending prompt visible through the ordinary input UI,
- route submit to approval response validation rather than normal user submit,
- do not append a fake canonical user item for the approval response,
- block ordinary `handle_event` calls for that session until resume succeeds,
- allow queued internal events to accumulate without draining them,
- suppress automatic follow-up turns for that session,
- discard the pending request if the session ends,
- and treat restart or snapshot/restore while approval is pending as
  unsupported in this phase.

The runtime-visible continuation boundary for this contract is
`pending_ui_request` plus `resume_ui_request`.

## Visible-history refresh contract

The host owns the projection from canonical history to moderator-visible
history. That projection must be refreshed after moderator-visible changes and
after host actions that replace canonical history, such as compaction.

In `chat_tui`, `App_runtime.refresh_messages` is the concrete anchor. It:

- computes moderator-effective history,
- rebuilds visible messages,
- rebuilds tool-output indexes,
- clamps selection,
- and clears image caches.

## Compaction contract

Compaction is a host action coordinated by the session controller rather than a
generic ChatML runtime feature.

The current `chat_tui` contract is:

1. queue a compaction request when foreground work is already active,
2. start compaction only when no foreground operation is active,
3. keep streaming and compaction serialized,
4. replace canonical history on successful completion,
5. refresh visible history after the new canonical history is installed,
6. then re-check idle moderator work before starting any more queued work.

This preserves workflow continuity while keeping compaction outside the generic
runtime.

## Non-goals

This contract intentionally does not define:

- a new compiled `Session_controller` module,
- a second runtime separate from `ChatML`,
- a UI framework,
- approval or prompt-specific product semantics,
- or a generalized concurrency model beyond the current single-active-turn
  host policy.

It exists to document the host layer that already ships today, so later work on
budget policy, safe-point docs, and async lifecycle can refer to one concrete
contract instead of rediscovering behavior from `chat_tui` internals.
