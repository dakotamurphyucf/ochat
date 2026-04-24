# ChatML Safe-Point and Effective-History Semantics

This guide makes the current safe-point, effective-history, and durable
moderator-state semantics explicit without changing runtime behavior.

The preferred public boundary is:

- `Chat_response.Chatml_moderator` for durable moderator state, effective
  history, snapshots, and queued internal events;
- `Chat_response.Chatml_turn_driver` for turn-start, turn-end, pre-tool, and
  post-tool boundaries.

The concrete implementation anchors today are:

- `lib/chat_response/chatml_moderator.{ml,mli}`
- `lib/chat_response/chatml_turn_driver.{ml,mli}`
- `lib/chat_response/in_memory_stream.{ml,mli}`
- `lib/chat_response/moderator_manager.ml`
- `lib/chat_response/moderation.mli`
- `lib/chat_tui/app_runtime.ml`
- `lib/chat_tui/app_reducer.ml`
- `lib/chat_tui/app_submit.ml`
- `lib/chat_tui/app.ml`
- `lib/session.mli`

This document is canonical for Phase 2 safe-point and effective-history
semantics. Other docs should summarize briefly and link here.

For the optional Phase 3 UI-only notification and ask-user layer, see
[`docs-src/chatml-ui-host-capabilities.md`](chatml-ui-host-capabilities.md).
That layer builds on the same terminology here: `Ui.notify` is host-local,
approval answers do not append fake canonical user items, and pending
approvals are live-session-only rather than part of the durable snapshot
schema.

## Purpose and scope

This guide explains:

- canonical history, effective history, and visible history;
- overlay application as the mechanism for transcript shaping;
- the exact safe boundaries already implemented in the turn driver and host;
- deferred steering semantics;
- durable snapshot and restore semantics through
  `Session.Moderator_snapshot.t`;
- how the public wrapper modules expose the boundary today; and
- how those pieces compose in request, tool, and idle-wakeup flows.

This guide does not define host scheduling policy beyond documenting the
current boundary. `Chat_response.Chatml_turn_driver` prepares inputs and
interprets surfaced runtime requests, but it does not decide when a host should
start, suppress, or retry turns.

## Canonical history, effective history, and visible history

The current implementation has three distinct history views.

### Canonical history

Canonical history is the durable session transcript stored as
`Session.history` and, during an active `chat_tui` run, as
`Model.history_items`.

This is the audit trail returned by the normal model/tool loop. Moderator
scripts do not rewrite canonical history in place. Instead they request overlay
operations that are stored separately in the moderator snapshot.

### Effective history

Effective history is the canonical history after moderator projection and
overlay application.

The public boundary for this is `Chat_response.Chatml_moderator.effective_history`.
The current implementation lives in `Moderator_manager.effective_history`,
which:

1. projects canonical `Openai.Responses.Item.t list` into structured
   moderation items via `Moderation.Projection.project_history`;
2. applies the durable overlay via `Moderation.Overlay.apply`; and
3. reconstructs `Openai.Responses.Item.t list` values for downstream model
   input and consumers.

This is the history view used by `Chat_response.Chatml_turn_driver` when it
prepares the next request after the relevant turn boundary has run.

### Visible history

Visible history is the host/UI view derived from effective history.

In `chat_tui`, `App_runtime.refresh_messages` recomputes visible transcript
state from `Manager.effective_history`, then rebuilds message buffers and tool
output indexes from that projected view.

This preserves the distinction:

- canonical history is the durable source transcript;
- effective history is the moderator-shaped request/downstream view;
- visible history is the host presentation of effective history.

## Overlay application is the transcript-shaping mechanism

The durable overlay stored in `Session.Moderator_snapshot.Overlay.t` contains:

- prepended synthetic system items;
- appended synthetic items;
- replacements keyed by target item id;
- deleted item ids; and
- an optional halt reason.

The runtime applies those overlay operations transactionally after successful
moderator event handling. The host does not rewrite canonical history directly
to achieve transcript shaping. Instead:

1. runtime effects are decoded into structured moderation outcomes;
2. durable overlay operations are applied to the stored overlay snapshot; and
3. later request or UI projections call `effective_history` to materialize the
   shaped view.

That makes overlay application the exact mechanism for transcript shaping in
both request preparation and visible-history refresh.

## Exact safe-point boundaries already in code

The codebase currently uses a mix of turn-driver boundaries and host-only safe
boundaries.

### Boundary table

| Boundary | Public entrypoint | Current anchors | Current behavior |
|---|---|---|---|
| Turn start | `Chat_response.Chatml_turn_driver.prepare_turn_inputs` | `In_memory_stream.prepare_turn_inputs` / `prepare_turn_request` | Runs `turn_start`, drains queued internal events at the turn-start boundary, computes effective history, then appends transient safe-point input only if the turn is still allowed to proceed. |
| Pre-tool-call | `Chat_response.Chatml_turn_driver.moderate_tool_call` | `In_memory_stream.moderate_tool_call` | Adds the pending tool call to temporary history, runs `pre_tool_call`, and returns the approved, rejected, rewritten, or redirected tool invocation. |
| Post-tool-response | `Chat_response.Chatml_turn_driver.handle_tool_result` | `In_memory_stream.handle_tool_result` | Runs `post_tool_response`, emits `item_appended` for the canonical tool output when appropriate, then drains queued internal events at the post-tool boundary. |
| Turn end | `Chat_response.Chatml_turn_driver.finish_turn` | `In_memory_stream.finish_turn` | Runs `turn_end`, drains queued internal events at the end-of-turn boundary, and surfaces runtime requests. |
| Idle internal-event drain | host-owned | `Moderator_session_controller.drain_internal_events` plus idle drain in `app_reducer` | Replays queued internal events only while the host is idle, then refreshes visible history and may schedule host follow-up work. |
| Startup / resume | host-owned | moderator startup in `app.ml` | Runs `session_start` or `session_resume`, then drains queued internal events unless startup already requested session end. |
| Compaction completion | host-owned | compaction completion path in `app_reducer` | Refreshes visible history after compaction success, then checks the idle safe boundary before starting queued submit or follow-up work. |

### Turn start

Turn start is the explicit request-preparation safe point. The turn driver:

1. runs `Moderation.Event.Turn_start`;
2. drains queued moderator internal events at `Turn_start_boundary`;
3. computes effective history through
   `Chat_response.Chatml_moderator.effective_history`; and
4. appends deferred safe-point input only after the boundary has decided the
   turn may proceed.

This ordering matters: deferred steering is added only to the outgoing request
view, not to canonical history.

### Pre-tool-call

Pre-tool-call is a moderation boundary around a pending tool invocation. The
driver constructs a provisional tool-call item, appends it to temporary
history, and runs `pre_tool_call` against that shaped context.

This boundary does not itself drain queued internal events. Its job is to let
the moderator approve, reject, rewrite, or redirect the tool invocation before
execution continues.

### Post-tool-response

Post-tool-response is the safe boundary after a tool output item exists. The
driver:

1. runs `post_tool_response`;
2. emits `item_appended` for the canonical tool output when the session is not
   already ending; and
3. drains queued internal events at `Post_tool_result_boundary`.

That makes post-tool handling the place where tool-output side effects become
visible to the moderator before the next model step.

### Turn end

Turn end is the boundary after a streamed turn completes. The driver runs
`turn_end`, drains queued internal events at `Turn_end_boundary`, and returns
the surfaced runtime requests to the host.

### Idle internal-event drain

Idle internal-event drain is host-owned rather than turn-driver-owned.

`chat_tui` marks moderator work dirty when a wakeup arrives, but it drains
queued internal events only when `App_runtime.is_idle` is true. After a
successful idle drain, the host refreshes visible history, enqueues any new
internal events, and may schedule a host follow-up turn.

This is why `Chat_response.Chatml_turn_driver` does not own general host
scheduling policy: the host decides when idle work may run.

### Startup / resume

When a moderated session starts, `chat_tui` chooses `session_start` for a fresh
session and `session_resume` when a persisted moderator snapshot exists. After
that initial event, the host immediately drains queued internal events unless
the startup outcome already requested session end.

This startup path is a safe boundary because it lets queued internal work and
durable overlay state be applied before the user sees or resumes the session.

### Compaction completion

Compaction completion is also host-owned. After compaction succeeds,
`chat_tui` replaces canonical history with the compacted result, refreshes
visible history from the effective projection, and then checks the idle safe
boundary before starting queued submit, compaction, or follow-up work.

Compaction errors follow the same host pattern for the idle safe boundary,
except canonical history is not replaced.

## Deferred steering semantics

Deferred steering is safe-point input only.

When the user submits text during an active streamed turn, `chat_tui` does not
append a new canonical user message mid-turn. Instead it:

1. stores a stripped deferred user note in the session-controller queue via
   `App_runtime.enqueue_deferred_user_note`;
2. exposes queued notes through
   `App_runtime.consume_deferred_user_notes_for_safe_point`; and
3. passes that source into the turn driver as
   `Chat_response.Chatml_turn_driver.Safe_point_input.t`.

The turn driver consumes that input only at request-preparation time through
the turn-start boundary. The rendered note is wrapped as transient system input
for the next request only.

The consequences are:

- deferred steering is request-only;
- it is not persisted as a canonical user item;
- it is not spliced into an already running request; and
- it becomes visible to the model only at the next allowed request boundary.

## Approval suspension is not deferred steering

Phase 3 `Approval.ask_text` and `Approval.ask_choice` use a different
continuation model from deferred safe-point input.

Approval suspension:

- pauses the current live script execution immediately,
- exposes a host-visible `pending_ui_request`,
- blocks ordinary `handle_event` progression for that session while waiting,
- keeps buffered local effects and emitted internal events uncommitted,
- leaves `current_state` on the last committed script state,
- and resumes only through `resume_ui_request` on the same live session.

Unlike deferred steering, approval waiting is not request-only input for a
future turn boundary. It resumes the same paused script frame and returns a
validated response directly to the paused builtin call.

While approval is pending, hosts may still enqueue internal events, but those
queued events are not drained until resume succeeds. Ending the session drops
the pending request, and snapshot/restore while approval is pending is
unsupported in this phase because `Session.Moderator_snapshot.t` does not
persist suspended approvals.

## Durable state and restore semantics

`Session.Moderator_snapshot.t` is the durable restore boundary for moderator
state. It stores:

- `script_id`;
- `script_source_hash`;
- `current_state`;
- `queued_internal_events`;
- `halted`; and
- the durable overlay snapshot.

`Chat_response.Chatml_moderator.snapshot` extracts that persisted payload
without changing the session schema.

On restore, the moderator manager validates:

1. the compiled script id matches the snapshot;
2. the script source hash matches the snapshot; and
3. the saved runtime state, queued internal events, halted flag, and overlay
   can all be restored successfully.

This is why snapshot restore is both durable and compatibility-checked. A
snapshot is only valid for the same compiled moderator script identity and
source hash.

Session persistence in `chat_tui` stores the moderator snapshot alongside
canonical session history, tasks, and key/value metadata. On startup,
`chat_tui` uses the presence of that snapshot to decide between
`session_start` and `session_resume`.

## Public wrapper boundary today

The wrapper-first public story is:

- `Chat_response.Chatml_moderator` is the durable effective-history and
  snapshot boundary;
- `Chat_response.Chatml_turn_driver` is the public safe-point and turn-input
  boundary.

### `Chat_response.Chatml_moderator`

Use this module to:

- compile and cache moderator scripts;
- instantiate or restore a moderator session;
- compute effective items or effective history;
- snapshot durable moderator state; and
- enqueue internal events for later replay.

The concrete implementation currently delegates to `Moderator_manager`.

### `Chat_response.Chatml_turn_driver`

Use this module to:

- prepare turn inputs at turn start;
- finish a turn at turn end;
- moderate a pending tool call; and
- handle a tool result at the post-tool boundary.

The concrete implementation currently delegates to `In_memory_stream`.

This module intentionally does not own host scheduling policy. Hosts still
decide when to start turns, when to drain idle work, and how to sequence
follow-up work around compaction or UI state.

## Examples grounded in the current flows

### Request flow

Normal user submit flow in `chat_tui` is:

1. `App_submit.start` appends the canonical user message when the host is
   starting a real user turn;
2. the host starts streaming from current canonical history;
3. `Chat_response.Chatml_turn_driver.prepare_turn_inputs` applies the
   turn-start boundary, effective-history projection, and any deferred
   safe-point input;
4. the streamed model/tool loop proceeds; and
5. `Chat_response.Chatml_turn_driver.finish_turn` applies the end-of-turn
   boundary before the host decides whether follow-up work should start.

This shows the intended separation: canonical history changes when the host
accepts a user submit, while effective history is computed only at the
request-preparation boundary.

### Tool flow

The current tool flow is:

1. `Chat_response.Chatml_turn_driver.moderate_tool_call` constructs a
   provisional tool-call item and runs `pre_tool_call`;
2. the host executes the approved or rewritten call, or uses a synthetic
   output if the moderator rejected it;
3. the resulting tool-output item is appended canonically; and
4. `Chat_response.Chatml_turn_driver.handle_tool_result` runs
   `post_tool_response`, optionally emits `item_appended`, and drains queued
   internal events at the post-tool boundary.

This keeps tool governance and post-tool transcript shaping inside the current
moderator-safe boundary model.

### Idle wakeup flow

The current idle wakeup flow is:

1. a background producer such as `Model.spawn` completion reinjects a queued
   internal event;
2. the session receives a moderator wakeup and marks moderator work dirty;
3. `chat_tui` drains queued internal events only when the session is idle;
4. the host refreshes visible history from effective history after the drain;
   and
5. if the moderator surfaced a turn request, the host may start an ordinary
   follow-up turn from the current canonical session state.

This is the boundary where safe-point semantics meet host scheduling policy:
the runtime surfaces requests, but the host still decides when idle work or
follow-up turns are allowed to run.
