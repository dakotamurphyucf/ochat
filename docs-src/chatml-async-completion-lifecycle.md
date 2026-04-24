# ChatML Async Completion Lifecycle

This document makes the current async completion lifecycle explicit for
embedders without introducing a new compiled `Job` API.

The current codebase already has a strong concrete anchor for model-backed
async work in `Chat_response.Model_executor`. It does not yet have an equally
general public contract that unifies `Model.spawn`, `Process.run`, and
`Schedule.after_ms` under one host-visible `Job` surface.

For the surrounding host/session-controller and safe-boundary semantics, see:

- [ChatML host session-controller contract](chatml-host-session-controller-contract.md)
- [ChatML safe-point and effective-history semantics](chatml-safe-point-and-effective-history.md)
- [ChatML budget policy](chatml-budget-policy.md)

## Purpose and scope

Phase 2 keeps the async story grounded in the current implementation:

- `Model.spawn` has a concrete OCaml execution and reinjection path through
  `Chat_response.Model_executor`;
- `Process.run` and `Schedule.after_ms` are available as async ChatML
  primitives, but they do not yet share a sufficiently general public
  host/executor contract;
- async completion continues to reinject as queued moderator internal events;
- hosts continue to wake, drain, refresh, and optionally schedule follow-up
  turns only at the safe boundaries already described elsewhere.

This document is descriptive. It does not add new runtime behavior.

## Async primitives currently in scope

Phase 2 recognizes three async script primitives:

### `Model.spawn`

`Model.spawn` is the most concrete async primitive in the current codebase.
Its host-facing implementation is `Chat_response.Model_executor`, which owns:

- job creation,
- a spawned-job cap through `create ?max_spawned_jobs`,
- per-session registration,
- delivery of completed results as queued moderator internal events,
- and optional session wakeup callbacks.

This is the current anchor for async work that completes outside the active
foreground turn and later rejoins the moderator runtime.

### `Process.run`

`Process.run` is available as an async builtin operation in the moderator
surface. It represents background process work at the scripting layer, but
Phase 2 does not yet provide a shared public OCaml contract analogous to
`Model_executor` for process jobs.

That means the primitive exists, but its lifecycle is not yet exposed through
a generic embedder-facing `Job` module.

### `Schedule.after_ms`

`Schedule.after_ms` is also part of the async builtin vocabulary. It expresses
delayed work in scripts, but it does not currently have the same concrete
public host contract that `Model.spawn` has through `Model_executor`.

Phase 2 therefore documents it as an existing primitive, not as evidence that
a generalized `Job` abstraction is already stable.

## Concrete model-job lifecycle today

The current end-to-end model-job reinjection path is:

1. a moderator script calls `Model.spawn(recipe, payload)`;
2. the host recipe implementation allocates and records a pending job in
   `Chat_response.Model_executor`;
3. the executor runs the job in background work;
4. when the work completes, the executor converts the result into a stable
   internal-event variant;
5. that event is enqueued on the session's `Chat_response.Chatml_moderator`
   state via the underlying `Moderator_manager`;
6. the executor triggers the registered session wakeup callback;
7. the host session controller marks moderator work dirty and later drains the
   queued internal event at an idle or other safe boundary;
8. the moderator may update overlay state, append items, emit notices, request
   a refresh, request compaction, or request another turn;
9. the host refreshes visible history and may start a follow-up turn from the
   current canonical session state.

Conceptually, the lifecycle is:

```text
Model.spawn
  -> Model_executor.spawn
  -> background job completes
  -> enqueue moderator internal event
  -> wake host/session
  -> host drains at a safe boundary
  -> moderator outcomes become refresh / notices / follow-up requests
```

## Current executor and reinjection anchors

The current concrete anchors are:

- `lib/chat_response/model_executor.{ml,mli}`
- `lib/chat_response/chatml_moderator.{ml,mli}`
- `lib/chat_response/moderator_manager.ml`
- `lib/chat_tui/app.ml`
- `lib/chat_tui/app_reducer.ml`

The key responsibilities split as follows:

### Spawn

`Chat_response.Model_executor.recipe_agent_prompt_v1` exposes the current
model-backed async recipe. Its `spawn` path:

- checks `max_spawned_jobs`,
- allocates a `job_id`,
- stores a pending job in the executor,
- and launches background work.

### Register session

`Chat_response.Model_executor.register_session` associates:

- a `session_id`,
- a `Moderator_manager`,
- and an optional wakeup callback.

This is what lets async model completions rejoin the correct moderator session
later.

### Finish job

When background work finishes, `Model_executor` updates the stored job status
to succeeded or failed and attempts delivery.

### Enqueue internal event

Delivery turns completion into a stable moderator internal event:

- `Model_job_succeeded(job_id, recipe_name, result_json)`, or
- `Model_job_failed(job_id, recipe_name, message)`.

That event is enqueued through the moderator manager, which means completion
becomes queued runtime work rather than direct mid-turn mutation.

### Wake host

After successful enqueue, the executor invokes the registered wakeup callback.
In `chat_tui`, that wakeup becomes `` `Moderator_wakeup `` in the reducer's
internal event stream.

### Drain at safe boundary

The host session controller does not apply async completion immediately during
active foreground work. Instead it:

- marks the session dirty,
- waits until the session is idle or another safe boundary is reached,
- drains queued moderator internal events,
- and applies any surfaced outcomes.

### Optionally request another turn

The async completion itself does not directly start a new model call. A
follow-up turn happens only if:

- the moderator emits `Runtime.request_turn()`,
- the host session controller accepts that request under its current policy,
- and the session is idle enough to start a new turn from current canonical
  state.

## Queued internal work versus immediate host or UI updates

The most important distinction in the current lifecycle is between:

- queued internal moderator work, and
- immediate host-visible state changes.

### Queued internal work

Async completion is first represented as queued moderator work:

- enqueue an internal event,
- wake the host,
- drain later through `Chat_response.Chatml_moderator.drain_internal_events`.

This preserves the existing invariants:

- no arbitrary mid-turn state mutation,
- no fake user messages,
- canonical history remains canonical,
- and overlay-visible effects still enter through safe boundaries.

### Immediate host or UI updates

Immediate host-visible changes happen only after drain outcomes are applied.
In `chat_tui`, this can include:

- refreshing visible history from moderator-effective history,
- recording one-time system notices,
- enqueueing reducer-internal follow-up actions,
- or starting a host follow-up turn.

The host can therefore react quickly to wakeups while still preserving the
safe-boundary rule for the actual moderator state transition.

## Wakeups and the host session-controller contract

The async lifecycle relies on the host/session-controller contract from Phase
2, not on the generic ChatML runtime alone.

In the current `chat_tui` path:

1. `Model_executor` enqueues an internal event and calls the registered
   session wakeup callback;
2. `app.ml` wires that callback to emit `` `Moderator_wakeup `` into the
   reducer's internal stream;
3. `app_reducer` handles `` `Moderator_wakeup `` by marking the moderator
   dirty and attempting an idle drain;
4. if the UI is not idle, the wakeup stays deferred until a later safe point;
5. once drained, the host may refresh visible history and schedule a follow-up
   turn according to the host/session-controller and budget-policy rules.

This is why the async lifecycle is partly a runtime concern and partly a host
policy concern:

- the runtime owns queued internal events and their eventual replay;
- the host owns wakeup delivery, idle-only drains, visible-history refresh,
  and the decision to start another turn.

## Relationship to the budget policy

Phase 2 intentionally keeps two different limit families separate:

### Spawned-job limit

`max_spawned_jobs` remains owned by:

- `Chat_response.Model_executor.create ?max_spawned_jobs`

It limits how many async model jobs may be in flight from that executor.
It is not part of `Chat_response.Runtime_semantics.policy`.

### Turn and drain budgets

The Phase 2 budget policy covers:

- self-triggered turn continuation inside `In_memory_stream`,
- host follow-up turn count and rate limiting,
- and the maximum number of internal events drained at a safe boundary.

Those budgets govern what happens after async work has already been queued or
completed. They do not replace the executor-owned spawned-job limit.

This split is intentional:

- `Model_executor` owns capacity for background model jobs;
- the runtime and host session controller own how completions are replayed and
  whether they can trigger more work.

## Example: idle model completion in `chat_tui`

The current model-completion path looks like this:

1. a moderator script calls `Model.spawn(...)`;
2. the executor records the job and returns a `job_id` immediately;
3. the current foreground turn may finish before the job does;
4. the background job later succeeds or fails;
5. the executor converts completion into `Model_job_succeeded` or
   `Model_job_failed`;
6. the event is enqueued on the moderator session and the wakeup callback
   fires;
7. `chat_tui` emits `` `Moderator_wakeup `` and marks the session dirty;
8. when idle, the reducer drains the queued internal event;
9. the moderator may append an item, request refresh, or request another turn;
10. the host refreshes visible history and optionally schedules a follow-up
    turn.

The important point is that the async completion does not bypass moderator
event handling. It re-enters through the same queued internal-event mechanism
used elsewhere.

## Why no generic `Job` module landed in Phase 2

Phase 2 records the deferral explicitly rather than leaving it implicit.

The repository does have a strong current anchor for one class of async work:

- `Chat_response.Model_executor` is a dedicated OCaml module;
- it has explicit session registration;
- it has explicit wakeup integration;
- it has explicit delivery as moderator internal events;
- it exposes job-state and await helpers for tests and embedders.

That is enough to document the model-job lifecycle precisely today.

The same is not yet true for a generalized `Job` surface spanning all async
primitives:

- `Process.run` exists as a builtin async operation, but not yet as a shared
  public OCaml job/executor contract;
- `Schedule.after_ms` exists as a builtin scheduling primitive, but it does
  not yet share the same concrete embedder-facing lifecycle surface as
  `Model.spawn`;
- cancellation, state observation, and ownership semantics are not yet unified
  across model, process, and scheduled work.

The current codebase therefore supports a documentation-first conclusion:

- keep `Model_executor` as the concrete Phase 2 anchor for async lifecycle
  docs,
- keep `Process.run` and `Schedule.after_ms` as existing primitives,
- and defer any generic `Job` module until multiple async capabilities share a
  sufficiently stable public contract.

## Non-goals

This document does not:

- add a compiled `Job` module;
- change async runtime semantics;
- make `Process.run` or `Schedule.after_ms` use the `Model_executor` path;
- or define a new host scheduler outside the existing session-controller
  contract.
