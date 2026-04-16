# ChatML async session controller implementation plan

These task descriptions are intentionally detailed so that an agent can
implement the async moderator/session-controller refactor from this file pair
alone.

The target outcome is an architecture in which:

- `Chat_response.Moderator_manager` remains the durable owner of moderator
  runtime state, overlay state, and queued internal events.
- `Chat_response.In_memory_stream` remains the owner of one active
  completion/tool-followup loop.
- `chat_tui` grows an explicit session-controller layer that can process
  moderator wakeups and background async completions while idle, without
  interrupting an active turn.
- user steering submitted during a running turn stays deferred and is only
  spliced in at a safe model-input boundary.

Design invariants to preserve throughout all tasks:

- Never inject a new canonical user history item into an in-flight turn.
- Never run more than one completion turn at a time.
- Background async producers may enqueue moderator/internal work and wake the
  TUI, but must not mutate the TUI model directly.
- Any moderator-visible transcript change must eventually trigger a refresh of
  visible messages via moderator effective-history projection.
- `Runtime.request_turn` must still work during active turns and must also
  become meaningful while the UI is idle.
- Deferred steering notes are an ephemeral session-control concept, not a new
  persisted transcript item format.
- Session wakeup registration must have explicit cleanup so model-executor or
  other async producers cannot keep stale TUI callbacks alive after shutdown.

Terminology to use consistently in code and docs:

- **session controller**: the chat_tui-owned serialized control layer built on
  top of the reducer/internal-stream loop
- **idle wakeup**: a signal that moderator/internal async work is available
  while no turn is currently active
- **deferred steering note**: a user-authored steering input captured during an
  active turn and held until a safe model-input boundary
- **safe point**: a boundary where the runtime may splice deferred steering or
  drain pending moderator work without mutating an already-issued model request

## 1) Introduce explicit chat_tui session-controller state and internal events

**Objective:** Add the core vocabulary and runtime state needed for idle
moderator wakeups, moderator-dirty tracking, deferred steering notes, and
turn starts that are not tied directly to a fresh user submit.

**Edits to make:**

- Extend `lib/chat_tui/app_events.ml` with session-level internal events for at
  least:
  - moderator wakeup / moderator work available
  - an explicit turn-start request that is not necessarily backed by a fresh
    user message
  - optional notification/system-notice delivery if that helps keep reducer
    branches smaller
- Extend `lib/chat_tui/app_runtime.ml` with explicit controller state such as:
  - `moderator_dirty : bool`
  - a FIFO queue of deferred user steering notes
  - `pending_turn_request : bool`
  - any small helper predicates required to decide whether the session is idle,
    active, or safe to start another turn
- Initialize the new fields in `App_runtime.create`.
- Update `lib/chat_tui/app.ml` and `lib/chat_tui/app_reducer.ml` signatures as
  needed so the new internal events and state can flow through the existing
  reducer loop.
- If helpful, add a tiny explicit type for turn origin or scheduled turn
  reason, such as user-submit vs moderator-request vs idle-followup, so later
  tasks do not have to reverse-engineer why a turn was started.

**How to refactor:**

- Keep the current reducer/event-loop model: all UI/session mutations should
  still happen by pushing `internal_event`s onto the internal stream and
  processing them serially in the reducer.
- Avoid embedding too much logic directly in new reducer match branches. This
  task is about introducing state and event vocabulary, not finishing the full
  behavior.
- Add small helper functions in `app_runtime.ml` for common controller checks,
  for example “has_active_turn”, “may_start_turn_now”, or “enqueue_deferred_note”.
- Keep the new fields narrowly scoped to session control. Do not turn
  `App_runtime.t` into a dumping ground for general UI state.

**Constraints:**

- Do not change submit semantics yet beyond any plumbing strictly required to compile after the new event/state additions.
- Do not add persistence for the new deferred-note queue unless a later task
  explicitly requires it.
- Keep compatibility with the current startup/resume path.

**Acceptance checks:**

- The project builds with the new state/event scaffolding in place.
- The reducer loop still compiles and can ignore the new events safely until
  later tasks give them full behavior.
- There is a clear, typed place in runtime state to store pending moderator
  wakeups and deferred steering notes.
- The new state model makes it obvious how a future idle `Request_turn` will be
  represented even before later tasks implement the full flow.

## 2) Add async moderator wakeup plumbing from background producers

**Objective:** Ensure that when background async work completes and enqueues a
moderator internal event, `chat_tui` is notified even if no turn is currently
active.

**Edits to make:**

- Extend `lib/chat_response/model_executor.ml` so that, after
  `Moderator_manager.enqueue_internal_event` succeeds for a session, a wakeup
  callback/notifier is triggered for that session.
- Add a session-level wakeup registration mechanism somewhere appropriate for
  the current architecture. The simplest acceptable design is a callback table
  keyed by `session_id`; if you choose another mechanism, keep it explicit and
  easy to reason about.
- Thread a notifier from `chat_tui` startup (`lib/chat_tui/app.ml`) into the
  model-executor/session registration path so the TUI can push a
  `Moderator_wakeup`-style internal event whenever async work lands.
- Add targeted tests around the notifier plumbing so that a completed
  background model job both enqueues the internal event and wakes the UI-side
  session controller.
- Add explicit teardown/unregister logic so callback tables or notifier
  registrations do not leak across session shutdown, restart, or tests.

**How to refactor:**

- Treat this as a generic wakeup hook for session-level async work, not just a
  one-off hack for model jobs. The current producer is `Model_executor`, but
  the shape should be reusable for future scheduler/process async capabilities.
- Keep the wakeup payload small. The TUI should use the wakeup only as a
  signal to drain/check the moderator queue, not as the carrier of the whole
  async result.
- Prefer idempotent or overwrite-safe registration semantics so that resume or
  repeated setup does not leave duplicate callbacks for the same session id.

**Constraints:**

- Do not let background workers mutate `Model.t` or call reducer helpers
  directly.
- If multiple wakeups coalesce, correctness matters more than exact wakeup
  count. It is acceptable for the TUI to observe one wakeup for several queued
  moderator events as long as all queued work is later drained.
- Preserve behavior when no moderator is configured or no notifier is
  registered.
- If the session is already halted or the UI is tearing down, wakeups should be
  harmless no-ops rather than raising or mutating freed state.

**Acceptance checks:**

- A completed background model job can wake an idle session controller.
- The new notifier path is inert for sessions without a registered TUI
  listener.
- Tests demonstrate that background completion no longer relies on “some later
  turn happens” to make queued internal events visible.
- Tests cover both registration and cleanup behavior.

## 3) Extract shared moderator outcome interpreter for session control

**Objective:** Centralize the interpretation of moderator runtime requests and
other session-visible moderation outcomes so that startup, idle drains, and
active-turn callbacks use one consistent code path.

**Edits to make:**

- Introduce a small helper module under `lib/chat_tui/`, for example
  `moderator_session_controller.ml` and `.mli`, or another name of similar
  scope.
- Move the logic that interprets moderator runtime requests out of ad hoc local
  reducer helpers and into this helper module.
- The shared interpreter should accept enough context to:
  - queue compaction
  - mark the session halted / add a system notice
  - record that an idle or deferred follow-up turn is requested
  - indicate whether visible messages should be refreshed
  - indicate whether another reducer/internal event should be enqueued
- Define the interpreter output shape explicitly, for example with fields such
  as:
  - `request_refresh`
  - `request_compact`
  - `request_turn`
  - `halt_reason`
  - `system_notices`
  - `internal_events_to_enqueue`
- Reuse this interpreter in:
  - startup/resume moderation handling in `app.ml`
  - streaming-time runtime-request handling in `app_reducer.ml`
  - idle/background drain handling added in later tasks

**How to refactor:**

- Make the interpreter return a small structured result instead of performing
  every side effect itself. This keeps the reducer as the final owner of UI
  mutations while avoiding duplicated branching logic.
- Collapse runtime requests before interpreting them if the existing
  `Runtime_semantics` helpers already encode desired precedence.
- Write down precedence rules in code or interface comments. At minimum make it
  obvious how `End_session`, `Request_turn`, and compaction requests interact.

**Constraints:**

- Do not duplicate `Runtime.request_turn` semantics in two unrelated places
  with slightly different behavior.
- Preserve current compaction/end-session behavior for the active-turn path.
- Keep helper functions short and explicit; avoid building a giant “god
  module.”

**Acceptance checks:**

- There is one obvious code path for interpreting moderator runtime requests in
  chat_tui.
- Startup, active-turn, and future idle-drain code all call into the shared
  interpreter instead of re-encoding the same logic.
- Existing runtime-request behavior remains intact for current tests.
- The helper API makes it straightforward for later tasks to add notifications
  or other session-visible effects without further duplicating reducer logic.

## 4) Implement idle/background moderator drain flow in chat_tui

**Objective:** Give the TUI an idle-time path that drains queued moderator
internal events, applies resulting session effects, refreshes the visible
transcript when needed, and schedules further work without requiring an active
turn.

**Edits to make:**

- Add reducer handling for the new moderator wakeup internal event.
- When the wakeup arrives and no turn is active:
  - call `Moderator_manager.drain_internal_events`
  - feed the resulting outcomes through the shared interpreter from task 3
  - refresh visible messages if overlay-visible state changed
  - schedule any required follow-up turn or compaction work
- When the wakeup arrives during an active turn:
  - set `runtime.moderator_dirty <- true`
  - do not interrupt the active stream
  - leave actual drain work to a later safe point
- Update startup/resume handling if needed so its existing initial drain path
  shares the same helper logic instead of remaining special-cased.
- Ensure the idle path can schedule a follow-up turn when the drained outcomes
  request one and the session is not halted or blocked by another active op.

**How to refactor:**

- Put the actual “drain moderator now” logic into a helper function, not
  directly in the reducer branch. The reducer branch should mostly decide
  whether to defer or execute.
- If `Moderator_manager` needs a small helper to expose “drain if queued” or a
  more ergonomic result shape, add that helper carefully and keep existing
  behavior intact.
- If multiple wakeups arrive before the first drain runs, one drain pass should
  be enough to consume all currently queued moderator events.

**Constraints:**

- Never drain or apply idle moderator work concurrently with an active turn in
  a way that could start a second turn.
- If a wakeup races with streaming completion, preserve serialized reducer
  order and rely on explicit state checks rather than guessing.
- Refresh the projected UI only at safe boundaries; do not reproject on every
  token.

**Acceptance checks:**

- Idle background completions can update moderator state and UI-visible session
  state without requiring another user submit.
- Wakeups arriving during a turn are remembered and not lost.
- The TUI can now surface idle moderator side effects such as end-session or
  queued compaction promptly.
- If an idle wakeup produces `Request_turn`, the controller can represent that
  request without requiring a synthetic submit.

## 5) Decouple turn-start scheduling from user submit flow

**Objective:** Make “start a turn” a first-class session-controller action so
that moderator-requested turns can run while idle, without pretending they
originated from a new user submit.

**Edits to make:**

- Refactor `lib/chat_tui/app_submit.ml`, `app_reducer.ml`, and runtime helpers
  so there are two conceptually separate flows:
  - append-user-message-and-start-turn
  - start-turn-from-current-session-state
- Introduce an internal event or helper path for starting a turn from current
  history with no newly appended user item.
- Update queueing/pending-op logic so moderator-requested turns and compaction
  still serialize with ordinary submits.
- Ensure active-turn `Request_turn` behavior still works as it does today, but
  if the request becomes relevant while idle the session controller can launch
  a new turn safely.
- Make explicit whether idle-started turns carry origin metadata for logging,
  debugging, and future script-visible semantics.

**How to refactor:**

- Keep existing user-submit UX intact for ordinary input.
- Reuse as much of the current streaming-start machinery as possible; the main
  change is separating scheduling from “there must be a new user message”.
- Be explicit about what history snapshot is used when starting an idle turn.
- Be explicit about what happens if both compaction and an idle turn are
  requested at nearly the same time; choose a serialized rule and document it.

**Constraints:**

- Do not synthesize fake user items just to trigger a moderator-requested turn.
- Preserve one-active-turn-at-a-time guarantees.
- Avoid coupling this new path to deferred-note delivery; deferred notes are
  handled in later tasks.

**Acceptance checks:**

- The UI can schedule a turn while idle without going through the standard
  submit path.
- Existing submit behavior remains unchanged for normal user input.
- Moderator-requested continuation can now be represented cleanly in the TUI’s
  own state model.
- Logs and/or tests make it clear whether a started turn came from user submit,
  tool follow-up, or moderator/idle scheduling.

## 6) Formalize deferred user steering notes as queued safe-point inputs

**Objective:** Preserve the product behavior that a user can steer an ongoing
turn without interrupting it, but model that behavior explicitly as queued
deferred steering notes rather than as a reducer-only special case.

**Edits to make:**

- Change the active-stream submit branch in `lib/chat_tui/app_reducer.ml` so it
  enqueues a deferred steering note into runtime state rather than directly
  writing raw text to the existing `system_event` stream.
- Add helper functions for:
  - enqueueing a note
  - draining/consuming queued notes
  - rendering notes into the exact reminder/system text format that
    `in_memory_stream` should splice into a later model-input boundary
- Preserve the existing behavioral intent of the current reminder formatting so
  the model still sees the user steering as a late, non-interrupting reminder
  rather than as a canonical new conversation item.
- Decide where to surface user feedback in the UI when a note is queued. A
  small system notice is acceptable if it improves clarity, but do not create a
  canonical conversation item yet.
- Update `app_streaming.ml` and the `in_memory_stream` interface to receive
  deferred-note access in a structured form rather than depending on reducer
  writes into a side-channel stream.

**How to refactor:**

- Keep the current semantic intent: the note should influence the next safe
  continuation boundary, especially around tool-result handoff, without
  disturbing the currently running model/tool chain.
- If preserving the existing `system_event` transport internally is the least
  disruptive implementation, wrap it behind a clearer deferred-note abstraction
  rather than exposing it as ad hoc reducer behavior.

**Constraints:**

- Do not convert these deferred notes into canonical history items in this
  task.
- Preserve note ordering FIFO if multiple steering notes are queued during one
  long-running turn.
- Avoid losing notes when tool outputs arrive in batches.
- If the turn completes without consuming queued notes, preserve them for the
  next eligible turn-start boundary rather than dropping them silently.

**Acceptance checks:**

- Submitting while streaming still does not interrupt the running turn.
- Deferred notes are now stored in explicit runtime/session-controller state.
- The code clearly documents that these notes are applied only at safe
  boundaries.
- Tests or logs make it easy to verify the exact boundary where queued notes
  were consumed.

## 7) Refactor in_memory_stream safe-point integration for deferred notes and pending moderator work

**Objective:** Make `in_memory_stream` consume deferred steering notes and any
pending moderator work only at explicit safe points, while keeping the current
single-turn orchestration model.

**Edits to make:**

- Replace the raw `system_event` dependency in `lib/chat_response/in_memory_stream.ml`
  with a more intentional safe-point input source, such as a callback that
  drains deferred notes when a safe point is reached.
- Define the safe points clearly in code. At minimum, cover:
  - post-tool-result augmentation before the next model-input boundary
  - the boundary before a follow-up turn caused by tool output or
    `Request_turn`
  - the end-of-turn boundary before the controller decides what to do next
- If `runtime.moderator_dirty` was set while the turn was active, ensure the
  turn engine or post-turn controller path drains the pending moderator queue
  at a safe point before losing that information.
- Keep the existing moderation event order coherent with the new safe-point
  logic so scripts still see sensible `Post_tool_response`, `Item_appended`,
  `Turn_end`, and internal-event sequencing.
- Make it explicit whether safe-point drain order is:
  1. consume pending deferred steering text,
  2. drain queued moderator internal events,
  3. apply runtime-request decisions,
  or another order; whichever order is chosen should be documented and tested.

**How to refactor:**

- Prefer small helper functions for “consume deferred steering text now” and
  “flush pending moderator queue now”.
- Keep the responsibilities explicit: `in_memory_stream` handles safe points
  during a turn; `chat_tui` handles idle drains when no turn is active.
- If a deferred note remains unused because a turn ended without another safe
  boundary, ensure it is still available for the next turn-start boundary.

**Constraints:**

- Never inject deferred steering into the middle of an already-issued model
  request.
- Do not create concurrent turn loops or recursive turn-start paths outside the
  existing turn-budget model.
- Preserve current no-moderator behavior as closely as possible.

**Acceptance checks:**

- Deferred steering notes are consumed only at explicit safe points.
- Pending moderator work discovered during a turn is not lost and is applied at
  the next safe opportunity.
- Active-turn follow-up semantics remain correct for tool-driven continuation
  and `Runtime.request_turn`.
- The chosen safe-point ordering is implementation-faithful and covered by at
  least one regression test.

## 8) Keep UI transcript, notifications, and halted state synchronized with moderator-visible state

**Objective:** Ensure that once moderator-visible state changes, the displayed
conversation, halt notices, and related UI indicators are refreshed at the
right boundaries instead of waiting only for final history replacement.

**Edits to make:**

- Audit and update `lib/chat_tui/app_runtime.ml`, `app_reducer.ml`, and
  `app_stream_apply.ml` so there is a consistent rule for when
  `App_runtime.refresh_messages` must run.
- Refresh after:
  - idle moderator drains that alter overlay-visible transcript state
  - final streaming reconciliation
  - compaction completion
  - any other safe-point outcome that changes effective history or halt state
- Ensure session-ended notices and similar UI-only placeholders are not applied
  twice if both active-turn and idle paths encounter the same logical request.
- If useful, introduce a tiny notification helper abstraction rather than
  appending raw placeholder messages from several branches.
- Review any auto-follow, selection, and tool-output indexing side effects that
  currently happen only on final history replacement so idle transcript refresh
  paths do not leave the model/UI in a partially updated state.

**How to refactor:**

- Keep token-level streaming responsive by continuing to apply raw patches
  during the active stream; the goal is not to fully reproject after every
  token.
- Refresh at safe points where the user would reasonably expect the projected
  transcript to “settle”.
- Rebuild any dependent indexes after a refresh if the current model layer
  requires it.

**Constraints:**

- Avoid quadratic or excessive full-refresh work on every streamed event.
- Do not let stale overlay state linger after an idle moderator drain.
- Preserve the current model/history split: canonical history stays canonical;
  visible messages remain a projection.

**Acceptance checks:**

- Idle overlay changes become visible in the UI without waiting for another
  submit.
- End-session/halt UI state stays consistent across active-turn and idle paths.
- Existing streaming responsiveness is preserved.
- Auto-follow, selection, and related message-model invariants remain valid
  after idle refreshes.

## 9) Add regression coverage for idle async moderation and deferred steering

**Objective:** Lock in the new session-controller behavior with tests that
cover both current guarantees and the newly introduced async/idle flows.

**Edits to make:**

- Add focused tests for:
  - background model-job completion while the UI/session is idle
  - moderator internal-event drain triggered by wakeup
  - moderator-requested idle turn scheduling
  - deferred steering note submission during an active turn
  - note delivery at a safe point rather than mid-request
  - active-turn wakeup setting `moderator_dirty` without starting a second turn
  - UI refresh after idle overlay changes
  - notifier registration cleanup or stale-session no-op behavior
  - queued deferred note survival across a turn boundary when no tool-result
    splice point was reached
- Prefer expect or deterministic unit tests over fragile timing-dependent tests.
- Add helper fakes if needed so tests can simulate model-executor completion and
  reducer wakeups without running a full terminal UI.

**How to refactor:**

- Test the reducer/session-controller logic at the smallest practical layer.
- Add end-to-end integration tests only for the highest-value scenarios where
  lower-level tests would miss a cross-module bug.
- Reuse existing moderation/runtime fixtures where possible.

**Constraints:**

- Keep tests deterministic; do not rely on arbitrary sleeps where a fake wakeup
  or direct internal event can be used.
- Do not regress existing moderation or streaming tests.
- Cover both “moderator configured” and “no moderator” cases where new
  controller paths could accidentally perturb baseline behavior.

**Acceptance checks:**

- The new async/session-controller behavior is exercised by automated tests.
- At least one regression test proves that an idle background completion now
  surfaces without requiring a later user action.
- At least one regression test proves that deferred steering does not interrupt
  an in-flight turn.

## 10) Update architecture docs and script-facing runtime semantics

**Objective:** Bring repository documentation up to date so future agents and
human contributors understand the new async session-controller model and the
safe-point semantics for deferred steering.

**Edits to make:**

- Update `Readme.md` and the most relevant docs under `docs-src/` to describe:
  - the role split between `Moderator_manager`, `in_memory_stream`, and
    `chat_tui`
  - the new idle/background moderator wakeup path
  - the fact that visible transcript state is refreshed at safe points through
    moderator effective history
  - deferred steering notes: what they are, why they exist, and when they are
    applied
- Update `test-scripts.md` if its runtime-semantics sections need to mention
  idle async wakeups, safe points, or idle `Request_turn`.
- If any event naming/docs were previously inconsistent, fix them while
  updating the architecture section.

**How to refactor:**

- Keep documentation implementation-faithful. If the code and old docs disagree,
  document the code that shipped.
- Include at least one end-to-end narrative of an idle async completion causing
  a moderator wakeup and follow-up behavior.
- Include at least one end-to-end narrative of deferred steering during a tool
  run.
- If helpful, add a small sequence diagram or numbered event timeline so the
  safe-point order is easy to audit without reading the code.

**Constraints:**

- Do not promise unsupported async behavior beyond what the implementation now
  guarantees.
- Keep terminology consistent: use “deferred steering note” or another single
  chosen phrase everywhere, not several near-synonyms.

**Acceptance checks:**

- A reader can understand the new architecture without reading the full code.
- The docs explain both idle async wakeups and the non-interrupting steering
  model.
- Documentation and tests describe the same semantics.
