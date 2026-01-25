# Chat TUI App Refactor: Two queues + single reducer + explicit runtime state

This document describes the implementation tasks referenced by `chat_tui_app_refactor_tasks.yml`.

Context:
- Target file: `lib/chat_tui/app.ml`
- Standard library: Core
- Concurrency: Eio 1.2
- Goal: Improve maintainability and responsiveness while preserving the app's stable behavior and edge-case handling.

## Mental model / glossary (read this first)

This refactor follows a “many producers → one reducer” architecture.

- **Reducer**: The single fiber that:
  - reads events from `input_stream` and `internal_stream`,
  - is the **only** code allowed to mutate `Model.t`,
  - owns `Runtime.t` (busy state, queue, quit flag),
  - requests redraws (via `Redraw_throttle.request_redraw`) and performs draws when it receives `\`Redraw`.

- **Producer**: Any background fiber spawned by the reducer (streaming, compaction, future ops). Producers:
  - must not mutate `Model.t`,
  - must communicate only by emitting `internal_event`s onto `internal_stream`,
  - should be attached to an Eio `Switch` so cancel/ESC can stop them.

- **input_stream**: `Eio.Stream.t` carrying Notty key events (`Notty.Unescape.event`). Large capacity so key repeats/pastes do not block Notty input handling.

- **internal_stream**: `Eio.Stream.t` carrying internal events from producers and the redraw throttler. Smaller capacity is OK because streaming is batched and redraw is level-triggered.

- **Operation / op**: The currently-running long-running activity. In this refactor:
  - `Streaming` (LLM request in flight)
  - `Compacting` (history compaction in flight)
  Exactly one may be active at a time (`Runtime.op : op option`), and it has a `Switch.t` for cancellation.

- **Queued actions**: Work requested while an op is active. Stored FIFO in `Runtime.pending`:
  - queued submits while *Compacting* (captured `{text; draft_mode}` at submit-time)
  - queued compactions
  The reducer starts the next action when the current op ends.

- **op_id**: A monotonically increasing integer attached to an op and to its op-scoped events. The reducer ignores stale events whose `op_id` does not match the active op. This prevents “late arriving” events from a cancelled/finished op from corrupting the model.

- **Level-triggered redraw**: Call `Redraw_throttle.request_redraw` whenever state changes. The throttler emits at most one `\`Redraw` per tick (FPS). Avoid pushing redraw events directly from random code paths.

## Non-goals (to reduce regression risk)

During this refactor, do **not** intentionally change:
- Prompt parsing, tool declaration parsing, or tool implementations.
- Rendering layout / UI appearance (other than incidental timing differences from better scheduling).
- OpenAI streaming semantics, including:
  - `parallel_tool_calls` behavior,
  - tool call output association/ordering,
  - incremental display of tool outputs and syntax highlighting.
- Error-handling behavior and edge cases (network failure, mid-stream failure, cancellation).
- Session persistence/export behavior (other than moving code without semantic change).

Global non-negotiable invariants:
1. **Single-writer**: only the reducer loop mutates `Model.t` and runtime state.
2. **Two streams**: Notty input must not be backpressured by internal event volume.
3. **No cancel-on-submit**: only Cancel/ESC can cancel the active op.
4. **Submit semantics while busy (intentional behavior):**
   - If the active op is **Streaming**: submitting enqueues a *system note* (it does **not** enqueue a submit request).
   - If the active op is **Compacting**: submitting enqueues a submit request FIFO to run after compaction.
5. **Preserve robustness**: do not weaken streaming error recovery; move it without changing semantics.
6. **Preserve tool output UX during streaming:** tool call outputs must remain incrementally visible and syntax-highlighted when they arrive (not only after final history replacement).

Important clarification:
- The Notty callback (`Notty_eio.Term.run ~on_event`) may run on a different fiber and must **never** touch the model.
- The controller (`Controller.handle_key`) is invoked by the reducer when it processes an input event; therefore controller-driven model edits are still within the single-writer rule.

Operational notes for Eio:
- Use `Eio.Stream.take_nonblocking` to drain input quickly without blocking.
- When needing to wait for either `input_stream` or `internal_stream`, use `Eio.Fiber.n_any` (not `Fiber.first/any`) to avoid losing an event if both become ready around the same time.
  - Pattern:
    - Use `Fiber.n_any [ (fun () -> \`Input (Stream.take input_stream)); (fun () -> \`Internal (Stream.take internal_stream)) ]`
    - Partition returned list into inputs+internals, then apply inputs first.
    - Do NOT put `handle_*` work inside the raced closures; only perform the `Stream.take` in them.

---

## 1) Add Runtime + event types scaffolding

**Objective:** Introduce types that make the new architecture explicit, while keeping the program behavior unchanged (or minimally changed but still compiling).

**Edits to make:**
- In `lib/chat_tui/app.ml`, add:
  - `type input_event = Notty.Unescape.event`
  - `type internal_event = ...` (include variants needed by later tasks; it is OK if unused initially)
  - `module Runtime` defining:
    - `type op = Streaming of {sw: Switch.t} | Compacting of {sw: Switch.t}`
    - `type submit_request = { text : string; draft_mode : Model.draft_mode }`
    - `type queued_action = Submit of submit_request | Compact`
    - `type t = { model : Model.t; mutable op : op option; pending : queued_action Queue.t; quit_via_esc : bool ref }`
    - constructor `create ~model`

**Constraints:**
- Do not change control flow yet; just introduce types and compile.
- Keep types located near the top of `app.ml` for easy discovery by future maintainers.
- Prefer record payloads over tuples in event variants (easier to extend later without changing call sites).

**Acceptance checks:**
- `dune build` succeeds.
- After completing this extraction task, re-run the Task 10 manual scenarios (especially tool-output highlighting and parallel tool calls) to catch regressions introduced by module splitting.
- No behavior changes yet.

---

## 2) Introduce two streams in run_chat and route Notty events

**Objective:** Split input events from internal events at ingestion time, while keeping a single reducer for model updates (later tasks).

**Edits to make:**
- In `run_chat` in `app.ml`:
  - Replace the single `ev_stream` with:
    - `input_stream : input_event Eio.Stream.t` (capacity e.g. `4096`)
    - `internal_stream : internal_event Eio.Stream.t` (capacity e.g. `1024`)
- In the `Notty_eio.Term.run ~on_event` callback:
  - Route key events (`#Notty.Unescape.event`) to `input_stream`.
  - Route `\`Resize` to `internal_stream` (as `\`Resize` internal event).
  - Route `\`Redraw` from Notty to `internal_stream` as a *force redraw* internal event (`\`Force_redraw`), distinct from throttled redraw ticks.

**Constraints:**
- Keep the rest of the code working; you may temporarily forward events to the old loop if needed, but prefer adapting the loop quickly.
- The Notty `on_event` callback must do only cheap work (`Stream.add` and simple pattern matching). It must not:
  - touch `model`,
  - do IO,
  - allocate large structures, or
  - block on other synchronization.
- Rationale for capacities (guidance, not a hard requirement):
  - `input_stream` should be large so paste/key repeats don't block the terminal input loop.
  - `internal_stream` can be smaller because stream events are batched and redraw is level-triggered.

**Acceptance checks:**
- `dune build` succeeds.
- Manually start the app and confirm keypresses still work (even if internals are not fully migrated yet).

---

## 3) Wire level-triggered redraw through internal stream

**Objective:** Make redraw scheduling level-triggered and ensure redraw events do not spam or compete with input.

**Edits to make:**
- Keep using `Redraw_throttle` as the debouncer/level-trigger.
- Change `Ui.init_throttler ~enqueue_redraw` to enqueue `\`Redraw` into `internal_stream` (not into input stream, not into a mixed stream).
- Ensure code paths that previously did `Stream.add ... \`Redraw` directly now call `Redraw_throttle.request_redraw throttler` instead.
- Keep `\`Force_redraw` (e.g., resize or Notty redraw) to call `redraw_immediate`.

**Constraints:**
- Do not make redraw happen in multiple places.
- Rendering must read the latest `model` state.
- There should be exactly one place that calls `Redraw_throttle.on_redraw_handled` (the reducer when it processes `\`Redraw`).
- If you need an immediate draw (resize), call `redraw_immediate ()` rather than enqueueing `\`Redraw`.

**Acceptance checks:**
- `dune build` succeeds.
- Manual: during streaming, UI still updates and does not freeze.

---

## 4) Make Controller submit/compact produce request events (no background work)

**Objective:** Controller/key handling should be fast and never launch long-running work directly.

**Edits to make:**
- Introduce internal request events:
  - `\`Submit_requested of Runtime.submit_request` (capture `text` and `draft_mode` at submit time)
  - `\`Compact_requested`
- On submit keypress:
  - Capture `text = Model.input_line model` and `draft_mode = Model.draft_mode model`.
  - Clear editor immediately (`Model.set_input_line model ""`, `Model.set_cursor_pos model 0`).
  - Enqueue `\`Submit_requested {text; draft_mode}` onto `internal_stream`.
  - Request redraw (do not enqueue redraw).
- On compact keypress:
  - Enqueue `\`Compact_requested` onto `internal_stream`.

**Constraints:**
- Do not start streaming or compaction in controller code.
- Do not cancel anything in controller code; cancellation is only via ESC/cancel path.
- Do not apply "submit start effects" here (add user message to history, placeholder thinking, scroll-to-bottom).
  Those must happen only when the queued submit actually starts (via `maybe_start_next_pending`).
- Clarified behavior (reducer responsibility):
  - If currently **Streaming**, `Submit_requested` becomes a *system note* (do not enqueue a submit request).
  - If currently **Compacting**, `Submit_requested` enqueues a submit request FIFO to run after compaction.
  - System note mechanism (preserve existing semantics):
    - Build the note exactly as today: `sprintf "This is a Note From the User:\n%s" (String.strip text)`
    - Send it to `system_event` using `Eio.Stream.add system_event note`
    - Do not start streaming, do not enqueue a submit request, and do not add a user message to history.

**Acceptance checks:**
- `dune build` succeeds.
- Manual: submit clears editor immediately.

---

## 5) Implement single reducer loop consuming input+internal streams with input priority

**Objective:** One loop owns all `Model.t` mutations and is the only consumer of both streams.

**How to refactor:**
- Update `Loop.run` to accept:
  - `runtime : Runtime.t`
  - `input_stream : input_event Stream.t`
  - `internal_stream : internal_event Stream.t`
- Implement scheduling:
  1. Drain up to N input events using `Eio.Stream.take_nonblocking input_stream` (N ~ 64).
  2. If an internal event is available via `take_nonblocking internal_stream`, handle one.
  3. If both empty, block using `Eio.Fiber.n_any` on:
     - `Stream.take input_stream`
     - `Stream.take internal_stream`
     Then process all returned results, handling input before internal (partition the list).

**Constraints:**
- All model changes must occur inside reducer handlers.
- Do not use `Fiber.first` for the blocking wait; use `Fiber.n_any`.
- Keep the reducer loop tail-recursive (or use a `while true do ... done` style) to avoid growing the stack.
- Cap the number of input events drained per iteration (e.g. 64) to prevent starvation of internal events.
- Be careful not to allocate a new fiber per event; the reducer should remain a single long-lived fiber.

**Acceptance checks:**
- `dune build` succeeds.
- Manual: keypress responsiveness is acceptable during heavy streaming.

---

## 6) Move compaction execution into reducer (producer-only compaction fiber + FIFO queue)

**Objective:** Compaction should be started/queued by reducer, run in a background fiber that emits events only, and apply model updates only in reducer.

**Edits to make:**
- Add a reducer helper `maybe_start_next_pending`:
  - If `runtime.op = None`, dequeue FIFO from `runtime.pending` and start it.
  - If busy, do nothing.
- On `\`Compact_requested`:
  - If idle: enqueue `Compact` and call `maybe_start_next_pending`.
  - If busy: enqueue `Compact` only.
- Starting compaction:
  - Immediately add placeholder "(compacting…)" message in reducer (or when starting).
  - Snapshot history at start (use `let history_snapshot = Model.history_items model`).
  - Snapshot any other model-derived inputs needed by the worker. Treat `model` as reducer-owned; avoid reading it directly inside the worker fiber.
  - Fork a fiber which:
    - runs in `Switch.run` and emits `\`Compaction_started sw`
    - performs the compaction on the snapshot
    - emits `\`Compaction_done history'` or `\`Compaction_error exn`
- Reducer handles `\`Compaction_done` by:
  - setting `runtime.op <- None`
  - applying model replacement: history/messages/tool index rebuild, clear selection, reset auto-follow
  - request redraw
  - call `maybe_start_next_pending`

**Constraints:**
- The compaction fiber must not mutate `model`.
- Cancel/ESC should cancel the active compaction via switch failure.
- Decide where session persistence happens:
  - Preferred: snapshot the session data in the reducer (history/tasks/kv_store) and have the worker persist that snapshot.
  - Acceptable (minimal change): worker calls persistence functions, but it must not mutate model.

**Acceptance checks:**
- `dune build` succeeds.
- Manual: press compact while idle works; press compact during streaming queues and runs afterward.
- Manual: while compacting, press submit; submit request is queued and starts after compaction completes (FIFO).

---

## 7) Refactor Streaming_submit to producer-only (emit internal events; no model mutation)

**Objective:** The streaming fiber must not mutate the model; reducer owns all model updates and busy state.

**Edits to make:**
- Change `Streaming_submit.handle_submit` signature to not accept `model` and not mutate it.
  - Pass `history : Res_item.t list` snapshot when starting streaming.
  - Pass `internal_stream` for emitting events.
- On start, emit `\`Streaming_started sw`.
- During streaming, emit:
  - `\`Stream ev` / `\`Stream_batch evs`
  - `\`Tool_output item`
- On completion emit `\`Streaming_done items` (items is the final history returned by driver).
- On exception emit `\`Streaming_error exn`.

**Constraints:**
- No `Model.set_*`, no `Model.apply_*` in streaming fiber.
- Keep existing stream batching behavior (OCHAT_STREAM_BATCH_MS), but only as a producer to internal events.
- Avoid reading the mutable model from the streaming fiber; pass any needed values as arguments:
  - history snapshot
  - cfg/tools/tool_tbl/system_event/datadir/parallel_tool_calls/etc.
- Make sure the batching daemon fiber is attached to the streaming switch so cancellation stops it.

**Acceptance checks:**
- `dune build` succeeds.
- Manual: streaming still renders incrementally (events are applied in reducer).

---

## 8) Implement reducer handlers for Streaming_* and preserve streaming error recovery

**Objective:** Move existing robust error recovery logic into reducer, preserving semantics.

**Edits to make:**
- Reducer handles:
  - `\`Streaming_started sw` → `runtime.op <- Some (Streaming {sw})`
  - `\`Stream` / `\`Stream_batch` / `\`Tool_output`:
    - apply patches via `Stream_apply.*`
    - request redraw via `Redraw_throttle.request_redraw throttler`
    - only apply these when `runtime.op` indicates streaming (ignore otherwise)
  - `\`Streaming_done items`:
    - `runtime.op <- None`
    - `Stream_apply.replace_history model redraw_immediate items`
    - `maybe_start_next_pending ()`
  - `\`Streaming_error exn`:
    - `runtime.op <- None`
    - run existing rollback + prune + synthetic tool output logic (copy from old streaming error handler)
    - add placeholder error message
    - request redraw
    - `maybe_start_next_pending ()`

**Constraints:**
- Preserve existing rollback behavior (fork state reset) and pruning semantics.
- Do not simplify away the synthetic tool output generation; keep edge-case UX stable.
- Ensure that `Streaming_error` leaves the model in a consistent "idle" state suitable for starting queued actions.
- Preserve tool-output incremental UX:
  - When a `Tool_output` internal event arrives, the reducer must apply it immediately in the same way as today (do not defer until final history replacement).
  - If syntax highlighting depends on an index that is currently only rebuilt in `replace_history`, ensure that the incremental path updates that index too (either incrementally or by calling `Model.rebuild_tool_output_index` when tool outputs arrive).
  - Current implementation detail (preserve this behavior):
    - The renderer decides specialised tool-output rendering via `Model.tool_output_by_index : (int, tool_output_kind) Hashtbl.t`.
    - Incremental tool-output classification is updated by `Model.apply_patch (Set_function_output {id; output})`, which:
      - overwrites the tool output buffer text,
      - sets `tool_output_by_index` for the corresponding message index,
      - and invalidates the render cache for that message so the next redraw re-renders with correct highlighting.
    - The patches that produce `Set_function_output` come from `lib/chat_tui/stream.ml` (`handle_tool_out` / `handle_fn_out`), so:
      - do not change that patch flow,
      - and ensure reducer still applies those patches as tool outputs arrive.
    - `stream.ml` also maintains `function_name_by_id` and `tool_path_by_call_id` as function call events/arguments arrive; this feeds into tool-output classification (e.g., `read_file` path-aware rendering). Do not break these updates.

**Acceptance checks:**
- `dune build` succeeds.
- Manual: simulate failure (e.g., invalid API key / network down) and verify the UI shows error and transcript remains consistent.
- Manual: trigger a tool call mid-stream and confirm the tool output is syntax-highlighted as soon as it appears (not only after the stream finishes and history is replaced).
  - If debugging is needed: verify that `Model.tool_output_by_index` gets populated for the tool-output message index as soon as the tool output arrives (via `Set_function_output` patch application).

---

## 9) Replace Model.fetch_sw busy-state usage with Runtime.op everywhere (cancel + gating)

**Objective:** Stop using `Model.fetch_sw` as the busy flag; the reducer/runtime state should be authoritative.

**Edits to make:**
- Remove or bypass `Model.fetch_sw` checks in:
  - submit handling
  - compact handling
  - stream apply gating
  - cancel/quit logic
- Implement cancel/ESC based on `runtime.op`:
  - If `Some op`, cancel by failing `op.sw` with the existing cancellation exception.
  - If `None`, set `runtime.quit_via_esc := true`.

**Constraints:**
- Submit never cancels active op; only cancel path cancels.
- Keep quit/export behavior unchanged except for using the new quit flag.
- After cancelling the current op, the reducer should eventually observe `*_error Cancelled` (or equivalent) and clear `runtime.op`, then start queued work.

**Acceptance checks:**
- `dune build` succeeds.
- Manual:
  - ESC/cancel during streaming cancels it and returns to idle state.
  - Any queued actions (compactions and submits queued during compaction) still run after cancellation completes.

---

## 10) Verification + cleanup pass (remove unused event variants, ensure no direct model mutations in fibers)

**Objective:** Ensure the refactor is complete, consistent, and ready for future enhancements.

**Edits to make:**
- Search for any remaining background fiber code mutating the model; move mutations into reducer.
- Ensure:
  - no direct `Stream.add \`Redraw` from random code paths (redraw should be requested via `request_redraw`).
  - all internal events are handled or intentionally ignored with a comment.
- If `Model.fetch_sw` is now unused, either:
  - remove uses (preferred), or
  - leave minimal compatibility but ensure it is not relied on for correctness.
- Re-check compile warnings and remove dead code introduced by the refactor (unused helpers, unused event variants, unused aliases).

**Acceptance checks:**
- `dune build` succeeds.
- Manual scenarios (run through each and confirm UI/model consistency):
  - Normal submit → streaming tokens render → done.
  - Submit while streaming → a system note is enqueued/recorded → streaming continues (no queued submit request).
  - Compact while streaming → compaction is queued → runs after streaming ends.
  - Cancel streaming (ESC/cancel) → streaming stops → app returns to idle → queued actions then run FIFO.
  - Resize terminal during streaming → no crash; redraw happens and UI remains responsive.
  - Regression check (parallel tool calls): trigger multiple parallel tool calls and confirm each tool output is correctly highlighted as it arrives and remains associated with the correct tool call.

**Optional regression-test hook (recommended if the repo has tests):**
- Add a small test that applies a `Set_function_output {id=call_id; output=...}` patch to a model and asserts:
  - `Model.tool_output_by_index` is populated for the correct message index,
  - and the message cache is invalidated for that index (so the next redraw re-renders with tool-output styling).
- This guards against the “tool output not highlighted until final Replace_history” regression.

---

## 11) Add op_id (operation id) to guard against stale internal events

**Objective:** Prevent stale internal events (especially streaming events already enqueued when a cancel happens) from being applied after the corresponding operation is no longer active.

This is important because this TUI will have many background operations over time, and event delivery can race with cancellation.

**Edits to make:**
- Add a monotonically increasing operation id in `Runtime.t`, e.g.:
  - `mutable next_op_id : int`
  - and/or store `id : int` inside `Runtime.op` variants.
- When starting any op (streaming/compaction), allocate a fresh id and store it in `runtime.op`.
- Recommended representation (example):
  - `type op = Streaming of { sw : Switch.t; id : int } | Compacting of { sw : Switch.t; id : int }`
  - `let current_op_id runtime = Option.map runtime.op ~f:(function Streaming {id; _} | Compacting {id; _} -> id)`
- Update op-scoped internal events to carry `op_id`, e.g.:
  - `Stream of { op_id : int; ev : Res_stream.t }`
  - `Stream_batch of { op_id : int; evs : Res_stream.t list }`
  - `Tool_output of { op_id : int; item : Res_item.t }`
  - `Streaming_done of { op_id : int; items : Res_item.t list }`
  - `Streaming_error of { op_id : int; exn : exn }`
  - similarly for compaction done/error if desired.
- Ensure producer fibers include the correct `op_id` with each emitted event.
- In reducer handlers:
  - Ignore events whose `op_id` doesn’t match the current active op id.
  - Keep `*_started` as the moment the op becomes active.
  - Be explicit in code about ignored events (a short comment is enough); do not raise.

**Constraints:**
- No additional cancellations beyond existing behavior.
- FIFO queueing remains FIFO; op_id only prevents stale event application.

**Acceptance checks:**
- `dune build` succeeds.
- Manual: cancel during streaming; ensure no further tokens/tool outputs appear after cancel completes.

---

## 12) Extract Runtime + event types into dedicated files

**Objective:** Make the codebase easier to digest by moving small, stable types out of `app.ml` into focused modules.

**Files to create:**
- `lib/chat_tui/app_runtime.ml` (and optional `app_runtime.mli`)
- `lib/chat_tui/app_events.ml` (and optional `app_events.mli`)

**What to move:**
- From `app.ml` → `app_events.ml`:
  - `type input_event`
  - `type internal_event`
  - any payload record types used by internal events (recommended).
- From `app.ml` → `app_runtime.ml`:
  - runtime types (`op`, `submit_request`, `queued_action`, `t`, constructor).

**How to refactor:**
- Keep names predictable. Example:
  - `module App_events : sig ... end`
  - `module App_runtime : sig ... end`
- Update `app.ml` to reference these modules explicitly (avoid excessive `open`).
- Watch for accidental cyclic dependencies:
  - `app_events` and `app_runtime` should be "leaf" modules (types only).
  - They should not depend on `App_reducer`/`App_streaming`/etc.
- If you add `.mli` files:
  - Expose only what `app.ml`/`App_reducer` needs.
  - Keep `Runtime.t` mutable fields private unless there is a reason to expose them.

**Constraints:**
- No behavior changes.
- Keep exported surfaces minimal; prefer `.mli` files if it helps enforce boundaries.
- Check whether the library stanza uses explicit `(modules ...)` in `lib/chat_tui/dune`. If so, add the new modules there (this repo's dune file is at `lib/chat_tui/dune`).

**Acceptance checks:**
- `dune build` succeeds.

---

## 13) Extract reducer loop + handlers into App_reducer module

**Objective:** Make the event loop easy to digest by moving the reducer (single-writer core) into its own module with small functions.

**File to create:**
- `lib/chat_tui/app_reducer.ml` (and optional `.mli`)

**What to move:**
- The core reducer loop currently in `Loop.run`, including:
  - input draining logic
  - blocking wait (`Fiber.n_any`) and partitioning
  - internal handler dispatch
  - `maybe_start_next_pending`
  - cancel/quit handling

**Recommended shape:**
- `App_reducer.run` should accept the same dependencies as `Loop.run` did (env, switches, streams, throttler, callbacks) and return the quit flag.
- Split into small helpers:
  - `drain_input_burst`
  - `wait_for_events`
  - `handle_input_event`
  - `handle_internal_event`
- Suggested signature sketch (adjust as needed for your codebase):
  - `val run : env:Eio.Stdenv.t -> ui_sw:Eio.Switch.t -> cwd:_ Eio.Path.t -> cache:Cache.t -> datadir:_ Eio.Path.t -> session:Session.t option -> term:Notty_eio.Term.t -> runtime:App_runtime.t -> input_stream:App_events.input_event Eio.Stream.t -> internal_stream:App_events.internal_event Eio.Stream.t -> system_event:string Eio.Stream.t -> throttler:Redraw_throttle.t -> redraw_immediate:(unit -> unit) -> redraw:(unit -> unit) -> prompt_ctx:prompt_context -> parallel_tool_calls:bool -> cancelled:exn -> unit -> bool`
  - Keep this signature stable; it becomes the backbone for future enhancements.

**Constraints:**
- Reducer remains the only model mutator.
- Preserve scheduling policy (bounded input burst + `n_any` fallback).
- Keep the reducer's public API narrow; avoid leaking internal helper types unless needed.

**Acceptance checks:**
- `dune build` succeeds.
- Manual: run through Task 10 scenarios quickly to confirm nothing regressed.

---

## 14) Extract submit/queueing + compaction starters into small modules

**Objective:** Isolate “start work” codepaths (submit and compaction) into small modules so future enhancements don’t bloat the reducer.

**Files to create:**
- `lib/chat_tui/app_submit.ml`
- `lib/chat_tui/app_compaction.ml`

**What to move:**
- Submit:
  - `submit_request` capture helpers (if not already in `App_runtime`)
  - “clear editor immediately on submit” helper
  - “apply submit start effects” helper (adds user msg, placeholder thinking, scroll-to-bottom, etc.)
- Compaction:
  - “start compaction” helper that snapshots history and forks a producer-only worker fiber emitting internal events.

**Implementation notes (to avoid subtle bugs):**
- Keep the semantics "submit start effects run only when the submit actually starts":
  - When user presses Enter while **Streaming**: reducer records a system note; do NOT enqueue a submit request and do NOT add user message to history.
  - When user presses Enter while **Compacting**: reducer enqueues a submit request; do NOT add user message to history yet.
  - When reducer later dequeues and starts the submit: that is when you add user message + placeholder thinking + scroll.
- The submit-start function will likely need access to:
  - `term` (to compute viewport height for scroll-to-bottom)
  - `cwd/env/cache` (for Raw XML conversion path)
  - `model` (mutation happens in reducer context)
- Prefer splitting submit into small functions:
  - `capture_request ~model : submit_request`
  - `clear_editor ~model : unit`
  - `apply_start_effects ~env ~cwd ~cache ~term ~model ~request : unit`

**Constraints:**
- Submit while **Streaming** records a system note and never cancels.
- Submit while **Compacting** queues FIFO and never cancels.
- Compaction while busy queues FIFO and never cancels.
- Worker fibers must not mutate the model.
- Prefer a pattern where the worker's only output is `internal_event`s (no callbacks touching model).

**Acceptance checks:**
- `dune build` succeeds.
- Manual: submit and compaction still work; queueing behavior unchanged.
- After completing this extraction task, re-run the Task 10 manual scenarios to catch regressions introduced by module splitting.

---

## 15) Extract streaming producer + stream-apply logic into small modules

**Objective:** Make streaming code digestible by separating (a) the producer fiber and (b) application of stream events/patches to the model.

**Files to create:**
- `lib/chat_tui/app_streaming.ml` (producer-only)
- `lib/chat_tui/app_stream_apply.ml` (apply-only; can wrap existing `Stream_apply`)

**What to move:**
- Producer (from current `Streaming_submit`):
  - batching window logic (`OCHAT_STREAM_BATCH_MS`)
  - conversion of OpenAI callbacks into internal events
  - emit `*_started/*_done/*_error` with `op_id`
- Apply logic (from current `Stream_apply`):
  - patch coalescing
  - apply stream batches
  - history append on output item done

**Implementation notes:**
- Producer module (`App_streaming`) should have a single entry point like:
  - `val start : env:Eio.Stdenv.t -> datadir:_ Eio.Path.t -> prompt_ctx:prompt_context -> system_event:string Eio.Stream.t -> internal_stream:App_events.internal_event Eio.Stream.t -> parallel_tool_calls:bool -> history_compaction:bool -> op_id:int -> history:Res_item.t list -> unit`
  - It should allocate and manage the streaming `Switch.run`, and emit:
    - `Streaming_started {op_id; sw}`
    - `Stream_batch {op_id; ...}` / `Tool_output {op_id; ...}`
    - `Streaming_done {op_id; items}` or `Streaming_error {op_id; exn}`
- Apply module (`App_stream_apply`) should stay "pure-ish":
  - `val apply_stream_event : model:Model.t -> throttler:Redraw_throttle.t -> ev:Res_stream.t -> unit`
  - `val apply_stream_batch : model:Model.t -> throttler:Redraw_throttle.t -> evs:Res_stream.t list -> unit`
  - Keep coalescing and "append history item on Output_item_done" close to these functions.
- Reducer must gate by:
  - active op is Streaming
  - op_id matches

**Constraints:**
- Producer fibers must not mutate model.
- Reducer must gate on `op_id` and the currently active op.
- Keep stream patch coalescing close to where patches are applied, so performance tweaks remain localized.
- Preserve tool output highlighting behavior:
  - Tool outputs must be visible and syntax-highlighted incrementally during streaming, not only after `Streaming_done` replaces history.
  - Preserve `parallel_tool_calls` behavior and ensure tool outputs are associated with the correct call (no cross-call mixing).
  - Do not “fix” highlighting by calling `Model.rebuild_tool_output_index` on every event:
    - The intended incremental mechanism is `Set_function_output` patches updating `tool_output_by_index` for the specific message index.
    - `rebuild_tool_output_index` is appropriate when replacing the entire history (startup/compaction/Streaming_done), not for per-event updates.

**Acceptance checks:**
- `dune build` succeeds.
- Manual: streaming renders incrementally; cancel stops; queued actions (if any) start afterward FIFO.
- Manual: trigger multiple parallel tool calls; ensure each tool output is correctly highlighted as it arrives and ends up associated with the correct tool call in the transcript.
- After completing this extraction task, re-run the Task 10 manual scenarios to catch regressions introduced by module splitting.

---

## 16) Cleanup: remove/feature-flag dead meta_refine branch

**Objective:** Remove confusing dead code so the refactored codebase is easier to digest and safer to modify.

Current issue in `Submit_local_effects.apply_local_submit_effects`:
- The condition `if String.is_empty orig_msg || true then ...` makes the meta-refine branch unreachable.

**Edits to make (choose one approach):**
1) **Remove meta-refine entirely (simplest):**
   - Delete the unreachable branch and any related diff placeholder logic if it is not used elsewhere.
   - Keep behavior equivalent to the current runtime behavior (i.e., no meta-refine by default).

2) **Properly feature-flag meta-refine (recommended if you want it later):**
   - Replace `|| true` with a real gate, e.g. an environment variable:
     - `OCHAT_META_REFINE=1` enables, otherwise disabled.
   - Default must be **disabled** to preserve current behavior.
   - Keep the existing “fallback to original prompt on exception” behavior.

**Constraints:**
- Do not change submit/streaming semantics other than removing dead code.
- Default runtime behavior must remain the same as before this cleanup task.

**Acceptance checks:**
- `dune build` succeeds.
- Manual: normal submit path still works; no visible behavior change with default config.

