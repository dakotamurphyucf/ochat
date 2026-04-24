# ChatML Budget Policy

This document defines the exact Phase 2 bounded-turn and budget policy that
Task 7 implements.

It is documentation-first: it does not add a new orchestration subsystem or a
script-visible budget API. The policy stays rooted in the existing OCaml
anchors:

- `Chat_response.Runtime_semantics` owns the runtime-facing budget record;
- `Chat_response.In_memory_stream` enforces the stream-level limits that apply
  during one active turn loop;
- `chat_tui` enforces the host-only follow-up, pause, and rate-limit policy
  through `Moderator_session_controller` and `App_runtime`;
- `Chat_response.Model_executor.create ?max_spawned_jobs` continues to own the
  spawned-job cap.

For the surrounding host/session-controller and safe-boundary semantics, see:

- [ChatML host session-controller contract](chatml-host-session-controller-contract.md)
- [ChatML safe-point and effective-history semantics](chatml-safe-point-and-effective-history.md)

## Scope and fixed decisions

Phase 2 fixes the policy surface exactly as follows:

```ocaml
type turn_rate_limit =
  { max_turns : int
  ; window_ms : int
  }

type pause_condition =
  | Pause_followup_turns
  | Pause_internal_event_drains

type budget_policy =
  { max_self_triggered_turns : int
  ; max_followup_turns : int
  ; max_internal_event_drain : int
  ; turn_rate_limit : turn_rate_limit option
  ; pause_conditions : pause_condition list
  }

type policy =
  { honor_request_turn : bool
  ; honor_request_compaction : bool
  ; budget : budget_policy
  }
```

The default values are fixed:

- `max_self_triggered_turns = 10`
- `max_followup_turns = 1`
- `max_internal_event_drain = 100`
- `turn_rate_limit = None`
- `pause_conditions = []`

`max_spawned_jobs` is intentionally not duplicated in
`Chat_response.Runtime_semantics.policy`. It remains configured only through
`Chat_response.Model_executor.create ?max_spawned_jobs`, preserving the current
default of `100`.

## Ownership and enforcement map

| Policy concept | Public OCaml home | Enforcement site | Default | Required semantics |
|---|---|---|---|---|
| `max_self_triggered_turns` | `Chat_response.Runtime_semantics.policy.budget.max_self_triggered_turns` | `Chat_response.In_memory_stream` | `10` | Replaces the current hard-coded consecutive `Request_turn` cap while preserving the existing default behavior. |
| `max_followup_turns` | `Chat_response.Runtime_semantics.policy.budget.max_followup_turns` | `chat_tui` host session controller | `1` | Limits automatic host-started follow-up turns only. |
| `max_internal_event_drain` | `Chat_response.Runtime_semantics.policy.budget.max_internal_event_drain` | `Chat_response.In_memory_stream` and `chat_tui` idle drains | `100` | Applies to every `Moderator_manager.drain_internal_events` call. |
| `turn_rate_limit` | `Chat_response.Runtime_semantics.policy.budget.turn_rate_limit` | `chat_tui` host session controller | `None` | Sliding-window limit for non-user follow-up turns only. |
| `pause_conditions` | `Chat_response.Runtime_semantics.policy.budget.pause_conditions` | `chat_tui` host session controller | `[]` | Host-only suppression of automatic follow-up turns and idle drains. |
| `max_spawned_jobs` | `Chat_response.Model_executor.create ?max_spawned_jobs` | `Chat_response.Model_executor` | `100` | Remains a model-executor ownership point, not a runtime-policy field. |

## Detailed field semantics

### `max_self_triggered_turns`

`max_self_triggered_turns` is the stream-level bound on consecutive
moderator-requested continuation turns inside `Chat_response.In_memory_stream`.

It counts only the path where a turn has already started, the turn reaches
`turn_end`, `Runtime_semantics.decide_after_turn_end` returns
`` `Continue `` because of `Request_turn`, and the stream starts another turn
from the same in-memory driver loop.

It does not count:

- the initial host-started turn;
- host-started follow-up turns whose start reason is
  `App_runtime.Moderator_request` or `App_runtime.Idle_followup`;
- tool-followup passes that continue because a tool call is still in flight;
- forked subcalls that the driver runs recursively for tool-managed work.

This limit replaces the current hard-coded `request_turn_budget_max = 10`
inside `In_memory_stream.run_turn`, but preserves the current default behavior
by keeping the default at `10`.

When the limit is exceeded:

- `In_memory_stream` keeps the current error behavior;
- the failure remains the same class of stream-level error as today;
- only the source of the limit changes, from a hard-coded constant to
  `Runtime_semantics.policy.budget.max_self_triggered_turns`.

Each host-started outer turn begins with a fresh self-triggered-turn counter.

### `max_followup_turns`

`max_followup_turns` is host-only. It is enforced only by the `chat_tui`
session controller when deciding whether to start another turn automatically.

It counts only started turns whose `App_runtime.turn_start_reason` is:

- `Moderator_request`, or
- `Idle_followup`.

It resets when a `User_submit` turn starts.

It does not count:

- the user-submitted turn itself;
- any in-turn `Request_turn` continuation already being handled inside
  `In_memory_stream`;
- tool-followup work that never re-enters the host scheduler.

When a host scheduling attempt would exceed `max_followup_turns`, the host
must:

- suppress scheduling;
- avoid appending fake user items;
- emit a one-time system notice through `App_runtime.add_system_notice_once`
  using the stable key `budget:max-followup-turns`;
- leave canonical history unchanged.

The counter increments only when a follow-up turn is actually started.
Suppressed attempts do not increment it.

### `max_internal_event_drain`

`max_internal_event_drain` applies to every
`Moderator_manager.drain_internal_events` call made by either:

- the turn driver inside `Chat_response.In_memory_stream`, or
- the host idle/session-controller path inside `chat_tui`.

That means the same numeric bound is used at all of the following safe
boundaries:

- turn start;
- post-tool-response;
- turn end;
- idle internal-event drain.

When the limit stops a drain early:

- outcomes produced by the drained prefix remain committed;
- remaining queued internal events stay queued for a later safe boundary;
- no fake user items are appended;
- canonical history remains unchanged except for whatever the drained events
  already legitimately caused;
- the host must leave the session eligible for another wakeup or idle drain.

The host does not emit a dedicated budget notice for this case. The intended
behavior is to yield and continue later, not to surface a policy violation.

### `turn_rate_limit`

`turn_rate_limit` is a host-only sliding-window limit for non-user follow-up
turns.

It applies only to host scheduling attempts for turns whose start reason is
`Moderator_request` or `Idle_followup`. User-submitted turns are never blocked
by this limit and never count toward it.

The policy record is:

```ocaml
type turn_rate_limit =
  { max_turns : int
  ; window_ms : int
  }
```

The host checks it exactly as follows:

1. use `now_ms` at the host scheduling point;
2. maintain timestamps of previously started non-user follow-up turns;
3. count timestamps `>= now_ms - window_ms`;
4. suppress the current scheduling attempt when that count is already
   `>= max_turns`;
5. record a timestamp only when a follow-up turn is actually started.

Suppressed attempts do not add a timestamp.

When `turn_rate_limit` suppresses a scheduling attempt, the host must:

- suppress scheduling;
- avoid appending fake user items;
- emit a one-time system notice through `App_runtime.add_system_notice_once`
  using the stable key `budget:turn-rate-limit`;
- leave canonical history unchanged.

### `pause_conditions`

`pause_conditions` are host-only switches. They are not script-visible and are
not enforced inside `Chat_response.In_memory_stream`.

The exact variants are fixed:

```ocaml
type pause_condition =
  | Pause_followup_turns
  | Pause_internal_event_drains
```

`Pause_followup_turns` means:

- suppress automatic follow-up turn scheduling in `chat_tui`;
- do not run rate-limit or follow-up-count checks after this pause has already
  matched;
- emit a one-time system notice using
  `App_runtime.add_system_notice_once ~key:"budget:pause-followup-turns"`;
- leave canonical history unchanged.

`Pause_internal_event_drains` means:

- suppress automatic idle/internal-event drain calls in `chat_tui`;
- do so before consuming any drain budget;
- emit a one-time system notice using
  `App_runtime.add_system_notice_once ~key:"budget:pause-internal-event-drains"`.

This pause does not suppress turn-driver safe-point drains at turn start,
post-tool-response, or turn end. It is only a host-side idle-drain pause.

### `max_spawned_jobs`

`max_spawned_jobs` remains owned by:

```ocaml
Chat_response.Model_executor.create ?max_spawned_jobs
```

This limit continues to:

- default to `100`;
- live on the model-executor instance rather than the runtime policy;
- be enforced only when spawning new model jobs.

Task 7 may document or test that ownership more clearly, but it must not
duplicate `max_spawned_jobs` as another field on
`Chat_response.Runtime_semantics.policy`.

## Required host bookkeeping

To implement this contract without inventing a second controller stack, the
host session controller needs bookkeeping for:

- the number of actually started non-user follow-up turns since the last
  `User_submit` turn;
- the timestamps of previously started non-user follow-up turns for the
  sliding-window rate limit;
- one-time notice keys routed through `App_runtime.add_system_notice_once`.

This bookkeeping belongs in the existing `chat_tui` host/runtime layer rather
than in `Chat_response.In_memory_stream` or the ChatML script surface.

## Precedence rules

Phase 2 fixes the precedence order exactly.

### Idle/internal-event drains

1. `Pause_internal_event_drains` suppresses the idle drain before any drain
   budget is consumed.
2. Otherwise the host may call `Moderator_session_controller.drain_internal_events`
   and the drain itself is bounded by `max_internal_event_drain`.

If the pause applies, emit only the one notice keyed
`budget:pause-internal-event-drains`.

### Automatic follow-up turn scheduling

When the host is considering an automatic follow-up turn, apply checks in this
order:

1. `Pause_followup_turns`
2. `turn_rate_limit`
3. `max_followup_turns`

`turn_rate_limit` is always checked before `max_followup_turns`.

When multiple suppressors could apply, emit only the first applicable notice
under that precedence order.

If none of the suppressors apply and the host actually starts the turn:

- record the start reason as a non-user follow-up reason;
- increment the follow-up-turn counter;
- record the timestamp for rate limiting.

User-submitted turns bypass these suppressors and also reset the follow-up-turn
counter used by `max_followup_turns`.

## Stable notice keys

The stable one-time notice keys are fixed exactly:

| Trigger | Stable key |
|---|---|
| Follow-up turns paused by host policy | `budget:pause-followup-turns` |
| Idle drains paused by host policy | `budget:pause-internal-event-drains` |
| Follow-up turn suppressed by count limit | `budget:max-followup-turns` |
| Follow-up turn suppressed by sliding-window rate limit | `budget:turn-rate-limit` |

The key is the stable contract. The human-readable notice text may evolve, but
Task 7 must deduplicate repeated notices through these exact keys.

## Non-goals

This Phase 2 contract does not:

- add any script-facing budget builtin modules or functions;
- move host follow-up or idle-drain policy into the ChatML runtime;
- add a compiled `Chatml_session_controller` package;
- change the snapshot schema;
- change canonical-history semantics.

It only specifies where each limit belongs and how each limit behaves so the
later code task can implement the policy directly through the existing anchors.
