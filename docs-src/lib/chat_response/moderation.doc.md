# `Chat_response.Moderation`

Shared host-side moderation helpers for ChatML-enabled chat drivers.

## Overview

`Moderation` is the bridge between two data models:

1. the generic ChatML moderator runtime in
   `Chatml_moderator_runtime`, and
2. the request/history structures used by `chat_response`
   (`Openai.Responses.Item.t`, tool descriptors, streaming tool outputs,
   and host-managed synthetic messages).

The module is intentionally **frontend-agnostic**.  It does not know
about Notty, reducer state, or any TUI-specific patches.  Its job is to
provide a reusable vocabulary that both `chat_tui` and non-UI drivers can
share.

## What it defines

### 1. Lifecycle phases

The v1 phase vocabulary is fixed:

- `session_start`
- `session_resume`
- `turn_start`
- `message_appended`
- `pre_tool_call`
- `post_tool_response`
- `turn_end`
- `internal_event`

`Moderation.Phase` keeps those names in one place so the host and the
script runtime do not drift apart.

### 2. Projection into ChatML `context`

ChatML moderator scripts consume a simplified structural `context`
record.  `Moderation.Projection` turns canonical
`Openai.Responses.Item.t list` history into:

- projected `message` records,
- projected tool descriptors,
- a `context` value ready for `Chatml_moderator_runtime.handle_event`.

The projection layer also assigns **stable host message ids** to history
items that do not already carry one.  The snapshot type is explicit so a
later task can persist it alongside the rest of moderator state.

### 3. Durable overlay snapshot

ChatML `Turn.*` operations should not mutate canonical history directly.
Instead, `Moderation.Overlay` defines:

- transactional overlay operations such as `Prepend_system`,
  `Append_message`, `Replace_message`, and `Delete_message`,
- a materialized overlay snapshot that can be stored separately from
  canonical model history,
- an `apply` helper that computes the effective moderated message view.

### 4. Structured local effect decoding

The runtime buffers committed local effects as generic ChatML
operations.  `Chatml_moderator_runtime.decode_local_effects` converts
them into structured runtime effects, and `Moderation.Outcome` groups
them into:

- overlay operations,
- at most one tool moderation decision,
- runtime requests such as compaction or end-session,
- emitted internal events.

This design keeps local effects transactional: the host interprets them
**after** a successful `handle_event`, instead of mutating driver state
from runtime callbacks.

### 5. Host capability registry

`Moderation.Capabilities` defines the host-provided callbacks for:

- diagnostic logging,
- synchronous and spawned tool calls,
- named model recipes,
- scheduling hooks.

`runtime_handlers` converts that registry into the
`Chatml_moderator_runtime.default_handlers` bundle expected by the
runtime.  The local transactional handlers intentionally stay at their
runtime defaults because committed effects are decoded separately.

## Typical flow

1. Project canonical history into `Moderation.Context.t`.
2. Convert the lifecycle event into `Moderation.Event.t`.
3. Call `Chatml_moderator_runtime.handle_event`.
4. Read committed local effects and decode them into
   `Moderation.Outcome.t`.
5. Update the durable overlay snapshot and drain queued internal events.

## Related modules

- [`driver.doc.md`](driver.doc.md) – high-level orchestration entrypoints
- [`response_loop.doc.md`](response_loop.doc.md) – recursive model/tool loop
- [`tool.doc.md`](tool.doc.md) – tool declaration loading
