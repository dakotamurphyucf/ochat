# agent_event_apply

Apply agent events to TUI state with stable identities. Durable terminal state clears activity even when a transient finish notification was lost.

See [agent-core embedding](../../agent-server/embedding.md),
[session/history behavior](../../agent-server/sessions-and-workspaces.md),
and [protocol synchronization](../../agent-server/protocol.md).

## Public contract

[Interface](../../../lib/chat_tui/agent_event_apply.mli) · [implementation](../../../lib/chat_tui/agent_event_apply.ml)

The following excerpt is the current callable contract. Eio switches own active
resources; preserve typed errors/cancellation and the host's authorization
boundary rather than bypassing the actor from a presentation adapter.

```ocaml
(** Render authoritative typed client state. Draft/activity sequence admission,
    gaps and terminal fencing belong to Agent_client, never a TUI event counter.
    Public views cannot populate writable canonical context. Actual tool results
    remain distinct from an ended operation whose tool outcome was not observed. *)
type t

val create : unit -> t

val apply
  :  t
  -> model:Model.t
  -> viewport_height:int
  -> Agent_projection.t
  -> (Model.projection_damage, Agent_protocol.Error.t) result
```
