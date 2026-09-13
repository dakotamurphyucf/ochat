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
open! Core

(** Applies server projections to the existing mutable TUI presentation
    model. Durable history is replaced by identity; recoverable provider
    events remain presentation-only. Sourced events own live text; their paired
    history-correlated notifications must not append the same delta twice.
    Committed IDs suppress stale live rows, while tool progress is applied once
    per operation sequence even when a durable revision rebuilds Chat rows.
    Reconciliation considers both revision and durable sequence because one
    commit may contain several visible changes. Durable terminal observations
    close Agent-page calls even when transient finish events were coalesced away;
    starting another operation clears the previous operation's transient calls.
    Work metadata is replaced on every projection, including scope/generation
    changes. The returned damage still describes Chat history layout only. *)

type t

val create : unit -> t

val apply
  :  t
  -> model:Model.t
  -> viewport_height:int
  -> Agent_projection.t
  -> (Model.projection_damage, Agent_protocol.Error.t) result
```
