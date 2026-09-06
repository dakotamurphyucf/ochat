# agent_projection

Client projection of scoped snapshots and ordered durable/recoverable events. Snapshot replacement preserves local drafts separately and restores bounded active calls without replaying executable work.

See [agent-core embedding](../../agent-server/embedding.md),
[session/history behavior](../../agent-server/sessions-and-workspaces.md),
and [protocol synchronization](../../agent-server/protocol.md).

## Public contract

[Interface](../../../lib/chat_tui/agent_projection.mli) · [implementation](../../../lib/chat_tui/agent_projection.ml)

The following excerpt is the current callable contract. Eio switches own active
resources; preserve typed errors/cancellation and the host's authorization
boundary rather than bypassing the actor from a presentation adapter.

```ocaml
open! Core

(** TUI-ready view of the rendering-neutral agent-client projection. *)

type t

val of_client_projection : Agent_client.Projection.t -> (t, Agent_protocol.Error.t) result
val snapshot : t -> Agent_protocol.Snapshot.t
val canonical_history : t -> History_entry.t list
val visible_history : t -> History_entry.t list
val messages : t -> Types.message list
val live_events : t -> Agent_protocol.Event.Recoverable.t list

(** Durable terminal observation retained by the attached client, if any. *)
val terminal_operation : t -> Agent_protocol.Operation.t option
```
