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
(** TUI read projection. Public entries remain immutable read views and never
    become canonical History_entry values or Model.history_items. *)
type t

val of_client_projection : Agent_client.Projection.t -> t
val snapshot : t -> Agent_protocol.Public.Snapshot.t
val fields : t -> Agent_protocol.Public.Snapshot.Fields.t
val canonical_history : t -> Agent_protocol.Public.History.t list
val visible_history : t -> Agent_protocol.Public.History.t list
val rows : t -> Projected_message.t list
val messages : t -> Types.message list
val live : t -> Agent_client.Live_projection.t
val synchronization : t -> Agent_client.Projection.synchronization
val terminal_operation : t -> Agent_protocol.Operation.t option
```
