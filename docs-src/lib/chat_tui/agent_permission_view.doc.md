# agent_permission_view

Map pending permission identities and offered choices to the moderator UI. A stale choice must not resolve another request; writable authority is enforced server-side.

See [agent-core embedding](../../agent-server/embedding.md),
[session/history behavior](../../agent-server/sessions-and-workspaces.md),
and [protocol synchronization](../../agent-server/protocol.md).

## Public contract

[Interface](../../../lib/chat_tui/agent_permission_view.mli) · [implementation](../../../lib/chat_tui/agent_permission_view.ml)

The following excerpt is the current callable contract. Eio switches own active
resources; preserve typed errors/cancellation and the host's authorization
boundary rather than bypassing the actor from a presentation adapter.

```ocaml
(** Shared permission presentation used by the terminal controller and trace
    tests. Repeated projections preserve the modal's client-local selection. *)
val choice_label : Agent_protocol.Permission.choice -> string

val sync
  :  Model.t
  -> current:Agent_protocol.Permission.t option
  -> Agent_projection.t
  -> Agent_protocol.Permission.t option
```
