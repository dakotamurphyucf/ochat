# connection_status

Explicit connected/reconnecting/disconnected client presentation. Transport status and agent operation activity are independent.

See [agent-core embedding](../../agent-server/embedding.md),
[session/history behavior](../../agent-server/sessions-and-workspaces.md),
and [protocol synchronization](../../agent-server/protocol.md).

## Public contract

[Interface](../../../lib/chat_tui/connection_status.mli) · [implementation](../../../lib/chat_tui/connection_status.ml)

The following excerpt is the current callable contract. Eio switches own active
resources; preserve typed errors/cancellation and the host's authorization
boundary rather than bypassing the actor from a presentation adapter.

```ocaml
open! Core

(** Client-local daemon connection state. This state is presentation-only and
    is never persisted into an agent session. *)

type phase =
  | Connected
  | Reconnecting of { attempt : int }
  | Disconnected
  | Failed of Agent_protocol.Error.t
[@@deriving sexp]

type t =
  { phase : phase
  ; changed_at : Agent_protocol.Timestamp.t
  }
[@@deriving sexp]

val connected : unit -> t
val reconnecting : attempt:int -> t
val disconnected : unit -> t
val failed : Agent_protocol.Error.t -> t
```
