# agent_session_client

Connected TUI client lifecycle, attachment, commands and reconnection. The daemon owns work; client close detaches and does not stop detached agents.

See [agent-core embedding](../../agent-server/embedding.md),
[session/history behavior](../../agent-server/sessions-and-workspaces.md),
and [protocol synchronization](../../agent-server/protocol.md).

## Public contract

[Interface](../../../lib/chat_tui/agent_session_client.mli) · [implementation](../../../lib/chat_tui/agent_session_client.ml)

The following excerpt is the current callable contract. Eio switches own active
resources; preserve typed errors/cancellation and the host's authorization
boundary rather than bypassing the actor from a presentation adapter.

```ocaml
open! Core

(** Attached daemon-session client for the TUI controller. *)

type update =
  | Projection of Agent_projection.t
  | Connection_changed of Connection_status.t
  | Connection_failed of Agent_protocol.Error.t

type t

type create_options =
  { prompt : string
  ; workspace : string
  ; liveness : Agent_protocol.Session.liveness
  ; permission_profile : string option
  ; display_name : string option
  ; labels : (string * string) list
  ; mode : Agent_protocol.Session.attachment_mode
  }

val attach
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> connection:Agent_client.Connection.t
  -> ?reconnect:
       (unit -> (Agent_client.Connection.t, Agent_protocol.Error.t) result) option
  -> session_id:Agent_protocol.Id.Session.t
  -> mode:Agent_protocol.Session.attachment_mode
  -> unit
  -> (t, Agent_protocol.Error.t) result

val create
  :  sw:Eio.Switch.t
  -> clock:_ Eio.Time.clock
  -> connection:Agent_client.Connection.t
  -> ?reconnect:
       (unit -> (Agent_client.Connection.t, Agent_protocol.Error.t) result) option
  -> create_options
  -> (t, Agent_protocol.Error.t) result

val projection : t -> Agent_projection.t
val status : t -> Connection_status.t
val next_update : t -> update
val take_update_nonblocking : t -> update option

val send_text
  :  t
  -> string
  -> (Agent_protocol.Method_result.Send_message.t, Agent_protocol.Error.t) result

val send_content
  :  t
  -> Agent_protocol.Session.Message_content.t
  -> (Agent_protocol.Method_result.Send_message.t, Agent_protocol.Error.t) result

val compact : t -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

(** Requests authoritative deletion at the client's current revision. Does not
    optimistically mutate the local projection. *)
val delete_history
  :  t
  -> History_entry.Id.t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val cancel_active_operation
  :  t
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val start
  :  t
  -> queue_if_limited:bool
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val stop
  :  t
  -> mode:Agent_protocol.Session.stop_mode
  -> (Agent_protocol.Session.t, Agent_protocol.Error.t) result

val respond_permission
  :  t
  -> permission_id:Agent_protocol.Id.Permission.t
  -> permission_generation:int
  -> choice:Agent_protocol.Permission.choice
  -> reason:string option
  -> (Agent_protocol.Permission.t, Agent_protocol.Error.t) result

val revoke_grant
  :  t
  -> grant_id:Agent_protocol.Id.Grant.t
  -> reason:string
  -> (Agent_protocol.Grant.t, Agent_protocol.Error.t) result

val read_audit
  :  t
  -> limit:int
  -> (Agent_protocol.Audit.t Agent_protocol.Page.t, Agent_protocol.Error.t) result

val detach : t -> (unit, Agent_protocol.Error.t) result
val close : t -> unit

(** [attachment t] exposes current attachment identity/mode, not credentials. *)
val attachment : t -> Agent_protocol.Session.Attachment.t option
```
