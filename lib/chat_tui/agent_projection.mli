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
