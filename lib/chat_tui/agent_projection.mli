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
