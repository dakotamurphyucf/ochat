open! Core

type t =
  { snapshot : Agent_protocol.Public.Snapshot.t
  ; canonical_history : Agent_protocol.Public.History.t list
  ; visible_history : Agent_protocol.Public.History.t list
  ; rows : Projected_message.t list
  ; live : Agent_client.Live_projection.t
  ; synchronization : Agent_client.Projection.synchronization
  ; terminal_operation : Agent_protocol.Operation.t option
  }

let of_client_projection projection =
  let snapshot = Agent_client.Projection.snapshot projection in
  let fields = Agent_protocol.Public.Snapshot.fields snapshot in
  let canonical_history = fields.canonical_history.entries in
  let visible_history =
    Option.value_map fields.effective_history ~default:canonical_history ~f:(fun window ->
      window.entries)
  in
  { snapshot
  ; canonical_history
  ; visible_history
  ; rows = Conversation.project_public_entries visible_history |> Conversation.rows
  ; live = Agent_client.Projection.live projection
  ; synchronization = Agent_client.Projection.synchronization projection
  ; terminal_operation = Agent_client.Projection.terminal_operation projection
  }
;;

let snapshot t = t.snapshot
let fields t = Agent_protocol.Public.Snapshot.fields t.snapshot
let canonical_history t = t.canonical_history
let visible_history t = t.visible_history
let rows t = t.rows
let messages t = List.map t.rows ~f:(fun row -> row.Projected_message.message)
let live t = t.live
let synchronization t = t.synchronization
let terminal_operation t = t.terminal_operation
