open! Core

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

let create phase = { phase; changed_at = Agent_protocol.Timestamp.now () }
let connected () = create Connected
let reconnecting ~attempt = create (Reconnecting { attempt })
let disconnected () = create Disconnected
let failed error = create (Failed error)
