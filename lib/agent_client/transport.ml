type t =
  { request :
      Agent_protocol.Command.t
      -> (Agent_protocol.Method_result.t, Agent_protocol.Error.t) result
  ; next_notification : unit -> Agent_protocol.Envelope.t option
  ; close : unit -> unit
  }

let create ~request ~next_notification ~close = { request; next_notification; close }
let request t command = t.request command
let next_notification t = t.next_notification ()
let close t = t.close ()
