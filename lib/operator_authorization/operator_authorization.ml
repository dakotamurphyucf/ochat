open! Core

type t =
  { principal : Agent_protocol.Principal.t
  ; is_current : unit -> bool
  }

let principal t = t.principal
let is_current t = t.is_current ()
let guarded ~principal ~is_current = { principal; is_current }
let trusted_local principal = guarded ~principal ~is_current:(fun () -> true)
let nonexpiring_static principal = guarded ~principal ~is_current:(fun () -> true)

let bounded ~principal ~now ~expires_at =
  let is_current () = Agent_protocol.Timestamp.compare (now ()) expires_at < 0 in
  if is_current ()
  then Ok (guarded ~principal ~is_current)
  else
    Error
      (Agent_protocol.Error.create
         Unauthenticated
         ~message:"original authentication expired"
         ~retryable:false
         ())
;;
