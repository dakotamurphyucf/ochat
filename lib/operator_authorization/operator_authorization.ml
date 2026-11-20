open! Core

type t =
  { principal : Agent_protocol.Principal.t
  ; origin : int
  ; is_current : unit -> bool
  }

let principal t = t.principal
let is_current t = t.is_current ()

(* Process-local identities only discriminate cursor bindings. Each actor still
   checks its own original authentication and any intervening restrictions. *)
let next_origin = Atomic.make 0

let rec fresh_origin () =
  let current = Atomic.get next_origin in
  if current = Int.max_value then failwith "authentication origin space exhausted";
  if Atomic.compare_and_set next_origin current (current + 1)
  then current
  else fresh_origin ()
;;

let origin t = t.origin
let guarded ~principal ~is_current = { principal; origin = fresh_origin (); is_current }

let restrict ?(original_first = false) t ~is_current:allowed =
  let is_current =
    if original_first
    then fun () -> t.is_current () && allowed ()
    else fun () -> allowed () && t.is_current ()
  in
  { t with is_current }
;;

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
