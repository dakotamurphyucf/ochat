open! Core

module Durable_local_host = struct
  type t = unit ref

  let create () = ref ()
end

module Local_recovery_host = struct
  type t =
    | Transient
    | Durable of Durable_local_host.t
end

type t =
  { principal : Agent_protocol.Principal.t
  ; origin : int
  ; durable_local : (Durable_local_host.t * Agent_protocol.Id.Server.t) option
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

let guarded ~principal ~is_current =
  { principal; origin = fresh_origin (); durable_local = None; is_current }
;;

let durable_local ~host ~server_id ~principal ~is_current =
  { (guarded ~principal ~is_current) with durable_local = Some (host, server_id) }
;;

let is_durable_local t = Option.is_some t.durable_local

let current_durable_local t ~host ~server_id =
  is_current t
  && Option.exists t.durable_local ~f:(fun (actual, server) ->
    phys_equal actual host && Agent_protocol.Id.Server.equal server server_id)
;;

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
