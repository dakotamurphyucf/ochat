open Core

type state =
  | Pending
  | Ready
  | Failed
  | Closed

type t = state Atomic.t

let create () = Atomic.make Pending

let check t =
  match Atomic.get t with
  | Ready -> Ok ()
  | Pending -> Error "shell registry activation is pending"
  | Failed -> Error "shell registry activation failed"
  | Closed -> Error "shell registry owner has closed"
;;

let ready t = if Atomic.compare_and_set t Pending Ready then Ok () else check t
let fail t = ignore (Atomic.compare_and_set t Pending Failed : bool)
let close t = Atomic.set t Closed
