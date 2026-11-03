open! Core
module P = Agent_protocol

type t =
  { original : Operator_authorization.t
  ; actor : Operator_authorization.t
  }

let denied () =
  P.Error.create
    Permission_denied
    ~message:"original agent execution authority is no longer current"
    ~retryable:false
    ()
;;

let create ~actor:original ~is_current =
  let actor =
    Operator_authorization.guarded
      ~principal:(Operator_authorization.principal original)
      ~is_current:(fun () -> Operator_authorization.is_current original && is_current ())
  in
  { original; actor }
;;

let actor t =
  if Operator_authorization.is_current t.actor then Ok t.actor else Error (denied ())
;;

let transfer t ~is_current =
  Result.map (actor t) ~f:(fun _ -> create ~actor:t.original ~is_current)
;;
