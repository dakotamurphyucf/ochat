open! Core
module P = Agent_protocol
module A = Goal_native_admission

type t =
  { get : admission:A.t -> goal_id:P.Id.Goal.t option -> (Jsonaf.t, P.Error.t) result
  ; create_goal :
      admission:A.t -> request:P.Goal_native_create_request.t -> (unit, P.Error.t) result
  ; control :
      admission:A.t -> request:P.Goal_native_control_request.t -> (unit, P.Error.t) result
  ; edit : admission:A.t -> request:P.Goal_edit.t -> (unit, P.Error.t) result
  ; start :
      admission:A.t -> request:P.Goal_native_start_request.t -> (unit, P.Error.t) result
  }

let create ~get ~create_goal ~control ~edit ~start =
  { get; create_goal; control; edit; start }
;;

let get t = t.get
let create_goal t = t.create_goal
let control t = t.control
let edit t = t.edit
let start t = t.start
