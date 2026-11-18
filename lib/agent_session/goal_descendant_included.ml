open! Core

type t = Goal_descendant_pending.t [@@deriving equal, sexp_of]

let create pending = pending
let pending t = t
let shape = Goal_descendant_pending.shape
let to_json = Goal_descendant_pending.to_json
let of_json raw = Result.map (Goal_descendant_pending.of_json raw) ~f:create
